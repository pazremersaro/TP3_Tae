#           Probando el primer grafico            #

library(shiny)
library(readr)
library(tidyverse)

# CARGA Y MANEJO DE DATOS
library(readr)
library(dplyr)
library(tidyr)

# MAPAS
# install.packages("sf")
library(sf) 
library(leaflet)
library(spData)
# install.packages("rnaturalearth")
library(rnaturalearth) 
library(rnaturalearthdata)
library(countrycode)
library(htmltools)

# PARA OBJETIVO 2
library(lubridate)

# PARA OBJETIVO 3 (GRÁFICOS INTERACTIVOS)
library(plotly)

# PARA OBJETIVO 4
library(ggplot2)

# OBJETIVO 5
library(scales)
# library(kableExtra)
library(DT)

# OBJETIVO 7
library(ggridges)
library(stringr)
library(forcats)
library(RColorBrewer)
library(shinyWidgets)


# CARGA Y MANEJO DE DATOS
datos <- read_delim("cookies.txt", delim = "\t",
                    escape_double = FALSE, trim_ws = TRUE)

cookies <- datos |>
  mutate(
    
    # Para poder hacer los continentes
    pais_original = pais,
    
    # Pasando los países a español
    pais = case_when(
      pais == "(not set)" ~ "Sin especificar",
      T ~ countrycode(
        sourcevar = pais,
        origin = "country.name.en",
        destination = "cldr.name.es")),
    
    # Continentes para el mapa
    # Continentes en inglés porque es lo que tiene r
    continente_ingles = countrycode(
      sourcevar = pais_original,
      origin = "country.name.en",
      destination = "un.region.name"
    ),
    
    # Pasando las cosas a español:
    # Paises
    iso_a3 = countrycode(
      sourcevar = pais,
      origin = "country.name",
      destination = "iso3c"
    ),
    # Dispositivos
    dispositivo = case_when(
      dispositivo == "desktop" ~ "Computadora",
      dispositivo == "mobile" ~ "Celular",
      dispositivo == "tablet" ~ "Tablet"
    ),
    # Continentes
    continente = case_when(
      continente_ingles == "Americas" ~ "América",
      continente_ingles == "Europe" ~ "Europa",
      continente_ingles == "Africa" ~ "África",
      continente_ingles == "Oceania" ~ "Oceanía",
      continente_ingles == "Asia" ~ "Asia",
      # Kosovo y Taiwan tiraban warnings así que los cambiamos
      # a mano
      pais == "Kosovo" ~ "Europa",
      pais == "Taiwán" ~ "Asia",
      T ~ "Sin especificar"
    )
  ) |> 
  # Eliminamos estos países y continentes que están en inglés
  select(-pais_original, -continente_ingles) |> 
  rename(Navegador = browser)


#---------------------------------------------------#
#    Limpieza de datos para interrogante 1 (mapa)   #  
#---------------------------------------------------#
nc_mapa <- cookies |> 
  group_by(ID) |> 
  summarise(compro = any(!is.na(gasto)),
            paises_dif = n_distinct(pais),
            not_set = any(pais == "Sin especificar"),
            # De todas formas debería ser el mismo, pero consideramos
            # el primero para que no se repita 
            pais = first(pais),
            
            # Agregamos iso_a3 para que lo tenga en cuenta más adelante y 
            # se pueda graficar bien
            iso_a3 = first(iso_a3),
            continente = first(continente),
            sesiones = n(),
            .groups = "drop") |> 
  filter((compro == F) & (paises_dif == 1 & not_set == F)) 


# Total de usuarios de cada país (incluye compradores Y no compradores)
total_pais <- cookies |> 
  filter(pais != "Sin especificar", pais != "Anguila") |> 
  group_by(ID) |> 
  summarise(
    paises_dif = n_distinct(pais),
    pais = first(pais),
    .groups = "drop"
  ) |> 
  filter(paises_dif == 1) |>  
  group_by(pais) |> 
  summarise(usuarios_totales = n(), .groups = "drop")



# Para las sesiones:
# NAVEGADOR
Navegador_pais <- cookies |> 
  # Agrupamos por navegador y país
  group_by(pais, Navegador) |> 
  # Contamos cuantas sesiones por país por dispositivo hay
  summarise(
    cantidad = n(),
    # Contamos cuántos países diferentes hay
    paises_dif = n_distinct(pais),
    .groups = "drop") |> 
  filter(paises_dif == 1) |> 
  # Nos quedamos con los países agrupados
  group_by(pais) |> 
  # Cantidad de sesiones por dispositivo por país 
  mutate(
    pct = round(cantidad / sum(cantidad) * 100, 1)
  ) |> 
  # Acomodamos de dispositivo con mayor uso por país a menor cantidad de uso
  arrange(pais, desc(cantidad)) |> 
  # Nos queremos quedar sólo con la primera 
  # (osea que nos quedamos con el navegador principal de cada país)
  slice(1) |> 
  select(pais,
         Navegador_top = Navegador, 
         pct_Navegador = pct)


# DISPOSITIVO
dispositivo_pais <- cookies |> 
  # Agrupamos por dispositivo y país
  group_by(pais, dispositivo) |> 
  # Contamos cuantas sesiones por país por dispositivo hay
  summarise(
    cantidad = n(),
    paises_dif = n_distinct(pais),
    .groups = "drop"
  ) |> 
  filter(paises_dif == 1) |> 
  group_by(pais) |> 
  # Cantidad de sesiones por dispositivo por país 
  mutate(
    pct = round(cantidad / sum(cantidad) * 100, 1)
  ) |> 
  # Acomodamos de dispositivo con mayor uso por país a menor cantidad de uso
  arrange(pais, desc(cantidad)) |> 
  # Nos queremos quedar sólo con la primera
  slice(1) |> 
  select(pais,
         dispositivo_top = dispositivo, 
         pct_dispositivo = pct)


# UNIMOS LOS DATASETS
para_mapa <- nc_mapa |> 
  group_by(pais) |> 
  summarise(
    n_nc = n(),
    # Agregamos esto para que funcione bien el mapa
    iso_a3 = first(iso_a3),
    continente = first(continente),
    .groups = "drop"
  ) |> 
  left_join(total_pais, by = "pais") |> 
  mutate(pct_nc = round(n_nc / usuarios_totales * 100, 2)) |> 
  left_join(Navegador_pais, by = "pais")|> 
  left_join(dispositivo_pais, by = "pais") 



poligonos <- ne_countries(scale = "medium", # Supuestamente para que tenga un nivel de detalle intermedio
                          # Decimos que devuelva un tipo de archivo sf
                          # (que es lo que se usa para los mapas)
                          returnclass = "sf") |> 
  mutate(
    pais = countrycode(name,
                       "country.name.en",
                       # Pedimos que convierta de acuerdo a un tipo de nombres
                       # en este caso supuestamente es en español
                       destination = "cldr.name.es")
  ) |> 
  # Agregamos codigo iso para unir después
  select(pais, 
         geometry, 
         name)


para_mapa_final <- para_mapa |> 
  # Seleccionamos las columnas que queremos mantener
  select(iso_a3, 
         pais,
         continente,
         n_nc,
         usuarios_totales,
         pct_nc,
         Navegador_top,
         pct_Navegador,
         dispositivo_top,
         pct_dispositivo)

mapa_datos <- poligonos |> 
  left_join(para_mapa_final,
            by = "pais") |> 
  filter(!is.na(n_nc)) 

#--------------------------------------------------------#
#    Fin limpieza de datos para interrogante 1 (mapa)    #  
#--------------------------------------------------------#



#--------------------------------------------------------#
#    Limpieza de datos para interrogante 5 (compradores) #  
#--------------------------------------------------------# 
compradores <- cookies |>
  group_by(ID) |>
  filter(
    any(!is.na(gasto)),           
    n_distinct(pais) == 1
  ) |>
  summarise(
    pais_label = first(pais),
    n_compras = sum(!is.na(gasto)),
    dispositivo = names(which.max(table(dispositivo))),
    Navegador = names(which.max(table(Navegador))),
    .groups = "drop"
  )

#--------------------------------------------------------#
# Fin limpieza de datos para interrogante 5 (compradores)#  
#--------------------------------------------------------#



#--------------------------------------------------------#
#    Limpieza de datos para interrogante 2 (tendencia)   #  
#--------------------------------------------------------# 
datos_objetivo2 <- cookies |>
  mutate(
    fecha = ymd(fecha),
    dia = factor(
      str_to_title(weekdays(fecha)),
      levels = c("Lunes", "Martes", "Miércoles", "Jueves",
                 "Viernes", "Sábado", "Domingo")),
    mes = factor(
      str_to_title(format(fecha, "%B")),
      levels = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio"))
  ) |>
  group_by(mes, dia) |>
  summarise(compras = sum(gasto > 0, na.rm = TRUE))

#--------------------------------------------------------#
# Fin limpieza de datos para interrogante 2 (tendencia)  #  
#--------------------------------------------------------#



#---------------------------------------------------------#
#    Limpieza de datos para interrogante 6 (dispersión)   #  
#---------------------------------------------------------# 
datos_objetivo6 <- cookies |>
  dplyr::filter(!is.na(gasto), gasto > 0) |> 
  dplyr::select(paginas, gasto)

#--------------------------------------------------------#
# Fin limpieza de datos para interrogante 6 (dispersión) #  
#--------------------------------------------------------#



#-----------------------------------------------------------#
#    Limpieza de datos para interrogante 7 (distribución)   #  
#-----------------------------------------------------------# 
compras <- cookies |>
  dplyr::filter(!is.na(gasto), gasto > 0)

# Estadisticas para el cartel
dispositivo_stats <- compras |>
  dplyr::group_by(dispositivo) |>
  dplyr::summarise(
    Media   = mean(gasto),
    Mediana = median(gasto),
    Min     = min(gasto),
    Max     = max(gasto),
    .groups = "drop"
  ) |>
  # Creamos una columna de texto para HTML
  dplyr::mutate(
    columna_text = paste0(
      "<b>Dispositivo: </b>", dispositivo, "<br>",
      "<b>Media: </b>",   scales::dollar(round(Media,   0)), "<br>",
      "<b>Mediana: </b>", scales::dollar(round(Mediana, 0)), "<br>",
      "<b>Mínimo: </b>",  scales::dollar(round(Min,     0)), "<br>",
      "<b>Máximo: </b>",  scales::dollar(round(Max,     0))
    )
  ) |>
  # Seleccionamos sólo la columna de texto y la variable de agrupación
  dplyr::select(dispositivo, columna_text)

compras_con_col_texto <- compras |>
  dplyr::left_join(dispositivo_stats, by = "dispositivo")

#----------------------------------------------------------#
# Fin limpieza de datos para interrogante 7 (distribución) #  
#----------------------------------------------------------#



#-------------------------------------------------------#
#    Limpieza de datos para interrogante 3 (burbujas)   #  
#-------------------------------------------------------# 
# Armamos un dataset que tenga los tiempos en minutos según navegador y dispositivo
datos_objetivo3 <- cookies |> 
  dplyr::mutate(
    # Pasamos los que tienen tiempo na a 0 y todo a minutos
    tiempo = ifelse (is.na(tiempo), 0, tiempo) / 60
  ) |> 
  # Agrupamos por Navegador y dispositivo:
  dplyr::group_by(Navegador, dispositivo) |> 
  dplyr::summarise(
    tiempo_medio = mean(tiempo),
    clicks_medio = mean(clicks),
    # Para el tamaño de los círculos
    frecuencia = n(), # SESIONES
    .groups = "drop"
  ) 


# Colores según los nvegadores
colores_Navegador <- c( 
  "Android" = "grey27",
  "Chrome" = "#FFA700",
  "Edge" = "#61C250",
  "Firefox" = "#E66000",
  "Internet Explorer" = "#00D4F9",
  "Opera" = "#FF0000",
  "Safari" = "#006CFF",
  "Otros" = "purple2"
)

#--------------------------------------------------------#
# Fin limpieza de datos para interrogante 3 (burbujas)   #  
#--------------------------------------------------------#



#---------------------------------------------------------#
#    Limpieza de datos para interrogante 4 (histograma)   #  
#---------------------------------------------------------# 
visitas_compra <- cookies |>
  dplyr::arrange(ID, ymd(fecha)) |> 
  dplyr::group_by(ID) |> 
  dplyr::mutate(compra =  gasto > 0, 
                visita_compra = ifelse(any(compra),          
                                       which(compra)[1], NA)) |> 
  dplyr::filter(!is.na(visita_compra)) |>
  dplyr::add_count(ID, dispositivo, name = "veces_dispositivo") |>
  dplyr::summarise(                  
    visitas_hasta_compra = dplyr::first(visita_compra),
    
    dispositivo_principal = dispositivo[which.max(veces_dispositivo)]
  ) |> 
  dplyr::mutate(
    categoria_visitas = cut(
      visitas_hasta_compra,
      breaks = c(0, 1, 4, 9, Inf),
      labels = c("1", "2–4", "5–9", "10 o más")
    )
  )

# Totales por categoría de visita (cuantos clientes hay en cada categoria)
totales_categoria <- visitas_compra |>
  dplyr::group_by(categoria_visitas) |>
  dplyr::summarise(total = n(),
                   .groups = 'drop') 

# Porcentajes por dispositivo dentro de cada categoría
porcentajes_dispositivo <- visitas_compra |>          
  # Agrupamos por categoria de visitas y dispositivo
  dplyr::group_by(categoria_visitas, dispositivo_principal) |>
  # Contamos cuantos clientes hay en cada combinacion
  dplyr::summarise(n = n(), 
                   .groups = 'drop') |>        
  # Agrupamos solo por categoria de visita
  dplyr::group_by(categoria_visitas) |>         
  dplyr::mutate(porcentaje = round(n / sum(n) * 100, 1)) |>  
  # Porcentaje de cada dispositivo dentro de su categoria
  dplyr::select(categoria_visitas,
                dispositivo_principal, 
                porcentaje)

# Formato ancho para tener una columna por dispositivo
porcentajes_ancho <- porcentajes_dispositivo |>
  tidyr::pivot_wider(
    names_from = dispositivo_principal,
    values_from = porcentaje,
    values_fill = 0
  )

# Unimos los totales con los porcentajes
datos_grafico <- totales_categoria |>
  dplyr::left_join(porcentajes_ancho,
                   by = "categoria_visitas")

# Hacemos el tooltip para que aparezca la etiqueta que cuando apoyas el mouse
datos_grafico <- datos_grafico |>
  dplyr::mutate(
    tooltip = paste0(
      "<b>Visitas hasta compra: ", categoria_visitas, "</b><br>",
      "Total clientes: ", total, "<br><br>",
      "<b>Por dispositivo:</b><br>",
      "️ Computadora: ", ifelse(is.na(Computadora), 0, Computadora), "%<br>",
      " Teléfono: ", ifelse(is.na(Celular), 0, Celular), "%<br>",
      " Tablet: ", ifelse(is.na(Tablet), 0, Tablet), "%"
    )
  )

#--------------------------------------------------------#
# Fin limpieza de datos para interrogante 4 (histograma) #  
#--------------------------------------------------------#




# PARA QUE APAREZCAN DISTINTOS SIDEBAR DE ACUERDO
# CON DISTINTAS PÁGINAS GUARDAMOS LOS SIDEBAR 
# AFUERA DE LAS FUNCIONES COMO UN OBJETO

# Guardamos los continentes para las opciones del sidebar:
continentes1 <- unique(cookies$continente) 
continentes <- continentes1[continentes1 != "Sin especificar"]

# SIDEBAR mapa:
sidebar_g1_mapa <- bslib::sidebar(
  title = "Mapa",
  # selectInput para poder escribir y elegir 
  # la palabra (en este caso el continente)
  shinyWidgets::pickerInput(
    inputId = "continente_mapa",
    label = "Continente/s",
    choices = sort(continentes),
    # Para que tenga todos seleccionados automáticamente ponemos
    # continentes (que es el vector que contiene los nombres)
    selected = continentes,
    multiple = T,
    options = list(
      # Para que se pueda buscar en el momento
      "live-search" = T,
      # Para seleccionar y deseleccionar todo:
      "actions-box" = T
    )
  )
)


# SIDEBAR tabla:
sidebar_g1_tabla <- bslib::sidebar(
  title = "Tabla",
  shinyWidgets::pickerInput(
    inputId = "paises_tabla",
    label = "País/es",
    choices = sort(unique(compradores$pais_label)),
    selected = "Argentina",
    multiple = T,
    options = list(
      "live-search" = T,
      "actions-box" = T
    )
  )
)


# SIDEBAR gráfico de tendencias:
sidebar_g2_tendencia <- bslib::sidebar(
  title = "Tendencia por Mes", 
  shinyWidgets::pickerInput(
    inputId = "mes_tendencia", 
    label = "Mes", 
    choices = unique(datos_objetivo2$mes),
    multiple = TRUE,
    selected = unique(datos_objetivo2$mes)[1],
  )
)


###############################################################


# ARMAMOS LA INTERFAZ

MiInterfaz <- bslib::page_navbar(
  title = tags$span("Google Analytics", style = "color: #40E0D0; font-weight: bold;"),
  bslib::nav_panel(
    title = "Introducción",
    bslib::layout_column_wrap(
      width = 1/2,
      height = 300,
      heights_equal = "row",
      bslib::card(
        full_screen = TRUE,
        bslib::card_header(tags$b("Seguimiento del comportamiento de los usuarios en un sitio web")),
        bslib::card_body(
          style = "height: 250px; overflow: auto; padding-right: 6px;",
          
          tags$p("La tienda oficial del merch de Google está interesada en obtener información que pueda ayudarle a mejorar su estrategia
           comercial."),
          tags$p("Para ello se cuenta con la base de datos proporcionada
           por Google Analytics, que contiene datos del primer semestre del
           año 2017."),
          tags$p("Mediante un análisis exploratorio se estudia el comportamiento de los visitantes de esta página web.")
        )
      ),
      bslib::card(
        full_screen = FALSE,
        bslib::card_header(tags$b("Variables de la base de datos")), # Título de la Card
        bslib::card_body(
          style = "height: 250px; overflow: auto; padding-right: 6px;",
          
          tags$ul(
            tags$p(shiny::icon("tag"), tags$b("ID:"), " Número que identifica a cada visitante del sitio web."),
            
            tags$p(shiny::icon("calendar-alt"), tags$b("Fecha:"), "Fecha de la visita al sitio web."),
            
            tags$p(shiny::icon("mouse-pointer"), tags$b("Clicks:"), "Cantidad de clicks realizados durante la visita al sitio web."),
            
            tags$p(shiny::icon("file"), tags$b("Paginas:"), "Cantidad de páginas accedidas durante la visita al sitio web."),
            
            tags$p(shiny::icon("clock"), tags$b("Tiempo:"), "Duración de la visita al sitio web, en segundos."),
            
            tags$p(shiny::icon("dollar-sign"), tags$b("Gasto:"), "Valor monetario de las compras realizadas durante la visita al sitio web, en dólares."),
            
            tags$p(shiny::icon("globe"), tags$b("Browser:"), "Tipo de navegador desde el que accedi´o al sitio web."),
            
            tags$p(shiny::icon("mobile-alt"), tags$b("Dispositivo:"), "Tipo de dispositivo desde el que accedió al sitio web."),
            
            tags$p(shiny::icon("flag"), tags$b("Pais:"), "País de residencia del visitante del sitio web.")
          )
          # [FIN DEL CONTENIDO CON ICONOS]
        )
      )
    )),
  
  # Primera pestaña
  bslib::nav_panel(
    title ="Características",
    fill = F,
    bslib::layout_sidebar(
      sidebar = sidebar_g1_mapa,
      fill = F,
      bslib::card(
        bslib::card_header("Mapa no compradores"),
        bslib::card_body(
          class = "p-0",
          leafletOutput("mapa_output", 
                        # Para que se vea bien, sino se veía muy chico el mapa
                        height = "75vh")
        )
      )
    ),
    
    # Separador visual
    hr(),
    
    # Segunda sección:
    bslib::layout_sidebar(
      sidebar = sidebar_g1_tabla,
      fill = F,
      bslib::card(
        bslib::card_header("Tabla compradores"),
        bslib::card_body(
          dataTableOutput("tabla_output")
        )
      )
    )
  ),
  
  # Segunda pestaña
  bslib::nav_panel(
    title = "Evolución de compras",
    bslib::layout_sidebar(
      sidebar = sidebar_g2_tendencia,
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Evolución de la cantidad de compras por día de la semana (según mes)"),
        shiny::plotOutput("tendencias_output")
      )
    )
  ),
  
  # Tercera pestaña
  bslib::nav_panel(
    title = "Análisis del gasto",
    bslib::layout_columns(
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Relación entre páginas visitadas y gasto por compra"),
        plotly::plotlyOutput(outputId = "dispersion_gasto")
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Distribución de gasto por compra según dispositivo"),
        plotly::plotlyOutput(outputId = "distribucion_compra")
      )
    )
  ),
  
  # Cuarta pestaña
  bslib::nav_panel(
    title = "Comportamiento del usuario",
    bslib::layout_columns(
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Cantidad de clientes según la cantidad de visitas hasta la primera compra"),
        plotly::plotlyOutput(outputId = "hist_ventas")
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Tiempo de visita por navegador y dispositivo"),
        plotly::plotlyOutput(outputId = "graf_burbujas")
      )
    )
  )
) 





# ARMAMOS EL SERVIDOR
MiServidor <- function(input, output, session) {
  # Página 1
  
  # Filtramos datos reactividad mapa (fuera del renderLeaflet)
  mapa_filtrado <- reactive({
    mapa_datos |> 
      filter(continente %in% input$continente_mapa) |> 
      st_as_sf()
  })
  
  
  # Mapa
  output$mapa_output <- renderLeaflet({
    
    mapa_filtrado2 <- mapa_filtrado()
    
    etiqueta <- paste0(
      "<b>Pais: </b>", mapa_filtrado2$pais, "<br>",
      "<b>No compradores:</b>", mapa_filtrado2$pct_nc,"% (n = ",mapa_filtrado2$n_nc,") <br>",
      "<b>Navegador principal:</b>",mapa_filtrado2$Navegador_top, "(",mapa_filtrado2$pct_Navegador,"%) <br>",
      "<i>(entre sesiones) </i><br>",
      "<b>Dispositivo principal:</b>", mapa_filtrado2$dispositivo_top, "(",mapa_filtrado2$pct_dispositivo,"%) <br>",
      "<i>(entre sesiones)</i>")
    
    leaflet(mapa_filtrado2) |>
      # Ponemos el mapa que elegimos
      addProviderTiles("CartoDB.PositronNoLabels") |>
      # Polígonos sin colores, solo bordes
      addPolygons(
        fillColor = "#40E0D0",
        fillOpacity = 0.3,
        weight = 1,
        color = "#40E0F3",
        label = lapply(etiqueta, htmltools::HTML),
        
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#40F0C0",
          fillOpacity = 0.7,
          fillColor = "#40F0B0"
        )
      )
  })
  
  
  
  # Tabla
  # Reactividad de la tabla
  paises_compradores <- reactive({
    compradores |> 
      filter(pais_label %in% input$paises_tabla)
  })  
  
  
  # Para dispositivo
  dispositivo_tabla <- reactive({
    cookies |> 
      filter(pais %in% input$paises_tabla) |> 
      group_by(ID) |> 
      filter(
        n_distinct(pais) == 1,
        any(!is.na(gasto))
      ) |> 
      ungroup() |> 
      group_by(pais, dispositivo) |> 
      summarise(cantidad = n(),
                .groups = "drop") |> 
      group_by(pais) |> 
      mutate(
        pct = round(cantidad/ sum(cantidad) * 100, 2)
      ) |> 
      arrange(pais, desc(cantidad)) |> 
      
      slice(1) |> 
      ungroup() |> 
      select(pais_label = pais,
             Dispositivo_top = dispositivo,
             pct_dispositivo_top = pct)
  })
  
  
  # Para navegador
  navegador_tabla <- reactive({
    cookies |> 
      filter(pais %in% input$paises_tabla) |> 
      group_by(ID) |> 
      filter(n_distinct(pais) == 1,
             any(!is.na(gasto))) |> 
      
      ungroup() |> 
      
      group_by(pais, Navegador) |> 
      summarise(cantidad = n(),
                .groups = "drop") |> 
      group_by(pais) |> 
      mutate(
        pct = round(cantidad/ sum(cantidad) * 100, 2)
      ) |> 
      arrange(pais, desc(cantidad)) |> 
      slice(1) |> 
      ungroup() |> 
      select(pais_label = pais,
             Navegador_top = Navegador,
             pct_navegador_top = pct)
  })
  
  # Output tabla
  output$tabla_output <- renderDataTable({
    
    compradores_seleccionados <- paises_compradores()
    
    disp_en_compradores <- dispositivo_tabla()
    
    brow_en_compradores <- navegador_tabla()
    
    
    tabla_pais <- cookies |> 
      filter(pais %in% input$paises_tabla) |> 
      group_by(ID) |> 
      filter(n_distinct(pais) == 1) |> 
      summarise(
        pais_label = first(pais), 
        compro = any(!is.na(gasto)),
        cant_compras = sum(!is.na(gasto)),
        .groups = "drop") |> 
      group_by(pais_label) |> 
      summarise(
        n_usuarios = n(),
        usuarios_compradores = sum(compro == T),
        conv = usuarios_compradores / n_usuarios,   
      )
    
    
    
    tabla <- tabla_pais |> 
      # Unimos Navegador y dispositivo
      left_join(disp_en_compradores, by = c("pais_label")) |> 
      left_join(brow_en_compradores, by = c("pais_label")) |> 
      mutate(
        "Porcentaje Compradores" = round(conv * 100, 2), 
        "Porcentaje Dispositivo Principal" = round(pct_dispositivo_top, 2),
        "Porcentaje Navegador Principal" = round(pct_navegador_top,2 )
      ) |> 
      select( 
        País = pais_label,
        "Porcentaje Compradores",
        "Navegador Principal"= Navegador_top,
        "Porcentaje Navegador Principal",
        "Dispositivo Principal" = Dispositivo_top,
        "Porcentaje Dispositivo Principal"
      )
    
    # Tabla en sí:
    datatable(
      tabla,
      options = list(
        pageLength = 10,
        dom = 't',
        ordering = TRUE
      ),
      rownames = FALSE
    ) |> 
      formatStyle(
        columns = 1:6,
        backgroundColor = styleEqual(0, "#00C1D4"),  
        color = "black"
      )
  })  
  
  
  # Página 2
  # Reactividad del gráfico de tendencias
  datos_filtrados <- shiny::reactive({
    dplyr::filter(
      datos_objetivo2, 
      mes %in% input$mes_tendencia)
  })
  
  # Output gráfico:
  output$tendencias_output <- shiny::renderPlot({
    datos <- datos_filtrados()
    
    ggplot2::ggplot(datos, ggplot2::aes(x = dia, y = compras, color = mes, group = mes)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_point(size = 3) +
      ggplot2::scale_color_manual(
        values = c(
          "Enero" = "#1abc9c",   
          "Febrero" = "#3498db",  
          "Marzo" = "#9b59b6",   
          "Abril" = "#e67e22",    
          "Mayo" = "#2ecc71",    
          "Junio" = "#e74c3c"     
        )
      ) +
      ggplot2::labs(
        x = "Día de la semana",
        y = "Número de compras",
        color = "Mes",
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
        legend.position = "bottom"
      )
  })
  
  
  # Página 3
  # Gráfico de dispersión
  output$dispersion_gasto <- plotly::renderPlotly({
    
    graf_obj6 <- ggplot2::ggplot(datos_objetivo6,
                                 ggplot2::aes(
                                   x = gasto,
                                   y = paginas,
                                   text = paste0(
                                     "Gasto: ",gasto,
                                     "<br>Páginas: ", paginas
                                   )
                                 )) +
      ggplot2::geom_point(alpha = 0.6, color = "#40E0D0") +
      ggplot2::scale_x_log10(
        breaks = c(10, 100, 1000, 10000),
        labels = scales::label_number()
      ) +
      ggplot2::labs(
        x = "Gasto por compra en USD (escala logarítmica)",
        y = "Cantidad de páginas visitadas"
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "right")
    
    plotly::ggplotly(graf_obj6, tooltip = "text")
  })
  
  
  
  
  #Gráfico de distribuciones
  output$distribucion_compra <- plotly::renderPlotly({
    graf_dist <- ggplot2::ggplot(
      compras_con_col_texto,
      ggplot2::aes(x = gasto, fill = dispositivo, text = columna_text)
    ) +
      ggplot2::geom_density(alpha = 0.4, color = "white") +
      ggplot2::scale_x_log10(
        breaks = c(10, 100, 1000, 10000),
        labels = scales::label_number()
      ) +
      ggplot2::scale_fill_brewer(palette = "Set2", name = "Dispositivo") +
      ggplot2::labs(
        title = "",
        subtitle = "En escala logarítmica",
        x = "Gasto por compra en USD (escala logarítmica)",
        y = "Densidad"
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = "top")
    
    plotly::ggplotly(graf_dist, tooltip = "text")
  })
  
  # Página 4
  #Burbujas
  output$graf_burbujas <- plotly::renderPlotly({
    grafico_obj3 <- ggplot2::ggplot(datos_objetivo3) +
      ggplot2::aes(
        x = tiempo_medio,
        y = clicks_medio,
        size = frecuencia,
        colour = Navegador,
        text = paste0(
          "<b>Navegador: </b>", Navegador, "<br>",
          "<b>Frecuencia: </b>", frecuencia, " <i>(sesiones)</i><br>",
          "<b>Tiempo medio: </b>", round(tiempo_medio, 2), " min<br>",
          "<b>Clicks medio: </b>", round(clicks_medio, 0)
        )
      ) + 
      ggplot2::geom_point(alpha = 0.4) +
      ggplot2::scale_colour_manual(values = colores_Navegador, name = "Navegador") +
      ggplot2::scale_size(range = c(2, 18), name = "Conteo de sesiones") +
      ggplot2::scale_x_continuous(limits = c(0, max(datos_objetivo3$tiempo_medio) * 1.5)) +
      ggplot2::scale_y_continuous(limits = c(0, max(datos_objetivo3$clicks_medio) * 1.5)) +
      ggplot2::facet_wrap(~dispositivo, ncol = 3) +
      ggplot2::labs(
        x = "Tiempo (minutos)",
        y = "Clicks"
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        strip.background = ggplot2::element_blank(),
        strip.text = ggplot2::element_text(face = "bold")
      )
    
    plotly::ggplotly(grafico_obj3, tooltip = "text") |>
      plotly::layout(legend = list(itemsizing = "constant"))
  })
  
  
  #Histograma
  output$hist_ventas <- plotly::renderPlotly({
    p <- ggplot2::ggplot(
      datos_grafico,
      ggplot2::aes(x = categoria_visitas, y = total, text = tooltip)
    ) +
      ggplot2::geom_col(fill = "#40E0D0") +
      ggplot2::labs(
        x = "Visitas hasta la compra",
        y = "Cantidad de clientes"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        strip.text = ggplot2::element_text(face = "bold")
      )
    
    plotly::ggplotly(p, tooltip = "text")
  })
  
  
  
}


shiny::shinyApp(ui = MiInterfaz, server = MiServidor)
