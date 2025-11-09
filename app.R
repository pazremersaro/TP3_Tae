#           Probando el primer grafico            #

library(shiny)
library(readr)
library(tidyverse)

# CARGA Y MANEJO DE DATOS
library(readr)
library(dplyr)
library(tidyr)

# MAPAS
# Como son muchos los países intento usar un mapa:
# install.packages("sf")

library(sf) # No me anduvo, VER
library(leaflet)
library(spData)
# install.packages("rnaturalearth")
library(rnaturalearth) # (usamos ne_countries)

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
library(kableExtra)



# OBJETIVO 7
library(ggridges)
library(stringr)
library(forcats)
library(RColorBrewer)
library(shinyWidgets)
# CARGA Y MANEJO DE DATOS

datos <- read_delim("cookies.txt", delim = "\t",
                    escape_double = FALSE, trim_ws = TRUE)
# Ponemos los países en español:

# VER DE CAMBIAR EL ORDEN POR EL WARNING
cookies <- datos |> 
  mutate(
    pais = case_when(
      pais == "(not set)" ~ "Sin especificar",
      T ~ countrycode(
        sourcevar = pais,
        origin = "country.name.en",
        destination = "cldr.name.es")),
    
    iso_a3 = countrycode(pais,
                         "country.name", "iso3c"),
    
    Navegador = browser,
    dispositivo = case_when(
      dispositivo == "desktop" ~ "Computadora",
      dispositivo == "mobile" ~ "Celular",
      dispositivo == "tablet" ~ "Tablet"
    ))


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
            sesiones = n(),
            .groups = "drop") |> 
  filter((compro == F) & (paises_dif == 1 & not_set == F))
  
  


# 
# nc_mapa <- no_compradores%>%
#   group_by(ID) %>%
#   summarise(
#     # De todas formas debería ser el mismo, pero consideramos
#     # el primero para que no se repita
#     pais = first(pais),
#     sesiones = n(),
#     .groups = "drop"
#   )


# Armamos un dataset que cuente cuántos usuarios no compradores hay por país (entre los que tienen país especificado y es sólo uno)
total_pais <- cookies |> 
  # Sacamos los usuarios que no tienen país registrado
  filter(pais != "Sin especificar") |> 
  # Eliminamos anguilla también porque no se tiene en cuenta adelante
  filter(pais != "Anguila") |> 
  # Agrupamos por país
  group_by(pais) |> 
  summarise(
    # Calculamos la cantidad de usuarios diferentes por país
    usuarios_totales = n_distinct(ID),
    .groups = "drop"
  )

# Para las sesiones (NAVEGADOR Y DISPOSITIVO):
# NAVEGADOR
Navegador_pais <- cookies |> 
  # Agrupamos por dispositivo y país
  group_by(pais, Navegador) |> 
  # Contamos cuantas sesiones por país por dispositivo hay
  summarise(cantidad = n(),
            .groups = "drop") |> 
  # Nos quedamos con los países agrupados
  group_by(pais) |> 
  # Cantidad de sesiones por dispositivo por país 
  mutate(pct = round(cantidad / sum(cantidad) * 100, 0)) |> 
  
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
  summarise(cantidad = n(),
            .groups = "drop") |> 
  group_by(pais) |> 
  # Cantidad de sesiones por dispositivo por país 
  mutate(pct = round(cantidad / sum(cantidad) * 100, 0)) |> 
  
  # Acomodamos de dispositivo con mayor uso por país a menor cantidad de uso
  arrange(pais, desc(cantidad)) |> 
  
  # Nos queremos quedar sólo con la primera
  slice(1) |> 
  select(pais,
         dispositivo_top = dispositivo, 
         pct_dispositivo = pct)



# INTERESA QUE APAREZCA:
# - % NO COMPRADORES (POR PAÍS)
# - CANTIDAD DE NO COMPRADORES (POR PAIS)
# - % Navegador (MAYOR CANTIDAD DE SESIONES POR PAÍS)
# - % DISPOSITIVO (MAYOR CANTIDAD DE SESIONES POR PAIS)


# UNIMOS LOS DATASETS
para_mapa <- nc_mapa |> 
  group_by(pais) |> 
  summarise(n_nc = n(),
            .groups = "drop") |> 
  left_join(total_pais, by = "pais") |> 
  mutate(pct_nc = round(n_nc / usuarios_totales * 100)) |> 
  left_join(Navegador_pais, by = "pais") |> 
  left_join(dispositivo_pais, by = "pais") 



# MAPA

# Para que aparezca la información a lo largo de
# todo el país, hay que usar los polígonos de países:
poligonos <- ne_countries(scale = "medium", # Supuestamente para que tenga un nivel de detalle intermedio
                          # Decimos que devuelva un tipo de archivo sf
                          # (que es lo que se usa para los mapas)
                          returnclass = "sf") |> 
  mutate(
    pais = countrycode(name,
                       "country.name.en",
                       
                       # Pide que convierta de acuerdo a un tipo de nombres
                       # en este caso supuestamente es en español
                       destination = "cldr.name.es")) |> 
  
  # Agregamos codigo iso para unir después
  select(pais, 
         geometry, 
         name)


# Convertimos nombres de países en para_mapa a ISO:
para_mapa_final <- para_mapa |> 
  mutate(
    iso_a3 = countrycode(pais,
                         "country.name",
                         "iso3c"
    )) |> 
  # Seleccionamos las columnas que queremos mantener
  select(iso_a3, 
         pais,
         n_nc,
         usuarios_totales,
         pct_nc,
         Navegador_top,
         pct_Navegador,
         dispositivo_top,
         pct_dispositivo)

#--------------------------------------------------------#
#    Fin limpieza de datos para interrogante 1 (mapa)    #  
#--------------------------------------------------------#
mapa_datos <- poligonos %>% 
  left_join(para_mapa_final,
            by = "pais") %>% 
  
  # Nos quedamos con los n_nc que son difernentes de NA
  filter(!is.na(n_nc)) 
mapa_datosf <- mapa_datos %>% 
  mutate(
    etiqueta = paste(
      "<b>Pais: </b>", mapa_datos$pais, "<br>",
      "<b>No compradores:</b>", mapa_datos$pct_nc,"% (n = ",mapa_datos$n_nc,") <br>",
      "<b>Navegador principal:</b>",mapa_datos$Navegador_top, "(",mapa_datos$pct_Navegador,"%) <br>",
      "<i>(entre sesiones) </i><br>",
      "<b>Dispositivo principal:</b>", mapa_datos$dispositivo_top, "(",mapa_datos$pct_dispositivo,"%) <br>",
      "<i>(entre sesiones)</i>"))
# Lo pasamos a tipo mapa
mapa_datosf <- sf::st_as_sf(mapa_datosf)
mapa <- leaflet(mapa_datosf) %>%
  # Ponemos el mapa que elegimos
  addProviderTiles("CartoDB.PositronNoLabels") %>%
  # Polígonos sin colores, solo bordes
  addPolygons(
    fillColor = "#7ADC87",
    fillOpacity = 0.3,
    weight = 1,
    color = "#6EBD4C",
    label = ~lapply(as.list(etiqueta), HTML),  
    
    highlightOptions = highlightOptions(
      weight = 2,
      color = "#CD5C5C",
      fillOpacity = 0.7,
      fillColor = "#FF6A6A"
    )
  )




#--------------------------------------------------------#
#    Limpieza de datos para interrogante 2 (tendencia)   #  
#--------------------------------------------------------# 
datos_objetivo2 <- cookies %>%
  mutate(
    fecha = ymd(fecha),
    dia = factor(str_to_title(weekdays(fecha)),
                 levels = c("Lunes", "Martes", "Miércoles", "Jueves",
                            "Viernes", "Sábado", "Domingo")),
    mes = factor(str_to_title(format(fecha, "%B")),
                 levels = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio"))
  ) %>%
  group_by(mes, dia) %>%
  summarise(compras = sum(gasto > 0, na.rm = TRUE))



#---------------------------------------------------------#
#    Limpieza de datos para interrogante 6 (dispersion)   #  
#---------------------------------------------------------# 

datos_objetivo6 <- cookies %>%
  dplyr::filter(!is.na(gasto), gasto > 0) %>% 
  dplyr::select(paginas, gasto)

#-----------------------------------------------------------#
#    Limpieza de datos para interrogante 7 (distribucion)   #  
#-----------------------------------------------------------# 
compras <- cookies %>%
  dplyr::filter(!is.na(gasto), gasto > 0)
# Estadisticas para el cartel
dispositivo_stats <- compras %>%
  dplyr::group_by(dispositivo) %>%
  dplyr::summarise(
    Media   = mean(gasto),
    Mediana = median(gasto),
    Min     = min(gasto),
    Max     = max(gasto),
    .groups = "drop"
  ) %>%
  #Creo una columna de texto para HTML
  dplyr::mutate(
    columna_text = paste0(
      "<b>Dispositivo: </b>", dispositivo, "<br>",
      "<b>Media: </b>",   scales::dollar(round(Media,   0)), "<br>",
      "<b>Mediana: </b>", scales::dollar(round(Mediana, 0)), "<br>",
      "<b>Mínimo: </b>",  scales::dollar(round(Min,     0)), "<br>",
      "<b>Máximo: </b>",  scales::dollar(round(Max,     0))
    )
  ) %>%
  # Selecciona solo la columna de texto y la variable de agrupación
  dplyr::select(dispositivo, columna_text)
compras_con_col_texto <- compras %>%
  dplyr::left_join(dispositivo_stats, by = "dispositivo")



#-------------------------------------------------------#
#    Limpieza de datos para interrogante 3 (burbujas)   #  
#-------------------------------------------------------# 

# Armamos un dataset que tenga los tiempos en minutos según navegador y dispositivo
datos_objetivo3 <- cookies %>% 
  dplyr::mutate(
    # Pasamos los que tienen tiempo na a 0 y todo a minutos
    tiempo = ifelse (is.na(tiempo), 0, tiempo) / 60
  ) %>% 
  # Agrupamos por Navegador y dispositivo:
  dplyr::group_by(Navegador, dispositivo) %>% 
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

#---------------------------------------------------------#
#    Limpieza de datos para interrogante 4 (histograma)   #  
#---------------------------------------------------------# 
visitas_compra <- cookies %>%
  dplyr::arrange(ID, ymd(fecha)) %>% 
  dplyr::group_by(ID) %>% 
  dplyr::mutate(compra =  gasto > 0, 
         visita_compra = ifelse(any(compra),          
                                which(compra)[1], NA)) %>% 
  dplyr::filter(!is.na(visita_compra)) %>%
  dplyr::add_count(ID, dispositivo, name = "veces_dispositivo") %>%
  dplyr::summarise(                  
    visitas_hasta_compra = dplyr::first(visita_compra),
    
    dispositivo_principal = dispositivo[which.max(veces_dispositivo)]
  ) %>% 
  dplyr::mutate(
    categoria_visitas = cut(
      visitas_hasta_compra,
      breaks = c(0, 1, 4, 9, Inf),
      labels = c("1", "2–4", "5–9", "10 o más")
    )
  )

# Totales por categoría de visita (cuantos clientes hay en cada categoria)
totales_categoria <- visitas_compra %>%
  dplyr::group_by(categoria_visitas) %>%
  dplyr::summarise(total = n(),
            .groups = 'drop') 

# Porcentajes por dispositivo dentro de cada categoría
porcentajes_dispositivo <- visitas_compra %>%          
  # Agrupamos por categoria de visitas y dispositivo
  dplyr::group_by(categoria_visitas, dispositivo_principal) %>%
  # Contamos cuantos clientes hay en cada combinacion
  dplyr::summarise(n = n(), 
            .groups = 'drop') %>%        
  # Agrupamos solo por categoria de visita
  dplyr::group_by(categoria_visitas) %>%         
  
  dplyr::mutate(porcentaje = round(n / sum(n) * 100, 1)) %>%  
  # Porcentaje de cada dispositivo dentro de su categoria
  dplyr::select(categoria_visitas,
         dispositivo_principal, 
         porcentaje) 
# Formato ancho para tener una columna por dispositivo
porcentajes_ancho <- porcentajes_dispositivo %>%
  tidyr::pivot_wider(
    names_from = dispositivo_principal,
    values_from = porcentaje,
    values_fill = 0
  )

# Unimos los totales con los porcentajes
datos_grafico <- totales_categoria %>%
  dplyr::left_join(porcentajes_ancho,
            by = "categoria_visitas")

# Hacemos el tooltip para que aparezca la etiqueta que cuando apoyas el mouse
datos_grafico <- datos_grafico %>%
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










# PARA QUE APAREZCAN DISTINTOS SIDEBAR DE ACUERDO
# CON DISTINTAS PÁGINAS HAY QUE GUARDAR LOS SIDEBAR 
# AFUERA DE LAS FUNCIONES COMO UN OBJETO


sidebar_g1_mapa <- bslib::sidebar(
  title = "Mapa",
    
  # selectInput para poder escribir y elegir 
  # la palabra (en este caso el continente)
    shinyWidgets::pickerInput(
      inputId = "continente_mapa",
      label = "Continente/s",
      choices = c("América del Norte", "América del Sur", "América Central"),
      options = list(
        # Para que se pueda buscar en el momento
        "live-search" = T,
        # Para seleccionar y deseleccionar todo:
        "actions-box" = T
      )
    )
)

sidebar_g1_tabla <- bslib::sidebar(
  title = "Tabla",
  shinyWidgets::pickerInput(
    inputId = "paises_tabla",
    label = "País/es",
    choices = c("EEUU", "Argentinaaaa", "Uruguay", "Chile"),
    options = list(
      "live-search" = T,
      "actions-box" = T
    )
  )
)

sidebar_g2_tendencia <- bslib::sidebar(
  title = "Tendencia por Mes", 
  shinyWidgets::pickerInput(
    inputId = "mes_tendencia", #ID para el servidor
    label = "Mes", 
    choices =  unique(datos_objetivo2$mes), 
    options = pickerOptions(),
  )
)
###############################################################


# ARMAMOS LA INTERFAZ

MiInterfaz <- bslib::page_navbar(
  title = "Google Analytics",
  bslib::nav_panel(
    title = "Introducción",
    bslib::layout_column_wrap(
      width = 1/2,
      height = 300,
      heights_equal = "row",
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("Introdución:"),
        bslib::card_body(
          style = "height: 250px; overflow: auto; padding-right: 6px;",
          tags$p("La tienda oficial del merch de Google está interesada en obtener información que pueda ayudarle a mejorar su estrategia
           comercial. Para ello se cuenta con la base de datos proporcionada
           por Google Analytics, que contiene datos del primer semestre del
           año 2017."),
          tags$p("Para analizar el comportamiento de los visitantes de esta 
           página web, surgieron varios interrogantes de interés que serán 
           respondidos mediante un análisis exploratorio.")
        )
      ),
      bslib::card(
        full_screen = FALSE,
        bslib::card_header("ver que poner como titulo"),
        bslib::card_body(
          style = "height: 250px; overflow: auto; padding-right: 6px;",
          tags$p(tags$b("Cátedra:"), " Análsis Exploratorio de Datos"),
          tags$p(tags$b("Fecha:"), " Noviembre 2025"),
          tags$p(tags$b("Autores:"), " Karen Ottersdtedt, María Paz Remersaro, Agustina Roura")
        )
      )
    )
    ),
  # Primera pestaña
  bslib::nav_panel(
                   title ="Características",
                   bslib::layout_sidebar(
                   sidebar = sidebar_g1_mapa,
                     bslib::card(
                       full_screen = T,
                       bslib::card_header("Mapa no compradores"),
                          bslib::card_body(
                            class = "p-0",
                            leafletOutput("mapa_output")
                            )
                          )
                   ),
                   
                   # Segunda sección:
                     bslib::layout_sidebar(
                       sidebar = sidebar_g1_tabla,
                       bslib::card(
                         full_screen = T,
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
MiServidor <- function(input, output) {
  # Página 1
  
  
  
  # Página 2
  datos_filtrados <- shiny::reactive({
    dplyr::filter(datos_objetivo2,
      mes == input$mes_tendencia
    )
  })
  output$tendencias_output <- shiny::renderPlot({
    datos <- datos_filtrados()
    ggplot2::ggplot(datos, ggplot2::aes(x = dia, y = compras, group = 1)) +
      ggplot2::geom_line(linewidth = 1, color = "#40E0D0") +
      ggplot2::geom_point(size = 3, color = "#40E0D0") +
      ggplot2::labs(
        x = "Día de la semana",
        y = "Número de compras",
        title = paste(stringr::str_to_title(input$mes_tendencia))
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"))
  })

  # Página 3
#Gráfico de dispersión
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
  
  plotly::ggplotly(grafico_obj3, tooltip = "text") %>%
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

