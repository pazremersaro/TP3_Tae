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

# ARMAMOS LA INTERFAZ

MiInterfaz <- bslib::page_navbar(
  title = "Google Analytics",
  
  # Primera pestaña
  bslib::nav_panel("Grupo 1",
                   
                   # Primera sección:
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
    title = "Pag 2",
    bslib::layout_sidebar(
      sidebar = bslib::sidebar(
        # shinyWidgets::airDatepickerInput()--> ver si sirve
        title = "sidebar calendario",
        open = TRUE,
        shinyWidgets::airDatepickerInput(
          inputId = "fecha_tendencia",
          label = "Seleccionar Rango de Fechas",
          range = TRUE,
          # poner bien las fechas VERR
          minDate = Sys.Date() - 365,
          maxDate = Sys.Date(),# el maximo seria la facha de la compu
          value = c(Sys.Date() - 90, Sys.Date()) #deberia mostrar como presetableciodo los ultimos 90 dias
        )
      ),
      
      bslib::layout_columns(
        bslib::card(
          full_screen = TRUE,
          bslib::card_header("TENDENCIAS"),
          shiny::plotOutput("tendencias_output")
        )
      )
    )
    
    
  ),
  
  # Tercera pestaña
  bslib::nav_panel(
    title = "Pag 3",
    bslib::layout_columns(
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("DISPERSION GASTO"),
        shiny::plotOutput("dispersion_gasto")
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("DISTRIBUCIÓN COMPRA"),
        shiny::plotOutput("distribucion_compra")
      )
  )
  ),
# Cuarta pestaña
bslib::nav_panel(
    title = "Pag 4",
    bslib::layout_columns(
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("HISTOGRAMA VENTAS"),
        shiny::plotOutput("hist_ventas")
      ),
      bslib::card(
        full_screen = TRUE,
        bslib::card_header("GRAF BUBUJAS"),
        shiny::plotOutput("graf_burbujas")
      )
    )
  )
  
)  








# ARMAMOS EL SERVIDOR

MiServidor <- function(input, output) {
  
#  mapa_datos <- poligonos |> 
#   left_join(para_mapa_final,
#             by = "pais") |> 
#   
#   # Nos quedamos con los n_nc que son difernentes de NA
#   filter(!is.na(n_nc)) 
#   
# 
# 
# mapa_datosf <- mapa_datos |> 
#   
#   mutate(
#     etiqueta = paste(
#       "<b>Pais: </b>", mapa_datos$pais, "<br>",
#       "<b>No compradores:</b>", mapa_datos$pct_nc,"% (n = ",mapa_datos$n_nc,") <br>",
#       "<b>Navegador principal:</b>",mapa_datos$Navegador_top, "(",mapa_datos$pct_Navegador,"%) <br>",
#       "<i>(entre sesiones) </i><br>",
#       "<b>Dispositivo principal:</b>", mapa_datos$dispositivo_top, "(",mapa_datos$pct_dispositivo,"%) <br>",
#       "<i>(entre sesiones)</i>"))
# 
# 
# # Lo pasamos a tipo mapa
# mapa_datosf <- sf::st_as_sf(mapa_datosf)
# 
# mapa <- leaflet(mapa_datosf) %>%
#   # Ponemos el mapa que elegimos
#   addProviderTiles("CartoDB.PositronNoLabels") %>%
#   # Polígonos sin colores, solo bordes
#   addPolygons(
#     fillColor = "#7ADC87",
#     fillOpacity = 0.3,
#     weight = 1,
#     color = "#6EBD4C",
#     label = ~lapply(as.list(etiqueta), HTML),  
#     
#     highlightOptions = highlightOptions(
#       weight = 2,
#       color = "#CD5C5C",
#       fillOpacity = 0.7,
#       fillColor = "#FF6A6A"
#     )
#   )


}


shiny::shinyApp(ui = MiInterfaz, server = MiServidor)

