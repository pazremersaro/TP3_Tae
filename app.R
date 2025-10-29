#           Probando el primer grafico            #

# CARGA Y MANEJO DE DATOS

datos <- read_delim("cookies.txt", delim = "\t",
                    escape_double = FALSE, trim_ws = TRUE)
# Ponemos los países en español:

cookies <- datos %>% 
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

# Armamos un DATASET que tenga la información de los NO COMPRADORES
no_compradores <- cookies %>% 
  
  # Usamos %in% para que mire si están ciertos valores en 
  # el conjunto de datos (pedimos que busque las filas ID que 
  # estén en id_interesantes que serían los no compradores)
  filter(ID %in% id_interesantes$ID) 


# Como hay países o lugares geográficos no reconocidos, los sacamos
# para poder analizar los que sí están definidos:

# Guardamos los no compradores que ingresaron desde diferentes países
no_compradores_rancios <- no_compradores %>% 
  group_by(ID) %>% 
  summarise(
    # Que cuente la cantidad de países de cada usuario
    paises_dif = n_distinct(pais),
    
    # Si hay algun usuario que tenga al menos una sesion 
    # con país no identificado
    not_set = any(pais == "Sin especificar")
  ) %>% 
  # Guarda las sesiones de los usuarios que no tienen país identeificado y los  que entraron desde más de un país
  filter(paises_dif > 1 | not_set == T) 


# nrow(no_compradores_rancios) 1305 usuarios de paises no reconocidos o entraron en varios paises


# PARA TRABAJAR CON EL MAPA
no_compradores <- no_compradores %>% 
  
  # Eliminamos los usuarios no compradores que ingresaron
  #  en más de un país y aquellos que no tienen país registrado
  filter(!ID %in% no_compradores_rancios$ID) 

# 
# # Verificamos que los usuarios que consideramos tienen un sólo país
# no_compradores %>% 
#   group_by(ID) %>% 
#   summarise(paises_dist = n_distinct(pais)) %>% 
#   filter(paises_dist > 1)
# # Funciona correctamente, no hay usuarios que hayan entrado de 
# # distintos países


# NOS QUEDAMOS CON:
# - UNA OBSERVACION POR USUARIO
# - EL PAIS
# - CANTIDAD DE SESIONES DEL USUARIO

nc_mapa <- no_compradores%>%
  group_by(ID) %>%
  summarise(
    # De todas formas debería ser el mismo, pero consideramos
    # el primero para que no se repita
    pais = first(pais),
    sesiones = n(),
    .groups = "drop"
  )


# Armamos un dataset que cuente cuántos usuarios no compradores hay por país (entre los que tienen país especificado y es sólo uno)
total_pais <- cookies %>% 
  # Sacamos los usuarios que no tienen país registrado
  filter(pais != "Sin especificar") %>%
  
  # Eliminamos anguilla también porque no se tiene en cuenta adelante
  filter(pais != "Anguila") %>% 
  # Agrupamos por país
  group_by(pais) %>% 
  summarise(
    # Calculamos la cantidad de usuarios diferentes por país
    usuarios_totales = n_distinct(ID),
    .groups = "drop"
  )

# Para las sesiones (NAVEGADOR Y DISPOSITIVO):
# NAVEGADOR
Navegador_pais <- no_compradores %>% 
  # Agrupamos por dispositivo y país
  group_by(pais, Navegador) %>% 
  # Contamos cuantas sesiones por país por dispositivo hay
  summarise(cantidad = n(),
            .groups = "drop") %>% 
  # Nos quedamos con los países agrupados
  group_by(pais) %>% 
  # Cantidad de sesiones por dispositivo por país 
  mutate(pct = round(cantidad / sum(cantidad) * 100, 0)) %>% 
  
  # Acomodamos de dispositivo con mayor uso por país a menor cantidad de uso
  arrange(pais, desc(cantidad)) %>% 
  
  # Nos queremos quedar sólo con la primera 
  # (osea que nos quedamos con el navegador principal de cada país)
  slice(1) %>% 
  select(pais,
         Navegador_top = Navegador, 
         pct_Navegador = pct)


# DISPOSITIVO
dispositivo_pais <- no_compradores %>% 
  # Agrupamos por dispositivo y país
  group_by(pais, dispositivo) %>% 
  # Contamos cuantas sesiones por país por dispositivo hay
  summarise(cantidad = n(),
            .groups = "drop") %>% 
  group_by(pais) %>% 
  # Cantidad de sesiones por dispositivo por país 
  mutate(pct = round(cantidad / sum(cantidad) * 100, 0)) %>% 
  
  # Acomodamos de dispositivo con mayor uso por país a menor cantidad de uso
  arrange(pais, desc(cantidad)) %>% 
  
  # Nos queremos quedar sólo con la primera
  slice(1) %>% 
  select(pais,
         dispositivo_top = dispositivo, 
         pct_dispositivo = pct)



# INTERESA QUE APAREZCA:
# - % NO COMPRADORES (POR PAÍS)
# - CANTIDAD DE NO COMPRADORES (POR PAIS)
# - % Navegador (MAYOR CANTIDAD DE SESIONES POR PAÍS)
# - % DISPOSITIVO (MAYOR CANTIDAD DE SESIONES POR PAIS)


# UNIMOS LOS DATASETS
para_mapa <- nc_mapa %>% 
  group_by(pais) %>% 
  summarise(n_nc = n(),
            .groups = "drop") %>% 
  left_join(total_pais, by = "pais") %>% 
  mutate(pct_nc = round(n_nc / total_pais$usuarios_totales * 100)) %>% 
  left_join(Navegador_pais, by = "pais") %>% 
  left_join(dispositivo_pais, by = "pais") 



# MAPA

# Para que aparezca la información a lo largo de
# todo el país, hay que usar los polígonos de países:
poligonos <- ne_countries(scale = "medium", # Supuestamente para que tenga un nivel de detalle intermedio
                          # Decimos que devuelva un tipo de archivo sf
                          # (que es lo que se usa para los mapas)
                          returnclass = "sf") %>% 
  mutate(
    pais = countrycode(name,
                       "country.name.en",
                       
                       # Pide que convierta de acuerdo a un tipo de nombres
                       # en este caso supuestamente es en español
                       destination = "cldr.name.es")) %>% 
  
  # Agregamos codigo iso para unir después
  select(pais, 
         geometry, 
         name)


# Convertimos nombres de países en para_mapa a ISO:
para_mapa_final <- para_mapa %>% 
  mutate(
    iso_a3 = countrycode(pais,
                         "country.name",
                         "iso3c"
    )) %>% 
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




# ARMAMOS LA INTERFAZ

MiInterfaz <- bslib::page_navbar(
  shiny::titlePanel("Google Analytics"),
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(),
  )
)









# ARMAMOS EL SERVIDOR

MiServidor <- function(input, output) {
  
#  mapa_datos <- poligonos %>% 
#   left_join(para_mapa_final,
#             by = "pais") %>% 
#   
#   # Nos quedamos con los n_nc que son difernentes de NA
#   filter(!is.na(n_nc)) 
#   
# 
# 
# mapa_datosf <- mapa_datos %>% 
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
