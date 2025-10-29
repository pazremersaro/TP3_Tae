#       Probando el primer grafico            #

# CARGA Y MANEJO DE DATOS
library(readr)

datos <- read_delim("cookies.txt", delim = "\t",
                    escape_double = FALSE, trim_ws = TRUE)




# ARMAMOS LA INTERFAZ

MiInterfaz <- bslib::page_fluid(
  shiny::titlePanel("Google Analytics"),
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(),
  )
)









# ARMAMOS EL SERVIDOR

MiServidor <- function(input, output) {
  
}


shiny::shinyApp(ui = MiInterfaz, server = MiServidor)
