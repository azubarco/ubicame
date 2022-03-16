library(shiny)
library(RMySQL)
library(DBI)
library(pool)
library(dplyr)
library(sodium)
library(lubridate)
library(shinyjs)
library(leaflet)
library(leaflet.extras)
library(shinyWidgets)
library(DT)
library(sf)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyauthr)


#Borrar
# show handler
show_loading <- function(elem) {
    session <- shiny::getDefaultReactiveDomain()
    session$sendCustomMessage("show_loading", elem)
}

# hide handler
hide_loading <- function(elem) {
    session <- shiny::getDefaultReactiveDomain()
    session$sendCustomMessage("hide_loading", elem)
}


shinyUI(
    
    dashboardPage(
        ##Opciones globales
        skin = "black",
        options = list(sidebarExpandOnHover = FALSE),##Para contraer/expandir el sidebar cuando se desplaze el mouse
        ##Encabezado
        header = dashboardHeader(
            title=tagList(
                span(class = "logo-lg", div(img(src = "logo.png"), "Ubícame 2.0")), 
                img(src = "logo.png")),
            
            tags$li(class="dropdown",div(class= "btn-auth",
                                         shinyauthr::logoutUI(id = "logout", label = "Salir")),
                    
                    ),
            dropdownMenu(icon = icon("user"), headerText = "Información de usuario",
                         badgeStatus = NULL,
                             messageItem(
                                 p(textOutput("nombre_usuario")),
                                 p(textOutput("credencial_usuario")),
                                )
                         )
        ),
        ##Barra lateral derecha
        sidebar = dashboardSidebar(
            sidebarMenu(
                id="sidebarid",collapsed = FALSE,
                menuItem("Mapa", tabName = "mapa", icon = icon("map-marked")),
                menuItem("Edición", tabName = "edicion", icon = icon("database")),
                menuItem("Usuarios", tabName = "users", icon = icon("users")),
                uiOutput("panel_inputs")
                
            )
        ),
        ##Contenido Página principal
        body = dashboardBody(
            ### shinyjs package para implementar funciones js en shiny
            useShinyjs(),
            ### Archivos CSS y javascript para funciones y estilos adicionales
            tags$head(
                tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
                tags$link(rel="shortcut icon", href="logo.png"),
                tags$script(src = "extra_file.js")
            ),
            tabItems(
                ## Página de mapa y procesos
                tabItem(tabName = "mapa",
                        fluidRow(
                            box(
                                title = textOutput("titulo_cd"), 
                                closable = FALSE,
                                width = 12,
                                status = "warning", 
                                solidHeader = FALSE, 
                                collapsible = TRUE,
                                dropdownMenu = boxDropdown(
                                    icon = icon("box-open"),
                                    div(class="pol-margin",
                                        h4("Políticas"),
                                        uiOutput("politicas")
                                    )
                                ),
                                div(
                                    fluidRow(id="panel_leaflet", class="panel-leaflet-ubicacion" ,
                                              div(style="float:right;",actionButton("close_leaflet_panel", "",class="btn-new",icon = icon("times-circle",  class="fa-lg"))),
                                              fluidRow(
                                                  h4("Buscar por código", class="margin-left-title"),
                                                  column(8,
                                                         numericInput(inputId = "cod_cliente", "Código cliente", value = 0)      
                                                  ), 
                                                  column(4,
                                                         actionButton("buscar_cliente", label = "Buscar", class="center-btn-panel-leaflet"))
                                              ),
                                             fluidRow(
                                                 h4("Buscar por coordendas", class="margin-left-title"),
                                                 div(
                                                     
                                                     column(6,numericInput(inputId = "latitud", "Latitud", value = -6.000)),
                                                     column(6,numericInput(inputId = "longitud", "Longitud", value = -80.000) ),
                                                     actionButton("coordenadas", label = h5("Buscar"), class="center-btn-3")
                                                 )
                                             )
                                    ),
                                    
                                    leafletOutput("mapa", height = "530px"),
                                    uiOutput("panel_descargas")
                                )
                            )
                        )
                ),
                
                ## Página para cargar clientes y nuevos poligonos
                tabItem(tabName = "edicion",
                        uiOutput("main_edicion")
                ),
                ## Página para crear nuevos administradores o usuarios
                tabItem(tabName = "users",
                        uiOutput("main_usuarios")
                        )
                
                
            )
        ),
        title = "Ubícame"
    )
    
)
