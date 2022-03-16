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

options(shiny.maxRequestSize=30*1024^2)

##Conexión local
#conexion_base <- dbPool(
#    drv = dbDriver("MySQL", max.con = 1000),
#    dbname = "ubicame",
#    host = "localhost",
#    user = 'ubicame_admin',
#    password = 'Rut@almercado_admin',
#    idleTimeout = 3600000 ###una hora
#)

##AWS password: admin.2321.  ##nombre db: db_ubicame
## user= admin

## conexion AWS
conexion_base <- dbPool(
  drv = dbDriver("MySQL", max.con = 1000),
  dbname = "ubicame",
  host = "database-ubicame.cccivncyjfw4.us-east-2.rds.amazonaws.com",
  user = 'admin',
  password = 'admin.2321.',
  idleTimeout = 3600000 ###una hora
)

#  Servidor
shinyServer(function(input, output) {
  
    ## Días para que la sesión expire
    cookie_caducidad<- 1
    
    ## Función para guardas información de las sesiones en la base de datos
    add_sessionid_to_db <- function(user, sessionid, conn = conexion_base) {
      tibble(user = user, sessionid = sessionid, login_time = as.character(now())) %>%
        dbWriteTable(conn, "sessionids", ., append = TRUE, row.names = FALSE)
    }
    
    ## Función que regresa un data frame con la información del usuario y la sesión
    get_sessionids_from_db <- function(conn = conexion_base, expiry = cookie_caducidad) {
      dbReadTable(conn, "sessionids") %>%
        mutate(login_time = ymd_hms(login_time)) %>%
        as_tibble() %>%
        filter(login_time > now() - days(expiry))
    }
    
    ## Obtener los usuarios de la base de datos
    sql_usuarios <- "SELECT * FROM usuarios"
    consulta_usuarios <- sqlInterpolate(conexion_base, sql_usuarios)
    usuarios_base<-dbGetQuery(conexion_base, consulta_usuarios)
    
    
    
    ##Modulo de la paquetería shinyauthr para el login de la página usando las credenciales creadas
    logout_init <- callModule(
        shinyauthr::logout,
        id = "logout",
        active = reactive(credenciales()$user_auth)
    )
    
    ## Llama al modulo de login con las columnas de las contraseñas y usuarios de la base de datosy
    ### crea las credenciales
    credenciales <- callModule(
        shinyauthr::login,
        id = "login",
        data = usuarios_base,
        user_col = user, 
        pwd_col = password,
        sessionid_col = sessionid, 
        cookie_getter = get_sessionids_from_db,
        cookie_setter = add_sessionid_to_db,
        sodium_hashed = TRUE,
        log_out = reactive(logout_init())
    )
    
    
    ##Lee las credenciales de acceso cuando unusuario está registrado o no
    user_data <- reactive({
        credenciales()$info
    })
    
    ## Ventana emergentes para inicio de sesión
     panel_inicio<- modalDialog(
                        title = "Bienvenido",
                        fluidRow(
                            shinyauthr::loginUI(id = "login", cookie_expiry = cookie_caducidad,
                                                title = "Iniciar Sesión",
                                                user_title = "Nombre de Usuario",
                                                pass_title = "Contraseña",
                                                login_title = "Log in",
                                                error_message = "Nombre de usuario o contraseña incorrectas!"
                                                ) 
                        ), 
                    easyClose = FALSE,
                    footer = NULL
                    )
     showModal(panel_inicio)
     
     
      
    ##Cierra la ventana emergente si el usuario ha iniciado sesión o abre la ventana emergente si alguien la cierra
    observe({
        if(credenciales()$user_auth%in% TRUE){
            removeModal()   
        }
    })
    ## Si se cierra sesión se pide al usuario registrarse de nuevo (La paquetería shinyauthr utiiza javascrip, por ello es necesario
    ###indica usar shinyjs para manejar sus elementos, en este caso el botón de logout)
    onclick("logout-button",{
        showModal(panel_inicio)
    })
    
   #Info de usuario
    output$nombre_usuario<- renderText({
      user_data()$user
    })
    
    output$credencial_usuario<- renderText({
      user_data()$permisos
    })
    
    
    ## Usuaios en mapa
    ### Conexión a base de datos y query de los datos de clientes
    sql_clientes <- "SELECT * FROM clientes"
    consulta_clientes <- sqlInterpolate(conexion_base, sql_clientes)
    clientes<-dbGetQuery(conexion_base, consulta_clientes)
    #### color inicial 
    clientes$colores<-rep("#F60C9A", nrow(clientes))
    clientes_mapa<-reactiveValues()
    
    clientes_mapa$base<-clientes
    ## Cada que se modifiquen los datos de la base se le añade colores por default
    
    ###Mapa base
    output$mapa <- renderLeaflet({
      
        req(credenciales()$user_auth)
        ##########Genera un warning, investigar
      
        mapa<-leaflet(options = leafletOptions(preferCanvas = TRUE)) %>% 
          addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OpenStreetMap.Mapnik",
                           options = providerTileOptions(
                             updateWhenZooming = FALSE,      
                             updateWhenIdle = FALSE,
                           )) %>%
            addProviderTiles(providers$CartoDB.Positron, group = "CartoDB.Positron", 
                             options = providerTileOptions(
                              updateWhenZooming = FALSE,     
                              updateWhenIdle = FALSE,
                            )) %>%
            addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB.DarkMatter",
                             options = providerTileOptions(
                               updateWhenZooming = FALSE,     
                               updateWhenIdle = FALSE,
                             )) %>%
            addProviderTiles(providers$Esri.WorldImagery, group = "Esri.WorldImagery", 
                             options = providerTileOptions(
                               updateWhenZooming = FALSE,      
                               updateWhenIdle = FALSE,
                             )) %>%
            addLayersControl(
                baseGroups = c("OpenStreetMap.Mapnik","CartoDB.Positron", "CartoDB.DarkMatter", "Esri.WorldImagery"),
                overlayGroups = c("layer_clientes"),
                options = layersControlOptions(collapsed = TRUE)
            )
          
                      
        mapa%>%addSearchOSM(options = searchOptions(hideMarkerOnCollapse = TRUE,
                                                    textPlaceholder = "Buscar...", 
                                                    textCancel = "Cancelar",
                                                    textErr = "Lugar no encontrado"))%>%
          addDrawToolbar(editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()),position="bottomleft") %>%
          addControl(actionButton("tootgle_panel_leaflet",label = "", icon = icon("search-plus")),position="topleft") %>%
          addControl(tags$div(title="Borrar poligonos",
                              actionButton("remove_polygons",label = "", icon = icon("trash"))
                              ),position="bottomright")
    })
    
    ### Mapa proxy
    map_proxy<-leaflet::leafletProxy("mapa")
    
    ## Agrega nuevos clientes al mapa después de cada proceso
    observe({
      req(credenciales()$user_auth)
      if(nrow(clientes_mapa$grupo) == 0 || is.null(clientes_mapa$grupo)){
        return()
      }
      if(!is.null(clientes_mapa$datos_intersectados)){
        ## Si el filtro de datos se hizo manualmente
        if(!is.null(clientes_mapa$poligono_wgs84)){
          map_proxy %>%
            clearMarkers()%>%
            clearGroup("poligono")%>%
            addCircleMarkers(data=clientes_mapa$grupo, radius=7, color=clientes_mapa$grupo$colores, 
                             popup = paste("<h4> Información del cliente</h4>", "<br>",
                                           "Código: ",clientes_mapa$grupo$cliente, "<br>",
                                           "Nombre: ", clientes_mapa$grupo$nombre_cliente, "<br>",
                                           "Direccion: ", clientes_mapa$grupo$calle, "<br>",
                                           "Dia de reparto: ", clientes_mapa$grupo$dia_reparto, "<br>",
                                           "Zona: ", clientes_mapa$grupo$zona, "<br>",
                                           "Distrito: ", clientes_mapa$grupo$distrito
                             ), 
                             stroke=TRUE,fillOpacity=0.8, group = "layer_clientes")%>%
            fitBounds(min(clientes_mapa$datos_intersectados$lon), min(clientes_mapa$datos_intersectados$lat), max(clientes_mapa$datos_intersectados$lon), max(clientes_mapa$datos_intersectados$lat)) %>%
            removeLayersControl()%>%
            addLayersControl(
              baseGroups = c("OpenStreetMap.Mapnik","CartoDB.Positron", "CartoDB.DarkMatter", "Esri.WorldImagery"),
              overlayGroups = c("layer_clientes"),
              options = layersControlOptions(collapsed = TRUE)
            )
          
          clientes_mapa$poligono_wgs84<-NULL
          
        }else if(!is.null(clientes_mapa$poligono_plot_sf)){
          map_proxy %>% clearMarkers()%>%clearGroup("poligono")%>%
            addCircleMarkers(data=clientes_mapa$grupo, radius=7, color=clientes_mapa$grupo$colores, 
                             popup = paste("<h4> Información del cliente</h4>", "<br>",
                                           "Código: ",clientes_mapa$grupo$cliente, "<br>",
                                           "Nombre: ", clientes_mapa$grupo$nombre_cliente, "<br>",
                                           "Direccion: ", clientes_mapa$grupo$calle, "<br>",
                                           "Dia de reparto: ", clientes_mapa$grupo$dia_reparto, "<br>",
                                           "Zona: ", clientes_mapa$grupo$zona, "<br>",
                                           "Distrito: ", clientes_mapa$grupo$distrito
                             ), 
                             stroke=TRUE,fillOpacity=0.8, group = "layer_clientes")%>%
            addPolygons(data = clientes_mapa$poligono_plot_sf %>% slice(1:nrow(clientes_mapa$poligono_plot_sf)), group = "poligono") %>%
            fitBounds(min(clientes_mapa$datos_intersectados$lon), min(clientes_mapa$datos_intersectados$lat), max(clientes_mapa$datos_intersectados$lon), max(clientes_mapa$datos_intersectados$lat)) %>%
            removeLayersControl()%>%
            addLayersControl(
              baseGroups = c("OpenStreetMap.Mapnik","CartoDB.Positron", "CartoDB.DarkMatter", "Esri.WorldImagery"),
              overlayGroups = c("layer_clientes", "poligono"),
              options = layersControlOptions(collapsed = TRUE)
            )
          
          clientes_mapa$poligono_plot_sf<- NULL
        }
        
      } else{
        map_proxy%>%clearMarkers()%>%clearGroup("poligono")%>%
          addCircleMarkers(data=clientes_mapa$grupo, radius=7, color=clientes_mapa$grupo$colores, 
                           popup = paste("<h4> Información del cliente</h4>", "<br>",
                                         "Código: ",clientes_mapa$grupo$cliente, "<br>",
                                         "Nombre: ", clientes_mapa$grupo$nombre_cliente, "<br>",
                                         "Direccion: ", clientes_mapa$grupo$calle, "<br>",
                                         "Dia de reparto: ", clientes_mapa$grupo$dia_reparto, "<br>",
                                         "Zona: ", clientes_mapa$grupo$zona, "<br>",
                                         "Distrito: ", clientes_mapa$grupo$distrito
                           ), 
                           stroke=TRUE,fillOpacity=0.8, group = "layer_clientes")%>%
          fitBounds(min(clientes_mapa$grupo$lon), min(clientes_mapa$grupo$lat), max(clientes_mapa$grupo$lon), max(clientes_mapa$grupo$lat)) %>%
          removeLayersControl()%>%
          addLayersControl(
            baseGroups = c("OpenStreetMap.Mapnik","CartoDB.Positron", "CartoDB.DarkMatter", "Esri.WorldImagery"),
            overlayGroups = c("layer_clientes"),
            options = layersControlOptions(collapsed = TRUE)
          )
        
        
      }
      
    })
    
   ### Verficar si se puede hacer desde js script
    onclick("tootgle_panel_leaflet", {
      toggle("panel_leaflet")
    })
    
    onclick("close_leaflet_panel", {
      toggle("panel_leaflet")
    })
    
    ## Añade las coberturas al mapa de acuerdo a las selecciones del usuario
    poligonos<- reactiveValues()
    sql_poligonos <- "SELECT * FROM poligonos"
    poligonos$base<-dbGetQuery(conexion_base, sql_poligonos)
    ### Añade las coberturas y filtra los datos
    observeEvent(input$add_cobertura,{
      req(credenciales()$user_auth)
      req(clientes_mapa$grupo)
      ##### Indica al usuario que el proceso puede tardar
      showModal(
        modalDialog(
          title = "Procesando",
          fluidRow( align="center",
                    column(12, align="center",
                           h4("Tus datos se están filtrando. Esto puede tomar algunos segundos")
                    )
          ), 
          easyClose = FALSE,
          footer = NULL
        )
      )
      
      poligono_plot<-poligonos$base %>%
                         filter(dia %in% input$coberturas_dia, cd %in% input$cd ) %>%
                         select(poligono)
      #### Si no hay poligonos lo indica al usuario
      if(nrow(poligono_plot) == 0){
        showNotification(
          h4("No hay coberturas cargadas para esta Región y CD"), 
          action = NULL, duration = 5, type = "error")
        
        removeModal()
        return()
      }
      ### Convierte de json a sf --- no es necesario se peude plotear como json
      clientes_mapa$poligono_plot_sf <- st_read(poligono_plot$poligono) %>%
        st_transform(4326)
      
      clientes_crs_planas<-clientes_mapa$grupo%>%
        st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
        st_transform(3857)
      
      poligonos_crs_planas<- st_transform(clientes_mapa$poligono_plot_sf, 3857)
      
      interseccion<-st_intersection(clientes_crs_planas, poligonos_crs_planas)
      ####Añadir una nueva columna a los datos con el color de acuerdo a si están dentro o fuera del poligono 
      clientes_mapa$grupo$colores<-ifelse(rownames(clientes_mapa$grupo) %in% rownames(interseccion), "#29DA1D", "#FF0F00") 
      
      clientes_mapa$datos_intersectados<-clientes_mapa$grupo %>%
        filter(colores %in% "#29DA1D")
      
      removeModal()
    })
    
    ### Genera inputs para descargar clientes intersectados por los poligonos
    output$panel_descargas<-
      renderUI(expr = if(unique(clientes_mapa$grupo$colores)[1] %in% "#F60C9A" 
                         && length(unique(clientes_mapa$grupo$colores)) != 0 ){
        NULL
      } else{
        if(is.null(unique(clientes_mapa$grupo$colores))){
          return()
        }
        req(credenciales()$user_auth)
        div( class=" margin-panel",
          column(12,
                 fluidRow( class="well",
                   h3("Descarga de clientes"),
                   column(6, 
                          radioButtons("filtro", label = h3("Selcciona uno"),
                                       choices = list("Dentro" = "#29DA1D", "Fuera" = "#FF0F00")) ####"#29DA1D", "#FF0F00" colores asignados al momento de calcular las coberturas
                   ),
                   column(6,
                          downloadButton("descarga_clientes_filtrados", label = "Descarga")
                   )
                 )
          )
        )
      })
    
    ### Descarga los clientes
    output$descarga_clientes_filtrados<-downloadHandler(
      
      filename="clientes.csv",
      content=function(file){
        req(credenciales()$user_auth)
        
        datos<-clientes_mapa$grupo%>%
          filter(colores %in% input$filtro) %>%
          select(-cliente_id, -colores)
       
        write.csv(datos,file, row.names = FALSE, fileEncoding = "UTF-8")
      }
    )
    
    ## panel de inputs 
    output$panel_inputs<-renderUI(expr = if(credenciales()$user_auth %in% TRUE){
      region_opciones<- unique(clientes$region) ##Este proceso se puede hacer desde la DB para mejorar el tiempo
      
      fluidRow(id="panel_opciones",
        fluidRow(
           column(10,
                  selectInput("region", label = "Región", 
                              choices = region_opciones, 
                              selected = 1)
           ) 
        ),       
        fluidRow(
          column(10,
                 uiOutput("cd_input_panel")
          ),
          column(2, 
                 actionButton("filtrar_por_cd_region", label = "",class="center-btn-2", icon = icon("filter"))
          )
        ),
        fluidRow(
          column(10, 
                 fileInput("files_filtrar_csv", label = "Búsqueda", accept=c('.csv'), 
                           multiple=FALSE, buttonLabel = "Cargar...", placeholder = "No has seleccionado archivos")
                 ),
          column(2, 
                 actionButton("filtrar_por_csv", label = "",class="center-btn-2", icon = icon("filter"))
                 )
        ),
        fluidRow(
          column(8, 
                 selectInput("coberturas_dia", label = "Coberturas", 
                             choices = c("Lunes", "Martes", "Miercoles","Jueves","Viernes", "Sabado"), 
                             selected = 1)
          ),
          column(4,
                 actionButton("add_cobertura", label = "Añadir",class="center-btn")
                 )
        )
      )
    })
    
    ## Genera los inputs de los cd de acuerdo a la selección de la región
    ### Filtra clientes por cd y región al hacer click en los inputs
    observeEvent(input$filtrar_por_cd_region,{
      ##Limpia datos de filtros anteriores si es que existen
      clientes_mapa$datos_intersectados<-NULL
      clientes_mapa$grupo<- clientes_mapa$base %>% 
                                         filter(region %in% input$region, cd %in% input$cd)
      
      clientes_mapa$base$colores<-rep("#F60C9A", nrow(clientes_mapa$base))
    })
    #### Crea el panel de inputs cd con los datos filtrados por región
    output$cd_input_panel<-renderUI({
      
      cd_opciones<- unique(clientes_mapa$base[clientes_mapa$base$region %in% input$region,c("cd")]) ##Este proceso se puede hacer desde la DB para mejorar el tiempo
      
      selectInput("cd", label = "CD", 
                  choices = cd_opciones, 
                  selected = 1)
    })
    #### Crea el panel de inputs clientes con los datos filtrados por región
    
    
    
    ##Título en el header del sd seleccionado
    output$titulo_cd<-renderText({
      paste("Mapa  - ", "Región: ", input$region," / ","CD: ",input$cd)
    })
    
    ## Información de las políticas
    sql_politicas <- "SELECT * FROM politicas"
    clientes_mapa$politicas_base<-dbGetQuery(conexion_base,sql_politicas)
    
    output$politicas<- renderUI({
      politica<- clientes_mapa$politicas_base %>%
        filter(region %in% input$region, cd %in% input$cd)
      
      if(nrow(politica) != 0){
        div(
          p(paste("Distancia máxima: ", politica[1, "distancia_maxima"], "Km")),
          p(paste("Pedido mínimo: ", politica[1, "pedido_minimo"], "cajas")),
          p(paste("Pedidos rechazados", politica[1, "pedidos_rechazados"], "/mes"))
        )
      } else {
        "No hay politicas cargadas para esta región y cd"
      }
    })
    
    ## Zoom al cliente seleccionado
    observeEvent(input$buscar_cliente,{
      cliente_coord<-clientes_mapa$grupo[clientes_mapa$grupo$cliente %in% input$cod_cliente, c("lat", "lon")]
      
      if(nrow(cliente_coord)<1 || is.null(cliente_coord)){
        showNotification(
          h4("El cliente seleccionado no se encuentra en esta región y cd"), 
          action = NULL, duration = 5, type = "warning") 
        return()
      }
      
      map_proxy %>%
        addCircleMarkers(lng= cliente_coord$lon, lat= cliente_coord$lat, 
                         radius = 20, fillOpacity = 0.5, stroke = FALSE, 
                         group = "poligono")%>%
        setView(lng= cliente_coord$lon, lat= cliente_coord$lat, zoom = 18) %>%
        removeLayersControl()%>%
        addLayersControl(
          baseGroups = c("OpenStreetMap.Mapnik","CartoDB.Positron", "CartoDB.DarkMatter", "Esri.WorldImagery"),
          overlayGroups = c("layer_clientes", "poligono"),
          options = layersControlOptions(collapsed = TRUE)
        )
    })
    ##Zoom a las coordenadas indicadas por el usuario
    observeEvent(input$coordenadas,{
      map_proxy %>%
        addCircleMarkers(lng= input$longitud, lat= input$latitud, 
                         radius = 20, fillOpacity = 0.5, stroke = FALSE, 
                         group = "poligono")%>%
        setView(lng= input$longitud, lat= input$latitud, zoom = 18)%>%
        removeLayersControl()%>%
        addLayersControl(
          baseGroups = c("OpenStreetMap.Mapnik","CartoDB.Positron", "CartoDB.DarkMatter", "Esri.WorldImagery"),
          overlayGroups = c("layer_clientes", "poligono"),
          options = layersControlOptions(collapsed = TRUE)
        )
    })
    
    ## filtra de acuerdo al archivo. csv
    ### Abre la ventana emergente al momento de cargar un archivo y filtrar
    observeEvent(input$filtrar_por_csv,{
      if(!is.null(input$files_filtrar_csv)){
        ###Elimina filtros de procesos anteriores para evitar problemas con el renderizado del mapa
        clientes_mapa$datos_intersectados<-NULL
        ###Inicia proceso
        clientes_mapa$csv<- read.csv(input$files_filtrar_csv$datapath, encoding = "UTF-8")
          
        showModal(
          modalDialog(title = "Datos del csv",
                      fluidRow(column(12,
                                      fluidRow(style="margin: 20px",
                                               uiOutput("nombres_csv"),
                                               dataTableOutput("tabla_clientes_csv")
                                      )
                                      )
                               ),
                      easyClose = FALSE,
                      size = "m",
                      footer = tagList(
                        actionButton("cancelar","Cancelar"),
                        actionButton("filtra_clientes_csv","Filtrar")
                      ) 
          )
        )
      }else{
        showNotification(
          h4("Selecciona un archivo"), 
          action = NULL, duration = 5, type = "error")
      }
      
    })
    #### Tabla con los datos cargados ds clientes desde el csv
    output$tabla_clientes_csv<-renderDataTable({
      req(credenciales()$user_auth)
      if(ncol(clientes_mapa$csv) > 1){
        datatable(clientes_mapa$csv, options = list(
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
          pageLength = 3,
          scrollX = TRUE,
          searching = FALSE
        ))
      } else {
        NULL
      }
      
    })
    
    ####De acuerdo al csv a filtrar crea los inputs para seleccionar la columna de clientes
    output$nombres_csv<-renderUI({
      req(credenciales()$user_auth)
      
      selectInput("nombre_columna", label = h2("Selecciona el nombre de la columna de clientes"),
                  choices = names(clientes_mapa$csv), width = "100%",
                  )
    })
    
    ### Filtra los datos de aceurdo al archivo csv
    observeEvent(input$filtra_clientes_csv,{
      req(credenciales()$user_auth)
      req(clientes_mapa$grupo)
      
      clientes_mapa$grupo<- clientes_mapa$base %>% 
        filter(region %in% input$region, cd %in% input$cd)
      
      
      union<- merge(clientes_mapa$grupo, clientes_mapa$csv%>%select(input$nombre_columna), by.x="cliente",by.y= input$nombre_columna, all=FALSE)
      
      if(nrow(union)<1){
        showNotification(
          h4("No hay usuarios clientes dentro de esta región y CD que coincidan"), 
          action = NULL, duration = 5, type = "error")
        return()
      }
      
      clientes_mapa$grupo<- union
      
      removeModal()
      
    })
    
    ## Filtrar datos de  acuerdo al poligono dibujado
    observeEvent(input$mapa_draw_new_feature,{
      ####
      req(credenciales()$user_auth)
      req(clientes_mapa$grupo)
      ##### Indica al usuario que el proceso puede tardar
      showModal(
        modalDialog(
          title = "Procesando",
          fluidRow( align="center",
                    column(12, align="center",
                           h4("Tus datos se están filtrando. Esto puede tomar algunos segundos")
                    )
          ), 
          easyClose = FALSE,
          footer = NULL
        )
      )
      #### Extrae información de los poligono dibujados
      pol<-input$mapa_draw_new_feature
      
      if(pol$properties$feature_type=="circle"){
        poligono<- st_point(c(pol$geometry$coordinates[[1]], pol$geometry$coordinates[[2]]))%>%
          st_sfc()%>%st_set_crs(4326)%>%
          st_transform(3857)%>%
          st_buffer(dist = pol$properties$radius)
      }else{
        coor<-unlist(pol$geometry$coordinates)
        datap<-data.frame(
          Longitud=coor[seq(1,length(coor), 2)], 
          Latitud=coor[seq(2,length(coor), 2)]
        )
        poligono<-datap %>%
          st_as_sf(coords = c("Longitud", "Latitud"), crs = 4326) %>%
          summarise(geometry = st_combine(geometry)) %>%
          st_cast("POLYGON")%>%
          st_transform(3857)
      }
      
      ### Datos de clientes en coordenadas planas
      clientes_crs_planas<-clientes_mapa$grupo%>%
        st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
        st_transform(3857)
      
      interseccion<-st_intersection(clientes_crs_planas, poligono)
      
      clientes_mapa$grupo$colores<-ifelse(rownames(clientes_mapa$grupo) %in% rownames(interseccion), "#29DA1D", "#FF0F00") #### #29DA1D-interseccion, #FF0F00-nointerseccion 
      
      clientes_mapa$datos_intersectados<-clientes_mapa$grupo %>%
        filter(colores %in% "#29DA1D")
      clientes_mapa$poligono_wgs84<- st_transform(poligono,4326)
      
      
      removeModal()
      
    })
    
    
    onclick('remove_polygons',{
      map_proxy%>%clearGroup("poligono")%>%clearShapes()%>%
        removeDrawToolbar(clearFeatures=TRUE) 
      
    })
    
    
    ## Genera la página de usuarios solo si se tienen las credenciales
    output$main_usuarios<-
      renderUI(expr = if(user_data()$permisos%in%"administrador"){
        req(credenciales()$user_auth)
        req(user_data()$permisos%in%"administrador")
        
        fluidRow(
          box(
            title = "Administrador de usuarios", 
            closable = FALSE,
            width = 12,
            status = "warning", 
            solidHeader = FALSE, 
            collapsible = TRUE,
            div(style="align-content: center;",
                column(width = 12,
                       dataTableOutput("tabla_usuarios"),
                       br(),
                       br(),
                       fluidRow(
                         column(width = 2, actionButton("eliminar", label = "Eliminar",  class = "btn-new")),
                         column(width = 2, actionButton("nuevo", label = "Nuevo",  class = "btn-new"))
                       )
                )
            )
          )
        )
      } else {
        h1("Registrate como administrador")
      })
    ### Genera la tabla de usuarios
    usuarios<- reactiveValues()
    sql_usuarios <- "SELECT * FROM usuarios"
    usuarios_base<-dbGetQuery(conexion_base, sql_usuarios)
    
    usuarios$datos<-usuarios_base
    
    output$tabla_usuarios<- renderDataTable({
      req(credenciales()$user_auth)
      req(user_data()$permisos %in%"administrador")
      
      usuarios_plot<- usuarios$datos
      
      datatable(usuarios_plot[,c("user", "permisos", "nombre")], options = list(
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
        pageLength = 10,
        scrollX = TRUE,
        searching = TRUE
      ))
    })
    
    ###Crear nuevo usuario
    #### Ventana emergente para introducir datos y crear un nuevo usuario
    observeEvent(input$nuevo,{
      showModal(
        modalDialog(
          title = "Nuevo usuario",
          fluidRow(
            column(width = 12,
                   textInput("usuarioN", label = h4("Usuario")),
                   passwordInput("passwordN",label = h4("Constraseña")),
                   radioButtons("permisosN", label = h4("Permiso"),
                                choiceNames = c("Usuario","Contribuidor","Administrador"),
                                choiceValues = c("usuario","contribuidor","administrador")
                   ),
                   textInput("nombreN",label = h4("Nombres"))
            )
          ),
          easyClose = FALSE,
          footer = tagList(
            actionButton("cancelar","Cancelar"),
            actionButton("guardarN","Guardar")
          )
        )
      )
    })
    
    #### Guarda los usuarios en la base de datos; antes aplica algunas restricciones para añdir un nuevo usuario
    observeEvent(input$guardarN,{
      req(credenciales()$user_auth)
      req(user_data()$permisos%in%"administrador")
      
      if(nchar(input$usuarioN)<1 || nchar(input$passwordN)<1 ||
         nchar(input$nombreN)<1){
        showNotification(
          h4("Debes completar todos los campos"), 
          action = NULL, duration = 5, type = "warning")
      } else {
          sql_usuarios <- "SELECT * FROM usuarios"
          usuarios_base<-dbGetQuery(conexion_base, sql_usuarios)
          
          if(length(grep(input$usuarioN,usuarios_base$user))>0){
            showNotification(
              h4("El nombre de usuario ya existe"), 
              action = NULL, duration = 5, type = "warning")   
          } else {
              sql_nuevo_usuario <- 'INSERT INTO usuarios (user,password,permisos,nombre) 
                    VALUES (?iduser,?idpassword,?idpermiso,?idnombre);'
              
              consulta_usuarios <- sqlInterpolate(conexion_base, sql_nuevo_usuario, iduser= input$usuarioN, 
                                                  idpassword=password_store(input$passwordN),
                                                  idpermiso=input$permisosN,idnombre=input$nombreN)
              dbGetQuery(conexion_base, consulta_usuarios)
              
              removeModal()
              
              showNotification(
                h4("Creación exitosa"), 
                action = NULL, duration = 5, type = "message")
              
              sql_usuarios <- "SELECT * FROM usuarios"
              usuarios$datos<-dbGetQuery(conexion_base, sql_usuarios)
            }
          
      }
    })
    ####Valores reactivos que guardaran info de las selecciones en la tabla
    selecciones_tabla<- reactiveValues()
    #### Ventana emergente para advertir de que se van a eliminar usuarios
    observeEvent(input$eliminar,{
      
      if(length(input$tabla_usuarios_rows_selected)>0){
        showModal(
          modalDialog(title = "Borrar",
                      fluidPage(column(12,h3("Cuidado: Estás a punto de borrar usuarios de la base de datos"),style="color:red;")),
                      easyClose = FALSE,
                      size = "m",
                      footer = tagList(
                        actionButton("cancelar","Cancelar"),
                        actionButton("borrar_usuario","Eliminar")
                      ) 
          )
        )
      } else {
        showNotification(
          h4("Selecciona un renglón"), 
          action = NULL, duration = 5, type = "warning") 
      }
       
    })
    
    ##### Elimina el usuario de la base de datos
    observeEvent(input$borrar_usuario,{
      req(credenciales()$user_auth)
      req(user_data()$permisos%in%"administrador")
      
      selecciones_tabla$renglon<-input$tabla_usuarios_rows_selected
      usuarios_a_borrar<-usuarios$datos[selecciones_tabla$renglon,"user"]
      
      sql_borrar_usuario <- paste("DELETE FROM usuarios WHERE user IN (",
                                  paste0(sprintf("'%s'",usuarios_a_borrar),collapse = ","),")")
      
      dbGetQuery(conexion_base, sql_borrar_usuario)
      removeModal()
      
      showNotification(
        h4("Usuario eliminado con éxito"), 
        action = NULL, duration = 5, type = "message")
      
      sql_usuarios <- "SELECT * FROM usuarios"
      usuarios$datos<-dbGetQuery(conexion_base, sql_usuarios) ###### Vuelve a leer la base para actualizar la tabla
      
    })
    
    
    ### Elimina por csv 
    #### Ventana emergente
    observeEvent(input$eliminar_por_csv,{
      
        showModal(
          modalDialog(title = "Datos del csv",
                      fluidRow(column(12,
                                      fluidRow(style="margin: 20px",
                                               fileInput("csv_eliminar_clientes", label = "Carga un csv", accept=c('.csv'), multiple=FALSE,
                                                         buttonLabel = "Cargar...", placeholder = "No hay archivos seleccionados"
                                               ),
                                               uiOutput("nombres_csv_borrar")
                                      )
                      )
                      ),
                      easyClose = FALSE,
                      size = "m",
                      footer = tagList(
                        actionButton("cancelar","Cancelar"),
                        actionButton("eliminar_clientes_csv","Eliminar")
                      ) 
          )
        )
      
    })
    #### panel con los nombres de las columnas
    output$nombres_csv_borrar<-renderUI(expr = if(!is.null(input$csv_eliminar_clientes)){
      
      clientes_mapa$csv_eliminar<- read.csv(input$csv_eliminar_clientes$datapath, encoding = "UTF-8")
      
      fluidRow(
        column(10, 
               selectInput("nombre_columna_eliminar", label = "Selecciona el nombre de la columna de clientes",
                           choices = names( clientes_mapa$csv_eliminar), width = "100%",
               )
               ),
        dataTableOutput("tabla_clientes_csv_borrar")
      )
    } else {
      NULL
    })
    #### tabla con los datos cargados
    output$tabla_clientes_csv_borrar<-renderDataTable({
      req(credenciales()$user_auth)
      if(ncol(clientes_mapa$csv_eliminar) > 0 ){
        datatable(clientes_mapa$csv_eliminar, options = list(
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
          pageLength = 3,
          scrollX = TRUE,
          searching = FALSE
        ))
      } else {
        NULL
      }
      
      
    })
    
    ### Elimina los clientes de la base de datos de acuerdo a los datos del csv
    observeEvent(input$eliminar_clientes_csv,{
      req(credenciales()$user_auth)
      req(user_data()$permisos%in%"administrador" || user_data()$permisos%in%"contribuidor" )
      
      clientes_csv_a_eliminar<- clientes_mapa$csv_eliminar[, input$nombre_columna_eliminar]
      
      sql_borrar_cliente_csv <-  paste("DELETE FROM clientes WHERE cliente IN (",
                                       paste0(sprintf("'%s'",clientes_csv_a_eliminar),collapse = ","),")")
      
      dbGetQuery(conexion_base, sql_borrar_cliente_csv)
      removeModal()
      
      showNotification(
        h4("Clientes eliminados con éxito"), 
        action = NULL, duration = 5, type = "message")
      
      sql_clientes <- "SELECT * FROM clientes"
      clientes_mapa$base<-dbGetQuery(conexion_base, sql_clientes) ####### Vuelve a leer la base para actualizar la tabla
      clientes_mapa$grupo<- clientes_mapa$base[clientes_mapa$base$cd %in% input$cd,]
    })
    
    
    
    
    ## Cierra cualquier modal al momento de dar cancelar
    observeEvent(input$cancelar,{
      removeModal()
    })
    
    
    ## Panel de edición 
    ### Panel restringido solo a admin o contribuidores
    output$main_edicion<-renderUI(expr = if(user_data()$permisos%in%"administrador" || user_data()$permisos%in%"contribuidor"){
      req(credenciales()$user_auth)
      req(user_data()$permisos%in%"administrador" || user_data()$permisos%in%"contribuidor" )
      
      fluidRow(
        tabBox(width = 12, height = "650px",
          title = "Edición",
          tabPanel("Base de datos", "Edición base de datos de clientes",
                   div(style="align-content: center;",
                       column(width = 12,
                              dataTableOutput("tabla_clientes"),
                              br(),
                              br(),
                              fluidRow(
                                column(width = 2, actionButton("eliminar_cliente", label = "Eliminar",  class = "btn-new")),
                                column(width = 2, actionButton("eliminar_por_csv", label = "Eliminar (.csv)",  class = "btn-new")),
                                column(width = 4, actionButton("nuevos_clientes", label = "Nuevo (.csv)",  class = "btn-new"))
                              )
                       )
                   )
                   ),
          tabPanel("Poligonos", "Agregar poligonos a la base de datos",
                   div(style="align-content: center;",
                       column(width = 12,
                              fluidRow(
                                column(4,
                                       fileInput("files_mapinfo", "", accept=c('.DAT','.ID','.MAP','.TAB'), multiple=TRUE,
                                                 buttonLabel = "Cargar...", placeholder = "No hay archivos seleccionados"
                                                 )
                                       ),
                                column(6,
                                        h4("Añadir a la base de datos"),
                                        uiOutput("panel_agregar_poligono")
                                )
                                
                              ),
                              fluidRow(
                                column(10,
                                       h2("Vista previa de los poligonos"),
                                       leafletOutput("mapa_prev_mapinfo")
                                       )
                                
                              )
                       )
                   )
                   ),
          tabPanel("Políticas", "Modificar politicas",
                   div(style="align-content: center;",
                       column(width = 10,
                              dataTableOutput("tabla_politicas"),
                              br(),
                              br(),
                              fluidRow(
                                column(width = 2, actionButton("eliminar_politica", label = "Eliminar",  class = "btn-new")),
                                column(width = 2, actionButton("nueva_politica", label = "Nuevo",  class = "btn-new"))
                              )
                       )
                   )
                   )
        )
      )
    } else {
      h2("Necesitas registrarte como administrador o contribuidor")
    })
    ### Tabla con los clientes de la base de datos
    output$tabla_clientes<-renderDataTable({
      req(credenciales()$user_auth)
      req(user_data()$permisos%in%"administrador" || user_data()$permisos%in% "contribuidor")
      
      datatable(clientes_mapa$grupo, options = list(
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
        pageLength = 5,
        scrollX = TRUE,
        searching = TRUE
      ))
    })
    
    ### Eliminar un cliente de la base dedatos
    #### Ventana emergente que alerta de borrrar un usuario
    selecciones_tabla_clientes<-reactiveValues()
    
    observeEvent(input$eliminar_cliente,{
      
      if(length(input$tabla_clientes_rows_selected)>0){
        showModal(
          modalDialog(title = "Borrar",
                      fluidPage(column(12,h3("Cuidado: Estás a punto de borrar clientes, estás modificando la base de datos"),style="color:red;")),
                      easyClose = FALSE,
                      size = "m",
                      footer = tagList(
                        actionButton("cancelar","Cancelar"),
                        actionButton("borrar_cliente","Eliminar")
                      ) 
          )
        )
      } else {
        showNotification(
          h4("Selecciona un renglón"), 
          action = NULL, duration = 5, type = "warning") 
      }
      
    })
    #### Se elimina el cliente de la base de datos
    observeEvent(input$borrar_cliente,{
      req(credenciales()$user_auth)
      req(user_data()$permisos%in%"administrador" || user_data()$permisos%in% "contribuidor")
      
      selecciones_tabla_clientes$renglon<-input$tabla_clientes_rows_selected
      clientes_a_borrar<-clientes_mapa$grupo[selecciones_tabla_clientes$renglon,"cliente"]
      
      sql_borrar_cliente <- paste("DELETE FROM clientes WHERE cliente IN (",
                                  paste0(sprintf("'%s'",clientes_a_borrar),collapse = ","),")")
      
      dbGetQuery(conexion_base, sql_borrar_cliente)
      removeModal()
      
      showNotification(
        h4("Usuario eliminado con éxito"), 
        action = NULL, duration = 5, type = "message")
      
      sql_clientes <- "SELECT * FROM clientes"
      clientes_mapa$base<-dbGetQuery(conexion_base, sql_clientes) ####### Vuelve a leer la base para actualizar la tabla
      clientes_mapa$grupo<- clientes_mapa$base[clientes_mapa$base$cd %in% input$cd,]
    })
    
    ### Añadir nuevo cliente
    #### ventana emergente para añadir datos
    observeEvent(input$nuevos_clientes,{
      showModal(
        modalDialog(
          title = "Nuevos clientes",
          fluidRow(
            column(12,
                   h2("Cagar nuevos clientes"),
                   fileInput("file_clientes", label = "", accept=c('.csv'), multiple=FALSE,
                             buttonLabel = "Cargar...", placeholder = "No hay archivos seleccionados"
                   ),
                   dataTableOutput("tabla_clientes_vista_previa")
                   )
          ),
          easyClose = FALSE,
          footer = tagList(
            actionButton("cancelar","Cancelar"),
            actionButton("guardar_nuevos_clientes","Guardar")
          )
        )
      )
    })
    
    #### Valores reactivos para guardar los datos de los clientes a guardar
    clientes_nuevos<- reactiveValues()
    
    output$tabla_clientes_vista_previa<-renderDataTable({
      req(credenciales()$user_auth)
      req(user_data()$permisos%in%"administrador" || user_data()$permisos%in% "contribuidor")
      
      if(is.null(input$file_clientes)){
        NULL
      } else {
        clientes_nuevos$datos<-read.csv(input$file_clientes$datapath, encoding = "latin1") ## encoding = "latin1" Resuelve el error del encoding en shiny server 
        
        datatable(clientes_nuevos$datos, options = list(
          language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
          pageLength = 5,
          scrollX = TRUE,
          searching = FALSE
        ))
      }
    })
    
    #### Añade el nuevo cliente a la base de datos
    observeEvent(input$guardar_nuevos_clientes,{
      req(credenciales()$user_auth)
      req(user_data()$permisos%in%"administrador" || user_data()$permisos%in% "contribuidor")
      
      #### Que los nombre de las columans de los nuevos datos conicidan con los nombres
      #### de las columnas de la base de datos excepto el id y el color
      names(clientes_nuevos$datos)<- c("cliente",                "nombre_cliente" ,        "calle" ,                
                                        "distrito",               "region" ,                "cd",                    
                                       "gerencia" ,              "supervisor" ,            "zona" ,                 
                                        "nombre_vendedor",        "tipo_atencion" ,         "tipo_agente",           
                                       "cadena" ,                "unidad_negocio" ,        "canal" ,                
                                       "sub_canal"  ,            "ivs_reparto"  ,          "dia_reparto" ,          
                                       "volumen_venta_mes_hi",   "volumen_venta_mes_caja", "contraruta",            
                                        "fee" ,                   "lat",                    "lon"  )
      
      if(ncol(clientes_nuevos$datos) != 24 ){
        showNotification(
          h4("El número de columnas no es el adecuado"), 
          action = NULL, duration = 5, type = "error")
        
        return()
      } 
      
      if(sum(is.na(clientes_nuevos$datos$lat)) > 0 || sum(is.na(clientes_nuevos$datos$lat)) > 0){
        showNotification(
          h4("No puede haber valores nulos en las coordendas"), 
          action = NULL, duration = 5, type = "error")
         
        return()
      }
      
      if(is.character(clientes_nuevos$datos$lat) == TRUE || is.character(clientes_nuevos$datos$lon) %in% TRUE ||
         is.character(clientes_nuevos$datos$fee) %in% TRUE || is.character(clientes_nuevos$datos$volumen_venta_mes_hi) %in% TRUE ||
         is.character(clientes_nuevos$datos$volumen_venta_mes_caja) %in% TRUE || is.character(clientes_nuevos$datos$ivs_reparto) %in% TRUE ||
         is.character(clientes_nuevos$datos$cliente) %in% TRUE){
        
        showNotification(
          h4("Deben ser de tipo NUMERIC las columnas: cliente, latitud, longitud, fee, volumen de venta, ivs de reparto"), 
          action = NULL, duration = 5, type = "error")
        
        return()
      }
      
      #### Indica que los datos se están procesando
      showModal(
        modalDialog(
          title = "Procesando",
          fluidRow( align="center",
                    column(12, align="center",
                           h4("Los datos se están guardando. Esto puede tomar algunos segundos")
                    )
          ), 
          easyClose = FALSE,
          footer = NULL
        )
      )
      
      ###### Primero elimna usuarios existentes si estos se repiten en los nuevos datos
     
      sql_borrar_clientes_repetidos <- paste("DELETE FROM clientes WHERE cliente IN (",
                                  paste0(sprintf("'%s'",clientes_nuevos$datos$cliente),collapse = ","),");")
      
      
      
      dbGetQuery(conexion_base, sql_borrar_clientes_repetidos)
      
      
      dbWriteTable(conexion_base, "clientes", clientes_nuevos$datos, row.names = FALSE, append = TRUE)
      ##### Vuelve a leer la base de datos para capturar las actualizaciones 
      sql_clientes <- "SELECT * FROM clientes"
      clientes_mapa$base<-dbGetQuery(conexion_base, sql_clientes) ####### Vuelve a leer la base para actualizar la tabla
      clientes_mapa$grupo<- clientes_mapa$base[clientes_mapa$base$cd %in% input$cd,]
      
      removeModal()
      
      showNotification(
        h4("Datos guardados con éxito"), 
        action = NULL, duration = 5, type = "message")
      
    })
    
    ### Renderiza mapa si existen los archivos
    archivos<- reactiveValues()
    
    output$mapa_prev_mapinfo<-renderLeaflet({
      req(credenciales()$user_auth)
      req(user_data()$permisos%in%"administrador" || user_data()$permisos%in% "contribuidor")
      if(!is.null(input$files_mapinfo)){
        #### Lee los 4 archivos de mapinfo
        files_mapinfo <- input$files_mapinfo
        previouswd <- getwd()
        uploaddirectory <- dirname(files_mapinfo$datapath[1])
        setwd(uploaddirectory)
        for(i in 1:nrow(files_mapinfo)){
          file.rename(files_mapinfo$datapath[i], files_mapinfo$name[i])
        }
        setwd(previouswd)
        ##### Se asegura que sean 4 archivos
        if(nrow(files_mapinfo) != 4){
          showNotification(
            h4("Son necesarios 4 arhivos de mapinfo (.MAP, .TAB, .ID, .DAT)"), 
            action = NULL, duration = 5, type = "message")
          return()
        }
        ##### Se asegura que se tengan los archivos correcots
        if(length(grep(".TAB", files_mapinfo$name)) == 0 ||
           length(grep(".MAP", files_mapinfo$name)) == 0 ||
           length(grep(".ID", files_mapinfo$name)) == 0 ||
           length(grep(".DAT", files_mapinfo$name)) == 0){
          showNotification(
            h4("Son necesarios 4 arhivos de mapinfo (.MAP, .TAB, .ID, .DAT)"), 
            action = NULL, duration = 5, type = "message")
          return()
        }
        
        poligonos_sf<- st_read(paste(uploaddirectory, files_mapinfo$name[grep(pattern="*TAB$", files_mapinfo$name)], sep="/"))
        #######Asegurarse que se tenga el mismo sistema de referencia
        archivos$poligonos_mapinfo <- st_transform(poligonos_sf, 4326)
        
        mapa_mapinfo<-leaflet() %>% 
          addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OpenStreetMap.Mapnik") %>%
          addProviderTiles(providers$CartoDB.Positron, group = "CartoDB.Positron") %>%
          addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB.DarkMatter") %>%
          addProviderTiles(providers$Esri.WorldImagery, group = "Esri.WorldImagery") %>%
          addLayersControl(
            baseGroups = c("OpenStreetMap.Mapnik","CartoDB.Positron", "CartoDB.DarkMatter", "Esri.WorldImagery")
          )%>%
          addPolygons(data = archivos$poligonos_mapinfo %>% slice(1:nrow(archivos$poligonos_mapinfo)))
        
        mapa_mapinfo%>%addSearchOSM()
        
      } else {
        NULL
      }
    })
    
    ### Renderiza los inputs para guardar un poligono
    output$panel_agregar_poligono<-renderUI(expr = if(!is.null(input$files_mapinfo) && nrow(input$files_mapinfo) == 4){
      fluidRow(
        column(4,
               selectInput("cd_poligono", label = "CD", 
                           choices = unique(clientes_mapa$base$cd), 
                           selected = 1)
               ),
        column(4,
               selectInput("dia_poligono", label = "Día", 
                           choices = c("Lunes", "Martes", "Miercoles","Jueves","Viernes", "Sabado"), 
                           selected = 1)
        ),
        column(2,
               actionButton("agregar_poligono", "Añadir", class = "btn-new")
               )
      )
    } else {
      NULL
    })
    
    observeEvent(input$agregar_poligono, {
      
      req(credenciales()$user_auth)
      req(user_data()$permisos %in%"administrador" || user_data()$permisos%in% "contribuidor")
      ######Elimna el poligono existente
      poligono_a_borrar<-input$dia_poligono
      dia_a_borrar<- input$cd_poligono
      sql_borrar_poligono <- paste("DELETE FROM poligonos WHERE dia IN (",
                                  paste0(sprintf("'%s'",as.character(poligono_a_borrar)),collapse = ","),")
                                  AND cd IN (", paste0(sprintf("'%s'",as.character(dia_a_borrar)),collapse = ","),")")
      
      dbGetQuery(conexion_base, sql_borrar_poligono)
      #######Lo guarda en SQL como json
      
      tf<-tempfile()
      prueba<- st_write(archivos$poligonos_mapinfo, tf, layer = "geojson", driver = "GeoJSON") 
      poligonos_json <- paste(readLines(tf), collapse=" ")
      file.remove(tf)
      
      sql_nuevo_poligono <- 'INSERT INTO poligonos (poligono, cd, dia) 
                    VALUES (?pol, ?cd, ?dia);'
      
      consulta_poligono <- sqlInterpolate(conexion_base, sql_nuevo_poligono,
                                          pol=poligonos_json, cd=input$cd_poligono, dia=input$dia_poligono)
      dbGetQuery(conexion_base, consulta_poligono)
      
      
      ## Actualiza los poligonos volviendo a leer la base de datos
      sql_poligonos <- "SELECT * FROM poligonos"
      poligonos$base<-dbGetQuery(conexion_base, sql_poligonos)
      
      showNotification(
        h4("Poligono añadido con éxito"), 
        action = NULL, duration = 5, type = "message")
      
    })
    
    ## CRUD políticas
    ### tabla de politicas
    output$tabla_politicas<- renderDataTable({
      req(credenciales()$user_auth)
      req(user_data()$permisos %in%"administrador" || user_data()$permisos%in% "contribuidor")
      
      
      datatable(clientes_mapa$politicas_base, options = list(
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json'),
        pageLength = 10,
        scrollX = TRUE,
        searching = FALSE
      ))
    })
    
    ### ventana emergente para eliminar politicas
    observeEvent(input$eliminar_politica,{
      
      if(length(input$tabla_politicas_rows_selected)>0){
        showModal(
          modalDialog(title = "Borrar",
                      fluidPage(column(12,h3("Cuidado: Estás a punto de borrar politicas de la base de datos"),style="color:red;")),
                      easyClose = FALSE,
                      size = "m",
                      footer = tagList(
                        actionButton("cancelar","Cancelar"),
                        actionButton("borrar_politica","Eliminar")
                      ) 
          )
        )
      } else {
        showNotification(
          h4("Selecciona un renglón"), 
          action = NULL, duration = 5, type = "warning") 
      }
      
    })
    ### Elimina la politica de la base de datos
    observeEvent(input$borrar_politica,{
      req(credenciales()$user_auth)
      req(user_data()$permisos%in%"administrador")
      
      selecciones_tabla$renglon_politica<-input$tabla_politicas_rows_selected
      politicas_a_borrar<-clientes_mapa$politicas_base[selecciones_tabla$renglon_politica,"politica_id"]
      
      sql_borrar_politica <- paste("DELETE FROM politicas WHERE politica_id IN (",
                                  paste0(sprintf("'%s'",politicas_a_borrar),collapse = ","),")")
      
      dbGetQuery(conexion_base, sql_borrar_politica)
      removeModal()
      
      showNotification(
        h4("Politica eliminada con éxito"), 
        action = NULL, duration = 5, type = "message")
      
      sql_politicas <- "SELECT * FROM politicas"
      clientes_mapa$politicas_base<-dbGetQuery(conexion_base, sql_politicas) ###### Vuelve a leer la base para actualizar la tabla
      
    })
    
    ### Nueva politica
    observeEvent(input$nueva_politica,{
      showModal(
        modalDialog(
          title = "Nueva politica",
          fluidRow(
            column(width = 12,
                   selectInput("region_politica", label = "Región", 
                               choices = unique(clientes_mapa$base$region), 
                               selected = 1),
                   selectInput("cd_politica", label = "CD", 
                               choices = unique(clientes_mapa$base$cd), 
                               selected = 1),
                   numericInput("distancia_maxima", label = "Distancia Máxima", value = 1),
                   numericInput("pedido_minimo", label = "Pedido Mínimo", value = 1),
                   numericInput("pedidos_rechazados", label = "Pedidos rechazados", value = 1)
            )
          ),
          easyClose = FALSE,
          footer = tagList(
            actionButton("cancelar","Cancelar"),
            actionButton("guardar_politica","Guardar")
          )
        )
      )
    })
    
    ### Guarda la política en la base de datos
    observeEvent(input$guardar_politica,{
      if(nchar(input$region_politica)< 1 || nchar(input$cd_politica)<1){
        showNotification(
          h4("Debes completar los campos de Región y CD"), 
          action = NULL, duration = 5, type = "error")
      }else{
        sql_nueva_politica <- 'INSERT INTO politicas (region,cd,distancia_maxima,pedido_minimo,pedidos_rechazados) 
                    VALUES (?reg, ?cd, ?dmax, ?pmin, ?prechazados);'
        
        consulta_politica <- sqlInterpolate(conexion_base, sql_nueva_politica, reg= input$region_politica, cd=input$cd_politica,
                                            dmax= input$distancia_maxima, pmin=input$pedido_minimo, prechazados=input$pedidos_rechazados)
        
        dbGetQuery(conexion_base, consulta_politica)
        
        removeModal()
        
        showNotification(
          h4("Creación exitosa"), 
          action = NULL, duration = 5, type = "message")
        
        sql_politicas <- "SELECT * FROM politicas"
        clientes_mapa$politicas_base<-dbGetQuery(conexion_base, sql_politicas)
      }
    })
    
    

})
