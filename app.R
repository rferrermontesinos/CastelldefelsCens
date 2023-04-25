# library(raster)
library(sf)
library(raster)
library(leaflet) #for making the interactive map
library(rgdal) #for importing vector data
library(htmlwidgets) # export html
library(tidyverse)
library(shiny)
library(shinydashboard)
library(RColorBrewer)
library(dplyr)
library(shinyWidgets)
library(shinythemes)
library(leaflet.extras)



shp = st_read("Shapes/Shape_Castelldefels_R.shp")
crs(shp)
new_crs <- '+init=epsg:4326' # the code for our new projection
shp_proj <- st_transform(shp, CRS(new_crs))
crs(shp_proj)

shp_limit = st_read(dsn= "Shapes/Limit_Castelldefels.shp")
crs(shp_limit)
shp_limit_proj <- st_transform(shp_limit, CRS(new_crs))
crs(shp_limit_proj)
shp_limit_proj_ext <- st_bbox(shp_limit_proj)
bbox_list <- as.list(shp_limit_proj_ext) #listar xmax ymin xmin ymin
library(RColorBrewer)



pal <- colorFactor(c("green", "red", "grey"), domain = c("Actiu", "Inactiu", "Sense Informació"))

# UI

ui <- fluidPage(
  tags$style(type="text/css",
             ".leaflet {height:100%;width:100%} ",
             ".leaflet-top {z-index:9999 !important; }",
             ".leaflet-bottom {z-index:9999 !important; }"
  ),
  leafletOutput(outputId = "map", height = "100vh"),
  absolutePanel(
    draggable = TRUE,
    class= "collapse in",
    top = 50,
    left = 50,
    right = NULL,
    bottom = NULL,
    width = 340,
    height = "auto",
    style = "background-color: white; 
            font-family: Arial;
            opacity: 0.8;
            padding:15px;
            box-shadow: 2px 2px 4px rgba(0,0,0,0.2);",
    tags$img(src = "logosolucionshoritzontal.png",
             width = "100%", 
             height = "100%", 
             style = "max-height: 150px;"),
    fluidRow(
      br(),
      column(12, h4("Cens del comerç a Castelldefels", align="center",style="font-weight: bold;")),
      column(12,
             tags$hr(),
             pickerInput(
               inputId = "stat",
               label = "Estat",
               choices = c("Inactiu", "Actiu", "Sense informació"),
               multiple = TRUE,
               selected = "NULL",
               options = list(
                `none-selected-text` = "Selecciona...")
             ),
             pickerInput(
               inputId = "sector",
               label = "Sector",
               choices = unique(shp_proj$N_SECTOR)[!is.na(unique(shp_proj$N_SECTOR))],
               multiple = TRUE,
               selected = "NULL",
               options = list(
                 `none-selected-text` = "Selecciona...")
             ),
             tags$span(textOutput("n_resultats"), align = "center"),
             br(),
             actionBttn(
               inputId = "cluster",
               label = "Clúster",
               color = "warning",
               style = "unite",
               size = "sm",
               block = FALSE
             ),
             actionBttn(
               inputId = "heatmap",
               label = "Mapa de calor",
               color = "warning",
               style = "unite",
               size = "sm",
               block = FALSE
             ),
             downloadBttn(
               outputId = "downloadInforme",
               label = "Informe",
               color = "success",
               style = "unite",
               size = "sm",
               block = FALSE,
               icon = shiny::icon("download")
             ),
             # downloadButton("downloadInforme","Informe")
      )
    )
  )
)


# ui <- dashboardPage(
#   skin = "black",
#   dashboardHeader(
#     title = "Cens de Comerç a Castelldefels",
#     titleWidth = 350),
#   dashboardSidebar(
#     width = 350,
#     # Filtre ESTAT
#     pickerInput(inputId = "stat",
#                 label = "Estat",
#                 choices = c("Inactiu", "Actiu", "Sense informació"),
#                 multiple = TRUE,
#                 selected = "NULL",
#                 options = list(
#                   # `actions-box` = TRUE,
#                   # `deselect-all-text` = "Cap",
#                   # `select-all-text` = "Tots",
#                   `none-selected-text` = "Selecciona..."
#                 )
#     ),
#     # Filtro SECTOR
#       pickerInput(
#         inputId = "sector",
#         label = "Sector",
#         choices = unique(shp_proj$N_SECTOR)[!is.na(unique(shp_proj$N_SECTOR))],
#         multiple = TRUE,
#         selected = NULL,
#         options = list(
#           `none-selected-text` = "Selecciona..."
#         )
#       ),
#       actionBttn(
#         inputId = "cluster",
#         label = "Clúster",
#         color = "warning",
#         style = "unite",
#         size = "sm",
#         block = FALSE
#       ),
#       actionBttn(
#         inputId = "heatmap",
#         label = "Mapa de calor",
#         color = "warning",
#         style = "unite",
#         size = "sm",
#         block = FALSE
#       ),
#     # actionButton(inputId = "heatmap", label = "Mapa de calor", icon = icon("fire")),
#     #  actionButton(inputId = "cluster", label = "Clúster", icon = icon("map-marker-alt"))
#     downloadButton("informe","Descarrega")
#   ),
#   dashboardBody(
#     fluidRow(
#       box(width = 12, status = "primary", 
#           leafletOutput(outputId = "map", height = "100vh"),
#           
#       )
#     )
#   )
# )

# Server
server <- function(input, output, session) {
  
  labels <- reactive({
    filtered_data <- filteredData()
    paste0(
      "<strong>",filtered_data$NOM_COMERC,"</strong>","<br/>",
      filtered_data$ADREÇA, ", ",
      filtered_data$NUMERO,
      " (",filtered_data$BARRI,")","<br/>",
      filtered_data$N_SECTOR, " - ",
      filtered_data$N_GRUP,"<br/>",
      "Estat: ", filtered_data$N_PRINCIP
    ) %>% lapply(htmltools::HTML)
  })
  
  filteredData <- reactive({
    filtered_data <- shp_proj
    
    if (!is.null(input$stat)) {
      filtered_data <- filtered_data[filtered_data$N_PRINCIP %in% input$stat, ]
    }
    
    if (!is.null(input$sector)) {
      filtered_data <- filtered_data[filtered_data$N_SECTOR %in% input$sector, ]
    }
    
    if (is.null(input$stat) && is.null(input$sector)) {
      filtered_data <- shp_proj
    }
    
    return(filtered_data)
  })
  
  
  #####NUMERO DE RESULTATS#####
  # n_resultats <- reactive({nrow(filteredData())
  # })
  # output$n_resultats <- renderText("Número de resultats: ", n_resultats())
  output$n_resultats <- renderText({
    paste("Número de resultats: ", nrow(filteredData()))
  })
  #####NUMERO DE RESULTATS#####
  
  

  observeEvent(input$cluster, {
    output$map <- renderLeaflet({
      leaflet(filteredData(), options = leafletOptions(attributionControl = FALSE, 
                                                       minZoom = 12,
                                                       zoomControl = FALSE,
                                                        )) %>%
        addProviderTiles(provider = "CartoDB.Positron") %>%
        setView(lng = 1.98, lat = 41.27, zoom = 14) %>%
        setMaxBounds(lng1 = bbox_list$xmin, lat1 = bbox_list$ymin, lng2 = bbox_list$xmax, lat2 = bbox_list$ymax)%>%
          addCircleMarkers(
            group = "Clúster",
            fillColor = ~pal(N_PRINCIP),
            fillOpacity = 0.6,
            stroke = FALSE,
            radius = 6,
            color = "blue",
            clusterOptions = markerClusterOptions(zoomToBoundsOnClick = TRUE,
                                                  spiderfyDistanceMultiplier = 1.5,
                                                  disableClusteringAtZoom = 18),
            label = labels(),
            labelOptions = labelOptions(
              style = list("font-weight" = "normal", padding = "3px 8px"),
              textsize = "14px",
              direction = "auto"
            )
          )
     })
  })

   observeEvent(input$heatmap, {
     filteredDataUpdated <- filteredData()
     output$map <- renderLeaflet({
       leaflet(filteredData(), options = leafletOptions(attributionControl = FALSE, 
                                                        minZoom = 12,
                                                       zoomControl = FALSE,
                                                       )) %>%
         addProviderTiles(provider = "CartoDB.Positron") %>%
         setMaxBounds(lng1 = bbox_list$xmin, lat1 = bbox_list$ymin, lng2 = bbox_list$xmax, lat2 = bbox_list$ymax)%>%
         setView(lng = 1.98, lat = 41.27, zoom = 14) %>%
        addHeatmap(data=filteredDataUpdated,
                   group="Mapa de calor",
                   radius = 5, blur = 10,
                   minOpacity = 0.1,
                   max = 0.5
        )
     })
   })

   current_view <- reactiveVal("markers") # Inicialmente se muestra el mapa de marcadores
   
   observeEvent(input$cluster, {
     current_view("markers")
     # Código para mostrar los marcadores de círculo
   })
   
   observeEvent(input$heatmap, {
     current_view("heatmap")
     # Código para mostrar el mapa de calor
   })

   #### OUTPUT PDF#####
   
   output$downloadInforme <- downloadHandler(
     
     # For PDF output, change this to "report.pdf"
     filename = "report.html",
     content = function(file) {
       # Copy the report file to a temporary directory before processing it, in
       # case we don't have write permissions to the current working dir (which
       # can happen when deployed).
       tempReport <- file.path(tempdir(), "informe.Rmd")
       file.copy("informe.Rmd", tempReport, overwrite = TRUE)
       
       # Set up parameters to pass to Rmd document
       params <- list(n = input$stat)
       
       # Knit the document, passing in the `params` list, and eval it in a
       # child of the global environment (this isolates the code in the document
       # from the code in this app).
       rmarkdown::render(tempReport, output_file = file,
                         params = params,
                         envir = new.env(parent = globalenv())
       )
     }
   )
   
   
   #### OUTPUT #####
   

  output$map <- renderLeaflet({
    leaflet(filteredData(), options = leafletOptions(attributionControl = FALSE, 
                                                     minZoom = 12,
                                                     zoomControl = FALSE, # desactiva los botones de zoom predeterminados
                                                     )) %>%
      addProviderTiles(provider = "CartoDB.Positron") %>%
      setView(lng = 1.98, lat = 41.27, zoom = 14) %>%
      setMaxBounds(lng1 = bbox_list$xmin, lat1 = bbox_list$ymin, lng2 = bbox_list$xmax, lat2 = bbox_list$ymax)%>%
      addCircleMarkers(
        group = "Clúster",
        fillColor = ~pal(N_PRINCIP),
        fillOpacity = 0.6,
        stroke = FALSE,
        radius = 6,
        color = "blue",
        clusterOptions = markerClusterOptions(zoomToBoundsOnClick = TRUE,
                                              spiderfyDistanceMultiplier = 1.5,
                                              disableClusteringAtZoom = 18),
        label = labels(),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "14px",
          direction = "auto"
        )
      )
  })

   observe({
     filteredDataUpdated <- filteredData()
     if (current_view() == "markers") {
       # Código para mostrar los marcadores de círculo
       leafletProxy("map", session = session) %>%
         clearMarkers() %>%
         addCircleMarkers(
           data = filteredDataUpdated,
           group = "Clúster",
           fillColor = ~pal(N_PRINCIP),
           fillOpacity = 0.6,
           stroke = FALSE,
           radius = 6,
           clusterOptions = markerClusterOptions(zoomToBoundsOnClick = TRUE,
                                                 spiderfyDistanceMultiplier = 1.5,
                                                 disableClusteringAtZoom = 18),
           label = labels(),
           labelOptions = labelOptions(
             style = list("font-weight" = "normal", padding = "3px 8px"),
             textsize = "14px",
             direction = "auto"
           )
         )
     } else {
       # Código para mostrar el mapa de calor
       leafletProxy("map", session = session) %>%
         clearHeatmap() %>%
         addHeatmap(data = filteredDataUpdated,
                    group = "Mapa de calor",
                    radius = 5, blur = 10,
                    minOpacity = 0.1,
                    max = 0.5
         )
     }
   })  
} 

shinyApp(ui, server)


