library(shiny)
library(shinythemes)
library(shinycssloaders)
library(leaflet)
library(dplyr)
library(glue)
library(shinyscreenshot)
library(sf)
library(stringr)
library(magrittr)

#Reading in Data
data <- readRDS("data/ehsaas_clean.rds")

#Reading in Shape file
pak_shp <- readRDS("data/pak_shp.rds")

#User Interface
ui <- navbarPage(tags$strong(tags$em("EHSAAS DASHBOARD")),
                 tabPanel("INTERACTIVE MAPS",     
                          bootstrapPage(theme = shinytheme("flatly")),
                          
                          tags$style(type = 'text/css', '#map {height: calc(100vh - 80px) !important;}', style= 'padding-top:0px;'),
                          leafletOutput("map") %>% 
                            withSpinner(),
                          br(),
                          tags$head(tags$style("#source{color:black; font-size:12px; font-style:italic; max-height: 110px; background: #ffe6cc; }")),
                          verbatimTextOutput("source"),
                          
                          absolutePanel(id = "controls", class = "panel panel-default", fixed= TRUE,
                                        draggable = TRUE, bottom = "auto", right = "auto", left = 70, top = 95,
                                        width = 260, height = "auto",
                                        style = "background-color: white;
                                                   opacity: 0.85;
                                                   padding: 20px 20px 20px 20px;
                                                   margin: auto;
                                                   border-radius: 5pt;
                                                   box-shadow: 0pt 0pt 6pt 0px rgba(61,59,61,0.48);
                                                   padding-bottom: 2mm;
                                                   padding-top: 1mm;",
                                        # br(),
                                        selectInput("cat_map",
                                                    "Choose Category",
                                                    choices = unique(data$category)),
                                        
                                        selectInput("stat_map",
                                                    "Choose Indicator",
                                                    choices = (unique(data$indicator)),
                                                    selected= (unique(data$indicator)[2])),
                                        
                                        # h6(tags$b(tags$em("Use the Radio-Buttons below to change the color-pallette using Actual Values or Deciles"))),
                                        
                                        # radioButtons("pallettes", "Change Color Scheme based on", inline =TRUE,
                                        #              choices = list("Values" = "pallette1", "Deciles" = "pallette2")),
                                        
                                        # radioButtons("polygon", "Switch b/w Tehsils and Districts", inline = TRUE,
                                        #              choices = list("Districts" = "polygon1", "Tehsils" = "polygon2")),
                                        
                                        h6(tags$b(tags$em("Use the buttons below to download the data underlying the current on-screen map and screenshot"))),
                                        downloadButton("mapdata", "Get Data", class = "btn-sm"),
                                        actionButton("screenshot", "Screenshot", class= "btn-sm", icon = icon("camera")),
                                        br()
                                        
                          )
                 ),
                 # style = 'padding-left:0px;',
                 # tags$head(tags$style("#source{color:black; font-size:12px; font-style:italic; max-height: 120px; background: yellow;}")),
                 # verbatimTextOutput("source")
)

server <- function(input, output, session){
  
  # d0 <- reactive({
  #   data %>% 
  #     filter(
  #       category == input$cat_map)
  # })
  
  d1 <- reactive({
    data %>% 
      filter(
        category == input$cat_map,
        indicator == input$stat_map)
  })
  
  # map_1 <- reactive({
    
    #Labelling for the Map       
    labels <- reactive({
      paste0(glue::glue("<b> District: <b> { str_to_title(unique(pak_shp$district)) }</br>"), glue::glue("<b> { d1()$indicator }: </b>"), " ", glue::glue("{ round(d1()$value, 2) }"), " ", unique(d1()$unit) , sep = "") %>%
        lapply(htmltools::HTML)
    })
    
    pal <- reactive({
      colorBin(palette = c('#d7191c','#fdae61','#ffffbf','#abd9e9','#2c7bb6'), 
               bins=7, na.color = "grey",  
               domain= d1()$value, 
               reverse=T)
    })
    
    # st_transform(pak_shp, crs = "+init=epsg:4326") %>%   #pak_shp, crs = "+init=epsg:4326" , "+proj=longlat +ellps=WGS84 +datum=WGS84"
    #Lealfet
    output$map <- renderLeaflet({
      # message("rendering map")
      leaflet(options = leafletOptions(zoomSnap = 0.20, zoomDelta = 0.20)) %>%
        addProviderTiles(providers$CartoDB, group = "CARTO") %>%
        setView(lng=69.5, lat = 30.5, zoom = 5.2)
    })
    
    
    # leaflet(pak_shp, options = leafletOptions(zoomSnap = 0.20, 
    #                                           zoomDelta = 0.20)) %>% 
    #   addProviderTiles(providers$Esri, group = "ESRI") %>% 
    #   addProviderTiles(providers$OpenStreetMap , group = "OpenStreetMap") %>% 
    #   addProviderTiles(providers$Stamen.Terrain,
    #                    options = tileOptions(minZoom = 0,
    #                                          maxZoom = 13),
    #                    group = "ST Terrain") %>%
    #   addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
    #   addProviderTiles(providers$Esri.WorldImagery , group = "ESRI IMG") %>% 
    #   addProviderTiles(providers$NASAGIBS.ViirsEarthAtNight2012 , group = "NASA Nightlights") %>% 
    #   
    #   setView(lng=69, lat = 31, zoom = 5.12) %>% 
    
    observe({
      
      req(input$stat_map)
      
      leafletProxy("map", 
                   data= st_transform(pak_shp, 
                                      crs=4326)) %>% 
        addPolygons(label= labels(),
                    labelOptions = labelOptions(
                      style = list("font-weight"= "normal",   
                                   padding= "3px 8px",
                                   "color"= "#cc4c02"), 
                      textsize= "15px",
                      direction = "auto"
                    ),
                    fillColor =  ~pal()(d1()$value),
                    fillOpacity = 0.9,
                    stroke = TRUE,
                    color= "white",
                    weight = 1,
                    opacity = 0.7,
                    fill = TRUE,
                    dashArray = NULL,
                    smoothFactor = 0.5,
                    highlightOptions = highlightOptions(weight= 5,
                                                        fillOpacity = 1,
                                                        opacity= 1,
                                                        bringToFront = TRUE), 
                    group = "Polygons") %>% 
      
      # addLayersControl(baseGroups = c("ESRI", "OpenStreetMap", "ST Terrain", "Toner Lite","ESRI IMG", "NASA Nightlights"),
      #                  overlayGroups = c("Polygons"),
      #                  options = layersControlOptions(collapsed = TRUE)) %>% 
      # 
      addMeasure() %>% 
        addScaleBar("bottomright") 
      
      leafletProxy("map", data= st_transform(pak_shp, 
                                             crs=4326)) %>%
        clearControls() %>%
        addLegend("bottomright",
                  pal= pal(),
                  values= ~d1()$value,
                  title = glue("Legend", " ", "{ unique(d1()$unit)}"),
                  opacity= 1,
                  labFormat = labelFormat(
                    between = "  :  ",
                    digits = 2))
      
      # addLegend("bottomright",
      #         pal= pal() ,
      #         values= ~d1()$value,
      #         title = glue("Legend", " ", "{ unique(d1()$unit)}"),
      #         opacity= 1)
    })
    
    map_2 <- reactive({
      
      #Labelling for the Map       
      labels <- reactive({
        paste0(glue::glue("<b> District: <b> { str_to_title(unique(pak_shp$district)) }</br>"), glue::glue("<b> { d1()$indicator }: </b>"), " ", glue::glue("{ round(d1()$value, 2) }") , " " , unique(d1()$unit), sep = "") %>%
          lapply(htmltools::HTML)
      })
      
      pal <- reactive({
        colorQuantile(palette = c('#a50026','#d73027','#f46d43','#fdae61','#fee090','#e0f3f8','#abd9e9','#74add1','#4575b4','#313695'), n= 10 , na.color = "grey",  domain= unique(d1()$value), reverse=T)
      }) 
      
      # st_transform(pak_shp, crs = "+init=epsg:4326") %>%   #pak_shp, crs = "+init=epsg:4326" , "+proj=longlat +ellps=WGS84 +datum=WGS84"
      leaflet(pak_shp, options = leafletOptions(zoomSnap = 0.20, 
                                                zoomDelta = 0.20)) %>% 
        addProviderTiles(providers$Esri, group = "ESRI") %>% 
        addProviderTiles(providers$OpenStreetMap , group = "OpenStreetMap") %>% 
        addProviderTiles(providers$Stamen.Terrain,
                         options = tileOptions(minZoom = 0,
                                               maxZoom = 13),
                         group = "ST Terrain") %>%
        addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
        addProviderTiles(providers$Esri.WorldImagery , group = "ESRI IMG") %>% 
        addProviderTiles(providers$NASAGIBS.ViirsEarthAtNight2012 , group = "NASA Nightlights") %>% 
        
        setView(lng=69, lat = 31, zoom = 5.12) %>% 
        addPolygons(label= labels(),
                    labelOptions = labelOptions(
                      style = list("font-weight"= "normal",   
                                   padding= "3px 8px",
                                   "color"= "#cc4c02"), 
                      textsize= "15px",
                      direction = "auto"
                    ),
                    fillColor =  ~pal()(d1()$value),
                    fillOpacity = 0.9,
                    stroke = TRUE,
                    color= "white",
                    weight = 1,
                    opacity = 0.7,
                    fill = TRUE,
                    dashArray = NULL,
                    smoothFactor = 0.5,
                    highlightOptions = highlightOptions(weight= 5,
                                                        fillOpacity = 1,
                                                        opacity= 1,
                                                        bringToFront = TRUE), 
                    group = "Polygons") %>%
        
        addLayersControl(baseGroups = c("ESRI", "OpenStreetMap", "ST Terrain", "Toner Lite","ESRI IMG", "NASA Nightlights"),
                         overlayGroups = c("Polygons"),
                         options = layersControlOptions(collapsed = TRUE)) %>% 
        
        addMeasure() %>% 
        addScaleBar("bottomright") %>%
        addLegend("bottomright",
                  pal= pal() ,
                  values= ~d1()$value,
                  title = "Deciles",
                  opacity= 1)
    })
    
    #Rendering both maps
    observeEvent(input$pallettes,{
      if(input$pallettes == "pallette1"){
        map_1()
        output$map <- renderLeaflet({
          map_1()
        })
      }
      if(input$pallettes == "pallette2"){
        map_2()
        output$map <- renderLeaflet({
          map_2()
        })
      }
    })
    
    #Main Map screenshot
    observeEvent(input$screenshot,{
      screenshot(filename = glue("{ input$stat_map }", " ", "map_screenshot"), selector = "#map", scale = 0.8, timer = 1)
      
    })
    #Main map current on-screen data download
    output$mapdata <- downloadHandler(
      filename = function(){
        paste(glue("{ input$stat_map }"), ".csv")
      },
      content = function(file){
        write.csv(d1(), file)
      }
    )
    #Source
    output$source <- renderText({
      glue(
        "{ unique(d1()$source2) }",
        "\n",
        "{ unique(d1()$source1) }"
        
        
      )
    })
    
  }
  
  shinyApp(ui, server)