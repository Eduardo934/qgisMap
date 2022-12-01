

library(shiny)
library(sf)
library(mapview)
library(leaflet)
library(leaflet.extras)
library(dplyr)

library(RPostgreSQL)
library(RPostgres)
library(DBI)

conn <- dbConnect(
  RPostgres::Postgres(),
  dbname = "QgisTest",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "postgres"
)

ui <- fillPage(

    div(leafletOutput("mapa", width = "100%", height = "100%"), style="width:100%; height:100%")
)

# Server functions
server <- function(input, output) {
  
  reactiveData <- reactiveValues()
  reactiveData$layers <- st_read(dsn = conn, geometry_column = "geom", EWKB = TRUE, query = "SELECT * FROM viastest") %>%
    st_transform(4326)
  

  output$mapa <- renderLeaflet({
    
    newData <- reactiveData$layers
    
    ## Non editable layers
    parcelas_rusticas <- st_read(dsn = conn, geometry_column = "geom", EWKB = TRUE, query = "SELECT * FROM parcelas_rusticas") %>%
      st_transform(4326)
    
    mapa<-leaflet() %>% 
      addPolygons(data = newData , color="blue", group = "layerA", dashArray= "20, 20",
                  popup = paste(
                                "via_id: ", newData$via_id, "<br>",
                                "masa: ", newData$masa, "<br>",
                                "parcela: ", newData$parcela, "<br>",
                                "<button onclick='Shiny.onInputChange(\"remove_feature\",  Math.random())' id='removeFeature' type='button' class='btn btn-default action-button'>Remove</button>",
                                "<button onclick='Shiny.onInputChange(\"update_feature\",  Math.random())' id='updateFeature' type='button' class='btn btn-default action-button'>Update</button>"
                  ), layerId = newData$via_id) %>%
      addProviderTiles(providers$OpenTopoMap, group = "OpenTopoMap") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri.WorldImagery") %>%
      addProviderTiles(providers$CartoDB.Positron, group = "CartoDB.Positron") %>%
      addLayersControl(
        baseGroups = c("OpenTopoMap", "Esri.WorldImagery", "CartoDB.Positron"),
        overlayGroups = c("layerA", "limites", "vias_urbanas", "inmuebles_urbanos","inmuebles_rusticos"),
        options = layersControlOptions(collapsed = TRUE)
      ) %>% 
      addDrawToolbar(
        targetGroup = "layerA",
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()),
        position="bottomleft")
    
    mapa
    
    
  })
  
  ### Mapa proxy
  map_proxy<-leaflet::leafletProxy("mapa")
  
  
  observeEvent(input$mapa_draw_new_feature,{
    
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
    
    dataFromDB <- dbGetQuery(conn, "SELECT * FROM viastest")
    
    newElement <- poligono %>% st_cast("MULTIPOLYGON") %>% st_transform(4326) %>%
      mutate(via_id = max(dataFromDB$via_id) + 1, masa=100, parcela = 100, area = 100) %>% 
      select(via_id, masa, parcela, area, geometry) %>%
      rename(geom=geometry)
    
    dbWriteTable(conn, "viastest", newElement,  append=TRUE,row.names = FALSE)
    
    newData <- st_read(dsn = conn, geometry_column = "geom", EWKB = TRUE, query = "SELECT * FROM viastest") %>%
      st_transform(4326)
    
    reactiveData$layers <- newData
    
  })
  
  
  observeEvent(input$remove_feature,{
    removedFeature <- editedFeature$featureId
    
    dbGetQuery(conn, paste0("DELETE FROM viastest WHERE via_id = ", removedFeature))
    
    #Update map
    newData <- st_read(dsn = conn, geometry_column = "geom", EWKB = TRUE, query = "SELECT * FROM viastest") %>%
      st_transform(4326)
    
    map_proxy%>%clearGroup("layerA")%>%
      clearShapes() %>%
      addPolygons(data = newData , color="blue", group = "layerA",
                  popup = paste(
                    "via_id: ", newData$via_id, "<br>",
                    "masa: ", newData$masa, "<br>",
                    "parcela: ", newData$parcela, "<br>",
                    "<button onclick='Shiny.onInputChange(\"remove_feature\",  Math.random())' id='removeFeature' type='button' class='btn btn-default action-button'>Remove</button>",
                    "<button onclick='Shiny.onInputChange(\"update_feature\",  Math.random())' id='updateFeature' type='button' class='btn btn-default action-button'>Update</button>"
                  ), layerId = newData$via_id)
    
    
  })
  
  editedFeature <- reactiveValues()
  observe({
    print(input$mapa_shape_click)
    updatedFeature <- input$mapa_shape_click[["id"]]
    editedFeature$featureId <- updatedFeature
    
    print(editedFeature$featureId)
  })
  
  observeEvent(input$update_feature,{
    req(editedFeature$featureId)
    newData <- st_read(dsn = conn, geometry_column = "geom", 
                       EWKB = TRUE, 
                       query = paste0("SELECT * FROM viastest WHERE via_id = ", editedFeature$featureId)) %>%
      st_transform(4326)
    
    
    map_proxy%>%clearGroup("layerA")%>%
      clearShapes() %>%
      addPolygons(data = newData , color="blue", group = "layerA",
                  popup = paste(
                    "via_id: ", newData$via_id, "<br>",
                    "masa: ", newData$masa, "<br>",
                    "parcela: ", newData$parcela, "<br>",
                    "<button onclick='Shiny.onInputChange(\"remove_feature\",  Math.random())' id='removeFeature' type='button' class='btn btn-default action-button'>Remove</button>",
                    "<button onclick='Shiny.onInputChange(\"update_feature\",  Math.random())' id='updateFeature' type='button' class='btn btn-default action-button'>Update</button>"
                  ), layerId = newData$via_id)
    
    
  })
  
  observeEvent(input$mapa_draw_edited_features,{
    pol <- input$mapa_draw_edited_features
    
    coor<-unlist(pol[["features"]][[1]][["geometry"]][["coordinates"]])
    datap<-data.frame(
      Longitud=coor[seq(1,length(coor), 2)], 
      Latitud=coor[seq(2,length(coor), 2)]
    )
    poligono<-datap %>%
      st_as_sf(coords = c("Longitud", "Latitud"), crs = 4326) %>%
      summarise(geometry = st_combine(geometry)) %>%
      st_cast("POLYGON")
    poligono <- poligono %>% st_cast("MULTIPOLYGON")
    geoemtryPolygon <- st_as_text(poligono$geometry)
    print(geoemtryPolygon)
    dbGetQuery(conn, paste0("UPDATE viastest 
  SET geom=
  ST_GeomFromText('",geoemtryPolygon,"',4326) WHERE via_id = ",editedFeature$featureId,";"))
    
    
    # Update the map 
    newData <- st_read(dsn = conn, geometry_column = "geom", EWKB = TRUE, query = "SELECT * FROM viastest") %>%
      st_transform(4326)
    
    map_proxy%>%clearGroup("layerA")%>%
      clearShapes() %>%
      addPolygons(data = newData , color="blue", group = "layerA",
                  popup = paste(
                    "via_id: ", newData$via_id, "<br>",
                    "masa: ", newData$masa, "<br>",
                    "parcela: ", newData$parcela, "<br>",
                    "<button onclick='Shiny.onInputChange(\"remove_feature\",  Math.random())' id='removeFeature' type='button' class='btn btn-default action-button'>Remove</button>",
                    "<button onclick='Shiny.onInputChange(\"update_feature\",  Math.random())' id='updateFeature' type='button' class='btn btn-default action-button'>Update</button>"
                  ), layerId = newData$via_id)
    
    
  })
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
