library(shiny)
library(shinythemes)
library(plyr)
library(sp)
library(rgeos)
library(maptools)
library(leaflet)
library(rgdal)
library(proj4)
library(raster)
library(DT)
library(yaml)



east_end <- readShapeSpatial("east_end_parcels.shp")
west_end <- readShapeSpatial("west_end_parcels.shp")
north_side <- readShapeSpatial("north_side_parcels.shp")
south_hills <- readShapeSpatial("south_hill_parcels.shp")
hoods_e <- levels(east_end$Neighborho)
hoods_w <- levels(west_end$Neighborho)
hoods_n <- levels(north_side$Neighborho)
hoods_s <- levels(south_hills$Neighborho)
hood_list <- unlist(list(hoods_e, hoods_w, hoods_n, hoods_s))
hood_list <- sort(hood_list)

ui <- shinyUI(navbarPage(windowTitle = "Burgh's Parcel Viewer",
                         title = HTML('<img src="finance.png" alt="Burghs Eye View" height="85%">'),
                         collapsible = TRUE,
                         theme = shinytheme("flatly"),
                         tabPanel("Map", class = "map",
                                  tags$head(tags$link(rel = "icon", type = "image/png", href="favicon.png")),
                                  tags$style(type="text/css", ".container-fluid {padding:0;}"),
                                  tags$style(type="text/css", ".navbar {margin-bottom:0px;}"),
                                  tags$style(type="text/css", ".navbar-brand {padding-top:0px; padding-bottom: 0px;}"),
                                  tags$style(type = "text/css", "#map {height: calc(100vh - 60px) !important;}"),
                                  tags$style(type="text/css", ".leaflet-popup-content {overflow-y: auto; max-height: 400px !important;}"),
                                  tags$head(tags$style(type="text/css", '.Map {
                                                       background-image: url("loading.png");
                                                       background-repeat: no-repeat;
                                                       background-position: center;
                                                       background-size: contain;
                                                       }')),
                          tags$style(type= "text/css", ".form-group {
                                     margin-bottom: 0px;
                                     }"),
                          leafletOutput("map"),
                          absolutePanel(
                            top = 70, left = 50, width = '300px',
                            wellPanel(id = "tPanel", style = "overflow-y:scroll; max-height: calc(100vh - 85px) !important; overflow: visible;",
                                      HTML("<br>"),
                                      selectInput("neigh_select",
                                                  label = "Neighborhood",
                                                  choices = hood_list,
                                                  selected = "Central Business District",
                                                  multiple = FALSE,
                                                  selectize = TRUE),
                                      textInput("search",
                                                label = NULL,
                                                placeholder = "Parcel Search")
                            ), style = "opacity: 0.88"
                          )
                                  ),
                         tabPanel("Data", class = "data",
                                  inputPanel(
                                    downloadButton("downloadData", "Export Report"),
                                    HTML('<ul>
                                         <li>Data displayed here represents selections made on the map
                                         </ul>')
                                    ),
  div(style = 'overflow-x: scroll', DT::dataTableOutput("datatable"))
                                  ),
                         tabPanel('About',
                                  HTML('<h2>About</h2>
                                       <ul>
                                       <li>This map was primarily designed for property process improvement for the Department of Finance.
                                       <li>Delinquent tax and property abatement data is extracted from the RealEstate database. 
                                       <li>2017 taxes are calculated based on assessed value, extracted from the Western Pennsylvania Regional Data Center. 
                                       <li>Shapefiles are extracted from the Western Pennsylvania Regional Data Center.
                                       <li>Approximately 10% of City parcels are not mapped due to unmatched City/County parcel identifiers in RealEstate.
                                       </ul>
                                       <br>
                                       <h2 align="center">Acknowledgements</h2>
                                       <p align="center"><i> This Map was generated by the City of Pittsburgh Department of Innovation & Performance in partnership with the Department of Finance.</p></i><br>')
                                  ),
                         #tabPanel("Recent Updates"),
                         tabPanel("More DoF Resources",
                                  HTML('<h2>Resources:</h2>
                                       <ul>
                                       <li><a href="http://rstudio.city.pittsburgh.pa.us:3838/sample-apps/Finance/Auditor%20Collection/">Finance Dashboard</a>
                                        <li><a href="http://rstudio.city.pittsburgh.pa.us:3838/sample-apps/Finance/Tax%20Delinquency/">Tax Delinquency Dashboard</a>
                                       </ul>'))
                         ))




# Define server logic required to draw a histogram
server <- shinyServer(function(input, output, session) {
  
  observe({
    # Trigger this observer every time an input changes
    reactiveValuesToList(input)
    session$doBookmark()
  })
  #Update page URL
  onBookmarked(function(url) {
    updateQueryString(url)
  })
  
  east_data <- reactive({
    east_data <- east_end
    
    if(nchar(input$search) > 14){
      east_data <- subset(east_data, pin == input$search | CITY_PIN == input$search)
    } else {
      east_data <- subset(east_data, Neighborho == input$neigh_select)
    }
    
    return(east_data)
  })
  
  west_data <- reactive({
    west_data <- west_end
    
    if(nchar(input$search) > 14){
      west_data <- subset(west_data, pin == input$search | CITY_PIN == input$search)
    } else {
      west_data <- subset(west_data, Neighborho == input$neigh_select)
    }
    
    return(west_data)
  })
  
  north_data <- reactive({
    north_data <- north_side
    
    if(nchar(input$search) > 14){
      north_data <- subset(north_data, pin == input$search | CITY_PIN == input$search)
    } else {
      north_data <- subset(north_data, Neighborho == input$neigh_select)
    }
    
    return(north_data)
  })
  
  south_data <- reactive({
    south_data <- south_hills
    
    if(nchar(input$search) > 14){
      south_data <- subset(south_data, pin == input$search | CITY_PIN == input$search)
    } else {
      south_data <- subset(south_data, Neighborho == input$neigh_select)
      
    }
    
    return(south_data)
  })
  
  
  output$map <- renderLeaflet({
    east_parcel <- east_data()
    west_parcel <- west_data()
    north_parcel <- north_data()
    south_parcel <- south_data()
    
    map <- leaflet() %>%
      addProviderTiles("OpenStreetMap.HOT",
                       options = providerTileOptions(noWrap = TRUE, minZoom = 1
                       )
      ) %>%
      addLegend("bottomright", colors = c("#19eebe", "#a820b8", "#f2f403", "#f20303"), labels = c("Abated", "URA Owned", "City Owned", "Delinquent"))
    
    
    if(nrow(east_parcel) > 0){ 
      print(nrow(east_parcel))
      map <- addPolygons(map, data = east_parcel,
                         stroke = TRUE, smoothFactor = 0.5, weight = 0.5, color = "#000000",
                         fill = TRUE, fillColor = ~color_val, fillOpacity = .5,
                         popup = ~(paste("<font color='black'><b>Parcel ID</b>", east_parcel$CITY_PIN,
                                         "<br><b>Owner</b>", east_parcel$OWNER,
                                         "<br><b>Address</b>", east_parcel$ADDRESS,
                                         "<br><b>Neighborhood</b>", east_parcel$Neighborho,
                                         "<br><b>Ward</b>", east_parcel$MUNIDESC,
                                         "<br><b>Owner Description</b>", east_parcel$OWNERDESC,
                                         "<br><b>Class Description</b>", east_parcel$CLASSDESC,
                                         "<br><b>Use Description</b>", east_parcel$USEDESC,
                                         "<br><b>Most Recent Sale Date</b>", east_parcel$SALEDATE,
                                         "<br><b>Most Recent Sale Price</b>", east_parcel$SALEPRICE,
                                         "<br><b>County Land Value</b>", east_parcel$COUNTYLAND,
                                         "<br><b>County Building Value</b>", east_parcel$COUNTYBUIL,
                                         "<br><b>County Total Value</b>", east_parcel$COUNTYTOTA,
                                         "<br><b>'17 City Taxes</b>", east_parcel$city_tax,
                                         "<br><b>'17 School Taxes</b>", east_parcel$school_tax,
                                         "<br><b>'17 Library Taxes</b>", east_parcel$lib_tax,
                                         "<br><b>Current Delinquent Taxes</b>", east_parcel$CURRENT_DE,
                                         "<br><b>Abatement Program</b>", east_parcel$PROGRAM_NA,
                                         "<br><b>Abatement Start Year</b>", east_parcel$START_YEAR,
                                         "<br><b>Abatement Period</b>", east_parcel$ABATEMENT_,
                                         "<br><b>Abatement Approved By:</b>", east_parcel$APPROVE_U, "</font><br>",
                                         paste0('<center><img id="imgPicture" src="http://photos.county.allegheny.pa.us/iasworld/iDoc2/Services/GetPhoto.ashx?parid=',east_parcel$pin, '&amp;jur=002&amp;Rank=1&amp;size=350x263" style="width:250px;"></center>'))))
    }
    if(nrow(west_parcel) > 0){ 
      print(nrow(west_parcel))
      map <- addPolygons(map, data = west_parcel,
                         stroke = TRUE, smoothFactor = 0.5, weight = 0.5, color = "#000000",
                         fill = TRUE, fillColor = ~color_val, fillOpacity = .5,
                         popup = ~(paste("<font color='black'><b>Parcel ID</b>", west_parcel$CITY_PIN,
                                         "<br><b>Owner</b>", west_parcel$OWNER,
                                         "<br><b>Address</b>", west_parcel$ADDRESS,
                                         "<br><b>Neighborhood</b>", west_parcel$Neighborho,
                                         "<br><b>Ward</b>", west_parcel$MUNIDESC,
                                         "<br><b>Owner Description</b>", west_parcel$OWNERDESC,
                                         "<br><b>Class Description</b>", west_parcel$CLASSDESC,
                                         "<br><b>Use Description</b>", west_parcel$USEDESC,
                                         "<br><b>Most Recent Sale Date</b>", west_parcel$SALEDATE,
                                         "<br><b>Most Recent Sale Price</b>", west_parcel$SALEPRICE,
                                         "<br><b>County Land Value</b>", west_parcel$COUNTYLAND,
                                         "<br><b>County Building Value</b>", west_parcel$COUNTYBUIL,
                                         "<br><b>County Total Value</b>", west_parcel$COUNTYTOTA,
                                         "<br><b>'17 City Taxes</b>", west_parcel$city_tax,
                                         "<br><b>'17 School Taxes</b>", west_parcel$school_tax,
                                         "<br><b>'17 Library Taxes</b>", west_parcel$lib_tax,
                                         "<br><b>Current Delinquent Taxes</b>", west_parcel$CURRENT_DE,
                                         "<br><b>Abatement Program</b>", west_parcel$PROGRAM_NA,
                                         "<br><b>Abatement Start Year</b>", west_parcel$START_YEAR,
                                         "<br><b>Abatement Period</b>", west_parcel$ABATEMENT_,
                                         "<br><b>Abatement Approved By:</b>", west_parcel$APPROVE_U, "</font><br>",
                                         paste0('<center><img id="imgPicture" src="http://photos.county.allegheny.pa.us/iasworld/iDoc2/Services/GetPhoto.ashx?parid=',west_parcel$pin, '&amp;jur=002&amp;Rank=1&amp;size=350x263" style="width:250px;"></center>'))))
    }
    if(nrow(north_parcel) > 0){ 
      print(nrow(north_parcel))
      map <- addPolygons(map, data = north_parcel,
                         stroke = TRUE, smoothFactor = 0.5, weight = 0.5, color = "#000000",
                         fill = TRUE, fillColor = ~color_val, fillOpacity = .5,
                         popup = ~(paste("<font color='black'><b>Parcel ID</b>", north_parcel$CITY_PIN,
                                         "<br><b>Owner</b>", north_parcel$OWNER,
                                         "<br><b>Address</b>", north_parcel$ADDRESS,
                                         "<br><b>Neighborhood</b>", north_parcel$Neighborho,
                                         "<br><b>Ward</b>", north_parcel$MUNIDESC,
                                         "<br><b>Owner Description</b>", north_parcel$OWNERDESC,
                                         "<br><b>Class Description</b>", north_parcel$CLASSDESC,
                                         "<br><b>Use Description</b>", north_parcel$USEDESC,
                                         "<br><b>Most Recent Sale Date</b>", north_parcel$SALEDATE,
                                         "<br><b>Most Recent Sale Price</b>", north_parcel$SALEPRICE,
                                         "<br><b>County Land Value</b>", north_parcel$COUNTYLAND,
                                         "<br><b>County Building Value</b>", north_parcel$COUNTYBUIL,
                                         "<br><b>County Total Value</b>", north_parcel$COUNTYTOTA,
                                         "<br><b>'17 City Taxes</b>", north_parcel$city_tax,
                                         "<br><b>'17 School Taxes</b>", north_parcel$school_tax,
                                         "<br><b>'17 Library Taxes</b>", north_parcel$lib_tax,
                                         "<br><b>Current Delinquent Taxes</b>", north_parcel$CURRENT_DE,
                                         "<br><b>Abatement Program</b>", north_parcel$PROGRAM_NA,
                                         "<br><b>Abatement Start Year</b>", north_parcel$START_YEAR,
                                         "<br><b>Abatement Period</b>", north_parcel$ABATEMENT_,
                                         "<br><b>Abatement Approved By:</b>", north_parcel$APPROVE_U, "</font><br>",
                                         paste0('<center><img id="imgPicture" src="http://photos.county.allegheny.pa.us/iasworld/iDoc2/Services/GetPhoto.ashx?parid=',north_parcel$pin, '&amp;jur=002&amp;Rank=1&amp;size=350x263" style="width:250px;"></center>'))))
    } 
    if(nrow(south_parcel) > 0){ 
      print(nrow(south_parcel))
      map <- addPolygons(map, data = south_parcel,
                         stroke = TRUE, smoothFactor = 0.5, weight = 0.5, color = "#000000",
                         fill = TRUE, fillColor = ~color_val, fillOpacity = .5,
                         popup = ~(paste("<font color='black'><b>Parcel ID</b>", south_parcel$CITY_PIN,
                                         "<br><b>Owner</b>", south_parcel$OWNER,
                                         "<br><b>Address</b>", south_parcel$ADDRESS,
                                         "<br><b>Neighborhood</b>", south_parcel$Neighborho,
                                         "<br><b>Ward</b>", south_parcel$MUNIDESC,
                                         "<br><b>Owner Description</b>", south_parcel$OWNERDESC,
                                         "<br><b>Class Description</b>", south_parcel$CLASSDESC,
                                         "<br><b>Use Description</b>", south_parcel$USEDESC,
                                         "<br><b>Most Recent Sale Date</b>", south_parcel$SALEDATE,
                                         "<br><b>Most Recent Sale Price</b>", south_parcel$SALEPRICE,
                                         "<br><b>County Land Value</b>", south_parcel$COUNTYLAND,
                                         "<br><b>County Building Value</b>", south_parcel$COUNTYBUIL,
                                         "<br><b>County Total Value</b>", south_parcel$COUNTYTOTA,
                                         "<br><b>'17 City Taxes</b>", south_parcel$city_tax,
                                         "<br><b>'17 School Taxes</b>", south_parcel$school_tax,
                                         "<br><b>'17 Library Taxes</b>", south_parcel$lib_tax,
                                         "<br><b>Current Delinquent Taxes</b>", south_parcel$CURRENT_DE,
                                         "<br><b>Abatement Program</b>", south_parcel$PROGRAM_NA,
                                         "<br><b>Abatement Start Year</b>", south_parcel$START_YEAR,
                                         "<br><b>Abatement Period</b>", south_parcel$ABATEMENT_,
                                         "<br><b>Abatement Approved By:</b>", south_parcel$APPROVE_U, "</font><br>",
                                         paste0('<center><img id="imgPicture" src="http://photos.county.allegheny.pa.us/iasworld/iDoc2/Services/GetPhoto.ashx?parid=',south_parcel$pin, '&amp;jur=002&amp;Rank=1&amp;size=350x263" style="width:250px;"></center>'))))
    }
    
    if (nrow(south_parcel) + nrow(north_parcel) + nrow(west_parcel) + nrow(east_parcel) == 0) {
      map <- map %>%
        setView(-79.9959, 40.4406, zoom = 8)
    }
    map
  })
  
  ##Data Tables
  dataexport <- reactive({
    total_parcels <- rbind(east_data(), west_data(), north_data(), south_data(), makeUniqueIDs = TRUE)
    total_parcels <- subset(total_parcels, select = c("CITY_PIN", "OWNER", "ADDRESS", "Neighborho", "TAXDESC", "OWNERDESC", "CLASSDESC",
                                                      "USEDESC", "SALEDATE", "SALEPRICE", "COUNTYBUIL", "COUNTYLAND", "COUNTYTOTA", "HOMESTEAD",
                                                      "CURRENT_DE", "PROGRAM_NA", "ABATEMENT_", "START_YEAR", "APPROVED_U", "city_tax", "school_tax", "lib_tax"))
    total_parcels <- as.data.frame(total_parcels)
    colnames(total_parcels) <- c("PIN", "OWNER", "ADDRESS", "NEIGH", "TAXDESC", "OWNERDESC", "CLASSDESC", "USEDESC", "SALEDATE", "SALEPRICE", "BUILDVAL", 
                                 "LANDVAL", "TOTALVAL", "HOMESTD", "CURRENT_DELINQ", "ABATEMENT_PROG", "ABATE_LENGTH", "ABATE_START", "APPROVED_USER",
                                 "CITY_TAX", "SCHOOL_TAX", "LIB_TAX")
    return(total_parcels)
  })
  output$datatable <- DT::renderDataTable({
    dataexport()
    
  }, filter = "top", options = list(pageLength = 15,
                   dom = "tp",
                   lengthMenu = c(15, 30, 45),
                   #scrollX = TRUE,
                   initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css({'background-color': '#95a5a6'});",
                     "}"),
                   searchHighlight = TRUE), 
  class = 'cell-border stripe',
  rownames = FALSE,
  escape = FALSE
  )  
  
  
  
  output$downloadData <- downloadHandler(
    filename = "Property_export.csv",
    content = function(file) {
      write.csv(dataexport(), file)
    }
  )
  

  
})

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")

