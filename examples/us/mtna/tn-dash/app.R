library(shiny)
library(rds.r)
library(geojsonio)
library(ggplot2)
library(leaflet)

## CASE DATA
# Get Case Data from RDS
covidServer <- get.rds("https://covid19.richdataservices.com/rds")
tnCatalog <- getCatalog(covidServer, "us_tn")
countyCases <- getDataProduct(tnCatalog, "us_tn_doh_county")
countyCaseData <-
  rds.tabulate(
    countyCases,
    dimensions = c("date_stamp", "us_county_fips"),
    measures = c("cnt_total:SUM(cnt_total)",
                 "cnt_active:SUM(cnt_active)",
                 "cnt_hospitalized:SUM(cnt_hospitalized)"),
    where = "(us_county_fips!=NULL)",
    totals = F
  )


# Create a subset for the latest date
indx <-max(countyCaseData@records$date_stamp, na.rm = TRUE)
latestCaseData <- countyCaseData@records[with(countyCaseData@records, date_stamp==indx),]

# Get the GeoJSON
spdf <-
  geojson_read(
    "https://raw.githubusercontent.com/deldersveld/topojson/master/countries/us-states/TN-47-tennessee-counties.json",
    what = "sp"
  )

# Merge the latest counts with the GeoJSON
spdf@data <-  merge(spdf@data, latestCaseData, by.x = "GEOID", by.y = "us_county_fips", sort = F)

# Get state level aggregate level by date
stateCaseData <-
  rds.tabulate(
    countyCases,
    dimensions = c("date_stamp"),
    measures = c(
      "cnt_total:SUM(cnt_total)",
      "cnt_active:SUM(cnt_active)",
      "cnt_hospitalized:SUM(cnt_hospitalized)"
    ),
    where = "(us_county_fips!=NULL)",
    totals = F
  )

# Get the county fips classification
countyFips <- getClassification(countyCases, "us_county_fips")

# County variables
countyVariables <-
  getVariables(
    countyCases,
    cols = "cnt_total,cnt_active,cnt_hospitalized"
  )

# county labels and descriptions
countyVarFrame <- data.frame()
for(variable in countyVariables){
  countyVarFrame <- rbind(countyVarFrame, data.frame(label=variable@label, description=variable@description, stringsAsFactors = F))
}

## MOBILITY DATA
# Get Mobility Data from RDS
usCatalog <- getCatalog(covidServer, "us")
googleMobility <- getDataProduct(usCatalog, "google_mobility_us_county")
mobilityData <-
  rds.tabulate(
    googleMobility,
    dimensions = c("date_stamp", "us_county_fips"),
    measures = c(
      "retail_recreation_pct:SUM(retail_recreation_pct)",
      "grocery_pharmacy_pct:SUM(grocery_pharmacy_pct)",
      "parks_pct:SUM(parks_pct)",
      "transit_station_pct:SUM(transit_station_pct)",
      "workplace_pct:SUM(workplace_pct)",
      "residential_pct:SUM(residential_pct)"
    ),
    where = "(us_state_fips=47)", orderby = c("date_stamp ASC", "us_county_fips ASC"), totals = F)

# Get variables to use in dropdown
mobilityVariables <-
  getVariables(
    googleMobility,
    cols = "retail_recreation_pct,grocery_pharmacy_pct,parks_pct,transit_station_pct,workplace_pct,residential_pct"
  )
# mobility labels and descriptions
mobilityFrame <- data.frame()
for(variable in mobilityVariables){
  mobilityFrame <- rbind(mobilityFrame, data.frame(label=variable@label, description=variable@description, id=variable@id, stringsAsFactors = F))
}

## SERVER
server <- function(input, output) {
  
  # Create the map
  output$map <- renderLeaflet({
    m <- leaflet(spdf, options = leafletOptions(zoomControl = FALSE, attributionControl=FALSE, minZoom=6, maxZoom = 8)) %>%
      addProviderTiles("MapBox",
                       options = providerTileOptions(
                         id = "mapbox.light",
                         accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')
                       ))
    bins <- c(0, 50, 100, 500, 1000, 2000, 5000, 10000, 50000)
    pal <-
      colorBin(
        c(
          "#e0ecf3",
          "#b3cfe1",
          "#80b0ce",
          "#4d90ba",
          "#2678ab",
          "#00609c",
          "#005894",
          "#004e8a"
        ),
        domain = spdf@data$cnt_total,
        bins = bins
      )
    
    labels <-
      sprintf(
        "<strong>%s</strong><br/>%g total cases<br/>%g active cases<br/>%g hospitalizations",
        spdf@data$NAME,
        spdf@data$cnt_total,
        spdf@data$cnt_active,
        spdf@data$cnt_hospitalized
      ) %>% lapply(htmltools::HTML)
    
    m <- m %>% addPolygons(
      layerId = ~GEOID,
      fillColor = ~ pal(cnt_total),
      weight = 2,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      ),
      label = labels,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"
      )
    )
    
    m %>% addLegend(
      pal = pal,
      values = ~ cnt_total,
      opacity = 0.7,
      title = NULL,
      position = "bottomleft"
    )
  })
  
  ## CASE CHART
  
  # cached does not work on rocker
  # output$cases <- renderCachedPlot(
  output$cases <- renderPlot(
    {
      id <- input$map_shape_click$id
      caseData <- countyCaseData@records
      if(!is.null(id)){
        caseData <- countyCaseData@records[countyCaseData@records[ , 2] == id, ]
      }else{
        caseData <- stateCaseData@records
      }
      
      ggplot(caseData, aes(x = date_stamp, y = cnt_total)) +
        scale_x_date(date_breaks  = "1 week") +
        geom_line(aes(colour = "Total Cases"), group = 1) +
        geom_line(aes(y = cnt_active, colour = "Active Cases"), group = 1) +
        geom_line(aes(y = cnt_hospitalized, colour = "Hospitalizations"), group = 1) +
        xlab("") + ylab("Count") +
        scale_colour_manual(name = "", values = c("orange", "red", "blue")) +
        theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
    }
    # ,
    # cacheKeyExpr = {input$map_shape_click$id}
  )
  
  # create list of options to choose from for mobility
  output$caseMeasures <- renderUI({
    selectInput(
      'caseMeasures',
      'Case Measures',
      choices = countyVarFrame['label'], selected = countyVarFrame[1, 1]
    )
  })
  
  # case measure descriptions
  output$caseMeasureDescription <- renderUI({
    inputVariable <- countyVarFrame[1, 1]
    if(!is.null(input$caseMeasures)){
      inputVariable <-input$caseMeasures
    }
    description <- countyVarFrame[countyVarFrame[, 'label'] == inputVariable, 'description']
    return (p(description))
  })
  
  # create list of options to choose from for mobility
  output$mobilityMeasures <- renderUI({
    selectInput(
      'mobilityMeasures',
      'Mobility Measures',
      choices = c(mobilityFrame['label']
      ), selected = mobilityFrame[1, 1]
    )
  })
  
  # Create the mobility for recreation chart
  # cached does not work on rocker
  #output$mobility <- renderCachedPlot(
  output$mobility <- renderPlot(
    { 
      id <- input$map_shape_click$id
      mobility <- mobilityData@records
      if(!is.null(id)){
        mobility <- mobilityData@records[mobilityData@records[ , 2] == id, ]
      }
      
      inputVariable <- mobilityFrame[1, 1]
      if(!is.null(input$mobilityMeasures)){
        inputVariable <-input$mobilityMeasures
      }
      variable <- mobilityFrame[mobilityFrame[, 'label'] == inputVariable, 'id']

      ggplot(mobility, aes(x = date_stamp, y = mobility[,variable])) +
        scale_x_date(date_breaks  = "1 week") +
        geom_point(group = 1, show.legend = FALSE) +
        stat_smooth(aes(y = mobility[,variable])) +
        xlab("") + ylab("Change in Mobility (%)") + ylim(-100,100) + theme_grey() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    # ,
    # cacheKeyExpr = { list(input$map_shape_click$id, input$mobilityMeasures )}
  )
  
  # The description of the mobility measure 
  output$mobilityDescription <- renderUI({
    inputVariable <- mobilityFrame[1, 1]
    if(!is.null(input$mobilityMeasures)){
      inputVariable <-input$mobilityMeasures
    }
    description <- mobilityFrame[mobilityFrame[, 'label'] == inputVariable, 'description']
    return (p(description))
  })

  
  # The name of the current county
  output$county <- renderUI({
    county <- "All Counties"
    id <- input$map_shape_click$id
    if(!is.null(id)){
      county <- countyFips[countyFips[ , 1] == id, "name"]
    }
    return (h3(county))
  })
  
}

shinyApp(
  ui = htmlTemplate("www/index.html"), 
  server = server
)