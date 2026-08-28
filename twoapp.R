#two app
library(shiny)
library(shinyWidgets) #install needed
library(ggplot2)
library(gganimate)
library(sf)
library(dplyr)
library(RColorBrewer)
library(htmltools)
library(leaflet)
library(rnaturalearth)
library(rnaturalearthdata)


# 1. Data Preprocessing ---------------------------------------
typhoonkorea <- read.csv("typhoonkorea.csv")
names(typhoonkorea) <- c("NAME", "DATE", "LAT", "LONG", "CNTRL_PRS", 
                         "MX_SPD_SCD", "MX_SPD_HR", "RDS15", "RDS25", 
                         "WND_STRNGTH", "DIR", "MVSPD")

typhoonkorea <- na.omit(typhoonkorea)
typhoonkorea <- typhoonkorea[!grepl("-", typhoonkorea$MX_SPD_HR),]
typhoonkorea$MX_SPD_SCD <- as.numeric(as.character(typhoonkorea$MX_SPD_SCD))
typhoonkorea$MX_SPD_HR <- as.numeric(as.character(typhoonkorea$MX_SPD_HR))

# month only for dotplot
typhoonkorea$MONTH <- gsub("(.*)([/])(.*)([/])(.*)([ ])(.*)", "\\1", typhoonkorea$DATE)
typhoonkorea$MONTH <- as.numeric(as.character(typhoonkorea$MONTH))

# set the size of the map and animation
min_long <- min(typhoonkorea$LONG)
max_long <- max(typhoonkorea$LONG)
min_lat <- min(typhoonkorea$LAT)
max_lat <- max(typhoonkorea$LAT)

#change date format
typhoonkorea$my_date <- as.POSIXlt(typhoonkorea$DATE, format="%m/%d/%y %H")

# change RDS15 type 
unique(typhoonkorea$RDS15)
typhoonkorea$RDS15 <- as.numeric(typhoonkorea$RDS15)

#WND_STRNGTH on bins
mybins    <- c(900, 920, 940, 960, 980, 1000, 1020)
mypalette <- colorBin(palette= "YlOrRd", 
                      domain = typhoonkorea$CNTRL_PRS, na.color="transparent", 
                      bins = mybins)

# Add a title
tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 28px;
    color: black;
  }
"))

title <- tags$div(
  tag.map.title, HTML("Typhoons hit South Korea")
)  

# 2. Shiny structure  ---------------------------------------
app1 <- function() {
  ui <- fluidPage(
    titlePanel("Trajectory in Map"),
    sidebarLayout(
      sidebarPanel(
        helpText("Data gathered from the Korea Meteorological Administration (기상청)"),
        pickerInput("name","Select Typhoon (one or more)", 
                    choices=c(unique(typhoonkorea$NAME)), 
                    selected = c(unique(typhoonkorea$NAME[1])),  
                    options = list(`actions-box` = TRUE), multiple = T),
        br(),
        plotOutput("month_plot")
      ),
      mainPanel(
        textOutput("printedText"),
        br(),
        leafletOutput("leafletPlot"),
        br(),
        verbatimTextOutput("code")
      )
    )
  )
  
#SERVER---------------
  server <- function(input, output, session) {
    # typhoon duration text
    output$printedText <- renderText({
      target_df <- typhoonkorea %>% filter(NAME %in% input$name)
      paste("Typhoon Time Duration:", first(target_df$my_date),"-", last(target_df$my_date))
    })
    
    # main map
    output$leafletPlot <- renderLeaflet({
      
      target_df <- typhoonkorea %>% filter(NAME%in%input$name)
      mytext <- paste(
        "Central Pressure: ", target_df$CNTRL_PRS, "<br/>", 
        "Wind Strength: ", target_df$WND_STRNGTH, "<br/>", 
        "Max Speed (m/s): ", target_df$MX_SPD_SCD, "<br/>",
        "Direction: ", target_df$DIR, "<br/>",
        "Date: ", target_df$DATE, sep="") %>%
        lapply(htmltools::HTML)
      
      m <- leaflet(target_df) %>% 
        addProviderTiles("Esri.WorldImagery") %>%
        setView(lat = mean(target_df$LAT), lng = mean(target_df$LONG) , zoom = 4) %>%
        addCircleMarkers(~ LONG, ~ LAT, 
                         fillColor = ~ mypalette(CNTRL_PRS), fillOpacity = 0.5, color ="black",
                         radius= ~ RDS15/30, weight = 2, dashArray = "5, 5",
                         # Make a call-out label readable to the layperson
                         label = mytext,
                         labelOptions = labelOptions( style = list("font-weight" = "normal"),
                                                      textsize = "13px", direction = "auto")) %>%
        addLegend(pal = mypalette, values = ~ CNTRL_PRS, opacity=0.9, 
                  title = "Central Pressure", position = "topright")
      m <- m %>% addMiniMap(width=150, height = 150, position = "bottomright") 
      return(m)
    })
    
    # dotplot 
    output$month_plot <- renderPlot({
      ggplot(typhoonkorea) +
        aes(x = MONTH) +
        geom_histogram(bins = 12, fill = "lightblue") +
        scale_x_continuous(breaks=seq(1,12,1), limits = c(1,12), 
        labels = c("JAN","FEB","MAR","APR","MAY","JUN",'JUL',"AUG","SEP","OCT","NOV","DEC")) +
        theme(axis.text.x = element_text(angle=45)) +
        ylab("Frequency of Typhoon Hits") + 
        theme_bw() +
        theme(panel.grid.major = element_blank())
    })
    
    output$code <- renderPrint({
      target_df <- typhoonkorea %>% filter(NAME %in% input$name)
      summary(target_df %>% select(-NAME,-DATE, -LAT, -LONG, -RDS25, -WND_STRNGTH, -DIR, -MONTH, -my_date))
    
  })
  }
  # ACTION map
  shinyApp(ui = ui, server = server, options = list(height = 1080))
}

# Define the second Shiny app
app2 <- function() {
  # UI --------------------------------------------------------
  ui <- fluidPage(
    titlePanel("Trajectory in Animation"),
    sidebarLayout(
      sidebarPanel(
        helpText("Data gathered from the Korea Meteorological Administration (기상청)"),
        pickerInput("name","Select Typhoon (one or more)", 
                    choices=c(unique(typhoonkorea$NAME)), 
                    options = list(`actions-box` = TRUE), multiple = T)
      ),
      mainPanel(
        textOutput("printedText"),
        br(),
        imageOutput("animated_plot"),
        br()
      )
    )
  )
  
  # SERVER ----------------------------------------------------
  server <- function(input, output, session) {
    
    output$animated_plot <- renderImage({
      # A temp file to save the output.
      # This file will be removed later by renderImage
      outfile <- tempfile(fileext='.gif')
      
      plot_df <- typhoonkorea %>% filter(NAME%in%input$name)
      world <- ne_countries(scale = "medium", returnclass = "sf")
      
      typhoon_map <- ggplot(data = world) +
        geom_sf(fill = NA) +
        geom_point(data = plot_df, aes(x = LONG, y = LAT, 
                                       size= MX_SPD_HR, color = NAME), alpha = 0.7) +
        scale_size_continuous(range = c(0.5,2)) +
        guides(colour = guide_legend(title = "Typhoon Name"), size = "none")
      
      typhoon_map <- typhoon_map +
        coord_sf(xlim = c(min_long, max_long),  ylim = c(min_lat, max_lat)) +
        labs(title = "Animation of Typhoon")
      
      map_with_animation <- typhoon_map +
        transition_states(seq_along(as.Date(plot_df$my_date)), transition_length = 1) +
        shadow_mark()
      
      anim_save("outfile.gif", animate(map_with_animation))
      # Return a list containing the filename
      list(src = "outfile.gif",
           contentType = 'image/gif'
      )}, deleteFile = TRUE)
    
    
    output$printedText <- renderText({
      target_df <- typhoonkorea %>% filter(NAME%in%input$name)
      paste("Typhoon Time Duration:", first(target_df$my_date),"-", last(target_df$my_date))
    })
  }
  
  shinyApp(ui = ui, server = server, options = list(height = 1080))
}

# Combine the two apps into a single app
ui <- navbarPage("Two apps, Map and animation",
                 tabPanel("App Typhoon map", app1()),
                 tabPanel("App Typhoon animation", app2())
)

# Run the combined app
shinyApp(ui, server = function(input, output) {})
