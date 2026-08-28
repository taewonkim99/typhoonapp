#two app
library(shiny)
library(shinyWidgets) #install needed
library(ggplot2)
library(gganimate)
library(gifski)
library(sf)
library(dplyr)
library(RColorBrewer)
library(htmltools)
library(leaflet)
library(rnaturalearth)
library(rnaturalearthdata)
library(psych)


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

# comments for each typhoon
comments <- c("stroke west coast.","didn't make a big damage.","made a huge damage to south coast.",
              "stroke west coast and acrossed to east.","made a damage along the west coast.","stroke south to east.",
              "covered South Korea with a massive damage.","stroke only Busan with moderate damage.","stroke Yeosu and acrossed South Korea to east coast.",
              "made a huge damage on Busan.","didn't make a big damage.","hit Yeosu and followed the south coast to east coast line.",
              "made a subtle damage on Busan.","made a huge damage along the east coast line.","made a huge damage along the east coast line.",
              "hit Jeju island and Busan with subtle damage.","made a massive damage on Jeju island and Busan.","stroke whole Korea peninsula straight south to north and made a huge damage.")

# setting corresponding comments for each typhoon
typhoonkorea$TYPHOON_comments <- NA
typhoonkorea$TYPHOON_comments[typhoonkorea$NAME == "EWINIAR"] <- comments[1]
typhoonkorea$TYPHOON_comments[typhoonkorea$NAME == "NARI"] <- comments[2]
typhoonkorea$TYPHOON_comments[typhoonkorea$NAME == "DIANMU"] <- comments[3]
typhoonkorea$TYPHOON_comments[typhoonkorea$NAME == "KOMPASU"] <- comments[4]
typhoonkorea$TYPHOON_comments[typhoonkorea$NAME == "KHANUN"] <- comments[5]
typhoonkorea$TYPHOON_comments[typhoonkorea$NAME == "TEMBIN"] <- comments[6]
typhoonkorea$TYPHOON_comments[typhoonkorea$NAME == "SANBA"] <- comments[7]
typhoonkorea$TYPHOON_comments[typhoonkorea$NAME == "CHABA"] <- comments[8]
typhoonkorea$TYPHOON_comments[typhoonkorea$NAME == "SOULIK"] <- comments[9]
typhoonkorea$TYPHOON_comments[typhoonkorea$NAME == "KING-REY"] <- comments[10]
typhoonkorea$TYPHOON_comments[typhoonkorea$NAME == "FRANCISCO"] <- comments[11]
typhoonkorea$TYPHOON_comments[typhoonkorea$NAME == "MITAG"] <- comments[12]
typhoonkorea$TYPHOON_comments[typhoonkorea$NAME == "JANGMI"] <- comments[13]
typhoonkorea$TYPHOON_comments[typhoonkorea$NAME == "MAYSAK"] <- comments[14]
typhoonkorea$TYPHOON_comments[typhoonkorea$NAME == "HAISHEN"] <- comments[15]
typhoonkorea$TYPHOON_comments[typhoonkorea$NAME == "OMAIS"] <- comments[16]
typhoonkorea$TYPHOON_comments[typhoonkorea$NAME == "JANGMI"] <- comments[17]
typhoonkorea$TYPHOON_comments[typhoonkorea$NAME == "HINNAMNOR"] <- comments[18]
typhoonkorea$TYPHOON_comments[typhoonkorea$NAME == "KHANUN2"] <- comments[19]


#change date format
typhoonkorea$my_date <- as.POSIXlt(typhoonkorea$DATE, format="%m/%d/%y %H")

#add year column
typhoonkorea$YEAR = format(as.Date(typhoonkorea$DATE, format="%m/%d/%y"),"%Y")
typhoonkorea$YEAR <- as.numeric(typhoonkorea$YEAR)

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
        helpText("Data gathered from the Korea Meteorological Administration (기상청).
                 From the source, we were able to gather around 19 different typhoons from the past 20 years
                 and the ones that have only hit South Korea. There are more than one observations in one typhoon;
                 therefore, the dataset has a total of 604 observations. Looking at the movement trend, most of the typhoons
                 start below South Korea, and moves northward. 
                 Looking at the histogram plot describing the frequency of typhoons measured and when they hit,
                 it's mostly during Summer (July - October). "),
        pickerInput("name","Select Typhoon (one or more)", 
                    choices=c(unique(typhoonkorea$NAME)), 
                    selected = c(unique(typhoonkorea$NAME[1])),  
                    options = list(`actions-box` = TRUE), multiple = T),
        helpText("Please select only one typhoon to see the description"),
        textOutput("explanationTYPHOON"),
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
    
    # comments Explanation
    output$explanationTYPHOON <- renderText({
      target_df <- typhoonkorea %>% filter(NAME %in% input$name)
      paste(first(target_df$NAME), first(target_df$TYPHOON_comments)) %>% 
        paste("And, its max speed reachs to ", max(target_df$MVSPD), "m/s")
    })
    
    # histogram
    output$month_plot <- renderPlot({
      ggplot(typhoonkorea) +
        aes(x = MONTH) +
        geom_histogram(bins = 12, fill = "lightblue") +
        scale_x_continuous(breaks=seq(1,12,1), limits = c(1,12), 
                           labels = c("JAN","FEB","MAR","APR","MAY","JUN",'JUL',"AUG","SEP","OCT","NOV","DEC")) +
        theme(axis.text.x = element_text(angle=45)) +
        ggtitle("When Typhoon usually Hit according to the Months") +
        ylab("Frequency of Typhoon Hits") + 
        theme_bw() +
        theme(panel.grid.major = element_blank())
    })
    
    output$code <- renderPrint({
      target_df <- typhoonkorea %>% filter(NAME %in% input$name)
      describeBy(target_df %>% 
                   select(-NAME,-DATE, -LAT, -LONG, -RDS25, -WND_STRNGTH, -DIR, -MONTH, -my_date), 
                 group=target_df$NAME, fast=TRUE)
    })
  }
  # ACTION map
  shinyApp(ui = ui, server = server, options = list(height = 1080))
}



# Define the third Shiny app animation
app2 <- function() {
  # UI --------------------------------------------------------
  ui <- fluidPage(
    titlePanel("Trajectory in Animation"),
    sidebarLayout(
      sidebarPanel(
        helpText("Data gathered from the Korea Meteorological Administration (기상청).
                 From the source, we were able to gather around 19 different typhoons from the past 20 years
                 and the ones that have only hit South Korea. There are more than one observations in one typhoon;
                 therefore, the dataset has a total of 604 observations. Looking at the movement trend, most of the typhoons
                 start below South Korea, and moves northward. 
                 Looking at the histogram plot describing the frequency of typhoons measured and when they hit,
                 it's mostly during Summer (July - October). "),
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
      
      animate(map_with_animation, renderer = gifski_renderer())
      anim_save("outfile.gif")
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
                 tabPanel("App Typhoon Animation", app2())
)

# Run the combined app
shinyApp(ui, server = function(input, output) {})
