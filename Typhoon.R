#CDS 301 Project

library(ggplot2)
library(sf)
library(dplyr)
library(htmltools)
library(shiny)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(plotly)
library(RColorBrewer)
library(leaflet) #for making the interactive map
library(maptools)
library(cartogram)
library(ggthemes)
library(gganimate)
library(gapminder)
library(gifski)
library(tidyverse)
library(lubridate)
library(stringr)


#Step 1 Data Collection
# Facility: Typhoons around South Korea in the year of 2023 from Jan - Sep (CSV file)
typhoon <- read.csv("typhoonkorea.csv")
names(typhoon) <- c("NAME", "DATE", "LAT", "LONG", "CNTRL_PRS", 
                      "MX_SPD_SCD", "MX_SPD_HR", "RDS15", "RDS25", 
                      "WND_STRNGTH", "DIR", "MVSPD")

typhoon <- na.omit(typhoon)
typhoon <- typhoon[!grepl("-", typhoon$MX_SPD_HR),]
typhoon$MX_SPD_HR <- as.numeric(as.character(typhoon$MX_SPD_HR))
#change date format
typhoon$my_date <- as.POSIXlt(typhoon$DATE, format="%m/%d/%y %H")

summary(typhoon$LAT)
summary(typhoon$LONG)
str(typhoon)


# leaflet ----
# Create a color palette with handmade bins
mypalette <- colorFactor(palette="viridis", domain=typhoon$NAME, na.color="transparent")

# Prepare the text for the tooltip:
mytext <- paste(
  "Central Pressure: ", typhoon$CNTRL_PRS, "<br/>", 
  "Wind Strength: ", typhoon$WND_STRNGTH, "<br/>", 
  "Direction: ", typhoon$DIR, "<br/>",
  "Date: ", typhoon$DATE, sep="") %>%
  lapply(htmltools::HTML)

# Final Map
a <- leaflet(typhoon) %>%
  addTiles()  %>% 
  setView(lat=mean(typhoon$LAT), lng=mean(typhoon$LONG) , zoom=3) %>%
  addProviderTiles("Stadia.AlidadeSmooth") %>%
  addCircleMarkers(~LONG, ~LAT, 
                   fillColor = ~mypalette(typhoon$NAME), fillOpacity = 0.7, color="white", 
                   radius= ~MX_SPD_HR/50, stroke=FALSE,
                   label = mytext,
                   popup = ~format(typhoon$my_Date),
                   labelOptions = labelOptions( style = list("font-weight" = "normal", padding = "3px 8px"), 
                                                textsize = "10px", direction = "auto")) %>%
  addLegend(pal=mypalette, values=~NAME, opacity=0.9, 
            title = "Typhoon", position = "bottomright") %>%
  addMiniMap(
    tiles = providers$Stadia.AlidadeSmooth,
    toggleDisplay = TRUE)
a


world <- ne_countries(scale = "medium", returnclass = "sf")

typhoon_map <- ggplot(data = world) +
  geom_sf(fill = NA) +
  geom_point(data = typhoon, aes(x = LONG, y = LAT, 
        size= MX_SPD_HR, color = NAME), alpha = 0.7) +
  scale_size_continuous(range = c(0.5,2)) +
  guides(colour = guide_legend(title = "Typhoon Name"), size = "none")

#Specify area through min and max of lat and long
min_long <- min(typhoon$LONG)
max_long <- max(typhoon$LONG)
min_lat <- min(typhoon$LAT)
max_lat <- max(typhoon$LAT)

typhoon_map <- typhoon_map +
  coord_sf(xlim = c(min_long, max_long),  ylim = c(min_lat, max_lat))

typhoon_map


map_with_animation <- typhoon_map +
  transition_states(seq_along(as.Date(typhoon$my_date))) +
  shadow_mark()

animate(map_with_animation)


