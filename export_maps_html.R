library(tidyverse)
library(sf)
library(tmap)
library(mapview)

# Data

## Section 1
# st_layers("osm-gpx/site/1_coimbra-madrid.gpx")
# Section1 = st_read("osm-gpx/site/1_coimbra-madrid.gpx", layer = "tracks")
# Section1_trains = st_read("osm-gpx/site/1_coimbra-madrid-trains.geojson")
# st_layers("osm-gpx/site/12_fundao-monsanto.gpx")
# Route1_2 = st_read("osm-gpx/site/12_fundao-monsanto.gpx", layer = "tracks")
# Train1 = st_read("osm-gpx/site/Train1.gpkg")
Places1 = st_read("osm-gpx/site/Places_stage1.gpkg")
Routes1 = st_read("osm-gpx/site/Routes_stage1.gpkg")


## Section 2
Places2 = st_read("osm-gpx/site/Places_stage2.gpkg")
st_geometry(Places2) = "geometry"
Routes2 = st_read("osm-gpx/site/Routes_stage2.gpkg")


## All together
Places = rbind(Places1, Places2)
Routes = rbind(Routes1, Routes2)
Routes$Stage[Routes$Start == "Madrid"] = 6
Routes$Stage[Routes$Start == "Portbou"] = 6
Routes$Stage[Routes$Start == "Canet-en-Roussillon"] = 7
Routes$Stage[Routes$Start == "Gruissan"] = 8
Routes$Stage[Routes$Start == "Vias Plage"] = 9
Routes$Stage[Routes$Start == "Montpellier"] = 10

# Maps function with route in different colors ans with Distance labeled
map3C_new = function(Bike, Train, Places, Title) {
  if (nrow(Train) == 0) {
    tm_shape(Bike) +
      tm_lines(
        lwd = 5,
        col = "Stage",
        # id = "End",
        popup.vars = c(
          "Name",
          "Start",
          # "End",
          "Distance"
          # "Gain",
          # "Duration",
          # "EuroVelo",
          # "Mode",
          # "Travel",
          # "Acomodation",
          # "Notes"
        )
      ) +
      tm_text("Distance") +
      tm_title(Title) +
      tm_basemap(providers$Esri.WorldGrayCanvas) +
      tm_basemap(providers$OpenStreetMap) +
      tm_basemap(providers$CyclOSM) +
      tm_basemap(providers$OpenTopoMap) +
      tm_basemap(providers$Esri.WorldImagery)
    
  } else {
    tm_shape(Places, zindex = 0) +
      tm_symbols(size = 0.3, fill = "grey40") +
      # tm_text("Place") +
    tm_shape(Bike) +
      tm_lines(
        lwd = 4,
        col = "Stage",
        # id = "End",
        popup.vars = c(
          "Name",
          "Start",
          # "End",
          "Distance"
          # "Gain",
          # "Duration",
          # "EuroVelo",
          # "Mode",
          # "Travel",
          # "Acomodation",
          # "Notes"
        )
      ) +
      tm_text("Distance") +
      tm_shape(Train) +
      tm_lines(
        lwd = 2,
        lty = "dashed",
        col = "grey20",
        # id = "End",
        popup.vars = c(
          "Name",
          "Start",
          # "End",
          "Distance",
          "Mode"
          # "Travel",
          # "Acomodation",
          # "Notes"
        )
      ) +
       tm_text("Distance") +
      tm_title(Title) +
      tm_basemap(providers$Esri.WorldGrayCanvas) +
      tm_basemap(providers$OpenStreetMap) +
      tm_basemap(providers$CyclOSM) +
      tm_basemap(providers$OpenTopoMap) +
      tm_basemap(providers$Esri.WorldImagery)
    # options = layersControlOptions(collapsed = FALSE) #not working
    
  }
}

map3C_all = function(Bike, Train, Places, Title) {
    tm_shape(Places, zindex = 0) +
      tm_symbols(size = 0.2, fill = "grey40") +
      # tm_text("Place") +
      tm_shape(Bike) +
      tm_lines(
        lwd = 4,
        col = "Stage",
        # id = "End",
        popup.vars = c(
          "Name",
          "Start",
          # "End",
          "Distance"
          # "Gain",
          # "Duration",
          # "EuroVelo",
          # "Mode",
          # "Travel",
          # "Acomodation",
          # "Notes"
        )
      ) +
      tm_legend(show = FALSE) +
      # tm_text("Distance") +
      tm_shape(Train) +
      tm_lines(
        lwd = 1.5,
        lty = "dashed",
        col = "grey20",
        # id = "End",
        popup.vars = c(
          "Name",
          "Start",
          # "End",
          "Distance",
          "Mode"
          # "Travel",
          # "Acomodation",
          # "Notes"
        )
      ) +
      # tm_text("Distance") +
      tm_title(Title) +
      tm_basemap(providers$Esri.WorldGrayCanvas) +
      tm_basemap(providers$OpenStreetMap) +
      tm_basemap(providers$CyclOSM) +
      tm_basemap(providers$OpenTopoMap) +
      tm_basemap(providers$Esri.WorldImagery)
    # options = layersControlOptions(collapsed = FALSE) #not working
    
}


## Plot maps

# Section 1
Bike1 = Routes1 |> filter(Mode == "Bike")
Train1 = Routes1 |> filter(Mode != "Bike")
Title1 = "Section 1: Coimbra - Madrid"

map3C_new(Train = Train1, Bike = Bike1, Places = Places1, Title = Title1)

# Section 2
Bike2 = Routes2 |> filter(Mode == "Bike")
Train2 = Routes2 |> filter(Mode != "Bike")
Title2 = "Section 2: Madrid - Montpellier"

map3C_new(Train = Train2, Bike = Bike2, Places = Places2, Title = Title2)


# All 3 sections
Bike_all = Routes |> filter(Mode == "Bike")
Train_all = Routes |> filter(Mode != "Bike")
Title_all = "Route 3c: Coimbra - Madrid - Montpellier"

map3C_all(Train = Train_all, Bike = Bike_all, Places = Places, Title = Title_all)
