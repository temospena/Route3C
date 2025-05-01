library(tidyverse)
library(sf)
library(tmap)
tmap_mode("view")
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
Routes1 = st_read("osm-gpx/site/Routes_stage1.gpkg", layer = "updated")


## Section 2
Places2 = st_read("osm-gpx/site/Places_stage2.gpkg")
st_geometry(Places2) = "geometry"
Routes2 = st_read("osm-gpx/site/Routes_stage2.gpkg")

## Section 3
Places3 = st_read("osm-gpx/site/Places_stage3.gpkg")
st_geometry(Places3) = "geometry"
Routes3 = st_read("osm-gpx/site/Routes_stage3.gpkg")

## All together
Places = rbind(Places1, Places2, Places3)
Routes = rbind(Routes1, Routes2, Routes3)
Routes$Stage[Routes$Start == "Madrid"] = 6
Routes$Stage[Routes$Start == "Portbou"] = 6
Routes$Stage[Routes$Start == "Canet-en-Roussillon"] = 7
Routes$Stage[Routes$Start == "Gruissan"] = 8
Routes$Stage[Routes$Start == "Vias Plage"] = 9
Routes$Stage[Routes$Start == "Montpellier"] = 10
Routes$Stage[Routes$Start == "Saint Gilles"] = 11
Routes$Stage[Routes$Start == "Cavaillon"] = 12
Routes$Stage[Routes$Start == "Pertuis"] = 13
Routes$Stage[Routes$Start == "Manosque"] = 13
Routes$Stage[Routes$Start == "Briançon"] = 14
Routes$Stage[Routes$Start == "Bardonechia"] = 14
Routes$Stage[Routes$Start == "Collegno"] = 14



# Maps function with route in different colors ans with Distance labeled
map3C_new = function(Bike, Train, Places, StartEnd, Title) {
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
    tm_shape(StartEnd, zindex = 0) +
      tm_symbols(fill = "darkred", col_alpha = 0, size = 0.6) +
      tm_text("Place", ymod = 2, xmod = -3) +
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
      tm_add_legend(type = "lines",
                    labels = c("Bike", "Train"),
                    fill = c("black", "grey20"),
                    lwd = c(4, 2),
                    lty = c("solid", "dashed"),
                    position = tm_pos_in("right", "top")) +
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

map3C_all = function(Bike, Train, Places, StartEnd, Title) {
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
        ),
        col.legend = tm_legend_hide()
      ) +
      # tm_legend(show = FALSE) +
      # tm_text("Distance") +
      tm_shape(Train) +
      tm_lines(
        lwd = 1.5,
        lty = "dashed",
        col = "grey20",
        # id = "End",
        # legend.show = TRUE,
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
    tm_add_legend(type = "lines",
      labels = c("Bike", "Train"),
      fill = c("black", "grey20"),
      lwd = c(4, 2),
      lty = c("solid", "dashed")) +
      tm_shape(StartEnd, zindex = 0) +
        # tm_markers() +
        tm_symbols(fill = "darkred", col_alpha = 0, size = 0.6) +
        tm_text("Place", ymod = 2, xmod = -3) +
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
StartEnd1 = Places1 |> filter(Place == "Coimbra" | Place == "Madrid")

map3C_new(Train = Train1, Bike = Bike1, Places = Places1, StartEnd = StartEnd1, Title = Title1)

# Section 2
Bike2 = Routes2 |> filter(Mode == "Bike")
Train2 = Routes2 |> filter(Mode != "Bike")
Title2 = "Section 2: Madrid - Montpellier"
StartEnd2 = Places2 |> filter(Place == "Madrid" | Place == "Montpellier")

map3C_new(Train = Train2, Bike = Bike2, Places = Places2, StartEnd = StartEnd2, Title = Title2)

# Section 3
Bike3 = Routes3 |> filter(Mode == "Bike")
Train3 = Routes3 |> filter(Mode != "Bike")
Train3$Distance[2] = NA # fide distance total train
Title3 = "Section 3: Montpellier - Torino"
StartEnd3 = Places3 |> filter(Place == "Montpellier" | Place == "Torino")

map3C_new(Train = Train3, Bike = Bike3, Places = Places3, StartEnd = StartEnd3, Title = Title3)

# All 3 sections
Bike_all = Routes |> filter(Mode == "Bike")
Train_all = Routes |> filter(Mode != "Bike")
Train_all$Distance[5] = NA # fide distance total train
Title_all = "Route 3c: Coimbra - Madrid - Montpellier"
StartEnd_all = Places |> filter(Place == "Coimbra" | Place == "Madrid" | Place == "Montpellier" | Place == "Torino")
StartEnd_all = StartEnd_all[-3,]

map3C_all(Train = Train_all, Bike = Bike_all, Places = Places, StartEnd = StartEnd_all, Title = Title_all)
