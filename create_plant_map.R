#######################
# Northampton plant map

library(tidyverse)
library(leaflet)
library(htmlwidgets)

# read csv of plant collection and observation records
plant_records <- read.csv("plant_records.csv")

# paste together information for map popups
plant_records <- plant_records |>
  mutate(link = ifelse(link != "", paste0("\"", link, "\'"), link)) |> # put link in escaped quotes
  mutate(source = ifelse(link == "", paste("source:", source), source)) |>
  mutate(source = ifelse(link != "", paste0("\"", "source: ", "<a href=", link, ">", source, "</a>", "\""), source)) |>
  mutate(date = ifelse(str_detect(date, "\\d{1,}, \\d{4}$"), paste("on", date), paste("in", date))) |> # on [month day year], in [month year]
  mutate(collection_observation = ifelse(basis_of_record == "human observation", paste("observed by", collector_observer, date), "")) |>
  mutate(collection_observation = ifelse(basis_of_record == "occurrence", paste("observed", date), collection_observation)) |>
  mutate(collection_observation = ifelse(basis_of_record == "preserved specimen", paste("collected by", collector_observer, date), collection_observation)) |>
  mutate(locality = ifelse(locality != "", paste("locality:", locality), locality)) |>
  mutate(image = ifelse(image != "", paste0("\"", image, "\""), image)) |> # put image link in escaped quotes
  mutate(image = ifelse(image != "", paste0("<img src=", image, ", width=200>"), "")) |> # add html for image
  mutate(image = ifelse(image != "", paste0("\"", image, "\""), image)) |> # put image html in escaped quotes
  mutate(popup_text = paste(sep = "<br/>", 
                            paste0("<b>", common_name, "</b>"), 
                            paste0("<i>", scientific_name, "</i>"), 
                            paste(family, "family"), 
                            growth_habit, 
                            source, 
                            collection_observation, 
                            locality, 
                            additional_information, 
                            image
                            )
         ) |>
  mutate(popup_text = str_remove_all(popup_text, "\"")) |> # remove escaped quotes
  mutate(popup_text = str_replace_all(popup_text, "<br/><br/><br/>", "<br/>")) |>
  mutate(popup_text = str_replace_all(popup_text, "<br/><br/>", "<br/>"))

# create map
map <- leaflet(plant_records) |>
  addProviderTiles(providers$Esri.WorldTopoMap) |>
  setView(-72.65, 42.32, zoom = 13) |>
  addCircleMarkers(
    lng = ~longitude, 
    lat = ~latitude, 
    popup = ~popup_text, 
    color = ~marker_color, 
    clusterOptions = markerClusterOptions(freezeAtZoom = 17)
  )
map

# save map as html file
saveWidget(map, file = "northampton_plant_map.html")

