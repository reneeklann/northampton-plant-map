# Northampton plant map

library(tidyverse)
library(leaflet)
library(htmlwidgets)
library(htmltools)

# read csv of plant collection and observation records
plant_records <- read.csv("plant_records.csv")

# paste together information for map popups
plant_records <- plant_records |>
  mutate(link = ifelse(link != "", paste0("\"", link, "\""), link)) |> # put link in escaped quotes
  mutate(source = ifelse(link == "", paste("Source:", source), source)) |>
  mutate(source = ifelse(link != "", paste0("\"", "Source: ", "<a href=", link, ">", source, "</a>", "\""), source)) |>
  mutate(date = ifelse(str_detect(date, "\\d{1,}, \\d{4}$"), paste("on", date), paste("in", date))) |> # on [month day year], in [month year]
  mutate(collection_observation = ifelse(basis_of_record == "human observation", paste("Observed by", collector_observer, date), "")) |>
  mutate(collection_observation = ifelse(basis_of_record == "occurrence", paste("Observed", date), collection_observation)) |>
  mutate(collection_observation = ifelse(basis_of_record == "preserved specimen", paste("Collected by", collector_observer, date), collection_observation)) |>
  mutate(locality = ifelse(locality != "", paste("Locality:", locality), locality)) |>
  mutate(image = ifelse(image != "", paste0("\"", image, "\""), image)) |> # put image link in escaped quotes
  mutate(image = ifelse(image != "", paste0("<img src=", image, ", width=200>"), "")) |> # add html for image
  mutate(image = ifelse(image != "", paste0("\"", image, "\""), image)) |> # put image html in escaped quotes
  mutate(popup_text = paste(sep = "<br/>", 
                            paste0("<b>", common_name, "</b>"), 
                            paste0("<i>", scientific_name, "</i>"), 
                            paste(family, "family"), 
                            plant_type, 
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
    radius = 12,
    color = ~marker_color,
    weight = 3,
    opacity = 0.6,
    fillOpacity = 0.5,
    clusterOptions = markerClusterOptions(
      freezeAtZoom = 17,
      iconCreateFunction = JS("
        function(cluster) {
          return new L.DivIcon({
            html: '<div><span>' + cluster.getChildCount() + '</span></div>',
            className: 'custom-cluster',
            iconSize: new L.Point(28, 28)
          });
        }
      ")
    )
  ) |>
  prependContent(
    tags$style(HTML("
      .custom-cluster {
        background-color: #f79a64;
        border-radius: 50%
      }
      .custom-cluster div {
        background-color: #f47d36;
        border-radius: 50%;
        width: 22px;
        height: 22px;
        margin-left: 3px;
        margin-top: 3px;
        text-align: center;
        color: white;
        font-weight: bold;
        line-height: 22px;
      }
    "))
  )
map

# save map as html file
saveWidget(map, file = "northampton_plant_map.html")

