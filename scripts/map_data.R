# map public charity 2018 locations/data

library(tidyverse)
library(sf)
library(leaflet)


#assume addresses are wrong, use OG coordinates

pc <- vroom::vroom("data/pc2018_coords.csv") %>% 
  filter(LEVEL1 == "PC")

pc_sp <- pc %>% filter(!is.na(LATITUDE) | LATITUDE != 0) %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = st_crs(county))


county <- read_sf("data/coloradoCounties.geojson")

county_refined <- county %>% filter(NAME %in% c("Larimer", "Weld", "Denver", "Morgan", "Pueblo",
                                                "Alamosa"))


# filter out points not in desired counties

pc_filtered <- st_intersection(pc_sp, county_refined)

# investigate
leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = county, fillOpacity = 0, popup = paste("County:", county$NAME)) %>% 
  addPolygons(data = county_refined, col = "red") %>% 
  addCircleMarkers(data = pc_filtered, radius = 2,
                   color = "black",
                   popup = paste("Name:", pc$NAME, "<br>",
                                 "Address:", pc$full_address))


# color by group
pal_group <- colorFactor(palette = "Set1", domain = pc_filtered$NTMAJ10)
labels_group <- c("Arts, Culture, and Humanities", "Education", "Environment",
                  "Health", "Human Services", "International", "Mutual Benefit",
                  "Public and Societal Benefit", "Religion", "Unknown")

leaflet() %>% 
  addTiles() %>% 
  #addPolygons(data = county, color = "black", fillOpacity = 0, stroke = 0.1, popup = paste("County:", county$NAME)) %>% 
  addPolygons(data = county_refined, col = "black", fillOpacity = 0, opacity = 0.9,
              popup = paste("County:", county_refined$NAME)) %>% 
  addCircleMarkers(data = pc_filtered, radius = 2, opacity = 1,
                   color = ~pal_group(NTMAJ10),
                   popup = paste("Name:", pc$NAME, "<br>",
                                 "Address:", pc$full_address)) %>% 
  addLegend(pal = pal_group, values = pc_filtered$NTMAJ10, opacity = 0.9,
            labFormat = function(type, cuts, p) {  # Here's the trick
              paste0(labels_group)
            })



# color by total revenue




# color by total donations





