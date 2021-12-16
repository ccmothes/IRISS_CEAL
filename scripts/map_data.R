# map public charity 2018 locations/data

library(tidyverse)
library(sf)
library(leaflet)


#assume addresses are wrong, use OG coordinates

pc <- vroom::vroom("data/pc2018_coords.csv") %>% 
  filter(LEVEL1 == "PC")



county <- read_sf("data/coloradoCounties.geojson")

county_refined <- county %>% filter(NAME %in% c("Larimer", "Weld", "Denver", "Morgan", "Pueblo",
                                                "Alamosa"))


pc_sp <- pc %>% filter(!is.na(LATITUDE) | LATITUDE != 0) %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = st_crs(county))


# filter out points not in desired counties and jitter

pc_filtered <- st_intersection(pc_sp, county_refined) %>% 
  st_jitter(amount = 0.0005, factor = 0.001) # I think default value is fine, just know addresses are off by a couple streets

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
  addCircleMarkers(data = pc_filtered, radius = 6, opacity = 1,
                   color = "black",
                   stroke = TRUE,
                   weight = 0.2,
                   fillOpacity = 1,
                   fillColor = ~pal_group(NTMAJ10),
                   popup = paste("Name:", pc_filtered$NAME, "<br>",
                                 "Address:", pc_filtered$full_address, "<br>",
                                 "Major Group:", pc_filtered$NTMAJ10, "<br>",
                                 "Contributions:", paste0("$", pc_filtered$CONT), "<br>",
                                 "Revenue:", paste0("$",pc_filtered$TOTREV), "<br>",
                                 "Expenses:", paste0("$",pc_filtered$EXPS), "<br>",
                                 "Year: 2018")) %>% 
  addLegend(pal = pal_group, values = pc_filtered$NTMAJ10, opacity = 0.9,
            labFormat = function(type, cuts, p) {  # Here's the trick
              paste0(labels_group)
            })



# color by total revenue (calculated, TOTREV)

pal_rev <- colorQuantile(palette = "RdYlBu", reverse = TRUE,domain = pc_filtered$TOTREV, n = 10)

leaflet() %>% 
  addTiles() %>% 
  #addPolygons(data = county, color = "black", fillOpacity = 0, stroke = 0.1, popup = paste("County:", county$NAME)) %>% 
  addPolygons(data = county_refined, col = "black", fillOpacity = 0, opacity = 0.9,
              popup = paste("County:", county_refined$NAME)) %>% 
  addCircleMarkers(data = pc_filtered, radius = 6, opacity = 1,
                   color = "black",
                   stroke = TRUE,
                   weight = 0.2,
                   fillOpacity = 1,
                   fillColor = ~pal_rev(TOTREV),
                   popup = paste("Name:", pc_filtered$NAME, "<br>",
                                 "Address:", pc_filtered$full_address, "<br>",
                                 "Major Group:", pc_filtered$NTMAJ10, "<br>",
                                 "Contributions:", paste0("$", pc_filtered$CONT), "<br>",
                                 "Revenue:", paste0("$",pc_filtered$TOTREV), "<br>",
                                 "Expenses:", paste0("$",pc_filtered$EXPS), "<br>",
                                 "Year: 2018")) %>% 
  addLegend(pal = pal_rev, values = pc_filtered$TOTREV, opacity = 0.9,
            title = "Total Revenue Percentile")



# color by total donations/contributions (CONT)
# pal_cont <- colorBin(palette = "RdYlBu", reverse = TRUE, domain = pc_filtered$CONT,
#                      pretty = TRUE)


pal_cont <- colorQuantile(palette = "RdYlBu", reverse = TRUE, domain = pc_filtered$CONT,
                     n = 7)


leaflet() %>% 
  addTiles() %>% 
  #addPolygons(data = county, color = "black", fillOpacity = 0, stroke = 0.1, popup = paste("County:", county$NAME)) %>% 
  addPolygons(data = county_refined, col = "black", fillOpacity = 0, opacity = 0.9
              #popup = paste("County:", county_refined$NAME)
              ) %>% 
  addCircleMarkers(data = pc_filtered, radius = 6, opacity = 1,
                   color = "black",
                   stroke = TRUE,
                   weight = 0.2,
                   fillOpacity = 1,
                   fillColor = ~pal_cont(CONT),
                   popup = paste("Name:", pc_filtered$NAME, "<br>",
                                 "Address:", pc_filtered$full_address, "<br>",
                                 "Major Group:", pc_filtered$NTMAJ10, "<br>",
                                 "Contributions:", paste0("$", pc_filtered$CONT), "<br>",
                                 "Revenue:", paste0("$",pc_filtered$TOTREV), "<br>",
                                 "Expenses:", paste0("$",pc_filtered$EXPS), "<br>",
                                 "Year: 2018")) %>% 
  addLegend(pal = pal_cont, values = pc_filtered$CONT, opacity = 0.9,
            title = "Total Contributions Percentile")


# color by total expenses (EXPS)


pal_exp <- colorQuantile(palette = "RdYlBu", reverse = TRUE, domain = pc_filtered$EXPS, n = 10)

leaflet() %>% 
  addTiles() %>% 
  #addPolygons(data = county, color = "black", fillOpacity = 0, stroke = 0.1, popup = paste("County:", county$NAME)) %>% 
  addPolygons(data = county_refined, col = "black", fillOpacity = 0, opacity = 0.9,
              popup = paste("County:", county_refined$NAME)) %>% 
  addCircleMarkers(data = pc_filtered, radius = 6, opacity = 1,
                   color = "black",
                   stroke = TRUE,
                   weight = 0.5,
                   fillOpacity = 1,
                   fillColor = ~pal_exp(EXPS),
                   popup = paste("Name:", pc_filtered$NAME, "<br>",
                                 "Address:", pc_filtered$full_address, "<br>",
                                 "Major Group:", pc_filtered$NTMAJ10, "<br>",
                                 "Contributions:", paste0("$", pc_filtered$CONT), "<br>",
                                 "Revenue:", paste0("$",pc_filtered$TOTREV), "<br>",
                                 "Expenses:", paste0("$",pc_filtered$EXPS), "<br>",
                                 "Year: 2018")) %>% 
  addLegend(pal = pal_exp, values = pc_filtered$EXPS, opacity = 0.9,
            title = "Total Expenses Percentile")







