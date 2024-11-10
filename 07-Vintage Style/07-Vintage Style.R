# Mapping the Villages and Waterways on Seagull Island

#### Setup ####

library(sf)
# library(rcityviews) # https://koenderks.github.io/rcityviews/
library(ggplot2)
library(tidyverse)
library(OpenStreetMap)
library(osmdata)
library(ggtext)
library(magick)

# define the spatial extent to OpenStreetMap
lat1 <- 22.855; lat2 <- 22.998; lon1 <- 113.476; lon2 <- 113.59202508003357
# bbbox
bbox <- c(xmin = lon1, ymin = lat1, xmax = lon2, ymax = lat2)




#### Data ####

# Query for waterways
waterways <- opq(bbox = bbox) %>%
  add_osm_feature(key = "waterway") %>%
  osmdata_sf()
waterways2 <- waterways$osm_lines %>%
  mutate(name = case_when(
    is.na(name) ~ "Other",
    TRUE ~ name
  )) %>%
  filter(!str_starts(name, "珠江三角洲"))
# Query for natural
natural <- opq(bbox = bbox) %>%
  add_osm_feature(key = "natural") %>%
  osmdata_sf()
seagull <- natural$osm_polygons %>%
  filter(name == "海鸥岛")
# Query for landuse
landuse <- opq(bbox = bbox) %>%
  add_osm_feature(key = "landuse") %>%
  osmdata_sf()
villages <- landuse$osm_polygons %>%
  filter(landuse == "residential") %>%
  mutate(name = case_when(
    is.na(name) ~ "island",
    name == "瑞成村" ~ "island",
    TRUE ~ name
  )) %>%
  filter(name == "island",
         !osm_id %in% c(449809649, 449809658, 921993169, 921990428, 925604363, 539855632, 	
                        1289841871, 539855639))

# Base Water
## Data source(maptiler) https://data.maptiler.com/downloads/dataset/osm/asia/china/guangzhou/#7.29/23.259/113.576
sf_data <- st_read("data/osm-2020-02-10-v3.11_china_guangzhou.mbtiles")
sf_data <- st_transform(sf_data, crs = 4326)
# ggplot(data = sf_data) +
#   geom_sf(fill = "#7AB2D3", color = NA)+
#   coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4]), expand = FALSE)




#### Plot ####

plot <- ggplot() +
  geom_sf(data = sf_data, fill = "#9EBFAA", color = NA) +
  
  # Add natural with a warm beige color
  geom_sf(data = seagull, fill = "#FFF7D8", color = NA) +
  
  # Add landuse with a warm beige color
  geom_sf(data = villages, fill = "#FF9C73", color = NA) +
  
  
  # Add waterways with a light teal color
  geom_sf(data = waterways2, color = "#9EBFAA", lwd = .5) +
  
  # Bouding Box
  coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4])) +

  # Customize map background to a cream color
  theme_void() +
  theme(panel.background = element_rect(fill = "#F5E4C3", color = NA)) 




#### Plot: save ####

ggsave(filename = "Outputs/map.png", plot = plot, height = 10, width = 7.5, units = "in", dpi = 300)
img <- image_read("Outputs/map.png")
img_final <- image_annotate(img, "Seagull Island", 
                            size = 200, color = "#6B4226",
                            font = "serif bold",
                            gravity = "northeast",
                            location = "+50+50")

interpretation <- "Seagull Island is an inland river island\nformed by the alluvial deposits of the Pearl River Delta.\nTraditional fishing villages (RED POLYGONS)\nare scattered across the island,\nadapted to the ebb and flow of the river,\nwith a modest settlement capacity."
img_final <- image_annotate(img_final, interpretation, 
                            size = 55, color = "#6B4226",
                            font = "serif bold",
                            gravity = "northeast",
                            location = "+50+300")

img_final <- image_annotate(img_final, 
                            "#30DayMapChallenge-2024\nDay 7: Vintage Style\nAuthor: Luming Xu\n@UPenn_Weitzman_MUSA\nlinkedin.com/in/luming-xu", 
                            size = 40, color = "#6B4226", 
                            font = "poppins",
                            gravity = "south",
                            location = "+50+50")
image_write(img_final, path = "Outputs/07-Luming-Vintage.png")
