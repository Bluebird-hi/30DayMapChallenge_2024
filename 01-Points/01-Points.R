# Mapping Yum Cha Restaurants Within 3 km of Sun Yat-sen University

#### Setup ####
# Packages
library(sf)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(googleway)
library(osmdata)
library(ggforce)
library(geosphere) # for distHaversine function
library(ggtext)
library(showtext) # font family
library(sysfonts) # font family
library(jpeg)
library(grid)

# Google Map API key
api_key <- "YOUR_API_KEY"

location <- c(23.09998960496676, 113.2941479085638)
radius <- 3500

# Keyword1: yum cha (饮茶)
Yum_cha_response1 <- google_places(
      keyword = "yum cha",
      location = location,
      radius = radius,
      place_type = "restaurant",
      key = api_key
    )
Sys.sleep(5)
Yum_cha_response2 <- google_places(
  keyword = "yum cha",
  location = location,
  radius = radius,
  place_type = "restaurant",
  key = api_key,
  page_token = Yum_cha_response1$next_page_token
)

Yum_cha_save1 <- Yum_cha_response1[["results"]] %>%
  mutate(
    lat = geometry$location$lat,
    lng = geometry$location$lng
  ) %>%
  select(lat, lng, name, place_id, rating)

Yum_cha_save2 <- Yum_cha_response2[["results"]] %>%
  mutate(
    lat = geometry$location$lat,
    lng = geometry$location$lng
  ) %>%
  select(lat, lng, name, place_id, rating)
Yum_cha_results <- rbind(Yum_cha_save1,Yum_cha_save2)

# Keyword2: dim sum restaurant (点心)
Dim_sum_response1 <- google_places(
  keyword = "dim sum restaurant",
  location = location,
  radius = radius,
  place_type = "restaurant",
  key = api_key
)
Sys.sleep(5)
Dim_sum_response2 <- google_places(
  keyword = "dim sum restaurant",
  location = location,
  radius = radius,
  place_type = "restaurant",
  key = api_key,
  page_token = Dim_sum_response1$next_page_token
)
Sys.sleep(5)
Dim_sum_response3 <- google_places(
  keyword = "dim sum restaurant",
  location = location,
  radius = radius,
  place_type = "restaurant",
  key = api_key,
  page_token = Dim_sum_response2$next_page_token
)

Dim_sum_save1 <- Dim_sum_response1[["results"]] %>%
  mutate(
    lat = geometry$location$lat,
    lng = geometry$location$lng
  ) %>%
  select(lat, lng, name, place_id, rating)

Dim_sum_save2 <- Dim_sum_response2[["results"]] %>%
  mutate(
    lat = geometry$location$lat,
    lng = geometry$location$lng
  ) %>%
  select(lat, lng, name, place_id, rating)

Dim_sum_save3 <- Dim_sum_response3[["results"]] %>%
  mutate(
    lat = geometry$location$lat,
    lng = geometry$location$lng
  ) %>%
  select(lat, lng, name, place_id, rating)
Dim_sum_results <- rbind(Dim_sum_save1,Dim_sum_save2,Dim_sum_save3)

# Dataset: Yum cha restaurants
Yum_cha_restaurants <- rbind(Yum_cha_results, Dim_sum_results) %>% 
  distinct(place_id, .keep_all = TRUE)

#### VISUALS ####
# Roads
bbox <- c(113.23, 23.05, 113.35, 23.15) 
roads <- opq(bbox = bbox) %>%
  add_osm_feature(key = "highway", value = c("motorway", "trunk", "primary", "secondary")) %>%
  osmdata_sf()

# Yum_cha_restaurants <- st_as_sf(Yum_cha_restaurants, coords = c("lng", "lat"), crs = 4326)
roads$osm_lines <- st_transform(roads$osm_lines, crs = 4326)

# Example coordinates
center_point <- c(113.2941479085638, 23.09998960496676)

CBD_point <- c(23.123519517518872, 113.31954468834963)
BJroad_point <- c(23.129754286006246, 113.25826932462109)
Xiadu_point <- c(23.10031126106761, 113.3028161836008)

# Calculate distances from the center point for each restaurant
Yum_cha_restaurants <- Yum_cha_restaurants %>%
  mutate(
    distance = distHaversine(cbind(lng, lat), center_point)
  )

# Load the font family
font_add_google("Poppins", "poppins")
showtext_auto() 

# Load the image
img <- readJPEG("data/01/Yum cha image.jpg") 

# Create a raster graphic object
img_grob <- rasterGrob(img, interpolate = TRUE)

# Plot
Points <- 
  ggplot() +
  # Circles
  geom_circle(aes(x0 = center_point[1], y0 = center_point[2], r = 0.037), fill = "#C0C78C",alpha = .3, color = NA) +
  geom_circle(aes(x0 = CBD_point[2], y0 = CBD_point[1], r = 0.013), 
              fill = NA, alpha = .5, color = "#F5E7B2", linetype = "dashed",size = .7, alpha = .8) +
  # geom_point(aes(x = CBD_point[2], y = CBD_point[1]), size = 3, color = "#F5E7B2") +
  geom_circle(aes(x0 = BJroad_point[2], y0 = BJroad_point[1], r = 0.011), 
              fill = NA, alpha = .5, color = "#F5E7B2", linetype = "dashed",size = .7, alpha = .8) +
  # geom_point(aes(x = BJroad_point[2], y = BJroad_point[1]), size = 3, color = "#F5E7B2") +
  geom_circle(aes(x0 = Xiadu_point[2], y0 = Xiadu_point[1], r = 0.008), 
              fill = NA, alpha = .5, color = "#F5E7B2", linetype = "dashed",size = .7, alpha = .8) +
  # geom_point(aes(x = Xiadu_point[2], y = Xiadu_point[1]), size = 3, color = "#F5E7B2") +
  
  # Roads
  geom_sf(data = roads$osm_lines, color = "gray80", alpha = .3) +
  
  # Points
  geom_point(data = Yum_cha_restaurants, aes(x = lng, y = lat, color = distance), size = 3) +
  
  # Legend
  scale_color_gradient(low = "#B80000", high = "#FF9800", name = "Distance (m)") +
  
  # Campus
  geom_point(aes(x = center_point[1], y = center_point[2]), size = 10, color = "#A6B37D") +
  
  # Theme
  theme_void(base_family = "Arial") +
  theme(
    plot.background = element_rect(fill = "black", color = NA),
    legend.position = c(0.65, 0.15),   # Move legend inside, specify (x, y) coordinates (adjust as needed)
    legend.direction = "horizontal", # Set legend to be horizontal
    legend.title = element_text(size = 9, color = "white"),   # Adjust legend title font size
    legend.text = element_text(size = 7, color = "white")     # Adjust legend text font size
  ) +
  
  # Set fixed x and y limits to maintain positions
  xlim(113.20, 113.40) +  # Adjust as needed for your data
  ylim(23.05, 23.15) +    # Adjust as needed for your data
  
  # Annotations
  ## image
  annotation_custom(img_grob, xmin = 113.23, xmax = 113.255, ymin = 23.075, ymax = 23.1) +
  ## title
  geom_richtext(aes(x = 113.24, y = 23.145, label = "<span style='color:white;'>Where to Savor </span><span style='color:#E0A75E;'>Yum Cha</span><br><span style='color:white;'>Around </span><span style='color:#A6B37D;'>Sun Yat-Sen University?</span>"), 
                size = 10, hjust = 0, fontface = "bold", family = "poppins", fill = NA,label.color = NA, lineheight = 1)+
  ## texts
  annotate("text", x = 113.31, y = 23.07, label = "3 KM", size = 6, hjust = 0, color = "#A6B37D",family = "Arial", lineheight = 1) +
  annotate("text", x = 113.26, y = 23.13, label = "Pedestrian Zone:\nBeijing Road Pedestrian Street",
           size = 4, hjust = 1, color = "#E0A75E", family = "poppins", lineheight = 1) +
  annotate("text", x = 113.33, y = 23.125, label = "CBD Area:\nZhujiang New Town",
           size = 4, hjust = 0, color = "#E0A75E", family = "poppins", lineheight = 1) +
  annotate("text", x = 113.3, y = 23.093, label = "Campus Commercial Area:\nXiadu Road",
           size = 4, hjust = 0.5, color = "#E0A75E", family = "poppins", lineheight = 1) +
  annotate("text", x = 113.231, y = 23.102, label = "Yum Cha ('Drink Tea')",
           size = 4, hjust = 0, color = "grey90", family = "poppins", lineheight = 1) +
  annotate(
    geom = "text",
    x = 113.23, y = 23.07,
    label = "#30DayMappingChallenge-2024\nDay 1: Points\nAuthor: Luming Xu\nMaster of Urban Spatial Analytics Program (MUSA) at Penn\n@Weitzman_musa\nlinkedin.com/in/luming-xu",
    color = "white",
    hjust = 0,
    vjust = 1,
    size = 3,family = "Arial", lineheight = 1
  )



# Save as a PNG file
ggsave("Outputs/01-luming-points.png", plot = Points, width = 10, height = 6, units = "in", dpi = 300)

