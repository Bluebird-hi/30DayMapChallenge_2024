# Potential Innovation Districts of Shenzhen (K-means Clustering Analysis)

#### Setup ####

library(tidyverse)
library(sf)
library(ggplot2)
library(dplyr)
library(ggmap)
library(magick)
setwd("E:/Fall/MUSA6310 Communication/HW4-30DayMapChallenge/30DayMapChallenge_2024/15-My Data")


#### Data ####

mydata <- st_read("data/mydata.geojson")
shenzhen <- st_read("data/shenzhen.geojson")
cluster <- mydata %>%
  dplyr::select(FID_1, Test.9.2, geometry)
cluster <- cluster %>%
  mutate(type = case_when(
    Test.9.2 == 1 ~ "No Pattern",
    Test.9.2 == 2 ~ "Science +",
    Test.9.2 == 8 ~ "High Tech +",
    Test.9.2 == 6 ~ "Fashion +",
    Test.9.2 == 7 ~ "Urbanised Manufacturing Park",
    Test.9.2 == 0 ~ "Urbanised Manufacturing Park (Richer Assets)",
    Test.9.2 == 5 ~ "Re-imagined Cultural Area",
    Test.9.2 == 3 ~ "Art and Literature +",
    Test.9.2 == 4 ~ "Re-imagined Manufacturing Area"
  )) %>%
  mutate(type = factor(type, levels = c(
    "No Pattern", 
    "Science +", 
    "High Tech +", 
    "Fashion +", 
    "Urbanised Manufacturing Park", 
    "Re-imagined Cultural Area", 
    "Art and Literature +", 
    "Re-imagined Manufacturing Area",
    "Urbanised Manufacturing Park (Richer Assets)"
  )))
cluster <- st_transform(cluster, st_crs(shenzhen))


#### Plot ####

# Define the bounding box or extent
bbox <- c(left = 113.6, bottom = 22.3, right = 114.8, top = 23)  # Example: San Francisco area

# Register your Stadia Maps API key
register_stadiamaps(key = "YOUR_STADIA_API_KEY")
# Get Stamen Toner Lite map tiles
stamen_map <- get_stadiamap(bbox, zoom = 12, maptype = "stamen_toner_lite")

plot <- 
  ggmap(stamen_map) +
  # Add a white overlay with alpha transparency (semi-transparent white)
  annotate("rect", xmin = bbox[1], xmax = bbox[3], ymin = bbox[2], ymax = bbox[4], 
           fill = "white", alpha = 0.7) +
  # Plot the 'No Pattern' data, but exclude it from the legend
  geom_sf(data = cluster %>% filter(type == "No Pattern"), 
          fill = "#FFFFFF80", 
          color = "#DED8E5", 
          lwd = .1, 
          inherit.aes = FALSE, 
          show.legend = FALSE) +  # Exclude 'No Pattern' from the legend
  # Plot the rest of the data, including 'No Pattern' color in the legend
  geom_sf(data = cluster %>% filter(type != "No Pattern"), 
          aes(fill = type), 
          color = "#DED8E5", 
          lwd = .1, 
          inherit.aes = FALSE) +
  # geom_sf(data = cluster, aes(fill = type), color = "#DED8E5", lwd = .1, inherit.aes = FALSE) +
  scale_fill_manual(values = c(
    # "No Pattern" = "#FFFFFF80",
    "Science +" = "#997ACD", 
    "High Tech +" = "#C392DA", 
    "Fashion +" = "#E9ABE6", 
    "Urbanised Manufacturing Park" = "#E4BDE6", 
    "Urbanised Manufacturing Park (Richer Assets)" = "#DED8E5", 
    "Re-imagined Cultural Area" = "#D6FAE4", 
    "Art and Literature +" = "#A6F0E5", 
    "Re-imagined Manufacturing Area" = "#6AC6D6"
  )) +
  geom_sf(data = shenzhen, fill = "transparent", color = "black", lwd = .4, inherit.aes = FALSE) +
  theme_void() +
  theme(legend.position = c(0.5, 0.12),
        legend.direction = "horizontal",
        legend.title = element_blank(),
        legend.text = element_text(size = 9, color = "#997ACD", face = "bold")) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))


ggsave(filename = "Outputs/map.png", plot = plot, height = 6, width = 9.5, units = "in", dpi = 300)
img <- image_read("Outputs/map.png")


img_final <- image_annotate(img, "Identifying and Classifying Potential Innovation Districts\nA Case Study of Shenzhen, China", 
                            size = 100, color = "#997ACD",
                            font = "serif bold",
                            gravity = "north",
                            location = "+50+50")
img_final <- image_annotate(img_final, "(K-Means Clustering Analysis)", 
                            size = 70, color = "#997ACD",
                            font = "serif bold",
                            gravity = "north",
                            location = "+50+300")
img_final <- image_annotate(img_final, 
                            "#30DayMapChallenge-2024 | Day 15: My Data | Author: Luming Xu | @UPenn_Weitzman_MUSA | linkedin.com/in/luming-xu", 
                            size = 40, color = "#997ACD", 
                            font = "poppins",
                            gravity = "south",
                            location = "+50+30")


image_write(img_final, path = "Outputs/15-Luming-My Data.png")
