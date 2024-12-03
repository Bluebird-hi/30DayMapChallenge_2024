# Philly Chinatown 3D map

#### Setup ####

setwd("E:/Fall/MUSA6310 Communication/HW4-30DayMapChallenge/30DayMapChallenge_2024/29-Overture")
library(sf)
# install.packages('overturemapsr')
# install.packages("devtools")
# devtools::install_github("denironyx/overturemapsr")
library(overturemapsr)
library(tidyverse)
library(dplyr)
library(mapdeck)
library(htmlwidgets)
library(ggplot2)
library(magick)


#### Data ####

# Bounding box of Chinatown
ct_bbox <- c(-75.1635, 39.953, -75.151, 39.9573) # xmin, ymin, xmax, ymax

# Fetching overture building data
ct_building <- record_batch_reader('building', bbox = ct_bbox)

ct_height <- ct_building %>%
  filter(height != 0) %>%
  dplyr::select(id, geometry, subtype, names, height) %>%
  mutate(ID = row_number()) %>%
  filter(!ID %in% c(1:14,15:17,99:109,134:138,145:149)) %>%
  mutate(subtype = case_when(
    is.na(subtype) ~ "Other",
    TRUE ~ subtype
  ))

# ggplot(ct_height) +
#   geom_sf(aes(fill = subtype)) +
#   theme_minimal()

set_token("your_mapbox_api_key")

ct_height_plot <- ct_height %>%
  # filter(!is.na(subtype)) %>%
  mutate(colour = case_when(
    subtype == "commercial" ~ "#FFA1F5E6",
    subtype == "residential" ~ "#FEB941E6",
    subtype == "entertainment" ~ "#FF6500E6",
    subtype == "education" ~ "#9EDE73E6",
    subtype == "civic" ~ "#FFF100E6",
    subtype == "religious" ~ "#CDFFFCE6",
    subtype == "transportation" ~ "#86A7FCE6",
    subtype == "Other" ~ "#B2A59B"
  ),
  legend_label = case_when(
    subtype == "commercial" ~ "Commercial",
    subtype == "residential" ~ "Residential",
    subtype == "entertainment" ~ "Entertainment",
    subtype == "education" ~ "Education",
    subtype == "civic" ~ "Civic",
    subtype == "religious" ~ "Religious",
    subtype == "transportation" ~ "Transportation",
    TRUE ~ "Other"
  ))

ct_height_plot <- st_simplify(ct_height_plot, dTolerance = 0.001) %>%
  st_make_valid()


#### Plot: 3D map ####

map <- mapdeck(style = mapdeck_style("light"), pitch = 60, zoom = 15, location = c(-75.155, 39.95),
        bearing = 45) %>%
  add_polygon(
    data = ct_height_plot,
    elevation = "height",
    fill_colour = "colour",
    tooltip = "subtype",
    auto_highlight = TRUE
  )
saveWidget(map, "Outputs/mapdeck_map.html", selfcontained = TRUE)


#### Plot: legend ####

# Define the data for the legend
legend_data <- data.frame(
  subtype = c("Commercial", "Residential", "Entertainment", "Education", 
              "Civic", "Religious", "Transportation", "Other"),
  color = c("#FFA1F5E6", "#FEB941E6", "#FF6500E6", "#9EDE73E6", 
            "#FFF100E6", "#CDFFFCE6", "#86A7FCE6", "#B2A59BE6")
)
# Plot the legend
legend_plot <- ggplot(legend_data, aes(x = 1, y = subtype, fill = color)) +
  geom_tile() +
  scale_fill_identity() + # Use the exact colors provided
  geom_text(aes(label = subtype), color = "black", size = 5, hjust = 0, nudge_x = 0.1) +
  labs(title = "Building Types") +
  theme_void() +
  theme(
    legend.title = element_text(size = 18),
    legend.text = element_text(size = 12)
  )
ggsave("Outputs/legend.png", plot = legend_plot, units = "in", dpi = 300)


#### Plot: combine ####

img <- image_read("Outputs/screenshot.png")
# Define the size and position of the white rectangle
rect_height <- 220  # Height of the rectangle
rect_width <- image_info(img)$width  # Full width of the image
# Create a white rectangle
white_rect <- image_blank(width = rect_width, height = rect_height, color = "white")
# Combine the white rectangle with the original image (place the rectangle at the top)
img_final <- image_composite(img, white_rect, operator = "over", offset = "+0+0")
rect_height2 <- 70  # Height of the rectangle
# Create a white rectangle
white_rect2 <- image_blank(width = rect_width, height = rect_height2, color = "white")
# Combine the white rectangle with the original image (place the rectangle at the top)
img_final <- image_composite(img_final, white_rect2, operator = "over", gravity = "south", offset = "+0+0")

img_final <- image_annotate(img_final, "Philadelphia Chinatown", 
                            size = 60, color = "black",
                            font = "serif bold",
                            gravity = "northwest",
                            location = "+50+50")

text <- "The predominantly Asian American neighborhood stretches from Vine Street on the north,\nArch Street on the south, North Franklin Street and N. 7th Street on the east, to North Broad Street on the west."
img_final <- image_annotate(img_final, text, 
                            size = 25, color = "black",
                            font = "serif bold",
                            gravity = "northwest",
                            location = "+50+130")
img_final <- image_annotate(img_final, 
                            "#30DayMapChallenge-2024 | Day 29: Overture Data | Author: Luming Xu | @UPenn_Weitzman_MUSA | linkedin.com/in/luming-xu", 
                            size = 20, color = "black", 
                            font = "poppins",
                            gravity = "south",
                            location = "+0+20")
image_write(img_final, path = "Outputs/final_map.png")

legend <- image_read("Outputs/legend.png")
# Optionally, resize the overlay image
legend_resized <- image_scale(legend, "400x200")

# Combine the images
combined_image <- image_composite(img_final, legend_resized, operator = "over", gravity = "southeast", offset = "+180+120")


image_write(combined_image, path = "Outputs/29-Luming-Overture.png")
