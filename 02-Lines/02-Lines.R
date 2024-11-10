# Mapping Roads of Guangzhou University City

#### Setup ####
library(tidyverse)
library(sf)
library(osmdata)
library(ggplot2)
library(leaflet) # mapbox
library(mapdeck) # mapbox style
library(mapboxapi) # interact with API
library(geojsonio)
library(showtext) # font family
library(sysfonts) # font family
library(mapview) # save static map
library(magick) # add annotation
library(ggtext)
# Load the font family
font_add_google("Poppins", "poppins")
showtext_auto() 

#### Data ####
# Roads
gz_roads <- st_read("data/roads2/ROAD.shp")
gz_roads_clean <- gz_roads %>%
  dplyr::select(ID, Type, RoadFuncti, geometry, DistIDs, Delete)
# Create a bounding box as an sf object
bbox <- st_bbox(c(xmin = 113.34, ymin = 23.03, xmax = 113.43, ymax = 23.09), crs = st_crs(gz_roads_clean))
# Convert bounding box to an sf object (polygon) to use for cropping
bbox_sf <- st_as_sfc(bbox)
# Crop the shapefile to the bounding box area
uc_roads <- st_intersection(gz_roads_clean, bbox_sf) %>%
  mutate(RoadFuncti = as.factor(RoadFuncti),
         Type = as.factor(Type)) %>%
  filter(DistIDs == 440113) %>%
  mutate(Delete = case_when(
    is.na(Delete) ~ 0,
    TRUE ~ Delete
  )) %>%
  filter(Delete != 1)

#### Plot: prepare ####
# Create a custom color palette and width mapping
color_mapping <- c("420100" = "#BC7C7C", 
                   "420300" = "#E4C087", 
                   "430120" = "#A2D2DF",
                   "430200" = "#E49BFF",
                   "430300" = "#8967B3",
                   "430400" = "#BBE9FF",
                   "430500" = "#A6B37D",
                   "430600" = "#ADD899")
width_mapping <- c("420100" = 4, 
                   "420300" = 3.5, 
                   "430120" = 4.5,
                   "430200" = 4.5,
                   "430300" = 1.5,
                   "430400" = 1.5,
                   "430500" = 1,
                   "430600" = .5)
# Set the desired order for `Type`
uc_roads$Type <- factor(uc_roads$Type, levels = c("430600", "430500", "430400", "430300", "430200", "430120", "420300", "420100"))
# Sort data based on the factor levels of `Type`
uc_roads_linewidth <- uc_roads %>%
  arrange(Type) %>%
  mutate(linewidth = width_mapping[as.character(Type)],
         color = color_mapping[as.character(Type)])

# Basemap
# Mapbox Access Token
mapbox_token <- "your_mapbox_access_token"

# Mapbox Style URL
style_url <- paste0("https://api.mapbox.com/styles/v1/bluebirdxlm/cm36cgrbf014p01qk7824737e/tiles/{z}/{x}/{y}?access_token=", mapbox_token)

# Create the map using leaflet and Mapbox basemap
basemap <- leaflet() %>%
  addTiles(urlTemplate = style_url) %>%   # Add the Mapbox style URL
  fitBounds(113.36, 23.04, 113.41, 23.08)  # Fit map to the bounding box
basemap

# Remove names from color column, if it's a named vector
uc_roads_linewidth$color <- unname(uc_roads_linewidth$color)
uc_roads_linewidth$linewidth <- unname(uc_roads_linewidth$linewidth)

#### Plot: roads ####
# Plot
Lines <- 
  leaflet(options = leafletOptions(
  zoomControl = FALSE,  # Disable zoom controls
  dragging = FALSE,     # Disable panning (dragging)
  touchZoom = FALSE,    # Disable pinch-to-zoom on touch devices
  scrollWheelZoom = FALSE # Disable zooming with the mouse scroll wheel
)) %>%
  addTiles(urlTemplate = style_url) %>%   # Add the Mapbox style URL
  fitBounds(113.36, 23.04, 113.385, 23.08) %>%
  addPolylines(data = uc_roads_linewidth,
               color = ~color,          # Use the 'Type' column as hex color codes
               weight = ~linewidth,    # Use the 'linewidth' column for line width
               opacity = 1)

# Save the map as a static image using mapview
mapshot(Lines, 
        file = "Outputs/static_map.png")

#### Plot: annotations ####
# Load the saved image
img <- image_read("Outputs/static_map.png")

# Add the text annotation (with a custom font)
# Initial position for text
x_pos <- 50
y_pos <- 55

# Annotate the first line of text
img_final <- image_annotate(img, "A Real Garden City?!", 
                      size = 40, color = "white", 
                      font = "poppins",
                      location = paste0("+", x_pos, "+", y_pos))
# Overlay the text slightly offset for thicker effect
img_final <- image_annotate(img_final, "A Real Garden City?!", 
                            size = 40, color = "white", 
                            font = "poppins", 
                            location = paste0("+", x_pos+2, "+", y_pos+2))  # Slight offset
# Annotate each character with specific colors for "Roads"
img_final <- image_annotate(img_final, "Ro", 
                      size = 40, color = "#E49BFF", 
                      font = "poppins",
                      location = paste0("+", x_pos, "+", y_pos + 50))  # Adjust y for new line
x_pos <- x_pos + 52  # Move right for next letters

img_final <- image_annotate(img_final, "a", 
                      size = 40, color = "#BBE9FF", 
                      font = "poppins",
                      location = paste0("+", x_pos, "+", y_pos + 50))
x_pos <- x_pos + 22

img_final <- image_annotate(img_final, "d", 
                      size = 40, color = "#A6B37D", 
                      font = "poppins",
                      location = paste0("+", x_pos, "+", y_pos + 50))
x_pos <- x_pos + 22

img_final <- image_annotate(img_final, "s", 
                      size = 40, color = "#ADD899", 
                      font = "poppins",
                      location = paste0("+", x_pos, "+", y_pos + 50))

# Annotate the rest of the title
img_final <- image_annotate(img_final, " of Guangzhou University City (17.9 km\u00B2)", 
                      size = 40, color = "white", 
                      font = "poppins",
                      location = paste0("+", x_pos + 22, "+", y_pos + 50))

img_final <- image_annotate(img_final, 
                            "#30DayMapChallenge-2024\nDay 2: Lines | Author: Luming Xu | @UPenn_Weitzman_MUSA | linkedin.com/in/luming-xu", 
                            size = 15, color = "white", 
                            font = "poppins",
                            location = paste0("+50+690"))

# Load the garden city image
# Source: https://www.pinterest.com/pin/282178732894559192/
GC <- image_read("data/GARDEN CITIES.png") 
# Resize overlay image if necessary
GC_resize <- image_resize(GC, "350x350")
# Overlay the image on top of the annotated image
img_final_GC <- image_composite(img_final, GC_resize, 
                             offset = "+50+190")

#### Plot: save ####
# Save the new image with the title
image_write(img_final_GC, path = "Outputs/02-Luming-Lines.png")
