# https://data.humdata.org/dataset/?ext_geodata=1&dataseries_name=WorldPop+-+Population+Density&groups=chn&groups=hkg&groups=mac&q=&sort=last_modified+desc&ext_page_size=25
# http://www.guihuayun.com/maps/region_datav.php
# Population Density in the Greater Bay Area
# Raster/Data: HDX/Time and Space

#### Setup ####

library(terra)
library(tidyverse)
library(sf)
library(ggplot2)
library(dplyr)
library(classInt)
library(magick)
setwd("E:/Fall/MUSA6310 Communication/HW4-30DayMapChallenge/30DayMapChallenge_2024/08-HDX")


#### Data ####

# Raster data: population density
chn <- rast("data/chn_pd_2020_1km.tif")
hk <- rast("data/hkg_pd_2020_1km.tif")
mac <- rast("data/mac_pd_2020_1km.tif")
merged <- merge(chn, hk, mac)

# Polygons: Boundries of Greater Bay Area
hk_bound <- st_read("https://geo.datav.aliyun.com/areas_v3/bound/810000.json") %>%
  st_transform(st_crs(hk)) %>%
  dplyr::select(adcode, name, geometry)
mac_bound <-  st_read("https://geo.datav.aliyun.com/areas_v3/bound/820000.json") %>%
  st_transform(st_crs(hk)) %>%
  dplyr::select(adcode, name, geometry)
gd_bound <- st_read("https://geo.datav.aliyun.com/areas_v3/bound/440000_full.json") %>%
  st_transform(st_crs(hk)) %>%
  dplyr::select(adcode, name, geometry)
gba_bound <- rbind(
  gd_bound %>%
    filter(name %in% c("广州市", "深圳市", "珠海市", "佛山市", "东莞市", "中山市", "江门市", "惠州市", "肇庆市")),
  hk_bound,
  mac_bound)
gba_union <- gba_bound %>%
  st_make_valid() %>%  # Ensure geometries are valid
  st_buffer(0) %>%     # Fix minor topology issues (if any)
  st_union()
## background
gx_bound <-  st_read("https://geo.datav.aliyun.com/areas_v3/bound/450000_full.json") %>%
  st_transform(st_crs(hk)) %>%
  dplyr::select(adcode, name, geometry)
ganzhou_bound <- st_read("https://geo.datav.aliyun.com/areas_v3/bound/360700.json") %>%
  st_transform(st_crs(hk)) %>%
  dplyr::select(adcode, name, geometry)
background <- rbind(
  gd_bound %>%
    filter(!(name %in% c("广州市", "深圳市", "珠海市", "佛山市", "东莞市", "中山市", "江门市", "惠州市", "肇庆市"))),
  gx_bound, ganzhou_bound)

# Convert the sf object to a SpatVector
gba_union_vect <- vect(gba_union)
# Crop the GeoTIFF to the polygon's bounding box
cropped <- crop(merged, gba_union_vect)
# Mask the GeoTIFF to the polygon area
clipped <- mask(cropped, gba_union_vect)
# Convert to a data frame for ggplot
clipped_df <- as.data.frame(clipped, xy = TRUE)
## Quantiles
clipped_df <- clipped_df %>%
  mutate(quantile = ntile(chn_pd_2020_1km, 5)) 
## Natural breaks
breaks <- classIntervals(clipped_df$chn_pd_2020_1km, n = 5, style = "fisher")$brks
clipped_df <- clipped_df %>%
  mutate(jenks = cut(chn_pd_2020_1km, breaks = breaks, include.lowest = TRUE))
formatted_labels <- paste0("[", round(breaks[-length(breaks)], 2), 
                           ", ", round(breaks[-1], 2), "]")
# Reassign formatted labels to the class variable
clipped_df <- clipped_df %>%
  mutate(jenks = factor(jenks, labels = formatted_labels))


#### Plot ####

# define the spatial extent
lat1 <- 21.4; lat2 <- 24.4; lon1 <- 111.2; lon2 <- 116
# bbbox
bbox <- c(xmin = lon1, ymin = lat1, xmax = lon2, ymax = lat2)

plot <- ggplot()+
  geom_raster(data = clipped_df, aes(x = x, y = y, fill = jenks)) +
  scale_fill_brewer(palette = "PuRd", name = "Natural Breaks") +
  coord_equal() + 
  geom_sf(data = gba_bound, fill = "transparent", color = "#525CEB", lwd = .4) +
  geom_sf(data = background, fill = "#BFCFE7", color = alpha("#525CEB", .5), lwd = .3) +
  # Bouding Box
  coord_sf(xlim = c(bbox[1], bbox[3]), ylim = c(bbox[2], bbox[4])) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#667BC6", color = NA),
        legend.position = c(0.62, 0.06),
        legend.direction = "horizontal",
        legend.title = element_text(size = 9, color = "#e7e1ef"),
        legend.text = element_text(size = 7, color = "#e7e1ef")) 

ggsave(filename = "Outputs/map.png", plot = plot, height = 7, width = 10, units = "in", dpi = 300)
img <- image_read("Outputs/map.png")
img_final <- image_annotate(img, "2020", 
                            size = 200, color = "#e7e1ef",
                            font = "serif bold",
                            gravity = "southeast",
                            location = "+50+500")
img_final <- image_annotate(img_final, "Population Density of\nthe Greater Bay Area", 
                            size = 100, color = "#e7e1ef",
                            font = "serif bold",
                            gravity = "southeast",
                            location = "+50+300")
img_final <- image_annotate(img_final, 
                            "#30DayMapChallenge-2024 | Day 8: HDX Data | Author: Luming Xu | @UPenn_Weitzman_MUSA | linkedin.com/in/luming-xu", 
                            size = 40, color = "#e7e1ef", 
                            font = "poppins",
                            gravity = "southeast",
                            location = "+50+50")
image_write(img_final, path = "Outputs/08-Luming-HDX.png")
  