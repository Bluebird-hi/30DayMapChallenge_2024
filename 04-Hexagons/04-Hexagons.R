setwd("E:/Fall/MUSA6310 Communication/HW4-30DayMapChallenge/30DayMapChallenge_2024/04-Hexagons")

# Load required libraries
library(tidycensus)
library(ggplot2)
library(sf)
library(dplyr)
library(magick)

# Load API
census_api_key("YOUR_API_KEY", overwrite = TRUE)

# Retrieve ACS 2022 data for the Hong Kong population in California
# Assuming the variable `B04006_009` represents "Hong Kong" ancestry (check variables with load_variables())
census2022 <- get_acs(
  geography = "tract",
  variables = "B04006_009",
  state = "CA",
  year = 2022,
  geometry = TRUE
) %>% 
  st_transform('EPSG:2227')

hk_population <- census2022 %>%
  filter(estimate != 0)

# Extract centroid coordinates for each geometry
hk_population_coords <- hk_population %>%
  mutate(
    lon = st_coordinates(st_centroid(geometry))[, 1],
    lat = st_coordinates(st_centroid(geometry))[, 2]
  )

# Create a data frame for hexbin (drop geometry for geom_hex)
hk_population_df <- hk_population_coords %>%
  st_drop_geometry()

CA <- census2022 %>% st_union()

# Create the hexbin map
plot <- ggplot(hk_population_df) +
  geom_sf(data = CA, fill = "white", color = "black", size = 2) +
  geom_hex(aes(x = lon, y = lat, fill = estimate), bins = 30) +
  scale_fill_viridis_c(option = "magma", name = "Hong Kong Pop") +
  theme_minimal() +
  theme(legend.position = c(.2,.16),
        plot.background = element_rect(fill = "#FFE7E7", color = NA),
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 26),
        axis.title = element_blank(),
        panel.grid.major = element_line(color = "grey80"))+
  stat_summary_hex(aes(x = lon, y = lat, z = estimate), fun = sum)

ggsave(filename = "Outputs/map.png", plot = plot, height = 7, width = 5, units = "in", dpi = 300)
img <- image_read("Outputs/map.png")


img_final <- image_annotate(img, "Where do Hong Kong people live in California?", 
                            size = 70, color = "black",
                            font = "serif bold",
                            gravity = "north",
                            location = "+0+80")
img_final <- image_annotate(img_final, 
                            "California has the most immigrants\nfrom Hong Kong in the United States.\nSource: U.S. Census Bureau, ACS 2022", 
                            size = 40, color = "black", 
                            font = "poppins",
                            gravity = "northwest",
                            location = "+700+600")
img_final <- image_annotate(img_final, 
                            "#30DayMapChallenge-2024 | Day 4: Hexagons\nAuthor: Luming Xu @UPenn_Weitzman_MUSA | linkedin.com/in/luming-xu", 
                            size = 30, color = "black", 
                            font = "poppins",
                            gravity = "southwest",
                            location = "+50+30")


image_write(img_final, path = "Outputs/04-Luming-Hexagons.png")
