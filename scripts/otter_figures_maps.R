# This file is to make maps for the synthesis document.  It is based on scratch_pad_map_chat in the GC folder.

# library ----
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
library(cowplot)
library(ggrepel)
library(ggspatial)
library(mapview)
library(leaflet)

# basic map ----
# layers 
# Get Canada map at province level
canada <- ne_states(country = "Canada", returnclass = "sf")

# Filter for Newfoundland and Labrador
newfoundland <- canada %>% 
  filter(name_en == "Newfoundland and Labrador")


ggplot(data = newfoundland) +
  geom_sf(fill = "lightblue", color = "black") +
  coord_sf() +
  theme_minimal() +
  ggtitle("Map of Newfoundland and Labrador")

# island ----
# Download GADM Canada at level 1 (province/territory)
# canada_prov <- st_read("https://geodata.ucdavis.edu/gadm/gadm4.1/shp/gadm41_CAN_shp.zip", 
#                        layer = "gadm41_CAN_1")
# Download GADM Canada at level 1 (province/territory)
canada_prov <- st_read("C:/Users/lewiske/Documents/CAFE/projects/restoration/Granite/analyses/restoration_Granite/data_geo", layer = "gadm41_CAN_1")

# Filter to Newfoundland and Labrador
nl <- canada_prov %>% filter(NAME_1 == "Newfoundland and Labrador")

# Cast into individual polygons
nl_polygons <- nl %>% st_cast("POLYGON")


# Compute area in square meters (default unit from st_area)
nl_polygons <- nl %>% st_cast("POLYGON")
areas <- st_area(nl_polygons)

# Set size threshold (e.g., 100 km² = 100,000,000 m²) to eliminate small islands and Labrador
threshold <- units::set_units(100, km^2)
threshold2 <- units::set_units(150000, km^2)

# Filter polygons larger than threshold
large_polygons2 <- nl_polygons[threshold2 > areas & areas > threshold, ]


# Filter polygons: east of -58 and south of 52; removes small islands around Labrador
centroids <- st_centroid(large_polygons2)
coords <- st_coordinates(centroids)
#coords <- st_coordinates(large_polygons2)
large_polygons_island <- large_polygons2[coords[, 1] > -58 & coords[, 2] < 52, ]


# Merge into one geometry (optional)
newfoundland_island <- st_union(large_polygons_island) %>% st_sf()


## inset CAN----
canada_outline <- ne_states(country = "Canada", returnclass = "sf")

inset_map <- ggplot(canada_outline) +
  geom_sf(fill = "white", color = "gray50") +
  geom_sf(data = nl, fill = "gray90", color = "black") +
  geom_rect(aes(xmin = -60, xmax = -51, ymin = 46.5, ymax = 52, colour = "red"), fill = NA, linewidth = 1.25) +
  coord_sf(
    xlim = c(-142, -50.5),
    ylim = c(41, 84),
    expand = FALSE
  ) +
  theme_void() +
  theme(legend.position = "none")

# Wrap the inset in a plot with a border
inset_map <- inset_map +
  theme(
    plot.background = element_rect(
      colour = "black",
      fill = "white",
      linewidth = 0.8
    ),
    plot.margin = margin(0, 0, 0, 0)
  )


# island ----
main_map <- ggplot(newfoundland_island) +
  geom_sf(fill = "grey", color = "black") +
  # geom_segment(data = study.areas,
  #              aes(x = xend, y = yend, xend = long, yend = lat),
  #              arrow = arrow(length = unit(0.01, "npc"))
  # ) +
  ylab("Latitude") +
  xlab("Longitude") +
  annotation_north_arrow(location = "tl",which_north = "true", 
                         pad_x = unit(0.1, "cm"), pad_y = unit(0.1, "cm"),
                         style = north_arrow_orienteering,width = unit(1, "cm"), 
                         height = unit(1, "cm")) +
  annotation_scale() +
  #  coord_fixed(1.3) +
  theme_minimal()
main_map
#ggsave("figures/3study_areas.png", main_map, width = 8, height = 6, dpi = 300, units = "in")

main_map_no_text_no_compass <- ggplot(newfoundland_island) +
  geom_sf(fill = "grey", color = "black") +
  #geom_point(data = study.areas, aes(long, lat)) +
  #geom_text(data = study.areas, aes(x, y, label = name )) +
  # geom_segment(data = study.areas,
  #              aes(x = xend, y = yend, xend = long, yend = lat),
  #              arrow = arrow(length = unit(0.01, "npc"))
  # ) +
  ylab("Latitude") +
  xlab("Longitude") +
  # annotation_north_arrow(location = "tl",which_north = "true", 
  #                        pad_x = unit(0.1, "cm"), pad_y = unit(0.1, "cm"),
  #                        style = north_arrow_orienteering,width = unit(1, "cm"), 
  #                        height = unit(1, "cm")) +
  annotation_scale() +
  #  coord_fixed(1.3) +
  theme_minimal()
main_map_no_text_no_compass
#ggsave("Figures/3study_areas.png", main_map, width = 8, height = 6, dpi = 300, units = "in")


# Combine using cowplot
final_map <- cowplot::ggdraw() +
  draw_plot(main_map) +
  draw_plot(inset_map, x = 0.58, y = 0.68, width = 0.3, height = 0.3)
print(final_map)
#ggsave("Figures/3study_areas_inset.png", final_map, width = 8, height = 6, dpi = 300, units = "in")


# study area ----
bbox_sf <- st_bbox(c(xmin = -53.35, ymin = 48.3, xmax = -54.32, ymax = 48.95),
                   crs = st_crs(newfoundland_island))  # match whatever CRS your CHS data is in
newfoundland_island_crop <- st_crop(newfoundland_island, bbox_sf)


df_all_new <- read.csv("data_derived/df_all_new.csv")

study_area_map <- ggplot(newfoundland_island_crop) +
  geom_sf(fill = "grey", color = "black") +
  geom_point(data = df_all_new, aes(long, lat, colour = type, shape = location)) +
  #geom_text(data = study.areas, aes(x, y, label = name )) +
  # geom_segment(data = study.areas,
  #              aes(x = xend, y = yend, xend = long, yend = lat),
  #              arrow = arrow(length = unit(0.01, "npc"))
  # ) +
  ylab("Latitude") +
  xlab("Longitude") +
  # annotation_north_arrow(location = "tl",which_north = "true", 
  #                        pad_x = unit(0.1, "cm"), pad_y = unit(0.1, "cm"),
  #                        style = north_arrow_orienteering,width = unit(1, "cm"), 
  #                        height = unit(1, "cm")) +
  annotation_scale() +
  #  coord_fixed(1.3) +
  theme_minimal()

study_area_map




# study area with polygon ----
# this plots a polygon around Alexander Bay
poly_coords <- data.frame(
  lon = c(-54.08, -53.87, -53.71, -53.77, -53.88, -54.06, -54.08),
  lat = c(48.85, 48.89, 48.83, 48.68, 48.62, 48.60, 48.85)
)


# Create polygon from coordinates
poly <- st_polygon(list(as.matrix(poly_coords)))

# Convert to sf object
poly_sf <- st_sfc(poly, crs = 4326)

# Convert points to sf
pts_sf <- st_as_sf(df_all_new,
                   coords = c("long", "lat"),
                   crs = 4326)

# Identify points inside polygon
inside <- st_within(pts_sf, poly_sf, sparse = FALSE)[,1]

# Create location variable
df_all_new$location <- ifelse(inside, "AB", "TN")


study_area_map_polygon <- ggplot(newfoundland_island_crop) +
  geom_sf(fill = "grey", color = "black") +
  geom_point(data = df_all_new, aes(long, lat, colour = type, shape = location)) +
  ylab("Latitude") +
  xlab("Longitude") +
  geom_path(data = poly_coords,
            aes(lon, lat),
            colour = "red",
            linewidth = 1) +
  annotation_scale() +
  theme_minimal()

study_area_map_polygon
