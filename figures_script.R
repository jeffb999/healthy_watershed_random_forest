# Random Forest Figures
# December 4, 2020
# Heili Lowman

# The following script will create some figures used to supplement random forest outputs for the Health Watersheds Partnership program.

# Ventura River Watershed figure

# Load packages.
library(tidyverse)
library(sf)
library(ggspatial)
library(nhdplusTools)
library(patchwork)
library(Metrics)

# Load datasets.
asci_stations <- read_csv("asci_stations.csv") # Loads ASCI analysis table merged with lustations datasets from the SMC database.

nhd_vr <- read_sf("/Users/heilil/Desktop/hw_datasets/NHD_Watersheds/VenturaRiver_NHD_Clip.shp") %>%
  mutate(COMID = as.numeric(COMID))

st_crs(nhd_vr) # Checks Coordinate reference system, so when I make the sf transformations below, they're the same. crs is NAD83 which is EPSG 7131.

# Tidy datasets.

asci_vr <- asci_stations %>% # Use loaded dataset.
  filter(smcshed == "Ventura") %>% # Filter only for Ventura River watershed sites.
  mutate(classification = case_when(result < 0.67~"Very Likely Altered",
    result < 0.82~"Likely Altered",
    result < 0.93~"Possibly Altered",
    result >= 0.93~"Likely Unaltered")) %>% # Create classification column for plotting.
  mutate(class_f = factor(classification, levels = c("Very Likely Altered", "Likely Altered", "Possibly Altered", "Likely Unaltered"))) %>% # Relevel classifications.
  drop_na(class_f) # Will prevent NAs from displaying in legend.

asci_vr_sf<- st_as_sf(asci_vr,
  coords = c("longitude", "latitude"), # note we put lon (or X) first!
  remove = F, # don't remove lat/lon cols from the dataframe
  crs = 7131) # add projection (this is NAD83)

# Map stream reaches with sampling sites as points.

vt_site_map <- ggplot(nhd_vr) +
  geom_sf(color = "black") +
  geom_point(data = asci_vr_sf, aes(x = longitude, y = latitude, color = class_f), size = 4, alpha = 0.75) +
  scale_color_manual(name = "Condition", values = c("red2", "lightpink", "lightskyblue2", "steelblue")) +
  labs(title = "Ventura River",
    x = " ",
    y = " ") +
  theme_bw()
  
vt_site_map

# ggsave("asci_sampled_Ventura.png",
#      path = "/Users/heilil/Desktop/R_figures",
#      width = 15,
#      height = 15,
#      units = "cm"
#    )

# End of script.