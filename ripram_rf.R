# RipRAM Random Forest Draft
# January 29, 2021
# Heili Lowman

# The following script will walk through a random forest created to predict state-wide RipRAM scores, with datasets from Kevin O'Connor, SMC, and StreamCat databases. The dependent variable in this case will be the Riparian Rapid Assessment Method (RipRAM) index state-wide.

# Step One - Load In ------------------------------------------------------

# Load packages.
library(quantregForest)
library(caret)
library(tidyverse)
library(tidymodels)
library(skimr)
library(sf)
library(ggspatial)
library(nhdplusTools)
library(patchwork)
library(Metrics)
library(gt)
library(sp)
library(maptools)
library(rgdal)

# Load datasets.

# Need to bind RipRAM data to NHD CA dataset to get COMIDs.
# Load in cleaned dataset from Kevin O'Connor's spreadsheet.
ripram_df <- read_csv("RipRAM_clean_012621.csv")

ripram_sf <- st_as_sf(ripram_df, # create sf compatible dataframe
    coords = c("D_long", "D_lat"), # identify lon & lat
    remove = F, # do not remove lat/lon columns
    crs = 4269) # use NAD83 projection

# Load in NHD_Plus_CA dataset from Annie as well as watersheds from Jeff.
# Full state of California
nhd_ca <- read_sf("/Users/heilil/Desktop/hw_datasets/NHD_Plus_CA/NHDPlus_V2_FLowline_CA.shp") %>%
  mutate(COMID = as.numeric(COMID))

nhd_z <- st_zm(nhd_ca)

EPSG <- make_EPSG() # create data frame of available EPSG codes
EPSG[grepl("NAD83$", EPSG$note), ]# search for NAD 83 code
# Units are in meters

# Join NHD dataset to RipRAM dataset.
# Using this method, provided by O. Liu, since it's closest to the ArcGIS workflow.
stream_samples_join <- st_join(nhd_ca, ripram_sf, 
  join = st_is_within_distance, # the predicate to st_join
  dist = 40) # joins samples within 40 m distance
stream_remaining <- stream_samples_join %>%
  filter(SiteTag != "NA")

# Another option is to make polygon buffer from lines.
# This yielded a lot of duplicate matches (150+) so I chose to go with the above workflow.
#streams_buff <- st_buffer(nhd_z, dist = 0.001) # width/thickness = 0.001 degrees
#streams_samples_join <- st_join(streams_buff, ripram_sf) # Joins points to polygons made in the line above.
#streams_remaining <- streams_samples_join %>%
#  filter(SiteTag != "NA")

# Exporting dataset for Annie to double-check in ArcGIS.
as_tibble(stream_remaining) %>% # remove geometry
  select(c(COMID, D_lat, D_long)) %>% # select necessary columns
  write_csv("ripram_sites.csv") # export as .csv

# End of script.
