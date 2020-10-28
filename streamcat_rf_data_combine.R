# StreamCat Data Combination Script
# Heili Lowman
# October 21, 2020

# The data used below will not be posted to GitHub, because the files are too large to store on GitHub. Instead, I'll use file paths to direct to my personal machine to retrieve them.

#### PACKAGES ####

library(tidyverse) # loaded for data manipulation

#### DATASETS ####

# Load in datasets (organized by state) downloaded from the streamcat website : https://www.epa.gov/national-aquatic-resource-surveys/streamcat-dataset-0

/Users/heilil/Desktop/

agnitrogen <- read_csv('/Users/heilil/Desktop/hw_datasets/AgriculturalNitrogen_CA.csv')
candensity <- read_csv('/Users/heilil/Desktop/hw_datasets/CanalDensity_CA.csv')
dams <- read_csv('/Users/heilil/Desktop/hw_datasets/Dams_CA.csv')
epa <- read_csv('/Users/heilil/Desktop/hw_datasets/EPA_FRS_CA.csv')
geochem <- read_csv('/Users/heilil/Desktop/hw_datasets/GeoChemPhys1_CA.csv')
kff <- read_csv('/Users/heilil/Desktop/hw_datasets/Kffact_CA.csv')
mines <- read_csv('/Users/heilil/Desktop/hw_datasets/Mines_CA.csv')
minesrp <- read_csv('/Users/heilil/Desktop/hw_datasets/MinesRipBuf100_CA.csv')
nabd <- read_csv('/Users/heilil/Desktop/hw_datasets/NABD_CA.csv')
nlcd16 <- read_csv('/Users/heilil/Desktop/hw_datasets/NLCD2016_CA.csv')
nlcd16rp <- read_csv('/Users/heilil/Desktop/hw_datasets/NLCD2016RipBuf100_CA.csv')
prism <- read_csv('/Users/heilil/Desktop/hw_datasets/PRISM_1981_2010_CA.csv')
rddensity <- read_csv('/Users/heilil/Desktop/hw_datasets/RoadDensity_CA.csv')
rddensityrp <- read_csv('/Users/heilil/Desktop/hw_datasets/RoadDensityRipBuf100_CA.csv')
rdstream <- read_csv('/Users/heilil/Desktop/hw_datasets/RoadStreamCrossings_CA.csv')
stats <- read_csv('/Users/heilil/Desktop/hw_datasets/STATSGO_Set2_CA.csv')

#### DATA TIDYING ####

# Select only columns containing data of interest.
# Note, the below has been edited to aggregate:
# (1) only parameters directly related to human activity
# (2) parameters by % urban, % ag, and % open land uses

agnitrogen1 <- agnitrogen %>%
  select(COMID, FertCat, FertWs, CBNFCat, CBNFWs, ManureCat, ManureWs)

candensity1 <- candensity %>%
  select(COMID, CanalDensCat, CanalDensWs)

dams1 <- dams %>%
  select(COMID, DamDensCat, DamDensWs)

epa1 <- epa %>%
  select(COMID, NPDESDensCat, NPDESDensWs, TRIDensCat, TRIDensWs, SuperfundDensCat, SuperfundDensWs)

# Note - hydraulic conductivity did not appear to be in the geochem dataset, so leaving out for now.

kff1 <- kff %>%
  select(COMID, AgKffactCat, AgKffactWs)

mines1 <- mines %>%
  select(COMID, MineDensCat, MineDensWs)

nabd1 <- nabd %>%
  select(COMID, NABD_DensCat, NABD_DensWs)

nlcd16_1 <- nlcd16 %>%
  select(COMID, PctUrbHi2016Cat, PctUrbHi2016Ws, PctUrbMd2016Cat, PctUrbMd2016Ws, PctUrbLo2016Cat, PctUrbLo2016Ws, PctUrbOp2016Cat, PctUrbOp2016Ws, PctCrop2016Cat, PctCrop2016Ws, PctHay2016Cat, PctHay2016Ws, PctDecid2016Cat, PctDecid2016Ws, PctConif2016Cat, PctConif2016Ws, PctMxFst2016Cat, PctMxFst2016Ws,PctBl2016Cat, PctBl2016Ws, PctOw2016Cat, PctOw2016Ws, PctIce2016Cat, PctIce2016Ws, PctHbWet2016Cat, PctHbWet2016Ws, PctWdWet2016Cat, PctWdWet2016Ws, PctShrb2016Cat, PctShrb2016Ws, PctGrs2016Cat, PctGrs2016Ws) %>% # pulls desired land uses
  mutate(PctUrbCat = PctUrbHi2016Cat + PctUrbMd2016Cat + PctUrbLo2016Cat + PctUrbOp2016Cat) %>% # creates urban catchment column
  mutate(PctUrbWs = PctUrbHi2016Ws + PctUrbMd2016Ws + PctUrbLo2016Ws + PctUrbOp2016Ws) %>% # creates urban watershed column
  mutate(PctAgCat = PctCrop2016Cat + PctHay2016Cat) %>% # creates ag catchment column
  mutate(PctAgWs = PctCrop2016Ws + PctHay2016Ws) %>% # creates ag watershed column
  mutate(PctOpCat = PctDecid2016Cat + PctConif2016Cat + PctMxFst2016Cat + PctBl2016Cat + PctOw2016Cat + PctIce2016Cat + PctHbWet2016Cat + PctWdWet2016Cat + PctShrb2016Cat + PctGrs2016Cat) %>% # creates open catchment column
  mutate(PctOpWs = PctDecid2016Ws + PctConif2016Ws + PctMxFst2016Ws + PctBl2016Ws + PctOw2016Ws + PctIce2016Ws + PctHbWet2016Ws + PctWdWet2016Ws + PctShrb2016Ws + PctGrs2016Ws) # creates open catchment column

# Using dataset with aggregated land use columns.
nlcd16_2 <- nlcd16_1 %>%
  select(COMID, PctUrbCat, PctUrbWs, PctAgCat, PctAgWs, PctOpCat, PctOpWs)

nlcd16rp1 <- nlcd16rp %>%
  select(COMID, PctUrbHi2016CatRp100, PctUrbHi2016WsRp100, PctUrbMd2016CatRp100, PctUrbMd2016WsRp100, PctUrbLo2016CatRp100, PctUrbLo2016WsRp100, PctUrbOp2016CatRp100, PctUrbOp2016WsRp100, PctCrop2016CatRp100, PctCrop2016WsRp100, PctHay2016CatRp100, PctHay2016WsRp100, PctDecid2016CatRp100, PctDecid2016WsRp100, PctConif2016CatRp100, PctConif2016WsRp100, PctMxFst2016CatRp100, PctMxFst2016WsRp100, PctBl2016CatRp100, PctBl2016WsRp100, PctOw2016CatRp100, PctOw2016WsRp100, PctIce2016CatRp100, PctIce2016WsRp100, PctHbWet2016CatRp100, PctHbWet2016WsRp100, PctWdWet2016CatRp100, PctWdWet2016WsRp100, PctShrb2016CatRp100, PctShrb2016WsRp100, PctGrs2016CatRp100, PctGrs2016WsRp100) %>% # pulls desired land uses
  mutate(PctUrbCatRp100 = PctUrbHi2016CatRp100 + PctUrbMd2016CatRp100 + PctUrbLo2016CatRp100 + PctUrbOp2016CatRp100) %>% # creates urban catchment column
  mutate(PctUrbWsRp100 = PctUrbHi2016WsRp100 + PctUrbMd2016WsRp100 + PctUrbLo2016WsRp100 + PctUrbOp2016WsRp100) %>% # creates urban watershed column
  mutate(PctAgCatRp100 = PctCrop2016CatRp100 + PctHay2016CatRp100) %>% # creates ag catchment column
  mutate(PctAgWsRp100 = PctCrop2016WsRp100 + PctHay2016WsRp100) %>% # creates ag watershed column
  mutate(PctOpCatRp100 = PctDecid2016CatRp100 + PctConif2016CatRp100 + PctMxFst2016CatRp100 + PctBl2016CatRp100 + PctOw2016CatRp100 + PctIce2016CatRp100 + PctHbWet2016CatRp100 + PctWdWet2016CatRp100 + PctShrb2016CatRp100 + PctGrs2016CatRp100) %>% # creates open catchment column
  mutate(PctOpWsRp100 = PctDecid2016WsRp100 + PctConif2016WsRp100 + PctMxFst2016WsRp100 + PctBl2016WsRp100 + PctOw2016WsRp100 + PctIce2016WsRp100 + PctHbWet2016WsRp100 + PctWdWet2016WsRp100 + PctShrb2016WsRp100 + PctGrs2016WsRp100) # creates open catchment column

# Using dataset with aggregated riparian land use columns.
nlcd16rp2 <- nlcd16rp1 %>%
  select(COMID, PctUrbCatRp100, PctUrbWsRp100, PctAgCatRp100, PctAgWsRp100, PctOpCatRp100, PctOpWsRp100)

# prism1 <- prism %>%
#   select(COMID, PrecipCat, PrecipWs, TmeanCat, TmeanWs)

rddensity1 <- rddensity %>%
  select(COMID, RdDensCat, RdDensWs)

rddensityrp1 <- rddensityrp %>%
  select(COMID, RdDensCatRp100, RdDensWsRp100)

rdstream1 <- rdstream %>%
  select(COMID, RdCrsCat, RdCrsWs)

# stats1 <- stats %>%
#   select(COMID, PermCat, PermWs)

#### DATA JOINING ####

A <- full_join(agnitrogen1, candensity1)
B <- full_join(A, dams1)
C <- full_join(B, epa1)
D <- full_join(C, kff1)
E <- full_join(D, mines1)
FF <- full_join(E, nabd1)
G <- full_join(FF, nlcd16_2)
H <- full_join(G, nlcd16rp2)
I <- full_join(H, rddensity1)
J <- full_join(I, rddensityrp1)
asci_streamcat_params <- full_join(J, rdstream1)

#### DATA EXPORT ####

write_csv(asci_streamcat_params, "streamcat_params.csv")

# End of R script.
