# PS6 Data Tidying Script
# Heili Lowman
# October 22, 2020

# This script will not be posted to GitHub, because the data files it will pull are too large to store on GitHub.

#### PACKAGES ####

library(tidyverse) # loaded for data manipulation

#### DATASETS ####

# Load in perennial stream assessment (PSA) dataset located on SCCWRP server in Maps > Data > PSA_Frame.

ps6 <- read_csv('hw_datasets/PSA_SurveyFrame_08312012.csv')

#### DATA TIDYING ####

# Select only columns containing data of interest.

ps61 <- ps6 %>%
  select(COMID, PSA6, Length_Fin)

# sorting by unique COMID, finding the max stream length/reach, and then joining the datasets to assign a single PS6 category to each COMID
ps62 <- ps61 %>%
  group_by(COMID) %>%
  summarize(Length_Fin = max(Length_Fin))

asci_ps6_params <- inner_join(ps61, ps62)

#### DATA EXPORT ####

write_csv(asci_ps6_params, "ps6_params.csv")

# End of R script.

