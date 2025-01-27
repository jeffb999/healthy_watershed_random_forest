# CRAM Random Forest Draft
# January 8 ,2021
# Heili Lowman

# The following script will walk through a random forest created to predict state-wide CRAM scores, with datasets from SMC and StreamCat databases. The dependent variable in this case will be the California Rapid Assessment Method (CRAM) state-wide.

# Note: this will need to be redone for the 4 sub-CRAM metrics if we choose to model them separately. The below code works through only the overall CRAM index score as the predicted variable.

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

# Load datasets.
# CRAM data available from SCCWRP database.
cram_df <- read_csv("cram_rf_data1.csv") %>% # Loads in dataset pulled on 12/23/20.
  rename(COMID = comid) %>%
  rename(cram = indexscore) %>%
  drop_na(cram) # Drop the NA values.
skim(cram_df) # Examine the dataset.
str(cram_df)

# Watershed characteristics' data available from StreamCat.
ca <- read_csv("streamcat_params.csv")
skim(ca)
str(ca) # Checking to be sure COMID is numeric in both datasets.
# Perennial stream assessment data available from SCCWRP server.
ps6 <- read_csv("ps6_params.csv")
# In the ps6_rf_data script, if there are multiple Length_Fin measures for a given COMID, I have chosen the maximum of them and the associated PSA6 designation with that maximum.

# Bind the datasets together.
mydf <- cram_df %>%
  select(stationcode, COMID, cram) %>% 
  inner_join(ca) %>% # Join with StreamCat watershed characteristics.
  inner_join(ps6) %>% # Join with PSA region dataset.
  select(-c(PctOpCat, PctOpWs, PctOpCatRp100, PctOpWsRp100, NPDESDensCat, 
    NPDESDensWs, TRIDensCat, TRIDensWs, SuperfundDensCat, SuperfundDensWs)) # Remove "open" land use and discharge site columns.
skim(mydf) # Examing completeness of this joined dataset.
length(unique(mydf$COMID)) # Checking for duplicates. 816 unique COMIDs.

# Pull out only one instance of each COMID.
set.seed(1) # Every time I run the code below, it's based on the same random pull of data.
mydf2 <- mydf %>% 
  filter(stationcode!="109PS0162") %>% #There's only one site missing RdDensCatRp100. Better to drop the site than to drop the metric
  group_by(COMID) %>%
  sample_n(size = 1) %>% 
  ungroup()

skim(mydf2) # Checking to make sure the dataset is complete.
# Important to have complete datasets for training data. For testing data, it's less critical.

# Step Two - Training Data ------------------------------------------------

# Create calibration and validation splits with tidymodels initial_split() function.

set.seed(4)
mydf2_split <- mydf2 %>%
  initial_split(prop = 0.75, strata = PSA6) # splits data into training and testing set.
# default is 3/4ths split (but 75% training, 25% testing).
# Stratification (strata) = grouping training/testing sets by region, state, etc.
# Using the "strata" call ensures the number of data points in the training data is equivalent to the proportions in the original data set. (Strata below 10% of the total are pooled together.)

# Create a training data set with the training() function
# Pulls from training and testing sets created by initial_split()
mydf2_train <- training(mydf2_split)
mydf2_test <- testing(mydf2_split)
# Examine the environment to be sure # of observations looks like the 75/25 split. 613:202.

# Create a separate dataset of available COMIDS that were not used in the training dataset.
nottrain <- ca %>% # all COMIDS from StreamCat data, sampled or not
  filter(!COMID %in% mydf2_train$COMID) # Removing sites used to train the model. n = 140,097

# Step Three - Kitchen Sink model -----------------------------------------

# Create finalized training dataset and include all possible variables. 
rf_dat <- mydf2_train %>%
  select(-stationcode, -COMID, -PSA6, -Length_Fin)

# Random forest -- 
# a decision tree model, using predictors to answer dichotomous questions to create nested splits.
# no pruning happens - rather, multiple trees are built (the forest) and then you are looking for consensus across trees
# training data goes down the tree and ends up in a terminal node.
# if testing data goes down the same route, then this upholds our conclusions. Or, if it goes awry, this allows us to look for patterns in how it goes awry.

set.seed(2) # assures the data pulled is random, but sets it for the run below (makes outcome stable)
myrf <- randomForest(y = rf_dat$cram, # dependent variable
  x = rf_dat %>%
    select(-cram), # selecting all predictor variables
  importance = T, # how useful is a predictor in predicting values (nothing causal)
  proximity = T, 
  ntrees = 500) # 500 trees. 

myrf # examine the results.
# 66.38% variance explained.

summary(myrf)
# mtry allows you to parameterize the number of splits

plot(myrf)
# model performance appears to improve most at ~150 trees

varImpPlot(myrf)
# displays which variables are most important
# helps to winnow down list of predictors
# recommended to weigh left pane more
# right pane also shows how evenly things split based on the list of predictors
# values close to 0 can be dropped, but don't have to be

# In both panes - impervious land cover, urban land use, and stream-road crossings appear important

importance <- myrf$importance
View(importance)
# displays the data plotted in the plot above

# predict()
# returns out of bag predictions for training data
# in the bag: every time a tree is built, it uses ~80% of the original 75% we set aside from the original dataset used to create a tree to assure random data selection
# out of bag: looking at the remaining 20% of the training data to predict, when you want to know what your model does at the training location sites

# Predict CRAM scores state-wide for all COMIDs.
nottrain_prediction <- nottrain %>% # taking all available COMIDS, that haven't been used in training
  na.omit() %>% # remove NAs
  mutate(cram_predicted = predict(myrf, newdata = nottrain %>% na.omit())) # using developed model (myrf), inputting predictor variables (nottrain - which contains COMIDs and associated StreamCat data) to predict output/dependent variable (cram_predicted a.k.a. CRAM).

# rePredict CRAM scores for training data.
mydf2_train$cram_predicted <- predict(myrf) # Add column of predicted CRAM values to training dataset.

# Creates new dataset of bound rows for both ...
ca_predictions <- bind_rows(nottrain_prediction %>%
                            mutate(Set = "Non-training"), # statewide COMIDs (not used for training)
                            mydf2_train %>%
                            mutate(Set = "Training")) # COMIDS from training dataset
# This creates the dataset that will be plotted to create a state-wide plot of predicted CRAM scores.

# Plot the data.
rf_plot1 <- ggplot(ca_predictions, aes(x = PctImp2011CatRp100, y = cram_predicted)) +
  geom_point(alpha = 0.1) +
  labs(x = "Mean % imperviousness within catchment and within a 100-m buffer of NHD stream lines",
    y = "Predicted CRAM Score") +
  theme_classic() +
  facet_wrap(.~Set)

rf_plot1

# Step Four - Predictor Selection -----------------------------------------

# Using caret to select the best predictors
# What are the parameters you want to use to run recursive feature elimination (rfe)?
my_ctrl <- rfeControl(functions = rfFuncs,
                      method = "cv",
                      verbose = FALSE,
                      returnResamp = "all")

# rfe = recursive feature elimination
# THIS STEP TAKES FOR-EV-ER!!!
set.seed(22)
my_rfe <- rfe(y = rf_dat$cram, # set dependent variable
              x = rf_dat %>% select(-cram), # set predictor variables
              size = c(3:10, 15, 20, 25, 30), # sets how many variables are in the overall model
              # I have 34 total possible variables, so I've chosen increments of 5 to look at.
              rfeControl = my_ctrl) # pull in control from above

# can you make your model even simpler?
# the following will pick a model with the smallest number of predictor variables based on the tolerance ("tol") that you specify (how much less than the best are you willing to tolerate?)
my_size <- pickSizeTolerance(my_rfe$results, metric = "RMSE", tol = 1, maximize = F)
# higher tol (~10) gives you less variables
# lower tol (~1) gives you more variables - "I'd like the simplest model within 1% of the best model."
pickVars(my_rfe$variables, size = my_size)

# pickVars (25): PctImp2011CatRp100, RdCrsCat, PctUrbCatRp100, RdDensCatRp100, RdDensCat, 
# RdDensWs, RdCrsWs, PctImp2011Cat, RdDensWsRp100, PctAgCatRp100,
# PctUrbCat, PctAgWs, PctUrbWs, PctAgCat, PctImp2011Ws,
# CanalDensWs, PctAgWsRp100, AgKffactCat, PctImp2011WsRp100, CBNFCat,           
# PctUrbWsRp100, DamDensWs, NABD_DensWs, FertCat, AgKffactWs 

# Proceed with a regular RF that yields mean weighted values and fit those into the following classification scheme:

#Likely condition approach: Compare mean to three CRAM thresholds (50, 75, 90) based on suggested condition classes : https://www.cramwetlands.org/sites/default/files/2019CRAM_TechnicalBulletin.pdf
# Very likely altered: mean < 50
# Likely altered: mean < 75
# Possibly altered: mean < 90
# Likely unaltered: mean >= 90

# Predict scores using the above 25 variables:

# Create re-finalized training dataset and include all possible variables. 
rf_dat2 <- mydf2_train %>%
  select(cram, PctImp2011CatRp100, RdCrsCat, PctUrbCatRp100, RdDensCatRp100, RdDensCat, RdDensWs, RdCrsWs, PctImp2011Cat, RdDensWsRp100, PctAgCatRp100, PctUrbCat, PctAgWs, PctUrbWs, PctAgCat, PctImp2011Ws, CanalDensWs, PctAgWsRp100, AgKffactCat, PctImp2011WsRp100, CBNFCat, PctUrbWsRp100, DamDensWs, NABD_DensWs, FertCat, AgKffactWs)

set.seed(4) # assures the data pulled is random, but sets it for the run below (makes outcome stable)
myrf2 <- randomForest(y = rf_dat2$cram, # dependent variable
  x = rf_dat2 %>%
    select(-cram),
  importance = T, 
  proximity = T, 
  ntrees = 500)  

myrf2 # examine the results. 
# 66.11% variance explained.
summary(myrf2)
plot(myrf2) # need min of 100 trees.
varImpPlot(myrf2)

importance2 <- as.data.frame(as.table(myrf2$importance))
View(importance2) # displays the data plotted in the plot above

# Nicer ggplot variable importance plot.
vip_plot_a <- importance2 %>%
  filter(Var2 == "%IncMSE") %>%
  mutate(Var1 = factor(Var1)) %>%
  mutate(Var1_f = fct_reorder(Var1, Freq)) %>%
  ggplot(aes(x = Freq, y = Var1_f)) +
  geom_point(size = 3, alpha = 0.75) +
  labs(x = "% Importance (MSE)",
    y = "Variables") +
  theme_bw()

vip_plot_b <- importance2 %>%
  filter(Var2 == "IncNodePurity") %>%
  mutate(Var1 = factor(Var1)) %>%
  mutate(Var1_f = fct_reorder(Var1, Freq)) %>%
  ggplot(aes(x = Freq, y = Var1_f)) +
  geom_point(size = 3, alpha = 0.75) +
  labs(x = "Node Purity",
    y = "Variables") +
  theme_bw()

vip_plot <- vip_plot_a + vip_plot_b

vip_plot

# ggsave("cram_vip_plot.png",
#      path = "/Users/heilil/Desktop/R_figures",
#      width = 25,
#      height = 10,
#      units = "cm"
#    )

# predict(myrf2) # returns out of bag predictions for training data

# Predict CRAM scores state-wide.
nottrain_prediction2 <- nottrain %>% # taking all COMIDS that haven't been used in training
  na.omit() %>% # remove NAs
  mutate(cram_predicted = predict(myrf2, newdata = nottrain %>% na.omit())) # using developed model (myrf2), inputting predictor variables (nottrain - COMIDs and associated StreamCat data) to predict output/dependent variable (cram_predicted a.k.a. CRAM).

# rePredict CRAM scores for training and testing data (to be used in validation below).
mydf2_train2 <- mydf2_train
mydf2_train2$cram_predicted <- predict(myrf2) # Add column of predicted CRAM scores to training dataset.

mydf2_test2 <- mydf2_test %>%
  mutate(cram_predicted = predict(myrf2, newdata = mydf2_test %>% select(-c(stationcode, cram, PSA6, Length_Fin)))) # Adds column of predicted ASCI values to testing dataset.

# Creates new dataset of bound rows for both ...
ca_predictions2 <- bind_rows(nottrain_prediction2 %>%
    mutate(Set = "Non-training"), # statewide COMIDs (not used for training data)
  mydf2_train2 %>%
    mutate(Set = "Training")) # COMIDS from training dataset (used for training the model).
# This creates the dataset that will be plotted.

# Create table of number of sites that fall into each category.

# Add classification column.
ca_predictions2 <- ca_predictions2 %>%
  mutate(classification = case_when(cram_predicted < 50 ~"Very Likely Altered",
    cram_predicted < 75 ~"Likely Altered",
    cram_predicted < 90 ~"Possibly Altered",
    cram_predicted >= 90 ~"Likely Unaltered")) %>%
  mutate(class_f = factor(classification, levels = c("Very Likely Altered", "Likely Altered", "Possibly Altered", "Likely Unaltered"))) # relevel classifications

#### Results .csv ####
# Export results.
#write_csv(ca_predictions2, "cram_rf_results.csv")

# Summary table by site #.
ca_summary <- ca_predictions2 %>%
  count(class_f) # count sites statewide by classification
# The numbering is greatly skewed to the "possibly altered" classification, so perhaps other thresholds are necessary.

# Summary table by stream length (m)
ca_summary_length <- ca_predictions2 %>%
  group_by(class_f) %>% # group by classification
  summarize(length = sum(Length_Fin, na.rm=TRUE)) # sum stream lengths

# Join and export.
ca_sum <- full_join(ca_summary, ca_summary_length)
#write_csv(ca_sum, "cram_rf_results_summary.csv")

# Step Five - Quantile Regression model -----------------------------------

# Note - for the Healthy Watersheds Project, I did not pursue this structure, but I've kept some example code below in case future iterations call for it.

# Quantile random forest regression mode, instead of looking at the mode of trees, can compare to 10th, 50th, 90th percentiles etc.

# Need to make a new dataset taking the above results of pickVars into account.
# Create finalized training dataset and include all possible variables. 
# qrf_dat <- mydf2_train %>%
#   select(asci, RdCrsWs, PctAgWs, PctUrbWsRp100, PctOpWsRp100, PctOpWs, DamDensWs, RdDensWs, NABD_DensWs, PctUrbWs, PctUrbCatRp100, RdDensWsRp100, PctOpCat, PctUrbCat, RdDensCat, CBNFWs, PctOpCatRp100, PctAgWsRp100, TRIDensWs, AgKffactWs, FertWs) 

# set.seed(20)
# myqrf <- quantregForest(y = qrf_dat$asci, # dependent variable
#               x = qrf_dat %>%
#                   select(-asci),
#               importance = T, 
#               proximity = T,
#               keep.inbag=T,
#               ntrees = 500) 

#predict(myqrf) # automatically presents 10th %tile, median, and 90th %tile
#predict(myqrf, what=c(0.2, 0.3, 0.999)) # to print specific quantiles

#plot(myqrf) # plots the results.
# Again appears to improve after ~100 trees.

# Step Six - Model validation ---------------------------------------------

# Compare predicted vs. actual results, including by PSA region.
# Adding lines of slope=1 and linear models to each plot.
val1 <- ggplot(mydf2_train2, aes(x = cram_predicted, y = cram)) +
  geom_point(color = "#2A3927", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "#2A3927") +
  labs(x = "CRAM predicted",
    y = "CRAM measured",
    title = "Training Data\nn=613") +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw()

val1

lm1 <- lm(cram~cram_predicted, data = mydf2_train2)
summary(lm1)

val2 <- ggplot(mydf2_test2, aes(x = cram_predicted, y = cram)) +
  geom_point(color = "#3793EC", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "#3793EC") +
  scale_x_continuous(breaks = c(0.5, 0.7, 0.9)) +
  labs(x = "CRAM predicted",
    y = "CRAM measured",
    title = "Testing Data\nn=202") +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw()

val2

lm2 <- lm(cram~cram_predicted, data = mydf2_test2)
summary(lm2)

# Create the full testing + training dataset to plot together.
mydf2_test2$set <- "Testing"
mydf2_train2$set <- "Training"
full_train_test <- bind_rows(mydf2_test2, mydf2_train2) %>%
  mutate(set_f = factor(set, levels = c("Training", "Testing")))

val3 <- ggplot(full_train_test, aes(x = cram_predicted, y = cram, color = set_f)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(name = "Set", values = c("#2A3927", "#3793EC"), drop = FALSE) +
  labs(x = "CRAM predicted",
    y = "CRAM measured",
    title = "All Data\nn=815") +
  geom_abline(intercept = 0, slope = 1, color = "black") +
  facet_wrap(~PSA6) +
  theme_bw()

val3

val_fig <- (val1 + val2) /
  (val3)

val_fig + plot_annotation(
  title = 'CRAM Random Forest Results',
  subtitle = 'All modeling performed using StreamCAT datasets.',
  caption = 'Linear models are colored according to dataset. Lines of slope = 1 are denoted in black.'
)

# Save figure.
# ggsave("cram_rfmodel_validation.png",
#      path = "/Users/heilil/Desktop/R_figures",
#      width = 35,
#      height = 25,
#      units = "cm"
#    )

lm3 <- lm(cram~cram_predicted, 
  data = full_train_test %>%
    filter(PSA6 == "Central_Valley") %>%
    filter(set_f == "Training"))

lm4 <- lm(cram~cram_predicted, 
  data = full_train_test %>%
    filter(PSA6 == "Central_Valley") %>%
    filter(set_f == "Testing"))

lm5 <- lm(cram~cram_predicted, 
  data = full_train_test %>%
    filter(PSA6 == "Chaparral") %>%
    filter(set_f == "Training"))

lm6 <- lm(cram~cram_predicted, 
  data = full_train_test %>%
    filter(PSA6 == "Chaparral") %>%
    filter(set_f == "Testing"))

lm7 <- lm(cram~cram_predicted, 
  data = full_train_test %>%
    filter(PSA6 == "Deserts_Modoc") %>%
    filter(set_f == "Training"))

lm8 <- lm(cram~cram_predicted, 
  data = full_train_test %>%
    filter(PSA6 == "Deserts_Modoc") %>%
    filter(set_f == "Testing"))

lm9 <- lm(cram~cram_predicted, 
  data = full_train_test %>%
    filter(PSA6 == "North_Coast") %>%
    filter(set_f == "Training"))

lm10 <- lm(cram~cram_predicted, 
  data = full_train_test %>%
    filter(PSA6 == "North_Coast") %>%
    filter(set_f == "Testing"))

lm11 <- lm(cram~cram_predicted, 
  data = full_train_test %>%
    filter(PSA6 == "Sierra") %>%
    filter(set_f == "Training"))

lm12 <- lm(cram~cram_predicted, 
  data = full_train_test %>%
    filter(PSA6 == "Sierra") %>%
    filter(set_f == "Testing"))

lm13 <- lm(cram~cram_predicted, 
  data = full_train_test %>%
    filter(PSA6 == "South_Coast") %>%
    filter(set_f == "Training"))

lm14 <- lm(cram~cram_predicted, 
  data = full_train_test %>%
    filter(PSA6 == "South_Coast") %>%
    filter(set_f == "Testing"))

# Chose not to compute confusion matrix / accuracy score since this is more applicable to categorical ouputs from random forest models -
# Instead, calculated Root Mean Squared Error (RMSE) of both training and test datasets.
# If test RMSE values are much greater than training, then possible the model has been over fit.

predtest <- predict(myrf2, mydf2_test2)
rmse(mydf2_test2$cram,predtest)
# 9.03

predtrain <- predict(myrf2, mydf2_train2)
rmse(mydf2_train2$cram,predtrain)
# 4.20

# Double checking using the original random forest dataset (rf_dat) with all 35 possible variables included to see where the error in number of predictors starts to increase dramatically (to help double check our decision to include 25 parameters).
dc <- rfcv(rf_dat %>%
    select(-cram), 
  rf_dat$cram,
  step = 0.7, # default is 0.5
  scale="log")

dc$error.cv
#34        24        17        12         8         6         4         3         2 
# 96.28062  97.29366  99.08743 102.93752 115.52580 120.63049 122.09563 123.74425 139.30181 
# 1 
# 206.58921 

# Appears between 34 and 24 variables, there is an insignificant increase in error.
# However, this model is much larger than the CSCI (20) and ASCI (10) models, so we may decide to trim this down in the future.

# Step Seven - Map results state-wide -------------------------------------

# Using ca_predictions2 dataset generated above. But need to first associate lat/lon with each COMID.

# Load in NHD_Plus_CA dataset from Annie as well as watersheds from Jeff.
# Full state of California
nhd_ca <- read_sf("/Users/heilil/Desktop/hw_datasets/NHD_Plus_CA/NHDPlus_V2_FLowline_CA.shp") %>%
  mutate(COMID = as.numeric(COMID))

# South Coast watersheds - Ventura River, San Juan Creek, San Diego River
nhd_vr <- read_sf("/Users/heilil/Desktop/hw_datasets/NHD_Watersheds/VenturaRiver_NHD_Clip.shp") %>%
  mutate(COMID = as.numeric(COMID))

nhd_sjc <- read_sf("/Users/heilil/Desktop/hw_datasets/NHD_Watersheds/SanJuanCreek_NHD_Clip.shp") %>%
  mutate(COMID = as.numeric(COMID))

nhd_sdr <- read_sf("/Users/heilil/Desktop/hw_datasets/NHD_Watersheds/SanDiegoRiver_NHD_Clip.shp") %>%
  mutate(COMID = as.numeric(COMID))

# Assign modeled COMIDs to mcomid.
mcomid <- ca_predictions2$COMID

# Filter by and plot only modeled stream reaches.

modeled_cram_map <- nhd_ca %>%
  filter(COMID %in% mcomid) %>%
  inner_join(ca_predictions2) %>%
  ggplot() +
  geom_sf(aes(color = class_f)) +
  scale_color_manual(name = "Condition", values = c("red2", "lightpink", "lightskyblue2", "steelblue"), drop = FALSE) +
  theme_bw()

modeled_cram_map
# Note, sometimes this takes forever to render in the "plot" pane.
# Best to just save to your machine (below) and then take a look.

# ggsave("cram_modeled_CA.png",
#      path = "/Users/heilil/Desktop/R_figures",
#      width = 35,
#      height = 35,
#      units = "cm"
#    )

# Ventura River inset

ventura_cram_map <- nhd_vr %>%
  filter(COMID %in% mcomid) %>%
  inner_join(ca_predictions2) %>%
  ggplot() +
  geom_sf(aes(color = class_f)) +
  scale_color_manual(name = "Condition", values = c("red2", "lightpink", "lightskyblue2", "steelblue"), drop = FALSE) +
  labs(title = "Ventura River") +
  theme_bw() #+
  #theme(legend.position = "none")

ventura_cram_map

# ggsave("cram_modeled_Ventura.png",
#      path = "/Users/heilil/Desktop/R_figures",
#      width = 15,
#      height = 15,
#      units = "cm"
#    )

# San Juan Creek inset

sanjuan_cram_map <- nhd_sjc %>%
  filter(COMID %in% mcomid) %>%
  inner_join(ca_predictions2) %>%
  ggplot() +
  geom_sf(aes(color = class_f)) +
  scale_color_manual(name = "Condition", values = c("red2", "lightpink", "lightskyblue2", "steelblue"), drop = FALSE) +
  labs(title = "San Juan Creek") +
  theme_bw() +
  theme(legend.position = "none")

sanjuan_cram_map

# San Diego River inset

sandiego_cram_map <- nhd_sdr %>%
  filter(COMID %in% mcomid) %>%
  inner_join(ca_predictions2) %>%
  ggplot() +
  geom_sf(aes(color = class_f)) +
  scale_color_manual(name = "Condition", values = c("red2", "lightpink", "lightskyblue2", "steelblue"), drop = FALSE) +
  labs(title = "San Diego River") +
  theme_bw() +
  theme(legend.position = "none")

sandiego_cram_map

# South coast sites inset figures

scoast <- (ventura_cram_map) /
  (sanjuan_cram_map) /
  (sandiego_cram_map)

scoast

# ggsave("cram_modeled_SCoast.png",
#      path = "/Users/heilil/Desktop/R_figures",
#      width = 20,
#      height = 40,
#      units = "cm"
#    )

# Additional Notes - Healthy Watersheds project ---------------------------

#Classification options:
#"Constrained" approach, following Beck et al. 2019: Compare q10, q50, and q90 to CRAM threshold (i.e., 75). This requires the quantile regression model that was not run but for which there is code provided above.
#Likely constrained: q90 < 50
#Possibly constrained: q50 < 75
#Possibly unconstrained: q50 >= 90 and q10 < 90
#Likely unconstrained: q10 > 90

#"Likely condition approach: Compare q50 to three CRAM thresholds (50, 75, 90) @ reference sites (1st, 10th, 30th percentiles)
# Very likely altered: q50 < 50
# Likely altered: q50 < 75
# Possibly altered: q50 < 90
# Likely unaltered: q50 >= 90

# Condition approach favored by Anna per meeting on 11/3/2020.

# Works Cited:

# Hill, Ryan A., Marc H. Weber, Scott G. Leibowitz, Anthony R. Olsen, and Darren J. Thornbrugh, 2016. The Stream-Catchment (StreamCat) Dataset: A Database of Watershed Metrics for the Conterminous United States. Journal of the American Water Resources Association (JAWRA) 52:120-128. DOI: 10.1111/1752-1688.12372.

# End of R script.