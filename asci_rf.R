# ASCI Random Forest Draft
# October 21, 2020
# Heili Lowman

# The following script will walk through a random forest created to predict state-wide ASCI scores, with datasets from SMC and StreamCat databases. The dependent variable in this case will be the Algal Stream Condition Index (ASCI) state-wide.

# Step One - Load In ------------------------------------------------------

# Load packages.
library(quantregForest)
library(caret)
library(tidyverse)
library(tidymodels)
library(skimr)

# Load datasets.
# ASCI data available from SCCWRP database.
asci_df <- read_csv("asci_rf_data2.csv") %>% # Loads in dataset pulled on 10/28/20.
  rename(COMID = comid) %>%
  drop_na(asci) # Drop the NA values.
skim(asci_df) # Examine the dataset.
str(asci_df)
# Watershed characteristics' data available from StreamCat.
ca <- read_csv("streamcat_params.csv")
skim(ca)
str(ca) # Checking to be sure COMID is numeric in both datasets.
# Perennial stream assessment data available from SCCWRP server.
ps6 <- read_csv("ps6_params.csv")
# In the ps6_rf_data script, if there are multiple Length_Fin measures for a given COMID, I have chosen the maximum of them and the associated PSA6 designation with that maximum.

# Bind the datasets together.
mydf <- asci_df %>%
  select(stationcode, COMID, asci) %>%
  inner_join(ca) %>%
  inner_join(ps6)
skim(mydf) # Examing completeness of this joined dataset.
length(unique(mydf$COMID)) # Checking for duplicates. 1215 unique COMIDs.

# Pull out only one instance of each COMID.
set.seed(1) # Every time I run the code below, it's based on the same random pull of data.
mydf2 <- mydf %>% 
  group_by(COMID) %>%
  sample_n(size = 1) %>% 
  ungroup()

# Important to have complete datasets for training data. For testing data, it's less critical.

# Step Two - Training Data ------------------------------------------------

# Create calibration and validation splits with tidymodels initial_split() function.

set.seed(4)
mydf2_split <- mydf2 %>%
  initial_split(prop = 0.75, strata = PSA6) # splits data into training and testing set.
# default is 3/4ths split (but 75% training, 25% testing).
# Stratification (strata) = grouping training/testing sets by region, state, etc.

# Create a training data set with the training() function
# Pulls from training and testing sets created by initial_split()
mydf2_train <- training(mydf2_split)
mydf2_test <- testing(mydf2_split)
# Examine the environment to be sure # of observations looks like the 75/25 split. 912:303.

# Create a separate dataset of available COMIDS that were not used in the training dataset.
nottrain <- ca %>% # all COMIDS from StreamCat data, sampled or not
  filter(!COMID %in% mydf2_train$COMID) # Removing sites used to train the model. n = 139798

# Step Three - Kitchen Sink model -----------------------------------------

# Create finalized training dataset and include all possible variables. 
rf_dat <- mydf2_train %>%
  select(-stationcode, -COMID, -PSA6, -Length_Fin, -RdDensCatRp100) # Dropping variable due to incompleteness.

# Random forest -- 
# a decision tree model, using predictors to answer dichotomous questions to create nested splits.
# no pruning happens - rather, multiple trees are built (the forest) and then you are looking for consensus across trees
# training data goes down the tree and ends up in a terminal node.
# if testing data goes down the same route, then this upholds our conclusions. Or, if it goes awry, this allows us to look for patterns in how it goes awry.

set.seed(2) # assures the data pulled is random, but sets it for the run below (makes outcome stable)
myrf <- randomForest(y = rf_dat$asci, # dependent variable
  x = rf_dat %>%
    select(-asci), # selecting all predictor variables
  importance = T, # how useful is a predictor in predicting values (nothing causal)
  proximity = T, 
  ntrees = 500) # 500 trees. 

myrf # examine the results.
# 40.27% variance explained.

summary(myrf)
# mtry allows you to parameterize the number of splits

plot(myrf)
# model performance appears to improve most at ~100 trees

varImpPlot(myrf)
# displays which variables are most important
# helps to winnow down list of predictors
# recommended to weigh left pane more
# right pane also shows how evenly things split based on the list of predictors
# values close to 0 can be dropped, but don't have to be

# In both panes - agricultural land use and stream-road crossings, both at the watershed scale, as well as open land use in the riparian areas appear important.

imp <- myrf$importance
View(imp)
# displays the data plotted in the plot above

predict(myrf)
# returns out of bag predictions for training data
# in the bag: every time a tree is built, it uses ~80% of the original 75% we set aside from the original dataset used to create a tree to assure random data selection
# out of bag: looking at the remaining 20% of the training data to predict, when you want to know what your model does at the training location sites

# Predict ASCI scores state-wide.
nottrain_prediction <- nottrain %>% # taking all available COMIDS, that haven't been used to train the model
  na.omit() %>% # remove NAs
  mutate(asci_predicted = predict(myrf, newdata = nottrain %>% na.omit())) # using developed model (myrf), inputting predictor variables (nottrain - which contains COMIDs and associated StreamCat data) to predict output/dependent variable (asci_predicted a.k.a. ASCI).

# rePredict ASCI scores for training data.
mydf2_train$asci_predicted <- predict(myrf) # Adds column of predicted ASCI values to training dataset.

# Creates new dataset of bound rows for both ...
ca_predictions <- bind_rows(nottrain_prediction %>%
                            mutate(Set = "Non-training"), # statewide COMIDs (not used for training data)
                            mydf2_train %>%
                            mutate(Set = "Training")) # COMIDS from training dataset (that were used for training the random forest model).
# This creates the dataset that will be plotted (i.e. you're trying to create a state-wide plot of predicted ASCI scores).

# Plot the data.
rf_plot1 <- ggplot(ca_predictions, aes(x = RdCrsWs, y = asci_predicted)) +
  geom_point(alpha = 0.1) +
  labs(x = "Watershed Density of Road-Stream Intersections",
    y = "Predicted ASCI Score") +
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
my_rfe <- rfe(y = rf_dat$asci, # set dependent variable
              x = rf_dat %>% select(-asci), # set predictor variables
              size = 3:30, # sets how many variables are in the overall model
              rfeControl = my_ctrl) # pull in control from above

# can you make your model even simpler?
# the following will pick a model with the smallest number of predictor variables based on the tolerance ("tol") that you specify (how much less than the best are you willing to tolerate?)
my_size <- pickSizeTolerance(my_rfe$results, metric = "RMSE", tol = 1, maximize = F)
# higher tol (~10) gives you less variables
# lower tol (~1) gives you more variables - "I'm taking the simplest model that's within 1% of the best model."
pickVars(my_rfe$variables, size = my_size)

# Results of pickVars: RdCrsWs, PctAgWs, PctUrbWsRp100, PctOpWsRp100, PctOpWs, 
# DamDensWs, RdDensWs, NABD_DensWs, PctUrbWs, PctUrbCatRp100, 
# RdDensWsRp100, PctOpCat, PctUrbCat, RdDensCat, CBNFWs
# PctOpCatRp100, PctAgWsRp100, TRIDensWs, AgKffactWs, FertWs
# NEXT STEPS - use these predictors in quant reg forest. and then apply to statewide.

# Step Five - Quantile Regression model -----------------------------------

# Quantile random forest regression mode, instead of looking at the mode of trees, can compare to 10th, 50th, 90th percentiles etc.

# Need to make a new dataset taking the above results of pickVars into account.
# Create finalized training dataset and include all possible variables. 
qrf_dat <- mydf2_train %>%
  select(asci, RdCrsWs, PctAgWs, PctUrbWsRp100, PctOpWsRp100, PctOpWs, DamDensWs, RdDensWs, NABD_DensWs, PctUrbWs, PctUrbCatRp100, RdDensWsRp100, PctOpCat, PctUrbCat, RdDensCat, CBNFWs, PctOpCatRp100, PctAgWsRp100, TRIDensWs, AgKffactWs, FertWs) 

set.seed(20)
myqrf <- quantregForest(y = qrf_dat$asci, # dependent variable
              x = qrf_dat %>%
                  select(-asci),
              importance = T, 
              proximity = T,
              keep.inbag=T,
              ntrees = 500) 

predict(myqrf) # automatically presents 10th %tile, median, and 90th %tile
#predict(myqrf, what=c(0.2, 0.3, 0.999)) # to print specific quantiles

plot(myqrf) # plots the results.
# Again appears to improve after ~100 trees.

# End of R script.