# CRAM Physical Structure Random Forest Script
# Revised: November 14, 2021
# Heili Lowman, Jeff Brown

# The following script will walk through a random forest created to predict state-wide CRAM physical structure index scores, with datasets from SMC and StreamCat databases. The dependent variable in this case will be the California Rapid Assessment Method (CRAM) physical index state-wide.

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
library(here)

# Load datasets.
# CRAM data available from SCCWRP database.
cram_df <- read_csv("data_raw/cram_rf_data1.csv") %>% # Loads in dataset pulled on 12/23/20.
  rename(COMID = comid) %>%
  rename(cram = indexscore) %>%
  drop_na(cram) # Drop the NA values.
skim(cram_df) # Examine the dataset.
str(cram_df)

# Watershed characteristics' data available from StreamCat.
ca <- read_csv("data_working/streamcat_params.csv")
skim(ca)
str(ca) # Checking to be sure COMID is numeric in both datasets.
# Perennial stream assessment data available from SCCWRP server.
ps6 <- read_csv("data_working/ps6_params.csv")
# In the ps6_rf_data script, if there are multiple Length_Fin measures for a given COMID, I have chosen the maximum of them and the associated PSA6 designation with that maximum.

# Bind the datasets together.
mydf <- cram_df %>%
  select(stationcode, COMID, physicalstructure) %>% 
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
# Examine the environment to be sure # of observations looks like the 75/25 split. 610:205.

# Create a separate dataset of available COMIDS that were not used in the training dataset.
nottrain <- ca %>% # all COMIDS from StreamCat data, sampled or not
  filter(!COMID %in% mydf2_train$COMID) # Removing sites used to train the model. n = 140,100

# Step Three - Kitchen Sink model -----------------------------------------
# Will walk through all steps of the protocol for the "kitchen sink" model
# but variable selection begins in Step Four below.

# Create finalized training dataset and include all possible variables. 
rf_dat <- mydf2_train %>%
  select(-stationcode, -COMID, -PSA6, -Length_Fin)

# Random forest -- 
# a decision tree model, using predictors to answer dichotomous questions to create nested splits.
# no pruning happens - rather, multiple trees are built (the forest) and then you are looking for consensus across trees
# training data goes down the tree and ends up in a terminal node.
# if testing data goes down the same route, then this upholds our conclusions. Or, if it goes awry, this allows us to look for patterns in how it goes awry.

set.seed(2) # assures the data pulled is random, but sets it for the run below (makes outcome stable)
myrf <- randomForest(y = rf_dat$physicalstructure, # dependent variable
  x = rf_dat %>%
    select(-physicalstructure), # selecting all predictor variables
  importance = T, # how useful is a predictor in predicting values (nothing causal)
  proximity = T, 
  ntrees = 500) # 500 trees. 

myrf # examine the results.
# 53.43% variance explained.

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
  mutate(physstructure_predicted = predict(myrf, newdata = nottrain %>% na.omit())) # using developed model (myrf), inputting predictor variables (nottrain - which contains COMIDs and associated StreamCat data) to predict output/dependent variable (physstructure_predicted a.k.a. CRAM).

# rePredict CRAM scores for training data.
mydf2_train$physstructure_predicted <- predict(myrf) # Add column of predicted CRAM values to training dataset.

# Creates new dataset of bound rows for both ...
ca_predictions <- bind_rows(nottrain_prediction %>%
                            mutate(Set = "Non-training"), # statewide COMIDs (not used for training)
                            mydf2_train %>%
                            mutate(Set = "Training")) # COMIDS from training dataset
# This creates the dataset that will be plotted to create a state-wide plot of predicted CRAM scores.

# Plot the data.
(rf_plot1 <- ggplot(ca_predictions, aes(x = PctImp2011CatRp100, y = physstructure_predicted)) +
  geom_point(alpha = 0.1) +
  labs(x = "Mean % imperviousness within catchment and within a 100-m buffer of NHD stream lines",
    y = "Predicted Physical Structure Score") +
  theme_classic() +
  facet_wrap(.~Set))

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
my_rfe <- rfe(y = rf_dat$physicalstructure, # set dependent variable
              x = rf_dat %>% select(-physicalstructure), # set predictor variables
              size = c(3:10, 15, 20, 25, 30), # sets how many variables are in the overall model
              # I have 34 total possible variables, so I've chosen increments of 5 to look at.
              rfeControl = my_ctrl) # pull in control from above

# can you make your model even simpler?
# the following will pick a model with the smallest number of predictor variables based on the tolerance ("tol") that you specify (how much less than the best are you willing to tolerate?)
my_size <- pickSizeTolerance(my_rfe$results, metric = "RMSE", tol = 1, maximize = F)
# higher tol (~10) gives you less variables
# lower tol (~1) gives you more variables - "I'd like the simplest model within 1% of the best model."
pickVars(my_rfe$variables, size = my_size)

# pickVars (20):
# "PctImp2011CatRp100" "PctUrbCatRp100"     "RdDensCatRp100"     "PctAgWsRp100"       "PctImp2011Cat"      "PctImp2011WsRp100" 
# "RdDensCat"          "PctUrbWsRp100"      "PctUrbCat"          "PctUrbWs"           "RdDensWs"           "RdDensWsRp100"     
# "PctAgWs"            "RdCrsWs"            "RdCrsCat"           "AgKffactWs"         "PctImp2011Ws"       "CBNFWs"            
# "PctAgCat"           "AgKffactCat"

# Proceed with a regular RF that yields mean weighted values and fit those into the following classification scheme:

#Likely condition approach: Compare mean to three CRAM thresholds (50, 75, 90) based on suggested condition classes : https://www.cramwetlands.org/sites/default/files/2019CRAM_TechnicalBulletin.pdf
# Very likely altered: mean < 50
# Likely altered: mean < 75
# Possibly altered: mean < 90
# Likely unaltered: mean >= 90

# Thresholds for physical structure based on 81 reference sites in eCRAM or sent to Lisa Fong
# Very likely altered: mean < 53
# Likely altered: mean < 66
# Possibly altered: mean < 76
# Likely unaltered: mean >= 76

# Predict scores using the above 15 variables:

# Create re-finalized training dataset and include all possible variables. 
rf_dat2 <- mydf2_train %>%
  select(physicalstructure, AgKffactCat,	AgKffactWs,	CBNFWs,	PctAgCat,	PctAgWs,	PctAgWsRp100,	PctImp2011Cat,	PctImp2011CatRp100,	PctImp2011Ws,	PctImp2011WsRp100,	PctUrbCat,	PctUrbCatRp100,	PctUrbWs,	PctUrbWsRp100,	RdCrsCat,	RdCrsWs,	RdDensCat,	RdDensCatRp100,	RdDensWs,	RdDensWsRp100)

set.seed(4) # assures the data pulled is random, but sets it for the run below (makes outcome stable)
myrf2 <- randomForest(y = rf_dat2$physicalstructure, # dependent variable
  x = rf_dat2 %>%
    select(-physicalstructure),
  importance = T, 
  proximity = T, 
  ntrees = 500)  

myrf2 # examine the results. 
# 53.31% variance explained.
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

vip_plot + plot_annotation(tag_levels = 'A')

# Export figure
ggsave("cram_physicalstructure_vip_plot.png",
     path = "figures",
     width = 25,
     height = 10,
     units = "cm"
   )

# predict(myrf2) # returns out of bag predictions for training data

# Predict CRAM physical scores state-wide.
nottrain_prediction2 <- nottrain %>% # taking all COMIDS that haven't been used in training
  na.omit() %>% # remove NAs
  mutate(physstructure_predicted = predict(myrf2, newdata = nottrain %>% na.omit())) # using developed model (myrf2), inputting predictor variables (nottrain - COMIDs and associated StreamCat data) to predict output/dependent variable (physstructure_predicted a.k.a. CRAM).

# rePredict CRAM scores for training and testing data (to be used in validation below).
mydf2_train2 <- mydf2_train
mydf2_train2$physstructure_predicted <- predict(myrf2) # Add column of predicted CRAM scores to training dataset.

mydf2_test2 <- mydf2_test %>%
  mutate(physstructure_predicted = predict(myrf2, newdata = mydf2_test %>% select(-c(stationcode, physicalstructure, PSA6, Length_Fin)))) # Adds column of predicted CRAM values to testing dataset.

# Creates new dataset of bound rows for both ...
ca_predictions2 <- bind_rows(nottrain_prediction2 %>%
    mutate(Set = "Non-training"), # statewide COMIDs (not used for training data)
  mydf2_train2 %>%
    mutate(Set = "Training")) # COMIDS from training dataset (used for training the model).
# This creates the dataset that will be plotted.

# Create table of number of sites that fall into each category.
# This went through a couple of iterations, so the unused versions have been commented out.

# Add classification column.
# ca_predictions2 <- ca_predictions2 %>%
#   mutate(classification = case_when(physstructure_predicted < 50 ~"Very Likely Altered",
#     physstructure_predicted < 75 ~"Likely Altered",
#     physstructure_predicted < 90 ~"Possibly Altered",
#     physstructure_predicted >= 90 ~"Likely Unaltered")) %>%
#   mutate(class_f = factor(classification, levels = c("Very Likely Altered", "Likely Altered", "Possibly Altered", "Likely Unaltered"))) # relevel classifications

# # 3 thresholds & 4 categories
# ca_predictions2 <- ca_predictions2 %>%
#   mutate(classification = case_when(round(physstructure_predicted, digits = 0) < 53 ~"Very Likely Altered",
#                                     round(physstructure_predicted, digits = 0) < 66 ~"Likely Altered",
#                                     round(physstructure_predicted, digits = 0) < 76 ~"Possibly Altered",
#                                     round(physstructure_predicted, digits = 0) >= 76 ~"Likely Unaltered")) %>%
#   mutate(class_f = factor(classification, levels = c("Very Likely Altered", "Likely Altered", "Possibly Altered", "Likely Unaltered"))) # relevel classifications

# 1 threshold & 2 categories
ca_predictions2 <- ca_predictions2 %>%
  mutate(classification = case_when(round(physstructure_predicted, digits = 0) < 66 ~"Degraded",
                                    round(physstructure_predicted, digits = 0) >= 66 ~"Intact")) %>%
  mutate(class_f = factor(classification, levels = c("Degraded", "Intact"))) # relevel classifications

#### Results .csv ####
# Export results.
write_csv(ca_predictions2, "data_model_outputs/cramphys_rf_results_R4.1.2.csv")

# Summary table by site #.
ca_summary <- ca_predictions2 %>%
  count(class_f) # count sites statewide by classification

# Summary table by stream length (m)
ca_summary_length <- ca_predictions2 %>%
  group_by(class_f) %>% # group by classification
  summarize(length = sum(Length_Fin, na.rm=TRUE)) # sum stream lengths

# Join and export.
ca_sum <- full_join(ca_summary, ca_summary_length)
write_csv(ca_sum, "data_model_outputs/cramphys_rf_results_summary_R4.2.1.csv")

# Step Five - Quantile Regression model -----------------------------------

# Note - for the Healthy Watersheds Project, I did not pursue this structure, but I've kept some example code below in case future iterations call for it.
# For additional information on how to pursue this method, see "asci_rf_V2.R" script in this project.

# Step Six - Model validation ---------------------------------------------

# Compare predicted vs. actual results, including by PSA region.
# Adding lines of slope=1 and linear models to each plot.
(val1 <- ggplot(mydf2_train2, aes(x = physstructure_predicted, y = physicalstructure)) +
  geom_point(color = "#2A3927", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "#2A3927") +
  labs(x = "CRAM Physical Structure predicted",
    y = "CRAM Physical Structure measured") +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw()) # n = 610

lm1 <- lm(physicalstructure~physstructure_predicted, data = mydf2_train2)
summary(lm1)

(val2 <- ggplot(mydf2_test2, aes(x = physstructure_predicted, y = physicalstructure)) +
  geom_point(color = "#3793EC", alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "#3793EC") +
  scale_x_continuous(breaks = c(0.5, 0.7, 0.9)) +
  labs(x = "CRAM Physical Structure predicted",
    y = "CRAM Physical Structure measured") +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw()) # n = 205

lm2 <- lm(physicalstructure~physstructure_predicted, data = mydf2_test2)
summary(lm2)

# Create the full testing + training dataset to plot together.
mydf2_test2$set <- "Testing"
mydf2_train2$set <- "Training"
full_train_test <- bind_rows(mydf2_test2, mydf2_train2) %>%
  mutate(set_f = factor(set, levels = c("Training", "Testing")))

# Create list and function with which to rename facet labels

facet_labels <- c("Central_Valley" = "Central Valley", 
                  "Chaparral" = "Chaparral", 
                  "Deserts_Modoc" = "Deserts (Modoc)", 
                  "North_Coast" = "North Coast", 
                  "Sierra" = "Sierra", 
                  "South_Coast" = "South Coast")

(val3 <- ggplot(full_train_test, aes(x = physstructure_predicted, y = physicalstructure, color = set_f)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(name = "Set", values = c("#2A3927", "#3793EC"), drop = FALSE) +
  labs(x = "CRAM Physical Structure predicted",
    y = "CRAM Physical Structure measured") +
  geom_abline(intercept = 0, slope = 1, color = "black") +
  facet_wrap(~PSA6, labeller = as_labeller(facet_labels)) +
  theme_bw() +
  theme(legend.position = "bottom")) # n = 815

val_fig <- (val1 + val2) /
  (val3)

val_fig + plot_annotation(tag_levels = 'A')

# Save figure.
# ggsave("cramphys_rfmodel_validation.png",
#      path = "figures",
#      width = 25,
#      height = 20,
#      units = "cm"
#    )

# Generate summary table of linear models for the figures above.
# Note to future self: make this a for loop

lm3 <- lm(physicalstructure~physstructure_predicted, 
  data = full_train_test %>%
    filter(PSA6 == "Central_Valley") %>%
    filter(set_f == "Training"))

lm4 <- lm(physicalstructure~physstructure_predicted, 
  data = full_train_test %>%
    filter(PSA6 == "Central_Valley") %>%
    filter(set_f == "Testing"))

lm5 <- lm(physicalstructure~physstructure_predicted, 
  data = full_train_test %>%
    filter(PSA6 == "Chaparral") %>%
    filter(set_f == "Training"))

lm6 <- lm(physicalstructure~physstructure_predicted, 
  data = full_train_test %>%
    filter(PSA6 == "Chaparral") %>%
    filter(set_f == "Testing"))

lm7 <- lm(physicalstructure~physstructure_predicted, 
  data = full_train_test %>%
    filter(PSA6 == "Deserts_Modoc") %>%
    filter(set_f == "Training"))

lm8 <- lm(physicalstructure~physstructure_predicted, 
  data = full_train_test %>%
    filter(PSA6 == "Deserts_Modoc") %>%
    filter(set_f == "Testing"))

lm9 <- lm(physicalstructure~physstructure_predicted, 
  data = full_train_test %>%
    filter(PSA6 == "North_Coast") %>%
    filter(set_f == "Training"))

lm10 <- lm(physicalstructure~physstructure_predicted, 
  data = full_train_test %>%
    filter(PSA6 == "North_Coast") %>%
    filter(set_f == "Testing"))

lm11 <- lm(physicalstructure~physstructure_predicted, 
  data = full_train_test %>%
    filter(PSA6 == "Sierra") %>%
    filter(set_f == "Training"))

lm12 <- lm(physicalstructure~physstructure_predicted, 
  data = full_train_test %>%
    filter(PSA6 == "Sierra") %>%
    filter(set_f == "Testing"))

lm13 <- lm(physicalstructure~physstructure_predicted, 
  data = full_train_test %>%
    filter(PSA6 == "South_Coast") %>%
    filter(set_f == "Training"))

lm14 <- lm(physicalstructure~physstructure_predicted, 
  data = full_train_test %>%
    filter(PSA6 == "South_Coast") %>%
    filter(set_f == "Testing"))


Rsq1      <- summary(lm1)$r.squared # get r-squared value
Slp1      <- lm1$coefficients[2] # get the slope
Pval1     <- summary(lm1)$coefficients[2,4] # get p-value also anova(lm1)$'Pr(>F)'[1]
Int1      <- lm1$coefficients[1] # get the y-intercept
PInt1     <- summary(lm1)$coefficients[1,4] # get the Intercept p-value

Rsq2      <- summary(lm2)$r.squared # get r-squared value
Slp2      <- lm2$coefficients[2] # get the slope
Pval2     <- summary(lm2)$coefficients[2,4] # get p-value also anova(lm2)$'Pr(>F)'[1]
Int2      <- lm2$coefficients[1] # get the y-intercept
PInt2     <- summary(lm2)$coefficients[1,4] # get the Intercept p-value

Rsq3      <- summary(lm3)$r.squared # get r-squared value
Slp3      <- lm3$coefficients[2] # get the slope
Pval3     <- summary(lm3)$coefficients[2,4] # get p-value also anova(lm3)$'Pr(>F)'[1]
Int3      <- lm3$coefficients[1] # get the y-intercept
PInt3     <- summary(lm3)$coefficients[1,4] # get the Intercept p-value

Rsq4      <- summary(lm4)$r.squared # get r-squared value
Slp4      <- lm4$coefficients[2] # get the slope
Pval4     <- summary(lm4)$coefficients[2,4] # get p-value also anova(lm4)$'Pr(>F)'[1]
Int4      <- lm4$coefficients[1] # get the y-intercept
PInt4     <- summary(lm4)$coefficients[1,4] # get the Intercept p-value

Rsq5      <- summary(lm5)$r.squared # get r-squared value
Slp5      <- lm5$coefficients[2] # get the slope
Pval5     <- summary(lm5)$coefficients[2,4] # get p-value also anova(lm5)$'Pr(>F)'[1]
Int5      <- lm5$coefficients[1] # get the y-intercept
PInt5     <- summary(lm5)$coefficients[1,4] # get the Intercept p-value

Rsq6      <- summary(lm6)$r.squared # get r-squared value
Slp6      <- lm6$coefficients[2] # get the slope
Pval6     <- summary(lm6)$coefficients[2,4] # get p-value also anova(lm6)$'Pr(>F)'[1]
Int6      <- lm6$coefficients[1] # get the y-intercept
PInt6     <- summary(lm6)$coefficients[1,4] # get the Intercept p-value

Rsq7      <- summary(lm7)$r.squared # get r-squared value
Slp7      <- lm7$coefficients[2] # get the slope
Pval7     <- summary(lm7)$coefficients[2,4] # get p-value also anova(lm7)$'Pr(>F)'[1]
Int7      <- lm7$coefficients[1] # get the y-intercept
PInt7     <- summary(lm7)$coefficients[1,4] # get the Intercept p-value

Rsq8      <- summary(lm8)$r.squared # get r-squared value
Slp8      <- lm8$coefficients[2] # get the slope
Pval8     <- summary(lm8)$coefficients[2,4] # get p-value also anova(lm8)$'Pr(>F)'[1]
Int8      <- lm8$coefficients[1] # get the y-intercept
PInt8     <- summary(lm8)$coefficients[1,4] # get the Intercept p-value

Rsq9      <- summary(lm9)$r.squared # get r-squared value
Slp9      <- lm9$coefficients[2] # get the slope
Pval9     <- summary(lm9)$coefficients[2,4] # get p-value also anova(lm9)$'Pr(>F)'[1]
Int9      <- lm9$coefficients[1] # get the y-intercept
PInt9     <- summary(lm9)$coefficients[1,4] # get the Intercept p-value

Rsq10      <- summary(lm10)$r.squared # get r-squared value
Slp10      <- lm10$coefficients[2] # get the slope
Pval10     <- summary(lm10)$coefficients[2,4] # get p-value also anova(lm10)$'Pr(>F)'[1]
Int10      <- lm10$coefficients[1] # get the y-intercept
PInt10     <- summary(lm10)$coefficients[1,4] # get the Intercept p-value

Rsq11      <- summary(lm11)$r.squared # get r-squared value
Slp11      <- lm11$coefficients[2] # get the slope
Pval11     <- summary(lm11)$coefficients[2,4] # get p-value also anova(lm11)$'Pr(>F)'[1]
Int11      <- lm11$coefficients[1] # get the y-intercept
PInt11     <- summary(lm11)$coefficients[1,4] # get the Intercept p-value

Rsq12      <- summary(lm12)$r.squared # get r-squared value
Slp12      <- lm12$coefficients[2] # get the slope
Pval12     <- summary(lm12)$coefficients[2,4] # get p-value also anova(lm12)$'Pr(>F)'[1]
Int12      <- lm12$coefficients[1] # get the y-intercept
PInt12     <- summary(lm12)$coefficients[1,4] # get the Intercept p-value

Rsq13      <- summary(lm13)$r.squared # get r-squared value
Slp13      <- lm13$coefficients[2] # get the slope
Pval13     <- summary(lm13)$coefficients[2,4] # get p-value also anova(lm13)$'Pr(>F)'[1]
Int13      <- lm13$coefficients[1] # get the y-intercept
PInt13     <- summary(lm13)$coefficients[1,4] # get the Intercept p-value

Rsq14      <- summary(lm14)$r.squared # get r-squared value
Slp14      <- lm14$coefficients[2] # get the slope
Pval14     <- summary(lm14)$coefficients[2,4] # get p-value also anova(lm14)$'Pr(>F)'[1]
Int14      <- lm14$coefficients[1] # get the y-intercept
PInt14     <- summary(lm14)$coefficients[1,4] # get the Intercept p-value

physicalstructure_lms <- data.frame("Region" = c("Statewide", "Statewide", "Central_Valley", "Central_Valley", "Chaparral", "Chaparral", "Deserts_Modoc", "Deserts_Modoc", "North_Coast", "North_Coast", "Sierra", "Sierra", "South_Coast", "South_Coast"),
                       "Dataset" = c("Training", "Testing", "Training", "Testing", "Training", "Testing", "Training", "Testing", "Training", "Testing", "Training", "Testing", "Training", "Testing"),
                       "R2" = c(Rsq1, Rsq2, Rsq3, Rsq4, Rsq5, Rsq6, Rsq7, Rsq8, Rsq9, Rsq10, Rsq11, Rsq12, Rsq13, Rsq14),
                       "Slope" = c(Slp1, Slp2, Slp3, Slp4, Slp5, Slp6, Slp7, Slp8, Slp9, Slp10, Slp11, Slp12, Slp13, Slp14),
                       "Slope_p" = c(Pval1, Pval2, Pval3, Pval4, Pval5, Pval6, Pval7, Pval8, Pval9, Pval10, Pval11, Pval12, Pval13, Pval14),
                       "Intercept" = c(Int1, Int2, Int3, Int4, Int5, Int6, Int7, Int8, Int9, Int10, Int11, Int12, Int13, Int14),
                       "Intercept_p" = c(PInt1, PInt2, PInt3, PInt4, PInt5, PInt6, PInt7, PInt8, PInt9, PInt10, PInt11, PInt12, PInt13, PInt14))

physicalstructure_lms <- physicalstructure_lms %>%
  mutate(Slope_p = round(Slope_p, digits=6)) %>%
  mutate(Slope_p = ifelse(Slope_p < 0.0001, "<0.0001", Slope_p))

# Export linear model results.
write_csv(physicalstructure_lms,"data_model_outputs/cramphys_lms_R4.1.2.csv" )

# Run the code and save table to a png file: 
(summary_table <- physicalstructure_lms %>%
  gt(groupname_col = "Region", rowname_col = "Dataset") %>%
  fmt_number(columns = vars(R2, Slope, Intercept, Intercept_p), decimals = 4) %>%
  tab_header(title = "Physicalstructure Results Validation",
             subtitle = "All modeling performed using StreamCAT datasets.") %>%
  cols_label(R2 = html("R<sup>2</sup"),
             Slope_p = html("<i>p</i>"),
             Intercept_p = html("<i>p</i>")) %>%
  cols_align(
    align = "left",
    columns = vars(R2, Slope, Slope_p, Intercept, Intercept_p)))

# Save table.
gtsave(summary_table,
  "cramphys_rfmodel_lms.png",
  path = "figures")

# Create overall process summary table.
process_summary <- data.frame("Dataframe" = c("ca", "ca_predictions", "ca_predictions2", "full_train_test", "mydf", "mydf2",
                                              "mydf2_test", "mydf2_test2", "mydf2_train", "mydf2_train2", "nottrain",
                                              "nottrain_prediction", "nottrain_prediction2", "ps6", "rf_dat", "rf_dat2"),
                              "Count" = c(nrow(ca), nrow(ca_predictions), nrow(ca_predictions2), nrow(full_train_test), nrow(mydf),
                                          nrow(mydf2), nrow(mydf2_test), nrow(mydf2_test2), nrow(mydf2_train), nrow(mydf2_train2),
                                          nrow(nottrain), nrow(nottrain_prediction), nrow(nottrain_prediction2), nrow(ps6),
                                          nrow(rf_dat), nrow(rf_dat2)))

# Export data
write_csv(process_summary, "data_model_outputs/cramphys_process_summary_R4.1.2.csv")

# Chose not to compute confusion matrix / accuracy score since this is more applicable to categorical ouputs from random forest models -
# Instead, calculated Root Mean Squared Error (RMSE) of both training and test datasets.
# If test RMSE values are much greater than training, then possible the model has been over fit.

predtest <- predict(myrf2, mydf2_test2)
rmse(mydf2_test2$physicalstructure,predtest)
# 14.34

predtrain <- predict(myrf2, mydf2_train2)
rmse(mydf2_train2$physicalstructure,predtrain)
# 6.36

# Double checking using the original random forest dataset (rf_dat) with all 35 possible variables included to see where the error in number of predictors starts to increase dramatically (to help double check our decision to include 25 parameters).
dc <- rfcv(rf_dat %>%
    select(-physicalstructure), 
  rf_dat$physicalstructure,
  step = 0.7, # default is 0.5
  scale="log")

dc$error.cv
#34       24       17       12        8        6        4        3        2        1 
#225.1524 226.3307 231.4618 238.3304 250.1926 261.6627 272.3757 266.4408 283.8253 335.2839 

# Appears between 34 and 24 variables, there is an insignificant increase in error.

# Step Seven - Map results state-wide -------------------------------------

# Please note, the spatial datasets used to create the final maps are not available
# on GitHub due to their size.
# All figures created and exported have also been mapped to the SCCWRP server rather 
# than this repository's directory.
# If you would like access to these datasets, please contact Jeff Brown (jeffb@sccwrp.org).

# Using ca_predictions2 dataset generated above. But need to first associate lat/lon with each COMID.

# Load in NHD_Plus_CA dataset from Annie as well as watersheds from Jeff.
# Full state of California
nhd_ca <- read_sf("L:/RipRAM_ES/Data/Working/MapStuff/NHDPlus_NAD83.shp") %>%
  mutate(COMID = as.numeric(COMID))
nhd_ca <- st_zm(nhd_ca)

# South Coast watersheds - Ventura River, San Juan Creek, San Diego River
nhd_ca <- read_sf("L:/RipRAM_ES/Data/Working/MapStuff/NHDPlus_NAD83.shp") %>%
  mutate(COMID = as.numeric(COMID))

# South Coast watersheds - Ventura River, San Juan Creek, San Diego River
nhd_vr <- read_sf("L:/RipRAM_ES/Data/Working/MapStuff/VenturaRiver_NHD_Clip.shp") %>%
  mutate(COMID = as.numeric(COMID))

nhd_sjc <- read_sf("L:/RipRAM_ES/Data/Working/MapStuff/SanJuanCreek_NHD_Clip.shp") %>%
  mutate(COMID = as.numeric(COMID))

nhd_sdr <- read_sf("L:/RipRAM_ES/Data/Working/MapStuff/SanDiegoRiver_NHD_Clip.shp") %>%
  mutate(COMID = as.numeric(COMID))

# Assign modeled COMIDs to mcomid.
mcomid <- ca_predictions2$COMID

# Filter by and plot only modeled stream reaches.

# Statewide map.  1 threshold & 2 categories
modeled_cram_map <- nhd_ca %>%
  filter(COMID %in% mcomid) %>%
  inner_join(ca_predictions2) %>%
  ggplot() +
  geom_sf(aes(color = class_f)) +
  scale_color_manual(name = "Condition", values = c("red2", "steelblue"), drop = FALSE) +
  theme_bw()

# png(file="physicalstructure_modeled_CA.png", units="in", width=8, height=5, res=300)
# png(file="L:/RipRAM_ES/Data/Working/healthy_watershed_random_forest/Results using published thresholds/physicalstructure_modeled_CA_81ref_R4.1.2.png", units="in", width=8, height=5, res=300)
# modeled_cram_map
# dev.off()

# Ventura River inset
(ventura_cram_map <- nhd_vr %>%
  filter(COMID %in% mcomid) %>%
  inner_join(ca_predictions2) %>%
  ggplot() +
  geom_sf(aes(color = class_f)) +
  scale_color_manual(name = "Condition", values = c("red2", "lightpink", "lightskyblue2", "steelblue"), drop = FALSE) +
  labs(title = "Ventura River") +
  theme_bw())

# png(file="physicalstructure_modeled_Ventura.png", units="in", width=8, height=5, res=300)
# png(file="L:/RipRAM_ES/Data/Working/healthy_watershed_random_forest/Results using published thresholds/physicalstructure_modeled_Ventura_81ref.png", units="in", width=8, height=5, res=300)
# ventura_cram_map
# dev.off()

# San Juan Creek inset
(sanjuan_cram_map <- nhd_sjc %>%
  filter(COMID %in% mcomid) %>%
  inner_join(ca_predictions2) %>%
  ggplot() +
  geom_sf(aes(color = class_f)) +
  scale_color_manual(name = "Condition", values = c("red2", "lightpink", "lightskyblue2", "steelblue"), drop = FALSE) +
  labs(title = "San Juan Creek") +
  theme_bw() +
  theme(legend.position = "none"))

# png(file="physicalstructure_modeled_SanJuanCreek.png", units="in", width=8, height=5, res=300)
# png(file="L:/RipRAM_ES/Data/Working/healthy_watershed_random_forest/Results using published thresholds/physicalstructure_modeled_SanJuanCreek_81ref.png", units="in", width=8, height=5, res=300)
# sanjuan_cram_map
# dev.off()

# San Diego River inset
(sandiego_cram_map <- nhd_sdr %>%
  filter(COMID %in% mcomid) %>%
  inner_join(ca_predictions2) %>%
  ggplot() +
  geom_sf(aes(color = class_f)) +
  scale_color_manual(name = "Condition", values = c("red2", "lightpink", "lightskyblue2", "steelblue"), drop = FALSE) +
  labs(title = "San Diego River") +
  theme_bw() +
  theme(legend.position = "none"))

# png(file="physicalstructure_modeled_SanDiegoRiver.png", units="in", width=8, height=5, res=300)
# png(file="L:/RipRAM_ES/Data/Working/healthy_watershed_random_forest/Results using published thresholds/physicalstructure_modeled_SanDiegoRiver_81ref.png", units="in", width=8, height=5, res=300)
# sandiego_cram_map
# dev.off()

# South coast sites inset figures
scoast <- (ventura_cram_map) /
  (sanjuan_cram_map) /
  (sandiego_cram_map)

scoast

# png(file="physicalstructure_modeled_SouthCoast.png", units="in", width=8, height=5, res=300)
# png(file="L:/RipRAM_ES/Data/Working/healthy_watershed_random_forest/Results using published thresholds/physicalstructure_modeled_SouthCoast_81ref.png", units="in", width=8, height=5, res=300)
# scoast
# dev.off()

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

# Condition approach favored per meeting on 11/3/2020.

# Works Cited:

# Hill, Ryan A., Marc H. Weber, Scott G. Leibowitz, Anthony R. Olsen, and Darren J. Thornbrugh, 2016. The Stream-Catchment (StreamCat) Dataset: A Database of Watershed Metrics for the Conterminous United States. Journal of the American Water Resources Association (JAWRA) 52:120-128. DOI: 10.1111/1752-1688.12372.

# End of R script.
