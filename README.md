# healthy_watershed_random_forest
Scripts generated for running random forest models for the Healthy Watersheds project at SCCWRP.

The workflow for this project consisted of:

(1) Assembling data from SCCWRP, external partners, and StreamCat.
(2) Ensuring all data was linked to a COMID.
(3) Building random forest models for ASCI, CSCI, CRAM, and RipRAM parameters using StreamCat landscape variables associated with human alteration.
(4) Using the built models to predict state-wide scores for all parameters.
(5) Assigning the scores for each parameter into four bins (very likely altered, likely altered, possibly altered, and likely unaltered).
(6) Plotting the scores on a map state-wide (and occasionally by watershed) using the shapefiles found at hw_datasets/NHD_Plus_CA/NHDPlus_V2_FLowline_CA.shp (assembled by Anne Holt).

Data - 

ASCI and CSCI datasets assembled from the SMC database. CRAM datasets downloaded from the SMC database, but come from eCRAM/CEDEN. RipRAM datasets were provided by Kevin O'Connor at Moss Landing Marine Laboraties/Central Coast Wetlands Group. Perennial Stream Assessment Region data assembled by SCCWRP. StreamCat variables assembled from https://www.epa.gov/national-aquatic-resource-surveys/streamcat-dataset-0, using California datasets only.

Models -

Random forest models have been created for ASCI, CSCI, CRAM (overall index score), and RipRAM along with initial validation figures.

Next steps: Creation of additional random forest models for 4 CRAM sub-metrics, and additional validation required for all existing models.

Files - 

Broad categories of files in this project are detailed below:
"XXX_rf.R" - random forest model + figures script for a given parameter
"XXX_rf_data1.csv" - training data used to build the random forest model for a given parameter
"XXX_rf_results.csv" - state-wide modeled values using the built random forest model for a given parameter (typically a very large file)
"XXX_rf_results_summary.csv" - state-wide kilometers of NHD stream reaches classified in a certain category (typically a very small file)
"XXX_lms.csv" - linear models of testing vs. predicted scores for a given parameter

Additional scripts help to compile the PSA Regional and StreamCat datasets to be used in each of the random forest modeling scripts.
