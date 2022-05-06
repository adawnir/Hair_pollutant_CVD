# Associations between environmental pollutants from hair and cardiovascular risk factors

Recommended workflow:
DATA PROCESSING:
00-data_preparation.R
Explore_pest.R, Explore_pahs.R, Explore_overlap.R: Descriptive analyses, correlation
  Imputation_nd.R: Recode non detectded data and impute mising data
  Make_uni_multi_data.R: Recode variables and make data for following analyses + create overlapping dataset

DESCRIPTIVE ANALYSIS:
  descriptive_by_country.R
  conc_country.R: Run test on concentration by countries
  correlation.R: check log-transformed levels as Gaussian is not too big of an assumption
  Concurrence.R: Check the concurrence of CVRFs
  PCA.R
  Network.R, Network_inspect.R: Graphical lasso network

UNIVARIATE ANALYSIS:
  Univariate, Univariate_viz

MULTIVARIATE ANALYSES:
  Denoised.R, Divide_by_outcomes.R, split_train_test.R
  LASSO.R, LASSO_viz.R: LASSO models for univariate Y
  sPLS_uni_y.R, sPLS_uni_y_viz.R: sPLS models for univariate Y
  Pred_perf.R: Prediction performance of all the models and inspect lasso models

SENSITIVITY ANALYSES:
  ./strat_sex/: LASSO and sPLS stratified by sex 
  ./strat_sex_same/: LASSO and sPLS stratified by sex using same no. of obs.
  ./strat_country/: LASSO and sPLS stratified by scountry
  ./strat_country_same/: LASSO and sPLS stratified by country using same no. of obs.


*** categorical file: Recode chemicals as categorical variables and re run analyses
DATA PROCESSING:
  cat_imputation_nd.R

DESCRIPTIVE ANALYSIS:
  cat_desc_analyses.R

UNIVARIATE ANALYSIS:
  cat_univariate.R, cat_univariate_viz.R

MULTIVARIATE ANALYSES:
  cat_denoised.R cat.divide_by_outcomes.R
  cat_lasso.R, cat_lasso_viz.R: LASSO models for univariate Y
  cat_spls_uni_y.R, cat_spls_uni_y_viz.R: sPLS models for univariate Y

sources:
*pls_functions
*penalisation_functions
*penalisation_functions_1: change some commands for country-stratified lasso models