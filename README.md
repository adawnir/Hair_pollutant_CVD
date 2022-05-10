# Associations between environmental pollutants from hair and cardiovascular risk factors

Below is the recommended workflow for this project:  
## Data preparation and processing
1. 00-data_preparation.R: Cleaning data and outcome ascertainment
2. 01-data_processing.R: Missing/detection rate, nd imputation, log transformation and NA imputation

## Descriptive analysis
1. 02-expo_distribution.R: Distribution of exposure stratified by country
2. 02-naimp_diagnostics.R: Distribution of exposure comparing observed and imputed values
3. 02-descriptive_statistics.R: Tables containing descriptive summary of covariates, exposures and outcomes
4. 02-venn_diagram.R: Venn diagrams showing concurrence of outcomes
5. 02-correlation.R: Spearman correlation matrices for exposures
6. 02-pca.R: Principal component analysis of population based on exposure matrices
7. 02-graphical_lasso_network.R, Graphical lasso network of exposures
8. 03-graphical_lasso_network_visualisation.R, Visualisation and inspection of graphical lasso network of exposures

## Univariate analyses
  Univariate, Univariate_viz

## Multivariate analyses
  Denoised.R, Divide_by_outcomes.R, split_train_test.R
  LASSO.R, LASSO_viz.R: LASSO models for univariate Y
  sPLS_uni_y.R, sPLS_uni_y_viz.R: sPLS models for univariate Y
  Pred_perf.R: Prediction performance of all the models and inspect lasso models

## Sensitivity analyses
  ./strat_sex/: LASSO and sPLS stratified by sex 
  ./strat_sex_same/: LASSO and sPLS stratified by sex using same no. of obs.
  ./strat_country/: LASSO and sPLS stratified by scountry
  ./strat_country_same/: LASSO and sPLS stratified by country using same no. of obs.

## Source codes
* functions.R: Custom functions
* graph_colours.R: Graph colour specifications
* pls_functions
* penalisation_functions
* penalisation_functions_1: change some commands for country-stratified lasso models