# Associations between environmental pollutants from hair and cardiovascular risk factors

Below is the recommended workflow for this project:  
## Data preparation and processing
1. 00-data_preparation.R: Cleaning data and outcome ascertainment
2. 02-data_preprocessing.R: Missing/detection rate, nd imputation, NA imputation
 and log transformation
3. 03-naimp_diagnostics.R: Distribution of exposure comparing observed and imputed values

 
## Descriptive analysis
1. 01-descriptive_statistics: Population characteristics, outcome prevalence, venn diagram
5. 02-correlation.R: Spearman correlation matrices for exposures
6. 02-pca.R: Principal component analysis of population based on exposure matrices
7. 02-graphical_lasso_network.R, Graphical lasso network of exposures
8. 03-graphical_lasso_network_visualisation.R, Visualisation and inspection of graphical lasso network of exposures

## Univariate analyses
1. univariate_analyses.R: Univariate analyses of association between exposures and outcomes
2. univariate_analyses_viz.R: Visualisation of uivariate analyses

## Multivariate analyses
1. 06-multivariate_analyses.R: Stability-based LASSO logistic regression per CVDRF outcome
2. 07-multivariate_analyses_inspect.R: Inspection of stability-based LASSO logistic regression, recalibration and prediction
3. 08-multivariate_analyses_viz.R: Visulisation of stability-based LASSO logistic regression

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