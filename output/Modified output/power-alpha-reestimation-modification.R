# THESIS: 'Interim sample size reestimation for adequately powered series of N-of-1 trials' 
# Daphne Weemering, 3239480, M&S for the Behavioral, Biomedical and Social Sciences,
# Utrecht University

# Load in the required packages 
library(dplyr)  # To use the 'mutate' function
library(tibble) # To use the 'add_column' function

# ------------------------------------------------------------------------------
# This file modifies the output from the simulation studies where interim sample 
# size reestimation was applied in series of N-of-1 trials so that it can be used
# for plotting. 
# ------------------------------------------------------------------------------

# Set working directory to where the output of the simulation study is stored and
# load in the output for the power and type I error rate
setwd('/Users/daphneweemering/Google Drive/UU/Thesis/MasterThesis/output/Raw output')
load('power-reestimation.RData')
load('type1errorrate.RData')

# Extract the column 'alpha' from the data frame 'resultsalpha'
alpha_temp <- as.data.frame(resultsalpha[,'alpha'])

# Rename this new variable
names(alpha_temp)[1] <- 'alpha'

# Combine the column 'alpha' from 'alpha_temp' including the type I error rates 
# with the results from the simulations for estimating the power
results <- add_column(resultspower, alpha_temp, .after = 6)

# Change data type to character for the true and hypothesized nuisance parameters 
# (first four columns) 
results$hyp_psi_cat <- as.character(ifelse(results$hyp_psi < 0.60, '0.5',
                                    ifelse(results$hyp_psi < 1.8, '1',
                                    ifelse(results$hyp_psi < 2.8, '2',))))

results$hyp_sigma_cat <- as.character(ifelse(results$hyp_sigma < 0.30, '0.25',
                                      ifelse(results$hyp_sigma < 0.60, '0.5',
                                      ifelse(results$hyp_sigma < 2, '1',))))

results$true_psi_cat <- as.character(ifelse(results$true_psi < 0.60, '0.5',
                                     ifelse(results$true_psi < 1.8, '1',
                                     ifelse(results$true_psi < 2.8, '2',))))

results$true_sigma_cat <- as.character(ifelse(results$true_sigma < 0.30, '0.25',
                                       ifelse(results$true_sigma < 0.60, '0.5',
                                       ifelse(results$true_sigma < 2, '1',))))

# Change data type of variable 'fraction' to character
results$fraction_cat <- as.character(ifelse(results$fraction < 0.30, '0.25',
                                     ifelse(results$fraction < 0.60, '0.50',
                                     ifelse(results$fraction < 0.80, '0.75'))))

# Paste 'f = ' in front of every value of 'fraction_cat'
results$fraction_cat <- paste0("f = ", results$fraction_cat)

# Combine 'true_sigma_cat' and 'true_psi_cat' into one variable and include '&'
# sign
results$true_sigma_psi <- paste(results$true_sigma_cat, results$true_psi_cat, sep = " & ")
results$hyp_sigma_psi <- paste(results$hyp_sigma_cat, results$hyp_psi_cat, sep = " & ")

# Add an additional column for the true sample sizes
results <- mutate(results, hline = case_when(true_sigma_cat == '0.25' & true_psi_cat == '0.5' ~ 7.4,
                                             true_sigma_cat == '0.25' & true_psi_cat == '1' ~ 11.2,
                                             true_sigma_cat == '0.25' & true_psi_cat == '2' ~ 19.0,
                                             true_sigma_cat == '0.5' & true_psi_cat == '0.5' ~ 8.7,
                                             true_sigma_cat == '0.5' & true_psi_cat == '1' ~ 12.5,
                                             true_sigma_cat == '0.5' & true_psi_cat == '2' ~ 20.3,
                                             true_sigma_cat == '1' & true_psi_cat == '0.5' ~ 11.2,
                                             true_sigma_cat == '1' & true_psi_cat == '1' ~ 15.1, 
                                             true_sigma_cat == '1' & true_psi_cat == '2' ~ 22.9))

# Change the working directory and save the modified output in a separate file
setwd('/Users/daphneweemering/Google Drive/UU/Thesis/MasterThesis/output/Modified output')
save(results, file = 'data-for-plotting-reestimation.RData')








