# THESIS: 'Interim sample size reestimation for adequately powered series of N-of-1 trials' 
# Daphne Weemering, 3239480, M&S for the Behavioral, Biomedical and Social Sciences,
# Utrecht University

# Load in the required packages 
library(dplyr) # To use the 'mutate' function

# ------------------------------------------------------------------------------
# This file modifies the output from the simulation studies with a fixed sample  
# size in series of N-of-1 trials so that it can be used for plotting. 
# ------------------------------------------------------------------------------

# Set working directory to where the output of the simulation study is stored and
# load in the output
setwd('/Users/daphneweemering/Google Drive/UU/Thesis/MasterThesis/output/Raw output')
load('power-fixed.RData')

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

# Combine 'true_sigma_cat' and 'true_psi_cat' into one variable and include '&'
# sign
results$true_sigma_psi <- paste(results$true_sigma_cat, results$true_psi_cat, sep = " & ")

# Change the working directory and save the modified output in a separate file
setwd('/Users/daphneweemering/Google Drive/UU/Thesis/MasterThesis/output/Modified output')
save(results, file = 'data-for-plotting-fixed.RData')


