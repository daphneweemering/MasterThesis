#### Data modification of data for power for series of N-of-1 trials with
#### fixed sample size

# Load in packages
library(dplyr)

# Set working directory to where the data of the simulation study is stored and
# load in the data for the power and type I error rate
setwd('/Users/daphneweemering/Google Drive/UU/Thesis/MasterThesis/data/Raw data')
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

# Combine 'true_sigma_cat' and 'true_psi_cat' into one variable
results$true_sigma_psi <- paste(results$true_sigma_cat, results$true_psi_cat, sep = " & ")

# Change the working directory and save the modified data in a separate file
setwd('/Users/daphneweemering/Google Drive/UU/Thesis/MasterThesis/data/Modified data')
save(results, file = 'data-for-plotting-fixed.RData')


