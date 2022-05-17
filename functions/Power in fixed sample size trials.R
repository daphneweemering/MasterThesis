# THESIS: 'Interim sample size reestimation for adequately powered series of N-of-1 trials' 
# Daphne Weemering, 3239480, M&S for the Behavioral, Biomedical and Social Sciences,
# Utrecht University

# Load in the required packages 
library(lme4)     # For the 'lmer' function fitting the linear mixed models
library(pwr)      # To calculate the sample size 
library(lmerTest) # Provides p-values in summary tables for linear mixed models

# ------------------------------------------------------------------------------
# The function 'fixed' runs through the usual process for series of N-of-1 trials
# with a fixed sample size.
# Input: 
# - x                     : x should include (these are specified in the simulation
#                           files):
#                           - The hypothesized value(s) for the variance of the 
#                             treatment effect: hvar_treatment;
#                           - The hypothesized value(s) for the variance of the 
#                             random error: hvar_error;
#                           - The 'true' value(s) for the variance of the treatment  
#                             effect used for data generation: tvar_treatment;
#                           - The 'true' value(s) for the variance of the random 
#                             error used for data generation: tvar_error.
# - avg_treatment_sampsize: Average treatment effect for calculating the sample size 
#                           (i.e., clinically relevant difference);
# - avg_treatment_data:     Average treatment effect for generating the data. This is set 
#                           equal to 1 for calculating the power;
# - N:                      Number of simulation runs;
# - seed:                   For reproducibility. 

# Steps the function runs through:
# 1. Calculate the initially required sample size under hvar_treatment, hvar_error,
#    avg_treatment_sampsize, alpha-level (0.05) and power (0.80) and take a fraction
#    of this initial sample size;
# 2. Simulate data for the sample size;
# 3. Fit a linear mixed model with the generated data and subtract the p-value of
#    the fixed effect;
# 4. Iterate this process N times;
# 5. Calculate the power as the portion of simulation runs that the p-valye was
#    significant. 
# ------------------------------------------------------------------------------

fixed <- function(x, n_cycles = 3, avg_treatment_sampsize = 1, avg_treatment_data = 1,
                    N = 10000, seed = 3239480){
  
  # Specify which value from list x is what
  hvar_treatment <- x[1] # Hypothesized variance of the treatment effect
  hvar_error <- x[2]     # Hypothesized variance of the random error
  tvar_treatment <- x[3] # Variance of treatment effect for data generation
  tvar_error <- x[4]     # Variance of random error for data generation
  
  # Set a seed for reproducibility
  set.seed(seed)
  
  # Create a matrix with one column that indicates whether the fixed effect is 
  # significant.
  output <- matrix(data = NA, nrow = N, ncol = 1)
  
  # Start loop that simulates a series of N-of-1 trials with a fixed sample size 
  # and iterate the process N times
  for (i in 1:N){
    
    # Calculate the standard deviation of the average treatment effect based on the 
    # number of cycles, hypothesized values for the variance of the treatment effect 
    # and the variance of the random error  
    sd_avg_treatment <- sqrt(hvar_treatment + (2*hvar_error)/n_cycles)
    
    # If effect size is larger than 10, it's set to 10 in order to avoid the sample
    # size to become too small. Otherwise the calculated effect size is used
    if(avg_treatment_sampsize/sd_avg_treatment > 10){
      # Calculate the sample size by plugging in sd_avg_treatment
      pwrcalc <- pwr.t.test(d = 10, power = 0.8, sig.level = 0.05, 
                            type = "one.sample", alternative = "two.sided")
    } else{
      # Calculate the sample size by plugging in sd_avg_treatment
      pwrcalc <- pwr.t.test(d = avg_treatment_sampsize/sd_avg_treatment, power = 0.8, 
                            sig.level = 0.05, type = "one.sample", alternative = "two.sided")
    }
    
    # Extract the rounded sample size from 'pwrcalc'
    sampsizehyp <- ceiling(pwrcalc$n)
    
    # Create a dataframe including patients, cycles and an index
    dat = data.frame(patient   = factor(sort(rep((1:sampsizehyp), n_cycles))),
                     cycle     = factor(rep(sort(rep(1:n_cycles)), sampsizehyp)),
                     index     = factor(rep(c(1:(n_cycles)), sampsizehyp)))
    
    # Make storage for the simulated values the treatment effect and the random error 
    treatment_effect <- matrix(data = NA, nrow = 1, ncol = sampsizehyp)
    random_error <- matrix(data = NA, nrow = 1, ncol = sampsizehyp*n_cycles)
    
    # Make storage for the simulated values of the outcome 
    d_ij <- matrix(data = NA, nrow = 1, ncol = sampsizehyp*n_cycles)
    
    # Simulate values for the treatment effect for the random error 
    treatment_effect <- as.data.frame(rnorm(n = sampsizehyp, mean = avg_treatment_data, 
                                            sd = sqrt(tvar_treatment)))
    random_error <- as.data.frame(rnorm(n = sampsizehyp*n_cycles, mean = 0, 
                                        sd = sqrt(2*tvar_error)))
    
    # Duplicate the values for the treatment effect for each cycle (as these should
    # be the same in each cycle)
    treatment_effect <- (sapply(treatment_effect, rep, each = 3))
    
    # Obtain values for the outcome ('d_ij') by adding the treatment effect and the 
    # random error and change column name
    d_ij <- as.data.frame(treatment_effect + random_error)
    colnames(d_ij)[1] <- 'd_ij'
    
    # Combine the dataframe that includes patient number, cycle number and index with 
    # the outcome values
    dat <- cbind(dat, d_ij)
    
    # Make storage for the estimated values of the fixed intercept, variance of the 
    # treatment effect, variance of the random error and the p-value of the fixed effect
    estim <- matrix(data = NA, nrow = 1, ncol = 2)
    
    # Fit the linear model using the generated data
    out <- lmer(formula = d_ij ~ 1 + (1 | patient), data = dat)
    
    # Extract the fixed effect and the p-value (respectively)
    estim[,1] <- summary(out)$coefficients[1,1]
    estim[,2] <- summary(out)$coefficients[1,5]
    
    # Indicate a significant fixed effect with a 1 and a non-significant one with 
    # a 0
    output[i,1] <- ifelse(estim[,2] < 0.05, 1, 0)
  }
  
  # Calculate the total power
  pwr <- sum(output[,1]/N)
  
  # Return the power 
  return(pwr)
}

# ------------------------------------------------------------------------------
