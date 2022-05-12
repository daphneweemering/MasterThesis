# 'Interim sample size reestimation for adequately powered series of N-of-1 trials' 

# Load in the required packages 
library(lme4)     # For the 'lmer' function fitting the linear mixed models
library(pwr)      # To calculate the sample size 
library(lmerTest) # Provides p-values in summary tables for linear mixed models

# ------------------------------------------------------------------------------
# The function 'reestim' runs through the interim sample size reestimation process 
# based on nuisance parameter values for series of N-of-1 trials. 
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
#                             error used for data generation: tvar_error;
#                           - The fraction(s) of the initial sample size: fn.
# - avg_treatment_sampsize: Average treatment effect for calculating the sample size 
#                           (i.e., clinically relevant difference);
# - avg_treatment_data:     Average treatment effect for generating the data. This is set 
#                           equal to 1 for calculating the power, and set to 0 for 
#                           calculating the type I error rate;
# - N:                      Number of simulation runs;
# - seed:                   For reproducibility. 

# Steps the function runs through:
# 1. Calculate the initially required sample size under hvar_treatment, hvar_error,
#    avg_treatment_sampsize, alpha-level (0.05) and power (0.80) and take a fraction
#    of this initial sample size;
# 2. Simulate data for the fraction of the initial sample size;
# 3. Fit a linear mixed model with the generated data;
# 4. Extract the variance of the treatment effect and the variance of the random error 
#    and use those values to calculate a new sample size in the same manner as before;
# 5. Subtract the already 'observed' subjects from the new sample size and generate 
#    data for the remainder;
# 6. Fit a linear mixed model with all the generated data and subtract the p-value
#    of the fixed effect;
# 7. Iterate this process N times;
# 8: Two separate simulation studies with each a different outcome measure: 
#     - 8a. Calculate the power as the portion of simulation runs that the p-value was 
#           significant under avg_treatment_data = 1. 
#     - 8b. Calculate the type I error rate as the portion of simulation runs that the 
#           p-value was significant under avg_treatment_data = 1. 
# ------------------------------------------------------------------------------

reestim <- function(x, n_cycles = 3, avg_treatment_sampsize = 1, avg_treatment_data = 1,
                    N = 10000, seed = 3239480){
  
  # Specify which value from list x is what
  hvar_treatment <- x[1]  # Hypothesized variance of the treatment effect
  hvar_error <- x[2]      # Hypothesized variance of the random error 
  tvar_treatment <- x[3]  # Variance of treatment effect for data generation
  tvar_error <- x[4]      # Variance of random error for data generation
  fn <- x[5]              # Fraction of the initial sample size 
  
  # Set a seed for reproducibility
  set.seed(seed)
  
  # Make storage for the fraction of the initial sample size that is used for 
  # reestimation, the reestimated sample size, and for a column that indicates 
  # whether the fixed effect is significant. 
  output <- matrix(data = NA, nrow = N, ncol = 3)
  
  # Start loop that simulates the process of interim sample size reestimation in 
  # series of N-of-1 trials and iterate the process N times
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
    
    # Extract the sample size from 'pwrcalc'
    sampsizehyp <- pwrcalc$n
    
    # Calculate the fraction (fn) of the initial sample size. This fraction is used
    # for the first stage of the two-stage design
    sampsizefrac <- ceiling(fn*sampsizehyp)
    
    # Create a dataframe including patients, cycles and an index. In this matrix
    # the generated data will be stored for the fraction of the initial sample size
    dat = data.frame(patient   = factor(sort(rep((1:sampsizefrac), n_cycles))),
                     cycle     = factor(rep(sort(rep(1:n_cycles)), sampsizefrac)),
                     index     = factor(rep(c(1:(n_cycles)), sampsizefrac)))
    
    # Make storage for the simulated values the treatment effect and the random error 
    treatment_effect <- matrix(data = NA, nrow = 1, ncol = sampsizefrac)
    random_error <- matrix(data = NA, nrow = 1, ncol = sampsizefrac*n_cycles)
    
    # Make storage for the simulated values of the outcome 
    d_ij <- matrix(data = NA, nrow = 1, ncol = sampsizefrac*n_cycles)
    
    # Simulate values for the treatment effect for the random error 
    treatment_effect <- as.data.frame(rnorm(n = sampsizefrac, mean = avg_treatment_data, 
                                            sd = sqrt(tvar_treatment)))
    random_error <- as.data.frame(rnorm(n = sampsizefrac*n_cycles, mean = 0, 
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
    estim <- matrix(data = NA, nrow = 1, ncol = 4)
    
    # Fit the linear model using the generated data
    out <- lmer(formula = d_ij ~ 1 + (1 | patient), data = dat)
    
    # Extract the fixed effect and the p-value (respectively)
    estim[,1] <- summary(out)$coefficients[1,1] 
    estim[,2] <- summary(out)$coefficients[1,5]
    
    # Extract the variance of the treatment effect and the variance of the random error
    # (respectively)
    temp <- as.data.frame(VarCorr(out))
    estim[,3] <- temp[1,5]
    estim[,4] <- temp[2,5]
    
    # Calculate the standard deviation of the average treatment effect based on the 
    # number of cycles, interim estimate of the variance of the treatment effect 
    # and interim estimate of the variance of the random error 
    sd_avg_treatment2 <- sqrt(estim[,3]^2 + (estim[,4]^2)/n_cycles)
    
    # If effect size is larger than 10, it's set to ten in order to avoid the sample
    # size to become too small. Otherwise the calculated effect size is used
    if(avg_treatment_sampsize/sd_avg_treatment2 > 10){
      # Calculate the sample size by plugging in sd_avg_treatment
      pwrcalc2 <- pwr.t.test(d = 10, power = 0.8, sig.level = 0.05, 
                             type = "one.sample", alternative = "two.sided")
    } else{
      # Calculate the sample size by plugging in sd_avg_treatment
      pwrcalc2 <- pwr.t.test(d = avg_treatment_sampsize/sd_avg_treatment2, power = 0.8, 
                             sig.level = 0.05, type = "one.sample", alternative = "two.sided")
    }
    
    # Extract the reestimated sample size from 'pwrcalc'
    sampsizefinal <- pwrcalc2$n
    
    # Round up the reestimated sample size 
    sampsizefinal <- ceiling(sampsizefinal)
    
    # Subtract the already observed number of patients from the reestimated sample size. 
    # The remainder still needs to be observed (i.e., data needs to be generated)
    sampsizeremain <- sampsizefinal - sampsizefrac
    
    # If the remaining sample size is zero or negative, store results and stop. If 
    # remaining sample size is larger than zero, observe remaining number of subjects
    if (sampsizeremain <= 0){
      output[i,1] <- sampsizefrac
      output[i,2] <- sampsizefinal
      
      # Indicate a significant fixed effect with a 1 and non-significant one with 
      # a 0
      output[i,3] <- ifelse(estim[,2] < 0.05, 1, 0)
    } else{
    
      # Make storage for the simulated values of the treatment effect and the random
      # error for the remaining number of patients
      treatment_effect2 <- matrix(data = NA, nrow = 1, ncol = sampsizeremain)
      random_error2 <- matrix(data = NA, nrow = 1, ncol = sampsizeremain*n_cycles)
      
      # Make storage for the simulated value of the outcome for the remaining number 
      # of patients
      d_ij2 <- matrix(data = NA, nrow = 1, ncol = sampsizeremain*n_cycles)
      
      # Simulate values for the treatment effect and the random error
      treatment_effect2 <- as.data.frame(rnorm(n = sampsizeremain, mean = avg_treatment_data, sd = sqrt(tvar_treatment)))
      random_error2 <- as.data.frame(rnorm(n = sampsizeremain*n_cycles, mean = 0, sd = sqrt(2*tvar_error)))
      
      # Duplicate the values of the treatment effect for each cycle 
      treatment_effect2 <- (sapply(treatment_effect2, rep, each = 3))
      
      # Obtain values for the outcome ('d_ij') by adding the treatment effect and the 
      # random error and change column name
      d_ij2 <- as.data.frame(treatment_effect2 + random_error2)
      colnames(d_ij2)[1] <- 'd_ij'
      
      # Combine the outcome of the first (before reestimation of sample size) and 
      # the second half (after reestimation of the sample size) of the patients
      outcome <- rbind(d_ij, d_ij2)
      
      # Make a dataframe for the final total sample size with patients number, cycle
      # number and an index
      dat2 <- data.frame(patient   = factor(sort(rep((1:sampsizefinal), n_cycles))),
                         cycle     = factor(rep(sort(rep(1:n_cycles)), sampsizefinal)),
                         index     = factor(rep(c(1:(n_cycles)), sampsizefinal)))
      
      # Combine the patient number, cycle number, index and outcome of the total sample
      dat2 <- cbind(dat2, outcome)
      
      # Rename the last column name of dat2
      colnames(dat2)[4] <- 'd_ij_full'
      
      # Make storage for the estimated values of the fixed intercept, variance of 
      # the treatment effect, variance of the random error and the p-value of the 
      # fixed effect
      estimfinal <- matrix(data = NA, nrow = 1, ncol = 4)
      
      # Fit the linear model using the generated data
      out2 <- lmer(formula = d_ij_full ~ 1 + (1 | patient), data = dat2)
      
      # Extract the fixed effect and the p-value (respectively)
      estimfinal[,1] <- summary(out2)$coefficients[1,1]
      estimfinal[,2] <- summary(out2)$coefficients[1,5]
      
      # Extract the variance of the treatment effect and the variance of the random
      # error (respectively)
      temp <- as.data.frame(VarCorr(out2))
      estimfinal[,3] <- temp[1,5]
      estimfinal[,4] <- temp[2,5]
      
      # Specify that sampsizefrac and sampsizefinal come in the first two columns of
      # the output matrix
      output[i,1] <- sampsizefrac
      output[i,2] <- sampsizefinal 
      
      # Indicate a significant fixed effect with a 1 and non-significant one with 
      # a 0
      output[i,3] <- ifelse(estimfinal[,2] < 0.05, 1, 0)
    }
  }
  
  # Calculate the total power/alpha rate (power if avg_treatment_data = 1, alpha rate
  # if avg_treatment_data = 0) 
  pwr_or_alpha <- sum(output[,3]/N)
  
  # Calculate the mean sample sizes
  initialsampsize <- mean(sampsizehyp) # Initial sample size 
  initialsampsize2 <- mean(output[,1]) # Fraction of initial sample size for 
                                       # reestimation
  finalsampsize <- mean(output[,2])    # Mean of N reestimated sample sizes
  
  # Specify variance and sd of reestimated sample size
  varfinalsampsize <- var(output[,2])
  sdfinalsampsize <- sd(output[,2])
  
  # Specify median, min and max values of the reestimated sample size
  medreestim <- median(output[,2])
  minreestim <- min(output[,2])
  maxreestim <- max(output[,2])
  
  # Output
  return(list(pwr_or_alpha, initialsampsize, initialsampsize2, finalsampsize,
              varfinalsampsize, sdfinalsampsize, medreestim, minreestim, maxreestim))
}

# ------------------------------------------------------------------------------

