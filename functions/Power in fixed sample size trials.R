# 'Interim sample size reestimation for adequately powered series of N-of-1 trials' 

# Load the required libraries
library(lme4)
library(pwr)
library(lmerTest)

# ------------------------------------------------------------------------------
################################################################################

# A function that runs through the whole sample size reestimation process. It 
# starts with calculating the initial sample size based on hypothesized parameter
# values. Then, a part of the subjects are observed and parameters are estimated. Based
# on these interim estimates, the sample size is recalculated and the remaining 
# subjects are observed and the parameters are again estimated. This process is
# iterated N times and after looping through N iterations, power is calculated. 
reestim <- function(x, n_cycles = 3, avg_treatment_sampsize = 1, avg_treatment_data = 1,
                    N = 10000, seed = 3239480){
  
  # Specify which value from list x is what
  hvar_treatment <- x[1]
  hvar_error <- x[2]
  tvar_treatment <- x[3]
  tvar_error <- x[4]
  fn <- x[5]
  
  # Set a seed for reproducibility
  set.seed(seed)
  
  # Make storage for the final output
  output <- matrix(data = NA, nrow = N, ncol = 1)
  
  for (i in 1:N){
    # ----------------------------------------------------------------------------
    # 1. Calculate the initial sample size based on hypothesized values for \psi^2
    #    and \sigma^2
    
    # Calculate the standard deviation of the average treatment effect 
    sd_avg_treatment <- sqrt(hvar_treatment + (2*hvar_error)/n_cycles)
    
    # If effect size is larger than 10, it's set to ten in order to avoid the sample
    # size to become too small. Otherwise the calculated effect size is used
    if(avg_treatment_sampsize/sd_avg_treatment > 10){
      # Calculate the corresponding sample size plugging in the standard deviation
      pwrcalc <- pwr.t.test(d = 10, power = 0.8, sig.level = 0.05, 
                            type = "one.sample", alternative = "two.sided")
    } else{
      # Calculate the corresponding sample size plugging in the standard deviation
      pwrcalc <- pwr.t.test(d = avg_treatment_sampsize/sd_avg_treatment, power = 0.8, 
                            sig.level = 0.05, type = "one.sample", alternative = "two.sided")
    }
    
    # Extract the sample size from 'pwrcalc'
    sampsizehyp <- pwrcalc$n
    
    # Calculate the fraction (c(0.25, 0.5, 0.75)) of the initial sample size. After
    # observing this fraction, sample size reestimation is performed
    sampsizefrac <- ceiling(fn*sampsizehyp)
    
    
    # ----------------------------------------------------------------------------
    # 2. Use the hypothesized sample size for sample size reestimation: estimate 
    #    parameters at interim and base final sample size on interim estimates of 
    #    \psi^2 and \sigma^2
    
    # Create a dataframe including patients, cycles and an index
    dat = data.frame(patient   = factor(sort(rep((1:sampsizefrac), n_cycles))),
                     cycle     = factor(rep(sort(rep(1:n_cycles)), sampsizefrac)),
                     index     = factor(rep(c(1:(n_cycles)), sampsizefrac)))
    
    # Make storage for the simulated values of the variance of the treatment effect 
    # and the variance of the random error 
    treatment_effect <- matrix(data = NA, nrow = 1, ncol = sampsizefrac)
    random_error <- matrix(data = NA, nrow = 1, ncol = sampsizefrac*n_cycles)
    
    # Make storage for the simulated value of the outcome 
    d_ij <- matrix(data = NA, nrow = 1, ncol = sampsizefrac*n_cycles)
    
    # First, simulate values for \psi^2 and \sigma^2
    treatment_effect <- as.data.frame(rnorm(n = sampsizefrac, mean = avg_treatment_data, sd = sqrt(tvar_treatment)))
    random_error <- as.data.frame(rnorm(n = sampsizefrac*n_cycles, mean = 0, sd = sqrt(2*tvar_error)))
    
    # Duplicate the values for the treatment effect for each cycle 
    treatment_effect <- (sapply(treatment_effect, rep, each = 3))
    
    # Simulate values for the outcome d_ij and change column name
    d_ij <- as.data.frame(treatment_effect + random_error)
    colnames(d_ij)[1] <- 'd_ij'
    
    # Combine the dataframe including patient number, cycle number and an index with 
    # the outcome values
    dat <- cbind(dat, d_ij)
    
    # Make storage for the estimated values of the fixed intercept, \psi^2, \sigma^2
    # and the t-value of the estimates.
    estim <- matrix(data = NA, nrow = 1, ncol = 4)
    
    # Estimate the mean treatment effect and store
    out <- lmer(formula = d_ij ~ 1 + (1 | patient), data = dat)
    
    # Extract the fixed effects and their t-value (respectively)
    estim[,1] <- summary(out)$coefficients[1,1]
    estim[,2] <- summary(out)$coefficients[1,4]
    
    output[i,1] <- ifelse(estim[,2] < 1.96, 0, 1)
  }
  
  # Calculate the total power/alpha rate (power if avg_treatment_data = 1, alpha rate
  # if avg_treatment_data = 0)
  pwr <- sum(output[,1]/N)
  
  # Output
  return(pwr)
}



################################################################################

