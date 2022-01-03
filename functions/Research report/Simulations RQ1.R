# Simulation thesis: 'Interim sample size reestimation for adequately powered
# series of N-of-1 trials' 

library(lme4)
library(pwr)
library(lmerTest)

# 1. ---------------------------------------------------------------------------
################################################################################
#### Function to calculate the initial sample size under hypothesized \psi^2 and 
#### \sigma^2 ####

initsampsize <- function(var_treatment, var_error, fn, n_cycles = 3){
  
  # Calculate the variance of the average treatment effect 
  sd_avg_treatment <- sqrt(var_treatment + (2*var_error)/n_cycles)
  
  # Calculate the corresponding sample size plugging in the standard deviation
  pwrcalc <- pwr.t.test(d = 1/sd_avg_treatment, power = 0.8, sig.level = 0.05, 
                        type = "paired", alternative = "two.sided")
  
  # Extract the sample size from 'pwrcalc'
  init_n_size <- pwrcalc$n
  
  # Calculate the fraction (c(0.25, 0.5, 0.75)) of the initial sample size. After
  # observing this fraction, sample size reestimation is performed
  sampsizefrac <- ceiling(fn*init_n_size)
  
  # Return the standard deviation of the average treatment effect (used to calculate
  # the sample size) and the sample size itself
  return(list(sd_avg_treatment, init_n_size, sampsizefrac))
}


# 2. ---------------------------------------------------------------------------
################################################################################
#### Function to run the model for different parameter settings #### 

before_reestim <- function(n_patients, true_var_treatment, true_var_error, N, 
                           avg_treatment = 1, n_cycles = 3, seed = 3239480){
  
  # Set a seed for reproducibility
  set.seed(seed)
  
  # Create a dataframe including patients and cycles
  dat = data.frame(patient   = factor(sort(rep((1:n_patients), n_cycles))),
                   cycle     = factor(rep(sort(rep(1:n_cycles)), n_patients)),
                   index     = factor(rep(c(1:(n_cycles)), n_patients)))
  
  # Make storage for the simulated values of the treatment effect (\tau_i) and the 
  # random error (\epsilon_ij)
  treatment_effect <- matrix(data = NA, nrow = n_patients, ncol = N)
  random_error <- matrix(data = NA, nrow = n_patients*n_cycles, ncol = N)
  
  # Make storage for the simulated value of the outcome (difference between 
  # treatment A and treatment B; d_ij)
  d_ij <- matrix(data = NA, nrow = n_patients*n_cycles, ncol = N)
  
  for(i in 1:N){
    # Simulate values for the treatment effect and the random error
    treatment_effect[,i] <- rnorm(n = n_patients, mean = avg_treatment, sd = sqrt(true_var_treatment))
    random_error[,i] <- rnorm(n = n_patients*n_cycles, mean = 0, sd = sqrt(2*true_var_error))
  }
  
  # Duplicate the values for the treatment effect for each cycle 
  treatment_effect <- (apply(treatment_effect, 2, rep, each = 3))
  
  for(i in 1:N){
    # Simulate values for the outcome d_ij using the population model from Senn (2019)
    d_ij[,i] <- treatment_effect[,i] + random_error[,i]
  }
  
  # Change the column names of d_ij
  colnames(d_ij) <- c(1:N) # Set column names to 1:N
  colnames(d_ij) <- paste("d_ij", colnames(d_ij), sep = "") # Add 'd_ij' before the value
  
  # Combine the dataframe including patient number, cycle number and an index with 
  # the outcome values. 
  dat <- cbind(dat, d_ij)
  
  ## Estimate the mean treatment effect & calculate power 
  # Make storage for the estimated values 
  estim <- matrix(data = NA, nrow = N, ncol = 4)
  
  # Change the column names for the 'estim' matrix
  colnames(estim) <- c('fixed intercept', 't-value fixed intercept', 'random intercept', 
                       'residual')
  
  # Make storage for the significant fixed effects
  sig <- matrix(data = NA, nrow = N, ncol = 1)
  
  # For each simulation, estimate the mean treatment effect and store
  for (i in 1:N){
    out <- lmer(formula = d_ij[,i] ~ 1 + (1 | patient), data = dat)
    
    # Extract the fixed effects and their t-value
    estim[i,1] <- summary(out)$coefficients[1,1]
    estim[i,2] <- summary(out)$coefficients[1,4]
    
    # Extract the sd of the random intercept and the residual
    temp <- as.data.frame(VarCorr(out))
    estim[i,3] <- temp[1,5]
    estim[i,4] <- temp[2,5]
    
    # Indicate which estimates are significantly different from zero 
    sig[i,] <- ifelse(estim[i,2] < 1.96, 0, 1)
    
    pwr <- sum(sig[,1] / N)
  }
  
  # Let the function return the dataframe of the estimated parameters and the 
  # estimated power
  estim <<- estim
  return(pwr)
}


# 3. ---------------------------------------------------------------------------
################################################################################
#### A function to recalculate the sample size with the interim estimates of \psi^2 
#### and \sigma^2. PLug in the dataframe 'estim' from the function 
#### 'before_reestim'. ####

finalsampsize <- function(estim, N, n_cycles = 3){
  
  ## First, we can plug in the interim estimates of \psi^2 and \sigma^2 into the 
  ## sample size calculation
  
  # Make some storage for the sd of the average treatment effect and the final
  # sample size
  sd_avg_treatment <- matrix(data = NA, nrow = N, ncol = 1)
  final_n_size <- matrix(data = NA, nrow = N, ncol = 1)
  
  for (i in 1:N){
    # Calculate the variance of the average treatment effect 
    sd_avg_treatment[i,] <- sqrt(estim[i,3]^2 + (estim[i,4]^2)/n_cycles)
    
    # Calculate the corresponding sample size necessary
    pwrcalc <- pwr.t.test(d = 1/sd_avg_treatment[i,], power = 0.8, sig.level = 0.05, 
                          type = "paired", alternative = "two.sided")
    
    # Extract the sample size from 'pwrcalc'
    final_n_size[i,] <- pwrcalc$n
  }
  
  # Return the whole 'final_n_size' dataframe (needed for next function)
  final_n_size <<- final_n_size
}


