# 'Interim sample size reestimation for adequately powered
# series of N-of-1 trials' 

library(lme4)
library(pwr)
library(lmerTest)

# ------------------------------------------------------------------------------
################################################################################

reestim <- function(hvar_treatment, hvar_error, tvar_treatment, tvar_error, fn, 
                    n_cycles = 3, avg_treatment = 1, N = 100, seed = 3239480){
  
  # Set a seed for reproducibility
  set.seed(seed)
  
  # Make storage for the final output
  output <- matrix(data = NA, nrow = N, ncol = 3)
  
  for (i in 1:N){
  # ----------------------------------------------------------------------------
  # 1. Calculate the initial sample size based on hypothesized values for \psi^2
  #    and \sigma^2
  
  # Calculate the standard deviation of the average treatment effect 
  sd_avg_treatment <- sqrt(hvar_treatment + (2*hvar_error)/n_cycles)
  
  # Calculate the corresponding sample size plugging in the standard deviation
  pwrcalc <- pwr.t.test(d = 1/sd_avg_treatment, power = 0.8, sig.level = 0.05, 
                        type = "paired", alternative = "two.sided")
  
  # Extract the sample size from 'pwrcalc'
  sampsizehyp <- pwrcalc$n
  
  # Calculate the fraction (c(0.25, 0.5, 0.75)) of the initial sample size. After
  # observing this fraction, sample size reestimation is performed
  sampsizefrac <- ceiling(fn*sampsizehyp)
  
  
  # ----------------------------------------------------------------------------
  # 2. Use the hypothesized sample size for sample size reestimation: estimate 
  #    parameters at interim and base final sample size on interim estimates of 
  #    \psi^2 and \sigma^2
  
  # Create a dataframe including patients and cycles
  dat = data.frame(patient   = factor(sort(rep((1:sampsizefrac), n_cycles))),
                   cycle     = factor(rep(sort(rep(1:n_cycles)), sampsizefrac)),
                   index     = factor(rep(c(1:(n_cycles)), sampsizefrac)))
  
  # Make storage for the simulated values of the variance of the treatment effect 
  # and the variance of the random error 
  treatment_effect <- matrix(data = NA, nrow = 1, ncol = sampsizefrac)
  random_error <- matrix(data = NA, nrow = 1, ncol = sampsizefrac*n_cycles)
  
  # Make storage for the simulated value of the outcome (difference between 
  # treatment A and treatment B; d_ij)
  d_ij <- matrix(data = NA, nrow = 1, ncol = sampsizefrac*n_cycles)
  
  # First, simulate values for \psi^2 and \sigma^2
  treatment_effect <- as.data.frame(rnorm(n = sampsizefrac, mean = avg_treatment, sd = sqrt(tvar_treatment)))
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
  
  # Extract the fixed effects and their t-value
  estim[,1] <- summary(out)$coefficients[1,1]
  estim[,2] <- summary(out)$coefficients[1,4]
  
  # Extract the sd of the random intercept and the residual
  temp <- as.data.frame(VarCorr(out))
  estim[,3] <- temp[1,5]
  estim[,4] <- temp[2,5]
  
  # Calculate the standard deviation of the average treatment effect 
  sd_avg_treatment2 <- sqrt(estim[,3]^2 + (estim[,4]^2)/n_cycles)
  
  # Calculate the final sample size necessary
  pwrcalc2 <- pwr.t.test(d = 1/sd_avg_treatment2, power = 0.8, sig.level = 0.05, 
                         type = "paired", alternative = "two.sided")
  
  # Extract the sample size from 'pwrcalc'
  sampsizefinal <- pwrcalc2$n
  
  # Round up the final sample size 
  sampsizefinal <- ceiling(sampsizefinal)
  
  # Subtract the observed number of patients from the final sample size 
  sampsizeremain <- sampsizefinal - sampsizefrac
  
  # Make storage for the simulated values of the variance of the treatment effect 
  # and the variance of the random error for the remaining number of patients
  treatment_effect2 <- matrix(data = NA, nrow = 1, ncol = sampsizeremain)
  random_error2 <- matrix(data = NA, nrow = 1, ncol = sampsizeremain*n_cycles)
  
  # Make storage for the simulated value of the outcome (difference between 
  # treatment A and treatment B; d_ij) for the remaining number of patients
  d_ij2 <- matrix(data = NA, nrow = 1, ncol = sampsizeremain*n_cycles)
  
  # First, simulate values for \psi^2 and \sigma^2
  treatment_effect2 <- as.data.frame(rnorm(n = sampsizeremain, mean = avg_treatment, sd = sqrt(tvar_treatment)))
  random_error2 <- as.data.frame(rnorm(n = sampsizeremain*n_cycles, mean = 0, sd = sqrt(2*tvar_error)))
  
  # Duplicate the values for the treatment effect for each cycle 
  treatment_effect2 <- (sapply(treatment_effect2, rep, each = 3))
  
  # Simulate values for the outcome d_ij2 
  d_ij2 <- as.data.frame(treatment_effect2 + random_error2)
  colnames(d_ij2)[1] <- 'd_ij'
  
  # Combine the outcome of the first and the second half of the patients
  outcome <- rbind(d_ij, d_ij2)
  
  # Make a dataframe for the final total sample size
  dat2 <- data.frame(patient   = factor(sort(rep((1:sampsizefinal), n_cycles))),
                     cycle     = factor(rep(sort(rep(1:n_cycles)), sampsizefinal)),
                     index     = factor(rep(c(1:(n_cycles)), sampsizefinal)))
  
  # Combine the patient number, cycle number, index and outcome of the total sample
  dat2 <- cbind(dat2, outcome)
  
  # Rename the last column name of dat2
  colnames(dat2)[4] <- 'd_ij_full'
  
  # Make storage for the estimated values of the fixed intercept, \psi^2, \sigma^2
  # and the t-value of the estimates.
  estimfinal <- matrix(data = NA, nrow = 1, ncol = 4)
  
  # Estimate the mean treatment effect and store
  out2 <- lmer(formula = d_ij_full ~ 1 + (1 | patient), data = dat2)
  
  # Extract the fixed effects and the t-value
  estimfinal[,1] <- summary(out2)$coefficients[1,1]
  estimfinal[,2] <- summary(out2)$coefficients[1,4]
  
  # Extract the sd of the random intercept and the residual
  temp <- as.data.frame(VarCorr(out2))
  estimfinal[,3] <- temp[1,5]
  estimfinal[,4] <- temp[2,5]
  
  # Specify that sampsizefrac and sampsizefinal come in the first two columns of
  # the output matrix
  output[i,1] <- sampsizefrac
  output[i,2] <- sampsizefinal
  
  # Indicate all the significant results with a 1 and non-significant results with 
  # a 0
  output[i,3] <- ifelse(estimfinal[,2] < 1.96, 0, 1)
  }
  
  # Calculate the total power
  pwr <- sum(output[,3]/N)
  
  pwr <<- pwr 
  output <<- output
  estimfinal <<- estimfinal
}



reestim(0.5, 0.25, 0.5, 0.25, 0.5)
