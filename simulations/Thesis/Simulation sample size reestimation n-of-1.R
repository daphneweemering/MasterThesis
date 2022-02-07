# 'Interim sample size reestimation for adequately powered
# series of N-of-1 trials' 

library(lme4)
library(pwr)
library(lmerTest)

# Specify the values for \sigma^2 (= 'varerror), \psi^2 (= 'vartreatment') and 
# the fraction of the initial sample size on which reestimation will be based, fn
# (= 'fnt')
hvar_treatment <- c(0.5, 1, 2)
hvar_error <- c(0.25, 0.5, 1)
tvar_treatment <- c(0.5, 1, 2)
tvar_error <- c(0.25, 0.5, 1)
fn <- c(0.25, 0.5, 0.75)

# Create data frame including all possible combinations of \sigma^2, \psi^2 and 
# fn
x <- expand.grid(hvar_treatment, hvar_error, tvar_treatment, tvar_error, fn)

# Use apply to calculate the initial sample size for every combination of 
# hypothesized \sigma^2 and \psi^2 and the fraction of the sample size (fn)
initial <- apply(x, 1, reestim)
