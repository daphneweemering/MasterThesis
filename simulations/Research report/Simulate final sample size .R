# 'Interim sample size reestimation for adequately powered
# series of N-of-1 trials' 

library(lme4)
library(pwr)
library(lmerTest)

# 1. ---------------------------------------------------------------------------
################################################################################
### Calculate sample size under different hypothesized values for \psi^2 and 
### \sigma^2 ###################################################################

# Specify the values for \sigma^2 (= 'varerror), \psi^2 (= 'vartreatment') and 
# the fraction of the initial sample size on which reestimation will be based, fn
# (= 'fnt')
varerror <- c(0.25, 0.5, 1)
vartreatment <- c(0.5, 1, 2)
fnt <- c(0.25, 0.5, 0.75)

# Create data frame including all possible combinations of \sigma^2, \psi^2 and 
# fn
x <- expand.grid(varerror, vartreatment, fnt)

# Use apply to calculate the initial sample size for every combination of 
# hypothesized \sigma^2 and \psi^2 and the fraction of the sample size (fn)
initial <- apply(x, 1, initsampsize)

# Place the initial sample sizes and the fractions of the initial sample sizes 
# in a matrix
numberofpatients <- matrix(unlist(initial), nrow = 2, ncol = 27)

# Seperate the number of patients in three separate data frames: one where fn is 
# 0.25, one where fn is 0.5 and one where fn = 0.75 (for the sake of interpretability)
fn25 <- t(numberofpatients[1:2,1:9])
fn50 <- t(numberofpatients[1:2,10:18])
fn75 <- t(numberofpatients[1:2,19:27])


# 2. ---------------------------------------------------------------------------
################################################################################
### Estimate the parameters and calculate the final sample size based on these 
### interim parameter estimates ################################################

# Specify the true values for \sigma^2 and \psi^2 based on which the data is
# simulated
truevarerror <- c(0.25, 0.5, 1)
truevartreatment <- c(0.5, 1, 2)

# Create three data frames including all possible combinations of the true values 
# for \sigma^2 and \psi^2 and the sample size under hypothesized \sigma^2 and 
# \psi^2. The first data frame is for when the fraction is 0.25, the second for 
# when fn = 0.5 and the third for when fn = 0.75
truefn25 <- expand.grid(truevarerror, truevartreatment, fn25[,2])
truefn50 <- expand.grid(truevarerror, truevartreatment, fn50[,2])
truefn75 <- expand.grid(truevarerror, truevartreatment, fn75[,2])

# Use apply to calculate the final sample size for all the different scenerios 
# (i.e., under different hypothesized and true \sigma^2 and \psi^2) and under 
# different fn (0.25, 0.5, and 0.75 respectively)
final1 <- apply(truefn25, 1, finsampsize)
final2 <- apply(truefn50, 1, finsampsize)
final3 <- apply(truefn75, 1, finsampsize)







