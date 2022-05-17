
set.seed(3239480)


############# \psi = 0.5, \sigma = 0.25 #################

# Test: \psi^2 = 0.5, \sigma^2 = 0.25, fn = 0.25
initsampsize(var_treatment = 0.5, var_error = 0.25, fn = 0.25) # 2
before_reestim(n_patients = 2, true_var_treatment = 2, true_var_error = 1, 
               N = 1000) # 0.2805
mean(estim[,1]) # 0.9768
set1 <- finalsampsize(estim = estim, N = 1000)
mean(set1) # 24.317 (25)
round_final_n_size1 <- matrix(NA, 1000, 1)
for (i in 1:1000){
  round_final_n_size1[i,] <- ceiling(set1[i,])
}
mean(round_final_n_size1) # 24.91
var(round_final_n_size1) # 861.15
sd(round_final_n_size1) # 29.35


# Test: \psi^2 = 0.5, \sigma^2 = 0.25, fn = 0.5
initsampsize(var_treatment = 0.5, var_error = 0.25, fn = 0.5) # 4
before_reestim(n_patients = 4, true_var_treatment = 2, true_var_error = 1, 
               N = 1000) # 0.3065
mean(estim[,1]) # 0.9817
set2 <- finalsampsize(estim = estim, N = 1000)
mean(set2) # 23.748 (24)
round_final_n_size2 <- matrix(NA, 1000, 1)
for (i in 1:1000){
  round_final_n_size2[i,] <- ceiling(set2[i,])
}
mean(round_final_n_size2) # 23.93
var(round_final_n_size2) # 296.07
sd(round_final_n_size2) # 17.21


# Test: \psi^2 = 0.5, \sigma^2 = 0.25, fn = 0.75
initsampsize(var_treatment = 0.5, var_error = 0.25, fn = 0.75) # 6
before_reestim(n_patients = 6, true_var_treatment = 2, true_var_error = 1, 
               N = 1000) # 0.3725
mean(estim[,1]) # 0.9768
set3 <- finalsampsize(estim = estim, N = 1000) 
mean(set3) # 22.972
round_final_n_size3 <- matrix(NA, 1000, 1)
for (i in 1:1000){
  round_final_n_size3[i,] <- ceiling(set3[i,])
}
mean(round_final_n_size3) # 23.75
var(round_final_n_size3) # 183.32
sd(round_final_n_size3) # 13.54



############# \psi = 1, \sigma = 0.5 #################
# Test: \psi^2 = 1, \sigma^2 = 0.5, fn = 0.25
initsampsize(var_treatment = 1, var_error = 0.5, fn = 0.25) # 4
before_reestim(n_patients = 4, true_var_treatment = 2, true_var_error = 1, 
               N = 1000) # 0.3065
mean(estim[,1]) # 0.9817
set4 <- finalsampsize(estim = estim, N = 1000)
mean(set4) # 23.748 (24)
round_final_n_size4 <- matrix(NA, 1000, 1)
for (i in 1:1000){
  round_final_n_size4[i,] <- ceiling(set4[i,])
}
mean(round_final_n_size4) # 23.93
var(round_final_n_size4) # 296.07
sd(round_final_n_size4) # 17.21


# Test: \psi^2 = 1, \sigma^2 = 0.5, fn = 0.5
initsampsize(var_treatment = 1, var_error = 0.5, fn = 0.5) # 7
before_reestim(n_patients = 7, true_var_treatment = 2, true_var_error = 1, 
               N = 1000) # 0.3905
mean(estim[,1]) # 0.9778
set5 <- finalsampsize(estim = estim, N = 1000) 
mean(set5) # 23.037 (24)
round_final_n_size5 <- matrix(NA, 1000, 1)
for (i in 1:1000){
  round_final_n_size5[i,] <- ceiling(set5[i,])
}
mean(round_final_n_size5) # 23.42
var(round_final_n_size5) # 138.64
sd(round_final_n_size5) # 11.77


# Test: \psi^2 = 1, \sigma^2 = 0.5, fn = 0.75
initsampsize(var_treatment = 1, var_error = 0.5, fn = 0.75) # 10
before_reestim(n_patients = 10, true_var_treatment = 2, true_var_error = 1, 
               N = 1000) # 0.490
mean(estim[,1]) # 0.9641
set6 <- finalsampsize(estim = estim, N = 1000)
mean(set6) # 23.013 (24)
round_final_n_size6 <- matrix(NA, 1000, 1)
for (i in 1:1000){
  round_final_n_size6[i,] <- ceiling(set6[i,])
}
mean(round_final_n_size6) # 23.41
var(round_final_n_size6) # 96.45
sd(round_final_n_size6) # 9.82



############# \psi = 2, \sigma = 1 #################
# Test: \psi^2 = 2, \sigma^2 = 1, fn = 0.25
initsampsize(var_treatment = 2, var_error = 1, fn = 0.25) # 6
before_reestim(n_patients = 6, true_var_treatment = 2, true_var_error = 1, 
               N = 1000) #  0.3725
mean(estim[,1]) # 0.9768
set7 <- finalsampsize(estim = estim, N = 1000)
mean(set7) # 22.972 (23)
round_final_n_size7 <- matrix(NA, 1000, 1)
for (i in 1:1000){
  round_final_n_size7[i,] <- ceiling(set7[i,])
}
mean(round_final_n_size7) # 23.75 
var(round_final_n_size7) # 183.32
sd(round_final_n_size7) # 13.54


# Test: \psi^2 = 2, \sigma^2 = 1, fn = 0.5
initsampsize(var_treatment = 2, var_error = 1, fn = 0.5) # 12
before_reestim(n_patients = 12, true_var_treatment = 2, true_var_error = 1, 
               N = 1000) # 0.550
mean(estim[,1]) # 0.9769
set8 <- finalsampsize(estim = estim, N = 1000)
mean(set8) # 23.301 (24)
round_final_n_size8 <- matrix(NA, 1000, 1)
for (i in 1:1000){
  round_final_n_size8[i,] <- ceiling(set8[i,])
}
mean(round_final_n_size8) # 23.77
var(round_final_n_size8) # 83.62
sd(round_final_n_size8) # 9.14


# Test: \psi^2 = 2, \sigma^2 = 1, fn = 0.75
initsampsize(var_treatment = 2, var_error = 1, fn = 0.75) # 18
before_reestim(n_patients = 18, true_var_treatment = 2, true_var_error = 1, 
               N = 1000) # 0.7205 
mean(estim[,1]) # 0.9907
set9 <- finalsampsize(estim = estim, N = 1000)
mean(set9) # 23.119 (24)
round_final_n_size9 <- matrix(NA, 1000, 1)
for (i in 1:1000){
  round_final_n_size9[i,] <- ceiling(set9[i,])
}
mean(round_final_n_size9) # 23.92
var(round_final_n_size9) # 53.02
sd(round_final_n_size9) # 7.28

# Combined the sample sizes obtained under each scenario for \psi_t^2 =2 and 
# \sigma_t^2 = 1
dat2 <- cbind(set1, set2, set3, set4, set5, set6, set7, set8, set9)









