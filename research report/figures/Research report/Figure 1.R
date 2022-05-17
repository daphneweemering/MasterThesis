par(mar = c(4.1, 4.5, 1, 14))

graphics::boxplot(dat2, outline = F, axes = F, ann = F)
abline(a = NULL, b = NULL, h = 22.93, col = 'red', lwd = 2)
points(1, mean(set1), pch = 16)
points(2, mean(set2), pch = 16)
points(3, mean(set3), pch = 16)
points(4, mean(set4), pch = 16)
points(5, mean(set5), pch = 16)
points(6, mean(set6), pch = 16)
points(7, mean(set7), pch = 16)
points(8, mean(set8), pch = 16)
points(9, mean(set9), pch = 16)
box()

# Make y-axis
axis(2, at = c(10, 20, 30, 40, 50, 60), labels = c(10, 20, 30, 40, 50, 60))

# Make x-axis 
axis(1, at = 1:9, labels = c(1, 2, 3, 4, 5, 6, 7, 8, 9))

title(xlab = expression(paste('Scenarios under varying values of ', psi[h]^2, ', ', sigma[h]^2, ' and f')))
title(ylab = 'Final sample size')

legend('topright', inset = c(-0.55,0), xpd = T, title = 'Values under different scenarios', 
       legend = c(expression(paste('1: ', psi[h]^2, ' = 0.5,', sigma[h]^2, ' = 0.25', ', f = 0.25')),
                  expression(paste('2: ',psi[h]^2, ' = 0.5,', sigma[h]^2, ' = 0.25', ', f = 0.5')),
                  expression(paste('3: ', psi[h]^2, ' = 0.5,', sigma[h]^2, ' = 0.25', ', f = 0.75')),
                  expression(paste('4: ', psi[h]^2, ' = 1,', sigma[h]^2, ' = 0.5', ', f = 0.25')),
                  expression(paste('5: ', psi[h]^2, ' = 1,', sigma[h]^2, ' = 0.5', ', f = 0.5')),
                  expression(paste('6: ',psi[h]^2, ' = 1,', sigma[h]^2, ' = 0.5', ', f = 0.75')),
                  expression(paste('7: ', psi[h]^2, ' = 2,', sigma[h]^2, ' = 1', ', f = 0.25')),
                  expression(paste('8: ', psi[h]^2, ' = 2,', sigma[h]^2, ' = 1', ', f = 0.5')),
                  expression(paste('9: ', psi[h]^2, ' = 2,', sigma[h]^2, ' = 1', ', f = 0.75'))))
                                                



       