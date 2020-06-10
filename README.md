# 126-project
#Variable selection I
#forward selection with aic
mod.0 <- lm(G3~1, data=d1)
mod.full <- lm(G3 ~., data=d1) 
step(mod.0, scope=list(lower=mod.0, upper=mod.full), direction = "forward", trace=0)
#result: G3 ~ G2 + famrel + absences + G1 + age + activities + Walc + romantic + school

#forward selection with bic
n <- length(d1$G3)
step(mod.0,scope = list(lower=mod.0,upper=mod.full), direction = "forward", k=log(n),trace=0)
#result: G3 ~ G2 + famrel + absences + G1 + age

#backward selection with aic
step(mod.full, scope=list(lower=mod.0, upper=mod.full), direction = "backward", trace=0)
#result: G3 ~ school + age + activities + romantic + famrel + Walc + absences + G1 + G2

#backward selection with bic
step(mod.full, scope=list(lower=mod.0, upper=mod.full), direction = "backward", k=log(n),trace=0)
#result: G3 ~ age + famrel + absences + G1 + G2

#stepwise selection
step(mod.0,scope = list(lower=mod.0,upper=mod.full),trace=0)
#result: G3 ~ G2 + famrel + absences + G1 + age + activities + Walc + romantic + school



#scattor plot
pairs(~G3 + G2 + famrel + absences + G1 + age + activities + Walc + romantic + school)
#There might be linear relationship between G3 and G1, G2 and G1, and G2 and G3.


#Variable selection II
library(leaps)
mod.reg <- regsubsets(cbind(G2, famrel, absences, G1, age, activities, Walc, romantic, school),G3, data=d1)
summary.reg<- summary(mod.reg)
summary.reg$which

#by plots
par(mfrow = c(2, 2))
plot(summary.reg$rsq, xlab = "Number of Variables", ylab = "RSq", type = "b")

plot(summary.reg$adjr2, xlab = "Number of Variables", ylab = "Adjusted RSq", type = "b")
best_adj_r2 = which.max(summary.reg$adjr2)
points(best_adj_r2, summary.reg$adjr2[best_adj_r2],
       col = "red",cex = 2, pch = 20)

plot(summary.reg$cp, xlab = "Number of Variables", ylab = "Cp", type = 'b')
best_cp = which.min(summary.reg$cp[-c(length(summary.reg$cp))])
points(best_cp, summary.reg$cp[best_cp],
       col = "red", cex = 2, pch = 20)

plot(summary.reg$bic, xlab = "Number of Variables", ylab = "BIC", type = 'b')
best_bic = which.min(summary.reg$bic)
points(best_bic, summary.reg$bic[best_bic],
       col = "red", cex = 2, pch = 20)


