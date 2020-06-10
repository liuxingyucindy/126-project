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


