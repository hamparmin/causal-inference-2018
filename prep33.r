library(arm)
library(Matching)
data(lalonde)

lm1 <- lm(lalonde$re78 ~ lalonde$age)

lm1$coef

# (Intercept) lalonde$age 
# 3946.18432    53.39136 

set.seed(123)

sim_results <- sim(lm1, n.sims = 20)

set.seed(232)

# 20 sims is too few to get reliable results
sim_results2 <- sim(lm1, n.sims = 10000)
mean(sim_results2@coef[,1])
mean(sim_results2@coef[,2])


