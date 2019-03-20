#Then, using the “lalonde” data set in the “Matching” package,
#experiment with logistic regression by estimating propensity
#scores (the probability of being in the group assigned to treatment).
#After estimating propensity scores, try to estimate the
#linear predictor (transforming the propensity scores using the
#formula given in the text).

library(Matching)
data("lalonde")
names(lalonde)

cor(lalonde)
glm.fit1 = glm(treat~re74+re78+u74+u75, data=lalonde, family = binomial())
summary(glm.fit1)

glm.fit2 = glm(treat~re78, data=lalonde, family=binomial())
summary(glm.fit2)
summary(glm.fit2)$coef

glm.pred=rep(0,1250)
glm.pred[glm.fit2.probs >.5]=1
