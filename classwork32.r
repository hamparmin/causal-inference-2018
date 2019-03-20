library(MASS)
data("Pima.tr")
Pima.tr$type <- as.numeric(Pima.tr$type) - 1
fit1 <- glm(type~npreg+glu+bp+skin+bmi+ped+age, data=Pima.tr, family = binomial)
summary(fit1)
prob_fit1 <- predict(fit1, type = "response")
prob_fit1[1:10]

predicted_ys <- rep(0, length(prob_fit1))
predicted_ys[prob_fit1 > 0.5] = 1
table(predicted_ys, Pima.tr$type)
 
fit2 <- glm(type~glu+ped+bmi+age+npreg, data=Pima.tr, family=binomial())
prob_fit2 <- predict(fit2, type="response")

predicted_ys2 <- rep(0, length(prob_fit2))
predicted_ys2[prob_fit2 > 0.5] = 1
table(predicted_ys, Pima.tr$type)
table(predicted_ys2, Pima.tr$type)

data(Pima.te)
Pima.te$type <- as.numeric(Pima.te$type) - 1
prob_test <- predict.glm(fit2, type = "response", newdata=Pima.te)
length(prob_test)

predicted_test <- rep(0, length(prob_test))
predicted_test[prob_test > 0.5] = 1
table(predicted_ys2, Pima.tr$type)
table(predicted_test, Pima.te$type)

