fred <- read.csv("fred.csv")
#fred test
x<- fred$size
y <- fred$sweetness
z <- fred$price

#fred training -RMSE
fit_best <- lm(z~x+y, data=fred)
fit_second <-loess(z~x, span = 1, degree = 1)
summary(fit_best)
RMSE_best <- sqrt( mean( (z - predict(fit_best))^2 ) )
RMSE_best2 <- sqrt( mean( (z - predict(fit_second))^2 ) )
#fred test - RMSE
fred_test <- read.csv("apple.csv")
x1 <- fred_test$size
y1 <- fred_test$sweetness
z1 <- fred_test$price
RMSE_test_best <- sqrt(mean((z1-predict(fit_best))^2))
RMSE_test_loess <- sqrt(mean((z1-predict(fit_second))^2))
summary(fit_best)
#own data - without random element
year <- c(seq(1801,2000,1))
infected <- c(rnorm(200, 1000, 90))
virulence <- 50+((2000-year)/4)*infected/100
#virulence <- jitter(virulence, factor = 100)

virus_df <- data.frame(virulence,year, infected)
fit_virus <- lm(virulence~year+infected)
RMSE_virus <- sqrt( mean( (z - predict(fit_virus))^2 ) )
