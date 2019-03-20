#how does sweetness and size of an apple affect its price?
fred_df <- read.csv("fred.csv")
x <- sort(fred$size)
z <- sort(fred$sweetness)
y <- sort(fred$price)

p1 <- plot(x,y, main="x=size, y=price");
fit1.0 <- loess(y~x, span = 1, degree = 1) # 
fit.5 <- loess(y~x, span = 0.5, degree = 1) # 
fit.2 <- loess(y~x, span = 0.05, degree = 1) # 
fit3 <- lm(y ~ x + z, data=fred_df)
lines(x, predict(fit1.0, x), lwd = 1, col = "red") # add lines to plot
lines(x, predict(fit.5, x), lwd = 1, col = "blue") # add lines to plot
lines(x, predict(fit.2, x), lwd = 1, col = "green")# add lines to plot

rmse1.0 <- sqrt( mean( (y - predict(fit1.0))^2 ) ) #calculate RMSE
rmse.5 <- sqrt( mean( (y - predict(fit.5))^2 ) ) # calculate RMSE
rmse.2 <- sqrt( mean( (y - predict(fit.2))^2 ) ) # calculate RMSE
rmse.4 <- sqrt( mean( (y - predict(fit3))^2 ) ) # calculate RMSE

p2 <- plot(x,y, main="x=size, y=price");
fit1.0 <- loess(y~x, span = 1, degree = 1) # 
fit.5 <- loess(y~x, span = 0.5, degree = 1) # 
fit.2 <- loess(y~x, span = 0.05, degree = 1) # 
lines(x, predict(fit1.0, x), lwd = 1, col = "red") # add lines to plot
lines(x, predict(fit.5, x), lwd = 1, col = "blue") # add lines to plot
lines(x, predict(fit.2, x), lwd = 1, col = "green")# add lines to plot
