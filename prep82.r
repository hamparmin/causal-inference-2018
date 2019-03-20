storage.vector <- NA

# Function that assigns treatment/control depending on 
# propensity scores (assignment probabilities)
experiment <- function(vector.of.probabilities = NULL) {
  k = 0
  for (i in 1:length(vector.of.probabilities)) {
    if(
      sample(x = c(1,0), size = 1, prob = c(vector.of.probabilities[i], 
                                            1 - vector.of.probabilities[i])) == 1) {
      storage.vector[k] <- i
      k = k + 1
    }
  }
  return(list(treated.units = storage.vector, 
              control.units = (1:(length(vector.of.probabilities)))[-storage.vector]))
}

#vector of probabilities
v_p=c(19/28,19/40,19/26,19/24,14/22,14/35,14/37,14/32,14/34)
exp_results=experiment(v_p)


# Incomes for the female-headed households without children are defined per the following code:
set.seed(123); nokids.income <- round(abs(exp(rnorm(1000, 5, 1))))
summary(nokids.income)
# Household sizes for the female-headed households with children are defined per this code:
set.seed(123); kids.hhsize <- round(sqrt(abs(rnorm(1000, 12, 100))) + .3)
summary(kids.hhsize)

hist(nokids.income)
hist(kids.hhsize)
