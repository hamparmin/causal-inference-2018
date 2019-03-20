assignment_data <- read.csv("assignment1_dataset.csv", stringsAsFactors = FALSE)

#checkconvert data to numeric format where necessary
assignment_data$undisbursed.amount <- as.numeric(assignment_data$undisbursed.amount)

#convert dates to appropriate format (use origin 1899-12-30 as saved by Google Sheets)
assignment_data$approval.date <- as.Date(assignment_data$approval.date, origin="1899-12-30")
assignment_data$implementation.start.date <- as.Date(assignment_data$implementation.start.date, origin="1899-12-30")
assignment_data$original.completion.date <- as.Date(assignment_data$original.completion.date, origin="1899-12-30")
assignment_data$revised.completion.date <- as.Date(assignment_data$revised.completion.date, origin="1899-12-30")
str(assignment_data)

# exclude projects approved pre-1/1/1995 & post-1/1/2017
assignment_data <- subset(assignment_data, approval.date>as.Date("1995-01-01"))
assignment_data <- subset(assignment_data, approval.date<as.Date("2017-01-01"))

#Name-based algorithm for question selection.
#surname: HAMP first three letters in integers: 2,1,13 (I choose the first digit of 13)
set.seed(211)
my_questions_are_these <- sort(sample(c(1:10), 3, replace = FALSE))
my_questions_are_these

#question 3 - What variables are measured for each project?
library(dplyr)
assignment_data %>% 
  select_if(function(x) any(is.na(x))) %>% 
  summarise_each(funs(sum(is.na(.)))) -> columns_NA
columns_NA
#question 8 - Create a histogram or a probability density plot that shows the distribution of project budgets. Be sure to label the x axis and give your plot a title.
# in order to get a nice data visualisation, i used the natural log function to transform my data
project_budget <- log(assignment_data$project.budget)
h1 <- hist(project_budget,
           ylim = c(0, 1500),
           main="Project Budget Distribution",
           sub="Cost: 10^x",
           xlab="Project Budget",
           ylab="Frequency",
           col="lightblue")
#question 9 - Approximately what fraction of projects get assessed at project completion?
projects_completed <- subset(assignment_data,status=="FINANCIALLY COMPLETED")
projects_assessed <- subset(projects_completed, success.rating==1 | success.rating==0)
fraction <- nrow(projects_assessed)/nrow(projects_completed)
fraction
