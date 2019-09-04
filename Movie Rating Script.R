#Andrew Carroll
#Project 4
#May 14, 2019

library(summarytools)
library(stargazer)
library(car)

#Import Data
movie <- read.csv(file.choose())

#Describe variable for easier recoding
descr(movie$budget)
descr(movie$duration)
descr(movie$cast_total_facebook_likes)
descr(movie$gross)

#change to remove outliers
movies <- subset(movie, budget < 400000000 & country == 'USA' & 
                   duration >= 80 & duration <= 240 & 
                   cast_total_facebook_likes >= 20000 & cast_total_facebook_likes <= 100000 & 
                   gross >= 100000 & gross <= 600000000)

#Vieing varibale to see if it was imported correctly
View(movies)
str(movies)
descr(movie)
descr(movies)

#Putting movies variables into their own object to put into the model
score <- movies$imdb_score
budget <- movies$budget / 1000000 #Scale by $1,000,000
castLikes <- movies$cast_total_facebook_likes / 10000 #Scale by 10,000
gross <- movies$gross / 10000000 #Scale by $10,000,000
duration <- movies$duration / 10 #Scale by 10

#Check the Descriptive statistics of each variable
descr(score)
descr(budget)
descr(gross)
descr(castLikes)
descr(duration)

#Make OLS model into own object and check
model <- lm(score ~ gross + duration + castLikes + budget)
model

#Check OLS asumptions
plot(model)
vif(model)

#Summary Statistics of OLS model
summary(model)
stargazer(model, type="html",
          title = "Regression Results",
          dep.var.labels = "Average Movie Score",
          covariate.labels = c("Gross", "Duration", "Cast Likes", "Budget"),
          out="PR4_Carroll_Andrew.html")





