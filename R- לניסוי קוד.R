library(dplyr)
library(BayesFactor)
library(stringr)
library(car)
library(ggplot2)
library(Rcpp)
update.packages('Rccp')
setwd("/Users/magic/OneDrive/שולחן העבודה/מתודולגיה ניסויית")
data<-read.csv("yellow_red_data.csv")

#Finding the mean and sd of the age
mean_age <- mean(data$age)
mean_age
sd_age <- sd(data$age)
sd_age
t_critical <- qt(p=.05/2, df=25, lower.tail=FALSE)
t_critical

#Checking the gender
sum(str_count(data$gender, "M"))
sum(str_count(data$gender, "F"))
sum(str_count(data$gender, "O"))

#Success checking for each question
S.Q1 <- sum(str_count(data$Q1.1, "1"))
S.Q2 <- sum(str_count(data$Q2.2, "1"))
S.Q3 <- sum(str_count(data$Q3.2, "1"))
S.Q4 <- sum(str_count(data$Q4.2, "1"))
S.Q5 <- sum(str_count(data$Q5.1, "1"))
S.Q6 <- sum(str_count(data$Q6.2, "1"))
S.Q7 <- sum(str_count(data$Q7.2, "1"))
S.Q8 <- sum(str_count(data$Q8.1, "1"))
S.Q9 <- sum(str_count(data$Q9.1, "1"))
S.Q10 <- sum(str_count(data$Q10.2, "1"))
S.Q11 <- sum(str_count(data$Q11.2, "1"))
S.Q12 <- sum(str_count(data$Q12.2, "1"))
S.Q13 <- sum(str_count(data$Q13.1, "1"))
S.Q14 <- sum(str_count(data$Q14.1, "1"))
S.Q15 <- sum(str_count(data$Q15.1, "1"))
S.Q16 <- sum(str_count(data$Q16.2, "1"))
S.Q17 <- sum(str_count(data$Q17.2, "1"))
S.Q18 <- sum(str_count(data$Q18.1, "1"))
S.Q19 <- sum(str_count(data$Q19.2, "1"))
S.Q20 <- sum(str_count(data$Q20.1, "1"))

#General mean and SD
mean_success <- (S.Q1+S.Q2+S.Q3+S.Q4+S.Q5+S.Q6+S.Q7+S.Q8+S.Q9+S.Q10+S.Q11+S.Q12+S.Q13+S.Q14+S.Q15+S.Q16+S.Q17+S.Q18+S.Q19+S.Q20)/26
mean_success
success <- (data$success_Y+ data$success_R)
sd_success <- sd(success)
sd_success

#Description of the distribution
mean_R <- mean(data$success_R)
mean_R
sd_R <- sd(data$success_R)
sd_R
mean_Y <- mean(data$success_Y)
mean_Y
sd_Y <- sd(data$success_Y)
sd_Y
dY_R <- data$success_Y- data$success_R
dY_R

#H0: mean_Y= mean_R
#H1: mean_Y isn't equal to mean_R
#Assumptions for t test- difference for dependents means- normality
qqnorm(dY_R)
normality <- shapiro.test(dY_R)
normality

meandY_R <- mean(dY_R) #distribution's mean
meandY_R
sddY_R <- sd(dY_R) #distribution's sd
sddY_R

#One sample t- test for paired tests
t.testY_R <- t.test(dY_R, alternative = "two.sided")
t.testY_R

#BF
bf <- ttestBF(x =data$yellow, y = data$red , alternative="two.sided", paired = TRUE )
bf

#D Cohen
d_cohen <- meandY_R/ sddY_R
d_cohen

