library(readxl)
flu <- read_excel('flu.xlsx')
flu

# 1
# train logistic regression
flu.fit = glm(Shot~Age, family=binomial(), data=flu)
flu.fit

# MLE estimates for beta0 and beta1
flu.fit$coefficients[1] # beta0
flu.fit$coefficients[2] # beta1

# plot
plot(flu$Age, flu.fit$fitted.values ,xlab='Age', ylab='estimated probability')

# estimate 55 y.o.
test.55=data.frame(55)
colnames(test.55)='Age'
prob=predict(flu.fit,test.55,type="response")
result=predict(flu.fit,test.55,se.fit = TRUE)
result

confidence_interval=c(result$fit-1.96*result$se.fit,result$fit+1.96*result$se.fit)
link_function=function(x){
  return(exp(x)/(1+exp(x)))
}
c(link_function(confidence_interval[1]),link_function(confidence_interval[2]))

# interpret exp(beta1hat)
exp(flu.fit$coefficients[2])

# the odds of getting the flu at age X + 1 is about 1.114869 times the odds of 
# getting the flu at age X. 

# 2a
# Obtain an estimate of the age at which the probability of getting the flu is 0.5.
logit=function(x){
  return(log(x/(1-x)))
}
age_0.5=(logit(0.5)-flu.fit$coefficients[1])/flu.fit$coefficients[2]

summary(flu.fit)

# 2c
c(0.10874-1.96*0.02737,0.10874+1.96*0.02737)

###

# 3
bottle <- read_excel('BottleReturn.xlsx')
bottle
colnames(bottle) <- c('Number_Returned', 'Number_sold', 'Deposit_level')

bottle.fit = glm(cbind(Number_Returned,Number_sold)~Deposit_level, data=bottle, family=binomial())
plot(bottle$Deposit_level, logit(bottle$Number_Returned/(bottle$Number_Returned+bottle$Number_sold)),
     ylab='logit of the sample proportions', xlab='Deposit level', main='question(a)')

# 3b
bottle.fit = glm(cbind(Number_Returned,Number_sold)~Deposit_level,data=bottle,family=binomial())
bottle.fit$coefficients[1]
bottle.fit$coefficients[2]

# 3c
plot(bottle$Deposit_level, bottle$Number_Returned/(bottle$Number_Returned+bottle$Number_sold),
     xlab='Deposit Level',ylab='Proportion', main='???')
points(bottle$Deposit_level, bottle.fit$fitted.values,col='red')
legend("topleft", c("Sample Proportion", "Fitted Proportion"), pch = 1, col = c("black", "red"))

# 3d
res.D=residuals(bottle.fit,type='deviance')
res.P=residuals(bottle.fit,type='pearson')
res.P.standard=rstandard(bottle.fit)
res.D.standard=rstudent(bottle.fit)

###
# 4a
test.15=data.frame(15)
colnames(test.15)='Deposit_level'
predict(bottle.fit,test.15,type='response')

# 4b
beetles_0.75=(logit(0.75)-bottle.fit$coefficients[1])/bottle.fit$coefficients[2]
beetles_0.75

# 4c
summary(bottle.fit)
c(0.061544-1.96*0.007674,0.061544+1.96*0.007674)

# 4d
anova(bottle.fit,test='Chi')
bottle.fit$null.deviance-bottle.fit$deviance > qchisq(0.95,1)

bottle.fit$deviance>qchisq(0.95,length(bottle.fit$fitted.values)-2)

1-pchisq(bottle.fit$deviance,length(bottle.fit$fitted.values)-2)

# 5b
expected_counts_returned = (bottle$Number_Returned+bottle$Number_sold)*bottle.fit$fitted.values
expected_counts_returned

expected_counts_sold = (bottle$Number_Returned+bottle$Number_sold)*(1-bottle.fit$fitted.values)
expected_counts_sold

sum((bottle$Number_Returned-expected_counts_returned)^2/expected_counts_returned)+sum((bottle$Number_sold-expected_counts_sold)^2/expected_counts_sold)

cbind(bottle$Number_Returned,expected_counts_returned,
      bottle$Number_sold, expected_counts_sold)

X2=sum((returned-expected_counts_killed2)^2/expected_counts_killed2)+sum((not_killed-expected_counts_not_killed2)^2/expected_counts_not_killed2)
1-pchisq(X2,4)