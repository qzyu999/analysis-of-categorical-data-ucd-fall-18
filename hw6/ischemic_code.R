#### check if mean and variance are similar
library("xlsx")
ischemic <- read_excel('ischemic.xlsx')
ischemic

mean(ischemic$visits)
var(ischemic$visits)

fit.poisson <- glm(visits~., data = ischemic, family = poisson())
summary(fit.poisson)
 # 8 parameters
G=1043.6
df=779
G>qchisq(0.95,df)
1-pchisq(G, df)
head(ischemic)


par(mfrow=c(2,2))
plot(ischemic$cost, fit.poisson$fitted.values, main = 'Estimated Counts vs. Cost',
     xlab = 'Cost', ylab = 'Estimated Counts')
plot(ischemic$age, fit.poisson$fitted.values, main = 'Estimated Counts vs. Age',
     xlab = 'Age', ylab = 'Estimated Counts')
plot(ischemic$drugs, fit.poisson$fitted.values, main = 'Estimated Counts vs. Number of Drugs Prescribed',
     xlab = 'Number of Drugs Prescribed', ylab = 'Estimated Counts')
plot(ischemic$inter, fit.poisson$fitted.values, main = 'Estimated Counts vs. Interventions',
     xlab = 'Interventions', ylab = 'Estimated Counts')
dev.off()
par(mfrow=c(2,2))
plot(ischemic$comorbidities, fit.poisson$fitted.values, main = 'Estimated Counts vs. Number of other Diseases',
     xlab = 'Number of other Diseases', ylab = 'Estimated Counts')
plot(ischemic$duration, fit.poisson$fitted.values, main = 'Estimated Counts vs. Days of Duration',
     xlab = 'Days of Duration', ylab = 'Estimated Counts')
boxplot(fit.poisson$fitted.values, ischemic$gender, main = 'Estimated Counts vs. Gender',
        xlab = 'Gender', ylab = 'Estimated Counts', names = c('Other', 'Male'))
plot(ischemic$complications, fit.poisson$fitted.values, main = 'Estimated Counts vs. Number of other Complications',
     xlab = 'Number of other Complications', ylab = 'Estimated Counts')
dev.off()
#
par(mfrow=c(2,2))
plot(log(ischemic$cost + 3/8), fit.poisson$fitted.values, main = 'log(Estimated Counts + 3/8) vs. Cost',
     xlab = 'Cost', ylab = 'log(Estimated Counts + 3/8)')
plot(log(ischemic$age + 3/8), fit.poisson$fitted.values, main = 'log(Estimated Counts + 3/8) vs. Age',
     xlab = 'Age', ylab = 'log(Estimated Counts + 3/8)')
plot(log(ischemic$drugs + 3/8), fit.poisson$fitted.values, main = 'log(Estimated Counts + 3/8) vs. Number of Drugs Prescribed',
     xlab = 'Number of Drugs Prescribed', ylab = 'log(Estimated Counts + 3/8)')
plot(log(ischemic$inter + 3/8), fit.poisson$fitted.values, main = 'log(Estimated Counts + 3/8) vs. Interventions',
     xlab = 'Interventions', ylab = 'log(Estimated Counts + 3/8)')
dev.off()
par(mfrow=c(2,2))
plot(log(ischemic$complications + 3/8), fit.poisson$fitted.values, main = 'log(Estimated Counts + 3/8) vs. Number of other Complications',
     xlab = 'Number of other Complications', ylab = 'log(Estimated Counts + 3/8)')
plot(log(ischemic$comorbidities + 3/8), fit.poisson$fitted.values, main = 'log(Estimated Counts + 3/8) vs. Number of other Diseases',
     xlab = 'Number of other Diseases', ylab = 'log(Estimated Counts + 3/8)')
plot(log(ischemic$duration + 3/8), fit.poisson$fitted.values, main = 'log(Estimated Counts + 3/8) vs. Days of Duration',
     xlab = 'Days of Duration', ylab = 'log(Estimated Counts + 3/8)')
dev.off()
### sqrt
par(mfrow=c(2,2))
plot(sqrt(ischemic$cost), fit.poisson$fitted.values, main = 'sqrt(Estimated Counts) vs. Cost',
     xlab = 'Cost', ylab = 'sqrt(Estimated Counts)')
plot(sqrt(ischemic$age), fit.poisson$fitted.values, main = 'sqrt(Estimated Counts) vs. Age',
     xlab = 'Age', ylab = 'sqrt(Estimated Counts)')
plot(sqrt(ischemic$drugs), fit.poisson$fitted.values, main = 'sqrt(Estimated Counts) vs. Number of Drugs Prescribed',
     xlab = 'Number of Drugs Prescribed', ylab = 'sqrt(Estimated Counts)')
plot(sqrt(ischemic$inter), fit.poisson$fitted.values, main = 'sqrt(Estimated Counts) vs. Interventions',
     xlab = 'Interventions', ylab = 'sqrt(Estimated Counts)')
dev.off()
par(mfrow=c(2,2))
plot(sqrt(ischemic$complications), fit.poisson$fitted.values, main = 'sqrt(Estimated Counts) vs. Number of other Complications',
     xlab = 'Number of other Complications', ylab = 'sqrt(Estimated Counts)')
plot(sqrt(ischemic$comorbidities), fit.poisson$fitted.values, main = 'sqrt(Estimated Counts) vs. Number of other Diseases',
     xlab = 'Number of other Diseases', ylab = 'sqrt(Estimated Counts)')
plot(sqrt(ischemic$duration), fit.poisson$fitted.values, main = 'sqrt(Estimated Counts) vs. Days of Duration',
     xlab = 'Days of Duration', ylab = 'sqrt(Estimated Counts)')
dev.off()

###
plot(rstudent(fit.poisson), main = 'Studentized Residuals vs. Index',
     ylab = 'Studentized Residuals')
range(rstudent(fit.poisson))

fit.poisson$coefficients # coeffiients
summary(fit.poisson)$coefficients[,2] # standard error

for (i in 1:9){ # exp(beta0-beta_6)
  print(exp(fit.poisson$coefficients[i]))
}

# square root
ischemic2 = ischemic
options(warn=-1)
# resource: https://stackoverflow.com/questions/17319647/create-sequence-of-numbers-excluding-certain-numbers
for (i in setdiff(1:9,3)){
    ischemic2[i] = sqrt(ischemic2[i])
}
options(warn=0)
fit.poisson2 <- glm(visits~., data = ischemic2, family = poisson()) # warnings
summary(fit.poisson2)

ischemic3 = ischemic
options(warn=-1)
for (i in setdiff(1:9,c(3,5,6,7))){
  ischemic3[i] = log(ischemic3[i] + 3/8)
}
options(warn=0)
fit.poisson7 <- glm(visits~., data = ischemic3, family = poisson()) # warnings
summary(fit.poisson7)

# fit1 <- lm(visits ~ ., ischemic)
# fit2 <- lm(visits ~ 1, ischemic)
# stepAIC(fit1,direction="backward", trace = FALSE)
# stepAIC(fit2,direction="forward",scope=list(upper=fit1,lower=fit2), trace = FALSE)
# stepAIC(fit2,direction="both",scope=list(upper=fit1,lower=fit2),trace = FALSE)

step_poi <- stepAIC(fit.poisson, direction = 'backward', trace=FALSE)
step_poi$anova
summary(fit.poisson)
summary(step_poi)
test_stat=1044.7-1043.6
df=781-779
test_stat>qchisq(0.95,df)
1-pchisq(test_stat, df)
# drop 6,7

# AIC: 3268.1 new
# AIC: 3271 old

par(mfrow=c(2,2))
res.D = residuals(step_poi, type = 'deviance')
plot(res.D, main = 'Deviance Residuals vs. Index', ylab = 'Deviance Residuals')
range(res.D)
res.D.standard = rstudent(step_poi)
plot(res.D.standard, main = 'Standardized Deviance Residuals vs. Index',
     ylab = 'Standardized Deviance Residuals')
range(res.D.standard)
res.P = residuals(step_poi, type = 'pearson')
plot(res.P, main = 'Pearson Residuals vs. Index', ylab = 'Pearson Residuals')
range(res.P)
res.P.standard = rstandard(step_poi)
plot(res.D.standard, main = 'Standardized Pearson Residuals vs. Index',
     ylab = 'Standardized Pearson Residuals')
range(res.P.standard)
dev.off()

###
res.D_pos = subset(res.D, res.D > 0)
length(subset(res.D_pos, res.D_pos > 3.5)) / length(res.D_pos)

res.D.standard_pos = subset(res.D.standard, res.D.standard > 0)
length(subset(res.D.standard_pos, res.D.standard_pos > 3.5)) / length(res.D.standard_pos)

res.P_pos = subset(res.P, res.P > 0)
length(subset(res.P_pos, res.P_pos > 3.5)) / length(res.P_pos)

res.P.standard_pos = subset(res.P.standard, res.P.standard > 0)
length(subset(res.P.standard_pos, res.P.standard_pos > 3.5)) / length(res.P.standard_pos)

### cost
c((step_poi$coefficients[2]) - 1.96*(summary(step_poi)$coefficients[,2][2]), 
  (step_poi$coefficients[2]) + 1.96*(summary(step_poi)$coefficients[,2][2]))
### age
c((step_poi$coefficients[3]) - 1.96*(summary(step_poi)$coefficients[,2][3]), 
  (step_poi$coefficients[3]) + 1.96*(summary(step_poi)$coefficients[,2][3]))
### gender
c((step_poi$coefficients[4]) - 1.96*(summary(step_poi)$coefficients[,2][4]), 
  (step_poi$coefficients[4]) + 1.96*(summary(step_poi)$coefficients[,2][4]))
### inter
c((step_poi$coefficients[5]) - 1.96*(summary(step_poi)$coefficients[,2][5]), 
  (step_poi$coefficients[5]) + 1.96*(summary(step_poi)$coefficients[,2][5]))
### drugs
c((step_poi$coefficients[6]) - 1.96*(summary(step_poi)$coefficients[,2][6]), 
  (step_poi$coefficients[6]) + 1.96*(summary(step_poi)$coefficients[,2][6]))
### duration
c((step_poi$coefficients[7]) - 1.96*(summary(step_poi)$coefficients[,2][7]), 
  (step_poi$coefficients[7]) + 1.96*(summary(step_poi)$coefficients[,2][7]))
