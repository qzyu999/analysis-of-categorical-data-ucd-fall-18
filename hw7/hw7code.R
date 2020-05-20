HD <- read.csv('HD.csv')
HD
# declare weight and heart as factors
HD$weight=as.factor(HD$weight)
HD$heart=as.factor(HD$heart)
# fit a log-linear model
fit=glm(Freq ~weight + heart,data=HD, family = "poisson")
fit
fit$fitted.values

n1p=55+30
n2p=65+50
np1=55+65
np2=30+50
n=sum(HD$Freq)
c(n1p*np1/n, n1p*np2/n, n2p*np1/n, n2p*np2/n)

fit
G=1.371
AIC=30.23
p_value=1-pchisq(G,1)
