dev.off()
par(mfrow=c(2,2))
cleaning <- read.csv("C:/Users/vigupta/OneDrive/Learning/DataScience/Statistics Texas A&M University/608/SheatherBook/Data/cleaning.txt", header = T, sep = '')
n=53
plot(cleaning$Crews,cleaning$Rooms)

fit.1 <- lm(cleaning$Rooms~cleaning$Crews)
abline(fit.1)
plot(fit.1)
summary(fit.1)
xbar=mean(cleaning$Crews)

SXX= sum((cleaning$Crews- xbar)^2)
hii.x = n^-1 + ((cleaning$Crews- xbar)^2)/SXX
RSS = sum(fit.1$residuals^2)
S.cleaning= sqrt((RSS/(n-2)))
std.residuals.cleaning = fit.1$residuals/(S.cleaning*sqrt(1- hii.x))

#plotting leverages 
plot(cleaning$Crews, hii.x)
abline(h=4/(n),lty=2)

#see the pattern here in residuals. 
plot(cleaning$Crews, std.residuals.cleaning)
plot(cleaning$Crews,sqrt(abs(std.residuals.cleaning)))
abline(lm(sqrt(abs(std.residuals.cleaning))~cleaning$Crews))

#look for any infuencing points. Note that there should be gap too here. 
cd =cooks.distance(fit.1)

plot(cleaning$Crews,cd)
abline(h=4/(n-2),lty=2)

#Transformation. Y can be modeeled by Poisson and if so then sqrt on Y works. Since X and Y are in same units we will transform X as well. 
cleaning$roomSq = sqrt(cleaning$Rooms)
cleaning$crewSq = sqrt(cleaning$Crews)
y = cleaning$roomSq
x=cleaning$crewSq
fit.2 <- lm(y~x)

#regression diagnostics proves valid model to work on..
plot(fit.2)

newdata= data.frame('x'=c(2,4))
#remeber to back transform the data dn and variance as E[g(Y)] != g[E(Y)]
predict(fit.2, newdata , interval = "prediction")

#use aov(fit.2) to get sigmaSquared. 

(2.789926^2 + 0.5939872^2) 
(5.21664^2+ 0.5939872^2) 
  


