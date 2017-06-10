par(mfrow=c(2,2))
production <- read.csv('C:/Users/vigupta/OneDrive/Learning/DataScience/Statistics Texas A&M University/608/SheatherBook/Data/production.txt', header = T,sep = "")
n=20
#Always draw the scatter plot to see strength. form, direction of relationship

plot(production$RunSize,production$RunTime, main = "Scatter Plot of the data")

#Run the model 
fit.1 <- lm (RunTime ~ RunSize, data=production)
summary(fit.1)
abline(fit.1)

#How to calculate residuals from the model (although the model spits it out)
production$RunTime-fit.1$fitted.values

plot(fit.1$fitted.values, fit.1$residuals, col=2, main = "Scatter Plot fitted vs residuals" )

#this needs modfification. Std residuals are not calculated like this.
plot(fit.1$fitted.values, sqrt(fit.1$residuals), col=3, main = "Scatter Plot fitted vs std residuals", pch="*" )

#this needs modfification. 
plot(production$RunSize, cooks.distance(fit.1), col=4, pch="-",main = "Scatter Plot cooks dist vs RunSize")


#Some hand calculations of se's 
SXX= (sum((production$RunSize-mean(production$RunSize))^2))
RSS = sum(fit.1$residuals^2)
S= sqrt((RSS/(n-2)))

se.beta1= S/sqrt(SXX)
se.beta0= S*sqrt(n^-1 + (mean(production$RunSize)^2)/SXX)

#confidence intervals of beta0

fit.1$coefficients[1] +qt(.05/2, n-2)*se.beta0
fit.1$coefficients[1] -qt(.05/2, n-2)*se.beta0
fit.1$model

#how to predict 
newdata <-data.frame('RunSize'=c(50,100))

#confidence below spits out the fitted value yhats for newdata, ITS CONFIDENCE INTERVAL not actual predicted value
predict(fit.1, newdata,interval = "confidence")
#Actaul preiction always higher than CI. Looks Sheather pg 26.

predict(fit.1, newdata,interval = "prediction")


