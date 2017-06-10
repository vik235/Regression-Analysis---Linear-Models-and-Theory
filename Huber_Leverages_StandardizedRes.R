#clear any exisitng plots 
dev.off()
par(mfrow=c(2,2))
huber <- read.csv("C:/Users/vigupta/OneDrive/Learning/DataScience/Statistics Texas A&M University/608/SheatherBook/Data/huber.txt", header = T, sep = '')
n=6
#scatter plot 
plot(huber$x,huber$YBad, main = "Scatter plot of Ybad vs X")
#Run the model 
fit.ybad <- lm (huber$YBad~huber$x)
summary(fit.ybad)
abline(fit.ybad)

#scatter plot 
plot(huber$x,huber$YGood, main = "Scatter plot of YGood vs X")
#Run the model 
fit.ygood <- lm (huber$YGood~huber$x)
summary(fit.ygood)
abline(fit.ygood)

xbar= mean(huber$x)
SXX = sum((huber$x - xbar)^2)
hii.x = n^-1 + ((huber$x- xbar)^2)/SXX

plot(huber$x,hii.x, ylab = "leverage values, hii", main = "Leverage Points of X",xlab = "X values" )
#these endpoints set the boundaries for  leverage points
abline(h=4/n,lty=2)


#stdresiduals calculations of YGood 
RSS.ygood = sum(fit.ygood$residuals^2)
S.ygood= sqrt((RSS.ygood/(n-2)))
std.residuals.ygood = fit.ygood$residuals/(S.ygood*sqrt(1- hii.x))

#stdresiduals calculations of YBad 
RSS.ybad = sum(fit.ybad$residuals^2)
S.ybad= sqrt((RSS.ybad/(n-2)))
std.residuals.ybad = fit.ybad$residuals/(S.ybad*sqrt(1- hii.x))

#plot.Ygood std residuals vs x

plot(huber$x,std.residuals.ygood, main = "Scatter plot of std.residuals.ygood vs X")
#these endpoints set the boundaries for outliers
abline(h=2,lty=2)
abline(h=-2,lty=2)

plot(huber$x,sqrt(abs(std.residuals.ygood)), main = "Scatter plot of sqrt.std.residuals.ygood vs X")

#plot.Ybad std residuals vs x

plot(huber$x,std.residuals.ybad, main = "Scatter plot of std.residuals.ybad vs X")
abline(h=2,lty=2)
abline(h=-2,lty=2)

plot(huber$x,sqrt(abs(std.residuals.ybad)), main = "Scatter plot of sqrt.std.residuals.ybad vs X")
