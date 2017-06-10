dev.off()
par(mfrow=c(2,2))
#par(mfrow=c(1,1))
responsetran <- read.csv('C:/Users/vigupta/OneDrive/Learning/DataScience/Statistics Texas A&M University/608/SheatherBook/Data/responsetransformation.txt', header = T, sep = '')

plot(responsetran$x, responsetran$y)

fit.1 <- lm(responsetran$y~responsetran$x)
lambda <- c(-1,-1/3,-1/4,-1/2,0,1/2,1/4,1/3,1)


library(alr3)

inverseResponsePlot(fit.1,lambda)
#we get lambda=1/3 as a good transformation. 

responsetran$yt=responsetran$y^.33

fit.2 <-lm(responsetran$yt~responsetran$x)

#Asses validity of the model
plot(fit.2)
#Looks very good