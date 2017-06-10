#clear any exisitng plots 
dev.off()
par(mfrow=c(2,2))
cot <- read.csv("C:/Users/vigupta/OneDrive/Learning/DataScience/Statistics Texas A&M University/608/SheatherBook/Data/changeover_times.txt", header = T, sep = '')

#box  plot 
plot(cot$Method,cot$Changeover)

#scatter plot : use encoding 
plot(cot$New,cot$Changeover)


#Run the model 
fit.1 <- lm (Changeover ~ New, data=cot)
summary(fit.1)
abline(fit.1)

#Run regression diagnostics, plots look ok. 
plot(fit.1)

#CI of the model params
confint(fit.1)

