library(readr)
data <- read.csv("Documents/data.csv")
View(data)
# least square regression line
scatter.smooth(x=data$X,y=data$Y, main='Scatter plot of the', col='red', lwd=2)  # scatterplot

# regression line 
library(ggplot2)
qplot(x = X, y =Y, data = data, geom = c("point", "smooth"),method = "lm", se = FALSE)
l=lm(data$X ~data$Y)
summary(l)
meanval=mean(data$X)
# testing with 95% confident interval of two sides
tval=t.test(data$X,conf=0.95)


# problem 3:
weigthtmean=mean(data$X)
heightmean=mean(data$Y)
plot(data$X, data$Y, pch=16, col='blue', 
     main='Scatter plot of whitefish',xlab ='Weight',ylab ='Hight')
abline(lm(data$Y ~ data$X))
abline(v=weigthtmean, col='orange', lwd=2)
abline(h=heightmean, col='orange', lwd=2)
prob=abs(100-(abs((heightmean/weigthtmean)-(480/850))*100))
cat('Least probability for 400mm length of 850gm weight is:', prob,'%')

# problem 1:g
error = qt(0.95,df=length(data$Y)-1)*sd(data$Y)/sqrt(length(data$Y)) # t-distribution
llevel <- mean(data$Y)-error
ulevel <- mean(data$Y)+error
cat('95% Confidence interval is: lower=',llevel, 'and upper=', ulevel)
error
 
# Problem: 1:h
linear=lm(data$X ~ data$Y)
premod=predict(linear, data)
error=premod - data$X
rsquare=sqrt(mean(error^2))
cat('R Square value for the given data is:', rsquare)

# R-square interpretation












