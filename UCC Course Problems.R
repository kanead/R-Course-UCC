# Problem 1
#The following distribution has a mean of 5 and a standard deviation of 2. 
# Using **pnorm** find the area under the curve from negative infinity to 4. 
pnorm(4,mean=5,sd = 1)
# How many standard deviations is 4 away from the mean? 1

# Problem 2
# create 2 vectors of numerical data
height <- c(163, 185, 155, 195, 168, 198, 200, 146, 179, 160, 180, 170, 190)
weight <- c(65, 85, 70, 120, 73, 100, 103, 50, 81, 64, 90, 78, 71)
# mean 
mean(height)
mean (weight)
# sd
sd(height)
sd(weight)
# plot - histogram 
hist(weight)
hist(height)
plot(height, weight)
# covariance and correlation of the two variables 
cov(height, weight)
cor.test(height, weight)

# Problem 3 
# create 2 vectors of numerical data
height <- c(163, 185, 155, 195, 168, 198, 200, 146, 179, 160, 180, 170, 190)
weight <- c(65, 85, 70, 120, 73, 100, 103, 50, 81, 64, 90, 78, 71)
plot(weight~height)
m1<-lm(weight~height)
abline(m1)
print(m1)
y<-coef(m1)["(Intercept)"] + coef(m1)["height"]*height[1];y
resid(m1)
y - 3.59117798
plot(m1)


# Problem 4 Chi-square test of independence
men = c(100, 120, 60)
women = c(350, 200, 90)


Observed <- matrix(c(100,120,60,350,200,90),nrow = 2,ncol = 3, byrow = T)
chisq.test(Observed)

#It provides strong evidence to suggest that men and women tend to have difference preferences for ice cream flavours.

#Problem 5 t test
before<-c(30,68,45,60,79,40,55,49,82,44,76)
after<-c(70,68,50,59,81,44,78,53,90,87,80)
boxplot(before, after, ylab="Test Scores",     
                 names=c("before","after"),                          
                 main="Effect of drug on test scores")

mean(before)
mean(after)
t.test(before,after, alternative = "less" )
t.test(before,after, alternative = "less" , paired = T)
#Note that the t.test() function always subtracts first group minus second group. Since the "before" were entered first into the function, and our hypothesis was that they would score higher on the test, this dictated the alternative hypothesis to be "difference between means is greater than zero."
mean(before) - mean(after)



