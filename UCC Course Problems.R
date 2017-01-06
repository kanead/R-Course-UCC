# Problem
#The following distribution has a mean of 5 and a standard deviation of 2. 
# Using **pnorm** find the area under the curve from negative infinity to 4. 
pnorm(4,mean=5,sd = 1)
# How many standard deviations is 4 away from the mean? 1

# Problem 1
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

# Problem 2 
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


# Problem 3 Chi-square test of independence
men = c(100, 120, 60)
women = c(350, 200, 90)


Observed <- matrix(c(100,120,60,350,200,90),nrow = 2,ncol = 3, byrow = T)
chisq.test(Observed)

#It provides strong evidence to suggest that men and women tend to have difference preferences for ice cream flavours.

#Problem 4 t test
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


set.seed(1)
N    <- 10
x    <- rnorm(N,10)
beta <- 3 + 0.4*rnorm(N)
y    <- 1 + x * beta + .75*rnorm(N)




x<-c(1:5)
y<-c(1,2,1.3,3.75,2.25)
m1<-lm(y~x)
summary(m1)
resid(m1)
resid(m1)^2
sum(resid(m1)^2)
sum(resid(m1)^2) / (length(x)-2)
sqrt(sum(resid(m1)^2) / (length(x)-2))



height <- c(163, 185, 155, 195, 168, 198, 200, 146)
weight <- c(65, 85, 70, 120, 73, 100, 103, 50)
m1<-lm(weight~height)
summary(m1)
resid(m1)
resid(m1)^2
sum(resid(m1)^2)
sum(resid(m1)^2) / (length(height)-2)
sqrt(sum(resid(m1)^2) / (length(height)-2))

mean(height)
height-mean(height)
(height-mean(height))^2
sum((height-mean(height))^2)

9.256727 / sqrt(sum((height-mean(height))^2))

pnorm(0.95)
qnorm(0.8289439)
qnorm(0.95)
