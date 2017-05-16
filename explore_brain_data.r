# Date 09/10/12
# Data Handling Course BD7054
# Linear Regression in R
# Template file on how to perform Linear Regression in R
#
# The data give the brain and body mass of several species of
# Aves and Mammalia

# -------------------------------------------------------------------
# Housekeeping

rm(list=ls()) # remove everything currently held in the R memory

graphics.off() # close all open graphics windows 

#x<-c(1:10)
#y<-c(11:20)

#m5<-lm(y~x)
#plot(x,y, xlim = c(0,10),ylim = c(10,20))
#summary(m5)
# -------------------------------------------------------------------
# Enter or read in your data from a file
setwd("C:\\Users\\adamdkane\\Desktop\\Science\\Teaching\\week 2\\Day 1 - hypothesis testing")
# read in data from our CSV file
# This is a comma separated file

mydata <- read.csv("brain_full.csv" , header=TRUE)

mydata$ratio <- mydata$brain / mydata$body

# make the data directly accessible by the column headers
# attach(mydata)


# --------------------------------------------------------------------
# Plot and explore your data

head(mydata,data=mydata)


# open up a new figure for plotting
# dev.new()
boxplot(ratio~class,ylab="brain/body ratio", cex.axis=1.2, cex.lab=1.2,data=mydata)


# dev.new()
plot(mydata$body,mydata$brain)
abline(lm(mydata$brain~mydata$body),col="red")


# dev.new()
plot(log(mydata$body), log(mydata$brain), cex.axis=1.2,cex.lab=1.2,bty="L")
abline(lm(log(mydata$brain)~log(mydata$body)),col="red")
m0<-lm(log(mydata$brain)~log(mydata$body))
print(m0)

# The interpretation of log-log regression is given as an expected percentage change 
# in Y when X increases by some percentage. In other words if we change X by one percent, 
# we'd expect Y to change by beta1 percent. In this example it would mean for a one percent change
# in body mass we'd predict a 0.6768 % change in brain mass

# It's easy to get confused when interpreting percentage change. Here's an example of the correct way to
# think about it: a change of 80 percentage means that the final value is (1 + 80/100) or 1.8 times the initial
# value. A change of -30 percentage means that the final value is (1 - 30/100) or 0.7 times the initial value.


plot(log10(mydata$body), log10(mydata$brain), cex.axis=1.2,cex.lab=1.2,bty="L")
abline(lm(log10(mydata$brain)~log10(mydata$body)),col="red")
m1<-lm(log10(mydata$brain)~log10(mydata$body))
print(m1)

# no interaction model 
plot(log10(mydata$body), log10(mydata$brain), col=mydata$class,
     cex.axis=1.2,cex.lab=1.2,bty="L",lwd=2,pch=16)
m4 <-  glm(log10(brain)~log10(body)+class,data=mydata)
summary(m4)
abline(-1.06642,0.66806,col="black")
abline(-1.06642+0.05825,0.66806,col="red")
legend("bottomright", c("Aves","Mammalia"), col=c("black","red"),
       pch=16, bty="n",cex=1.5,lwd=2,lty=NA)


# interaction model
# dev.new()
plot(log10(mydata$body), log10(mydata$brain), col=mydata$class,
  cex.axis=1.2,cex.lab=1.2,bty="L",lwd=2,pch=16)
m3 <-  glm(log10(brain)~log10(body)*class,data=mydata)
summary(m3)
abline(-0.76256,0.53036,col="black")
abline(-0.76256+-0.40595,0.53036+0.17582,col="red")

legend("bottomright", c("Aves","Mammalia"), col=c("black","red"),
  pch=16, bty="n",cex=1.5,lwd=2,lty=NA)

# --------------------------------------------------------------------

# allometric relationships 
# for birds
y.birds<-10^-0.76256*(mydata$body[mydata$class=="Aves"])^0.53036; y.birds # birds
hist(y.birds)
hist(log10(y.birds))
max(y.birds)
max(mydata$brain[mydata$class=="Aves"]) # emperor penguin

# allometric relationships 
# for mammals
y.mammals<-10^-1.16851*(mydata$body[mydata$class=="Mammalia"])^0.70618; y.mammals # mammals

hist(y.mammals)
hist(log10(y.mammals))
max(y.mammals)
max(y.mammals)/1000 # get in kg
max(mydata$brain[mydata$class=="Mammalia"]) # Sperm Whale
max(mydata$brain[mydata$class=="Mammalia"])/1000 # get in kg

# --------------------------------------------------------------------

# Skeleton body mass relationship
# setwd("C:\\Users\\adamdkane\\Desktop\\Science")
# skel.data<-read.csv("skeletons.csv",header=T,sep=",")
# head(skel.data)
# plot(log10(skel.data$skeleton)~log10(skel.data$body))
# m1<-lm(skel.data$skeleton~skel.data$body)
# m1
# m2<-lm(log10(skel.data$skeleton)~log10(skel.data$body))
# m2
# summary(m2)
# abline(m2)
# 10^-1.216
# 0.0608135*skel.data$body^1.083


# hist(y)
# yplot(y~x)
# lm(y~x)
