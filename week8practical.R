lm(weight~height,data = women)
summary(women)

View(cars)
lm(dist~speed,data=cars)
summary(cars)
windows(20,10)

scatter.smooth(x=cars$speed,
               y=cars$dist,main="dist~speed")


windows(20,10)
par(mfrow=c(1,3))
?boxplot

boxplot(x=cars$speed,main="speed")
windows(20,10)
boxplot(x=cars$dist,main="distance")
#density plot speed
windows(20,12)
par(mfrow=c(1,3))

plot(density(cars$speed),
     main="density plot for speed",
     ylab = "Frequency")
polygon(density(cars$speed),col="blue")
#density plot for distance
plot(density(cars$dist),
     main="density plot for distance",
     ylab = "Frequency")
polygon(density(cars$dist),col="red")

#corelation
cor(cars$speed,cars$dist)
cor(cars)
#build a linear model
attach(cars)
linearmod<-lm(dist ~ speed)
linearmod
#distance ~-17.579+3.932*speed
summary(linearmod)


AIC(linearmod)
BIC(linearmod)
