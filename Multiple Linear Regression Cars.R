install.packages("psych")
library(psych)
getwd()
Cars <- read.csv ("Cars.csv")
View(Cars)
attach(Cars)
qqnorm(HP)
qqline(HP)
summary(Cars)
hist(HP)
plot(HP, MPG)

# Explore the data

plot(HP,MPG) # Plot relation ships between each X with Y
plot(VOL,MPG)
## Or make a combined plot
#pairs(Cars)# Scatter plot for all pairs of variables
pairs.panels(Cars)
cor(HP,MPG)
cor(VOL,MPG)
cor(WT,VOL)
plot(WT,VOL)
cor(Cars) # correlation matrix

# The Linear Model of interest
model.car <- lm(MPG~VOL+HP+SP+WT, data = Cars) #lm(Y ~ X)
summary(model.car)

model.carV<-lm(MPG~VOL)
summary(model.carV)

model.carW <- lm(MPG~WT)
summary(model.carW)

model.carVW <- lm(MPG~VOL+WT)
summary(model.carVW)
###Partial Correlation matrix
install.packages("corpcor")
library(corpcor)
cor(Cars)
View(Cars)

cor2pcor(cor(Cars))


# Diagnostic Plots
install.packages("car")
library(car)
plot(model.car)# Residual Plots, QQ-Plos, Std. Residuals vs Fitted, Cook's distance

#qqplot(model.car)# QQ plots of studentized residuals, helps identify outliers

# Deletion Diagnostics for identifying influential variable
influence.measures(model.car)
influenceIndexPlot(model.car) # Index Plots of the influence measures
influencePlot(model.car)# A user friendly representation of the above
?influencePlot


## Regression after deleting the 77th observation
model.car1<-lm(MPG~VOL+HP+SP+WT, data=Cars[-77,])
summary(model.car1)


### Variance Inflation Factors
vif(model.car)  # VIF is > 10 => collinearity
VIFWT<-lm(WT~VOL+HP+SP)
VIFVOL<-lm(VOL~WT+HP+SP)
#VIFHP<-lm(HP~VOL+WT+SP)
#VIFSP<-lm(SP~VOL+HP+WT)
summary(VIFWT)
summary(VIFVOL)
#summary(VIFHP)
#summary(VIFSP)
#### Added Variable Plots ######
avPlots(model.car, id.n=5, id.cex=100, col="red")
install.packages("MASS")
library("MASS")
stepAIC(model.car) # backward

plot(model.car)

model.final <- lm(MPG~VOL+HP+SP, data=Cars)
summary(model.final)
pred1 <- predict(model.final,interval="predict")
pred1

model.final1 <- lm(MPG~VOL+HP+SP, data=Cars[-77,])
summary(model.final1)
pred2 <- predict(model.final1,interval="predict")
pred2

avPlots(model.final1, id.n=2, id.cex=0.8, col="red")

vif(model.final1)

# Lower the AIC (Akaike Information Criterion) value better is the model. AIC is used only if you build
# multiple models.