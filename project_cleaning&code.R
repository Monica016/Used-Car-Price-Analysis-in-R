# Data cleaning
library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
audi <- read_csv("audi.csv")
summary(audi)
sum(is.na(audi)) #checking wether there are NAs in the csv file
#0 means no NA in audi's used car markets data collection

audi[duplicated(audi) | duplicated(audi, fromLast=TRUE),]
# finding the replicate rows, since the same model, same year,same mog
# and same size is very rare to see in the market, we don't know the collection
# process, therefore, in order to make our data more precise, we delete the 
#replicate rows
audirepeat <- audi[duplicated(audi) | duplicated(audi, fromLast=TRUE),]
audi <- anti_join(audi,audirepeat)
# the new audi dataset aleady remove the repeat rows.
audi$brand <- rep(c("Audi"), times = 10483)
# creat a brand columns in order to combine with orther brands csv.
summary(audi)
#final useful dataset


#do the same cleaning process for other data
bmw <- read_csv("bmw.csv")
summary(bmw)
sum(is.na(bmw))
bmw[duplicated(bmw) | duplicated(bmw, fromLast=TRUE),]
bmwrepeat <- bmw[duplicated(bmw) | duplicated(bmw, fromLast=TRUE),]
bmw <- anti_join(bmw,bmwrepeat)
bmw$brand <- rep(c("BMW"), times = dim(bmw)[1])
summary(bmw)


merc <- read_csv("merc.csv")
summary(merc)
sum(is.na(merc))
merc[duplicated(merc) | duplicated(merc, fromLast=TRUE),]
# Although the number of rows in the merc.csv is larger than the previous two
# csv, it shows that there is more repeat rows in this file as well.
mercrepeat <- merc[duplicated(merc) | duplicated(merc, fromLast=TRUE),]
merc <- anti_join(merc,bmwrepeat)
merc$brand <- rep(c("Benz"), times = dim(merc)[1])
summary(merc)

ds <- rbind(audi,bmw, merc)

trans.unq <- unique(ds$transmission)
trans.unq
ds$transmission[ds$transmission == "Manual"] = 1
ds$transmission[ds$transmission == "Automatic"] = 2
ds$transmission[ds$transmission == "Semi-Auto"] = 3
ds$transmission[ds$transmission == "Other"] = 4
ds$transmission <- as.numeric(ds$transmission)
ds <- na.omit(ds)

fuel.unq <- unique(ds$fuelType)
fuel.unq
ds$fuelType[ds$fuelType == "Petrol"] = 1
ds$fuelType[ds$fuelType == "Diesel"] = 2
ds$fuelType[ds$fuelType == "Hybrid"] = 3
ds$fuelType[ds$fuelType == "Electric"] = 4
ds$fuelType[ds$fuelType == "Other"] = 5
ds$fuelType <- as.numeric(ds$fuelType)

summary(ds)

numericds <-cbind(ds[,2],ds[,3],ds[,4],ds[,5],ds[,6],ds[,7],ds[,8],ds[9]) 
round(cor(numericds),digits = 2)
#corrlation of all terms which round in 2 decimals

library(ggplot2)



ggplot(data = ds) +
  geom_point(mapping = aes(x = year, y = price, shape=brand, color=brand))+
  facet_wrap(~brand)+
  labs(title = "Registration year vs price scatterplot in £")
# Registration year vs price scatterplot

ggplot(data = ds) +
  geom_boxplot(mapping = aes(x = transmission, y = price, shape=brand, color=brand))+
  facet_wrap(~brand)+
  labs(title = "type of gearbox vs price in £")
# type of gearbox vs price


ggplot(data = ds) +
  geom_point(mapping = aes(x = mileage, y = price, shape=brand, color=brand))+
  facet_wrap(~brand)+
  labs(title = "distance used vs price in £")
#distance used vs price


ggplot(data = ds) +
  geom_boxplot(mapping = aes(x = fuelType, y = price, shape=brand, color=brand))+
  facet_wrap(~brand)+
  labs(title = "engine fuel vs price  in £")
# engine fuel vs price 


ggplot(data = ds) +
  geom_point(mapping = aes(x = tax, y = price, shape=brand, color=brand))+
  facet_wrap(~brand)+
  labs(title = "road tax vs price in £")
#road tax vs price

ggplot(data = ds) +
  geom_point(mapping = aes(x = mpg, y = price, shape=brand, color=brand))+
  facet_wrap(~brand)+
  labs(title = "miles per gallon vs pricein £")
# miles per gallon vs price


ggplot(data = ds) +
  geom_point(mapping = aes(x = engineSize, y = price, shape=brand, color=brand))+
  facet_wrap(~brand)+
  labs(title = "size in litres vs price in £")
# size in litres vs price 

### Project Code

pr.out <- prcomp(numericds,scale.=TRUE)
biplot(pr.out,scale=0)
## First we do PCA on the dataset, from the graph, we can see that transmission, mileage, year and engineSize are close to price,
## showing that these four variables are highly correlated with price, which is the response variable. 
## This result can also be verified by the correlation matrix. These four variables have the four highest correlation 
## coefficients with price.
## However, year and mileage has a correlation of 0.76, so we can drop the variable mileage.

year_sp <- qplot(x = year, y = price, data = ds)
trans_sp <- qplot(x = transmission, y = price, data = ds)
engine_sp <- qplot(x = engineSize, y = price, data = ds)

gridExtra::grid.arrange(year_sp, trans_sp,engine_sp,ncol=1) #This graph shows the linear relationship between each variable and price.

## Therefore, we first examine the linear model : price ~ year  + enginesize + transmission
lreg <- lm(price ~ year + engineSize + transmission, data = ds)
summary(lreg)

lreg1 <- lm(log(price) ~ year + engineSize + transmission, data = ds) ## model with log transformation
summary(lreg1)

## ANOVA analysis
anova(lreg)
anova(lreg1)
## From the p-value, both model looks significant, so we examine the qqplot and the residual vs fitted plot.

layout(matrix(c(1,2,3,4),2,2)) 
plot(lreg)## four plots of the model, qqplot, residual vs fitted, etc. 
par(mfrow=c(1,1))

layout(matrix(c(1,2,3,4),2,2)) 
plot(lreg1) ## qqplot and other three plots of the transformed model
par(mfrow=c(1,1))
## From the qq plot, we can see that the transformed model is better.

## residual plot for the transformed model
plot(lreg1$residuals) 

### Outlier removal
par(mfrow=c(2,2))
boxplot(ds$year,main="Year")
boxplot(ds$transmission,main="transmission")
boxplot(ds$engineSize,main="engineSize") ## boxplot showing that there are some outliers 
boxplot(ds$year)
par(mfrow=c(1,1)) 

summary(ds$year) ## remove outliers that are beyond 1st quantile
ds.rem <- subset(ds, ds$year >= 2016)

summary(ds$engineSize) ## remove outliers that are beyond 1st quantile
ds.rem <- subset(ds, ds$engineSize >= 1.600)

## After removing the outliers, we fit the regression model again:
lreg.out <- lm(log(price) ~ year + engineSize + transmission, data = ds.rem)
summary(lreg.out)
layout(matrix(c(1,2,3,4),2,2)) 
plot(lreg.out)

par(mfrow=c(1,1))
plot(lreg.out$residuals)

## We can see from the residual plot and qq plot that this model fits well. 

## Now we use cross validation to see the fitness of the model.
n.dim <- dim(ds.rem)
set.seed(3)
tr <- sort(sample(21960,21960/2)) ## create training dataset

install.packages("DAAG")
install.packages("lattice")
library(lattice)
library(DAAG)
ds.rem <- as.data.frame(ds.rem)
cross.v <- cv.lm(ds.rem, lreg.out, 10)
head(cross.v, 10)

RMSE<-sqrt(mean((cross.v$cvpred - cross.v$`log(price)`)^2))
RMSE
## Since the RMSE (Root Mean Square Error) is very small, it shows that the model fits the known data as well as unknown data.



