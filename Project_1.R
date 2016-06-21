setwd("C:\\PERSONAL DATA\\SELF TRAINING\\Business Analytics\\New folder\\New folder")
getwd()
read.csv("Sales_dataset.csv",header = T)
SalesData<-read.csv("Sales_dataset.csv",header = T)
SalesData
View(SalesData)
attach(SalesData)
search()
head(SalesData)
str(SalesData) #get metadata of the imported file.

summary(SalesData)#Descriptive analysis
plot(density(SalesData$advertise))
plot(density(SalesData$sales))
plot(density(SalesData$attractiveness))
plot(density(SalesData$plays))

boxplot(SalesData)
which(SalesData$plays==0)

# exclude the attractiveness for further analysis
SalesData[,-4]
cor((SalesData[,-4]))

flm<-lm(sales~advertise+plays+attractiveness)
flm
summary(flm)
plot(flm)
#  advertise, plays and attractiveness is significant but attractiveness is a factor with invalid intercept value
flm<-lm(sales~advertise+plays+factor(attractiveness) , data=SalesData)
flm
summary(flm)
# now we have 10 variables created for attractiveness factor. Lets create dummy variables for attractiveness factor.

#forward step
fit_lm<-step(lm(sales~advertise+plays+factor(attractiveness) , data = SalesData),method='forward')

#not a good approach 
#$this is the best fit model, no modification. let's create dummy variables.'

dummy(SalesData$attractiveness, p="all" , SalesData)

library(dummy)

head(dummy(SalesData))

Sales<-(dummy(SalesData))

df<-cbind(SalesData[,-4],Sales)



head(df)

names(df)
attach(df)

mod1<-lm(df$sales~. ,data = df)
step(mod1, method= 'forward') 

predict(mod1,df)

library(dummy)
summary(mod1) # final step, write interpretations for all estimated values
forecast(fit_1lm)
forcast(fit_lm)
forecast::forecast.lm(fit_lm)
forecast.lm(fit_lm)
plot(fit_lm)
fit_lm<-lm(sales~advertise+plays+attractiveness, SalesData)
summary(fit_lm)
anova(fit_lm)
