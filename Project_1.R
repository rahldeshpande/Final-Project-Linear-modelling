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

# exclude the attractiveness for further analysis
SalesData[,-4]
cor((SalesData[,-4]))

fit_sa<-lm(sales~advertise, data=SalesData)
summary(fit_sa)

fit_sa<-step(lm(sales~advertise, data=SalesData), methode='forward')

plot(sales,advertise)
abline(fit_sa)


fit_sp<-lm(sales~plays, data=SalesData)
summary(fit_sp)

plot(sales,plays)
abline(fit_sp)

plot(sales,attractiveness)
plot(sales,as.factor(attractiveness))
fit_sat<-lm(sales~factor(attractiveness), data=SalesData)
summary(fit_sat)

flm<-lm(sales~advertise+plays+attractiveness)
flm
summary(flm)
plot(flm)
#  advertise, plays and attractiveness is significant but attractiveness is a factor with invalid intercept value
flm<-lm(sales~advertise+plays+factor(attractiveness) , data=SalesData)
flm
summary(flm)
# now we have 19 variables created for attractiveness factor. Lets create dummy variables for attractiveness factor.

#forward step
fit_lm<-step(lm(sales~advertise+plays+factor(attractiveness) , data = SalesData),method='forward')

#not a good approach 
#$this is the best fit model, no modification. let's create dummy variables.'
library(dummy)

SalesData$attractiveness<-as.factor(SalesData$attractiveness)

head(dummy(SalesData))

Sales_dummy<-dummy(SalesData)

New_SalesData<-cbind(SalesData[,-4],Sales_dummy)

head(New_SalesData)

summary(New_SalesData)

names(New_SalesData)


final_model<-lm(New_SalesData$sales~. ,data = New_SalesData)

summary(final_model) # final step, write interpretations for all estimated values
plot(final_model)

step(final_model, method= 'forward') 

