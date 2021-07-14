
#Insurance Costs Train data
dataset=read.csv('TrainInsurance.csv')

#Exploring data

summary(dataset)
ExpData(data=dataset, type=1)
ExpData(data=dataset, type=1)
ones=sum(dataset$smoker)
# Fitting linear Regression for charges vs all variables

regressor<-lm(formula=charges~ bmi+children+smoker+Age+Sex, data<-dataset) 

#Summary table
summary(regressor)
anova(regressor)
par(mfrow = c(2, 2))  # Split the plotting panel into a 2 x 2 grid
plot(regressor) 
par(mfrow = c(1, 1))

one.way <- aov(charges ~ smoker, data = dataset)

summary(one.way)

boxplot(charges~smoker,data=dataset, main="Insurance Cost",
        xlab="Smoker", ylab="Charges")

testset<-read.csv('Xtest.csv')

yhat<-predict(regressor, newdata = testset, interval = "prediction")
write.csv(yhat, 'yhat.csv')
