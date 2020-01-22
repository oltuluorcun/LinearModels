#data preparation

data <- read.csv("hour.csv",T,sep=",")
head(data)
data.2011 <- data[which(data$yr==0),]
dim(data.2011)
##2011##
N <- 800
set.seed(6)
random <- sample(1:8645,replace=FALSE,size=800)
data.2011 <- data.2011[random,-c(1,2,4,5,8,12,15,16)]
dim(data.2011)
attach(data.2011)
#creating dummy variables
#dummies for season variable
season.dummy1 <- ifelse(season==2,1,0)
season.dummy2 <- ifelse(season==3,1,0)
season.dummy3 <- ifelse(season==4,0,1)
new.data.2011 <- cbind(data.2011,season.dummy1,season.dummy2,season.dummy3)
new.data.2011 <- new.data.2011[,-1]
head(new.data.2011)
#dummies for weathersit
weather.dummy1 <- ifelse(weathersit==2,1,0)
weather.dummy2 <- ifelse(weathersit==3,1,0)
new.data.2011 <- cbind(new.data.2011,weather.dummy1,weather.dummy2)
new.data.2011 <- new.data.2011[,-4]
head(new.data.2011)
class(new.data.2011)
detach(data.2011)
attach(new.data.2011)
new.data.2011$holiday <- as.factor(holiday)
new.data.2011$workingday <- as.factor(workingday)
new.data.2011$season.dummy1 <- as.factor(season.dummy1)
new.data.2011$season.dummy2 <- as.factor(season.dummy2)
new.data.2011$season.dummy3 <- as.factor(season.dummy3)
new.data.2011$weather.dummy1 <- as.factor(weather.dummy1)
new.data.2011$weather.dummy2 <- as.factor(weather.dummy2)
new.data.2011$temp <- new.data.2011$temp * 41 
new.data.2011$hum <- new.data.2011$hum * 100
new.data.2011$windspeed <- new.data.2011$windspeed * 67
attach(new.data.2011)
head(new.data.2011)

##test_set##
kalan <- data[-random,-c(1,2,4,5,8,12,15,16)]
dim(kalan)
set.seed(7)
random.test <- sample(1:7845,replace=FALSE,size=800)
test <- data[random.test,-c(1,2,4,5,8,12,15,16)]
dim(test)

#creating dummy variables for test set
#dummies for season variable
season.dummy1 <- ifelse(test$season==2,1,0)
season.dummy2 <- ifelse(test$season==3,1,0)
season.dummy3 <- ifelse(test$season==4,0,1)
new.test <- cbind(test,season.dummy1,season.dummy2,season.dummy3)
new.test<- new.test[,-1]
head(new.data.2011)
#dummies for weathersit
weather.dummy1 <- ifelse(test$weathersit==2,1,0)
weather.dummy2 <- ifelse(test$weathersit==3,1,0)
new.test <- cbind(new.test,weather.dummy1,weather.dummy2)
new.test <- new.test[,-4]
head(new.test)
dim(new.test)

new.test$holiday <- as.factor(new.test$holiday)
new.test$workingday <- as.factor(new.test$workingday)
new.test$season.dummy1 <- as.factor(new.test$season.dummy1)
new.test$season.dummy2 <- as.factor(new.test$season.dummy2)
new.test$season.dummy3 <- as.factor(new.test$season.dummy3)
new.test$weather.dummy1 <- as.factor(new.test$weather.dummy1)
new.test$weather.dummy2 <- as.factor(new.test$weather.dummy2)
#they were normilized, we converted them to original.
new.test$temp <- new.test$temp * 41 
new.test$hum <- new.test$hum * 100
new.test$windspeed <- new.test$windspeed * 67

##Assupmtion Checking##
#count data
summary(cnt)
cnt[sample(1:800,replace=FALSE,size=50)]

#poisson distribution
hist(cnt,col="dark green",prob=TRUE,main="Histogram of Count",xlab="Count")
lines(density(cnt),lwd=3)

#mean = variance
mean(cnt);var(cnt)

#multicollinearity checking
install.packages("usdm")
library(usdm)
data.2011 <- data.frame(data.2011)
data.2011<-data.2011[,-5] # omitting categorical variable weathersit
data.2011<-data.2011[,-8] # omitting the response varieble cnt
vif(data.2011)

##models##
model.p1 <- glm(cnt ~ . , data=new.data.2011, family = poisson(link = "log"))
summary(model.p1)

model.p2 <- glm(cnt ~ hr + temp + hum + season.dummy1 + season.dummy2 + 
                  season.dummy3 + weather.dummy2, 
                data=new.data.2011, family = poisson(link = "log"))
summary(model.p2)

##glm.nb##
library(MASS)

model.nb <- glm.nb(cnt ~ . , data=new.data.2011)
summary(model.nb)

model.nb2 <- glm.nb(cnt ~ hr + temp + hum + season.dummy1 + season.dummy3 + weather.dummy2,
                    data = new.data.2011, link = "log")
summary(model.nb2)

model.nb3 <- glm.nb(cnt ~ hr + temp + hum + season.dummy3 + weather.dummy2,
                    data = new.data.2011, link = "log")
summary(model.nb3)
#after eliminating the insignificant variables.

model.nb7 <- glm.nb(cnt ~ hr + I(temp^2) + I(hum^2)  + season.dummy3 + weather.dummy2 ,
                    data = new.data.2011, link = "log")
summary(model.nb7)

##predictions##
prediction.1 <- predict(model.nb3, newdata=new.test)
par(mfrow=c(1,2))
hist(cnt,col="dark green", main="Original Data",xlab="counts")
hist(exp(prediction.1),col="red", main="Predicted",xlab="counts")

prediction.2 <- predict(model.nb7, newdata=new.test)
par(mfrow=c(1,2))
hist(cnt,col="dark green", main="Original Data",xlab="counts")
hist(exp(prediction.2),col="red", main="Predicted",xlab="counts")

##Cross Validation##
N <- 800
subset.train <- list()
for(j in 1:10)
  subset.train[[j]] <- setdiff(1:N, ((j-1) * N / 10 + 1):(j * N) / 10)

CV.nb <- c()
for(j in 1:10){
  reg.nb <- glm.nb(cnt ~ hr + temp + hum + season.dummy3 + weather.dummy2 , 
                   subset = subset.train[[j]])
  
  test.nb <- data.frame(hr[-subset.train[[j]]],temp[-subset.train[[j]]],
                        hum[-subset.train[[j]]],season.dummy1[-subset.train[[j]]],
                        season.dummy3[-subset.train[[j]]], weather.dummy2[-subset.train[[j]]])
  colnames(test.nb) <- c("hr","temp","hum","season.dummy1","season.dummy3","weather.dummy2")
  prediction.nb <- exp(predict(reg.nb, newdata=test.nb))
  CV.nb <- c(CV.nb, mean((prediction.nb - cnt[-subset.train[[j]]])^2))
}
CV.nb
av.mse.nb <- mean(CV.nb);av.mse.nb

CV.nb2 <- c()
for(j in 1:10){
  reg.nb2 <- glm.nb(cnt ~ hr + I(temp^2) + I(hum^2) + season.dummy3 + weather.dummy2 , 
                    subset = subset.train[[j]])
  
  test.nb2 <- data.frame(hr[-subset.train[[j]]],temp[-subset.train[[j]]],
                         hum[-subset.train[[j]]],season.dummy1[-subset.train[[j]]],
                         season.dummy3[-subset.train[[j]]], weather.dummy2[-subset.train[[j]]])
  colnames(test.nb2) <- c("hr","temp","hum","season.dummy1","season.dummy3","weather.dummy2")
  prediction.nb2 <- exp(predict(reg.nb2, newdata=test.nb2))
  CV.nb2 <- c(CV.nb2, mean((prediction.nb2 - cnt[-subset.train[[j]]])^2))
}
CV.nb2
av.mse.nb2 <- mean(CV.nb2);av.mse.nb2

###research question-2
#Which type of weather had a most influence on the number of rental bikes?

new.data.2011$weathersit<-as.factor(new.data.2011$weathersit)
names(data.2011)
library(ggplot2)

ggplot(data.2011, aes(x=weathersit, y=cnt, fill=weathersit)) +
  geom_boxplot(outlier.shape=19, outlier.size=1 ) + ggtitle("Count vs. Weather type") +
  xlab("Weather") + ylab("Count")
names(data.2011)
model.weather <- glm(cnt ~weathersit , data=data.2011, family = poisson(link="log"))
model.weather
summary(aov(model.weather))

###research question-3 
#Are there any relationship between humidity and temperature?

cor(new.data.2011$hum, new.data.2011$temp)

ggplot(new.data.2011, aes(x=hum, y=temp))+  geom_point(shape=16, color="blue")+
  geom_smooth(method=lm, linetype="dashed",
              color="darkred", fill="blue") + ggtitle("Temperature vs. Humidity") +
  xlab("Humidity") + ylab("Temperature") 

###research question-4 
#Does wind speed decrease humidity?

cor(new.data.2011$windspeed, new.data.2011$hum)

ggplot(new.data.2011, aes(x=windspeed, y=hum))+  geom_point(shape=16, color="blue")+
  geom_smooth(method=lm, linetype="dashed",
              color="darkred", fill="blue") + ggtitle("Humidity vs. Windspeed") +
  xlab("Windspeed") + ylab("Humidity")

###research question-5 
#Are there any relationship between temperature and number of rental bikes(count)?

cor(new.data.2011$temp, new.data.2011$cnt)

ggplot(new.data.2011, aes(x=temp, y=cnt))+  geom_point(shape=16, color="blue")+
  geom_smooth(method=lm, linetype="dashed",
              color="darkred", fill="blue") + ggtitle("Count vs. Temperature") +
  xlab("Temperature") + ylab("Count") 
