library(RColorBrewer)
library(modeest)
library(psych)
library(plyr)
library(dplyr)
library(GGally)
library(caret)
library(corrplot)
library(ggplot2)
library(tidyverse)
library(gridExtra)
library(corrgram)
library(mlbench)
library(glmnet)
library(rpart)
library(rpart.plot)
library(ranger)
library(Metrics)

setwd("C:\\Users\\NIKIL\\Desktop\\Project electric motor")
motor_train <- read.csv("train.csv")
motor_test <- read.csv("Test.csv")
head(motor_train)
str(motor_train)
attach(motor_train)

#Business moment decisions
summary(motor_train[,-1]) 
describe(motor_train[,-1])
sapply(motor_train[,-1],mfv)

#checking for NA's
colSums(is.na(motor_train))

#correlation among variables
cor(motor_train[,-c(1,14)])
corrplot(cor(motor_train[,-c(1,14)]),method = 'number',type = 'lower',bg = "black",diag = FALSE)
corrplot(cor(motor_train[,-c(1,14)]),method = 'circle',outline = TRUE,bg='black')

#--------------------------------------------- visualisation -----------------------------------------------#
#no of measurements per profile_id
unique(motor_train$profile_id)
motor_train$profile_id <- as.factor(motor_train$profile_id)
a <- plyr::count(profile_id)
ggplot(data = a)+geom_bar(mapping=aes(a$x,a$freq),stat = 'identity',width = 0.8,fill='yellow',color="black")+theme_light()+geom_label(aes(a$x,a$freq),label=a$freq,)
b <- a[order(a$freq),]
ggplot(data = b,aes(x=reorder(as.factor(b$x),b$freq),y=b$freq,fill=as.factor(a$x)))+geom_bar(stat = 'identity')+coord_flip()+labs(title = 'profile id vs measurements',x='profile id',y='measurements')

#analysis based on profile_id and pm(permanent magnet temperature)
ggplot(data = motor_train,aes(x=profile_id,y=pm,color=profile_id))+geom_boxplot()

groupby_profileid <- motor_train%>%group_by(profile_id)%>%summarise(maxtemp=max(pm),medtemp=median(pm),mintemp=min(pm))
groupby_profileid

plot1 <- ggplot(data = groupby_profileid,aes(x=profile_id,y=maxtemp,group=1))+geom_point(color='blue')+geom_line(color='red')+ggtitle('max pmtemp')+theme_bw()
plot2 <- ggplot(data = groupby_profileid,aes(x=profile_id,y=mintemp,group=1))+geom_point(color='green')+geom_line(color='brown')+ggtitle("min pmtemp")+theme_bw()
grid.arrange(plot1,plot2,nrow=2) #max & min pm for each profile id

#density plot
ggplot(motor_train, aes(pm))+ geom_density(aes(fill=profile_id), alpha=0.5) + 
  labs(title="Density plot using profile id and PM temp",x="PM temp",fill="Profile id")

#analysis based on motor speed
motor_train$clk_anti_clk <- ifelse(motor_speed>0,'clock','anticlock')
motor_train$clk_anti_clk
plot3 <- ggplot(data = motor_train)+geom_bar(aes(x=profile_id,y=motor_speed,fill=clk_anti_clk),stat = 'identity')
plot4 <- ggplot(data = motor_train,aes(x=profile_id,fill=clk_anti_clk,stat='identity'))+geom_bar(position='dodge')
grid.arrange(plot3,plot4,nrow=2)
motor_train <- motor_train[,-15]

#violin plot
motor_train%>%gather(-X,-profile_id,key='key',value = 'value')%>%ggplot(aes(key,value))+geom_violin(fill='steelblue')+theme_dark()

#------------------------------- random profile_id --------------------------------------------------#
uniq_id <- as.data.frame(unique(profile_id))
profile <- sample(52,1)

#heatmap for random profile_id
motor_train_1 <- motor_train[,-c(1)]
motor_test_1 <- motor_test[,-1]
corr <- cor(motor_train_1[motor_train_1$profile_id==uniq_id[profile,],-13])
corr
heatmap(corr,main = (paste0('heatmap for profileid :',uniq_id[profile,])))

#pairs for random profile_id
ggpairs(motor_train_1[motor_train_1$profile_id==uniq_id[profile,],-13])

#corrgram for random profile_id
corrgram(motor_train_1[motor_train_1$profile_id==uniq_id[profile,],-13],lower.panel = panel.shade,upper.panel = panel.pie,text.panel = panel.txt,order = TRUE)
corrgram(motor_train_1[motor_train_1$profile_id==uniq_id[profile,],-13],lower.panel = panel.shade,upper.panel = panel.pie,text.panel = panel.txt)

#boxplot for random profile_id
boxplot(motor_train_1[motor_train_1$profile_id==uniq_id[profile,],-13],col=brewer.pal(8,'Dark2'),las=2)

#barplot for random profile_id
plot_data <- motor_train_1[motor_train_1$profile_id==uniq_id[profile,],-13]
plot_data%>%gather()%>%ggplot(aes(value))+facet_wrap(~key,scales = "free")+geom_histogram(fill='yellow',color='green',binwidth = 0.5)+theme_dark()

#density plot for random profile_id
plot_data%>%gather()%>%ggplot(aes(value))+facet_wrap(~key,scales = 'free')+geom_density(fill='blue',color='yellow')+theme_dark()

#normality check for random profile_id
plot_data%>%gather()%>%ggplot(aes(sample=value))+facet_wrap(~key,scales = 'free')+stat_qq(col='green')+stat_qq_line(col='skyblue')+theme_dark()

#scatterplot for random profile_id (w.r.t pm)
plot_data%>%gather(-pm,key='key',value = 'value')%>%ggplot(aes(x=value,y=pm))+facet_wrap(~key,scales = 'free')+geom_point(color='green')+geom_smooth(method = 'lm')

#------------------------------------------- outlier Treatment -------------------------------------------#
z<- motor_train_1%>%gather(-profile_id,key='key',value = 'value')%>%ggplot(aes(key,value))+geom_boxplot(fill='steelblue')+theme_dark()
z+theme(axis.text.x  = element_text(angle = 90))


out_treatment <- function(x){
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  caps <- quantile(x, probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  return(x)
}

motor_train_1$ambient <- out_treatment(motor_train_1$ambient)
motor_train_1$torque <- out_treatment(motor_train_1$torque)
motor_train_1$i_q <- out_treatment(motor_train_1$i_q)
motor_test_1$ambient <- out_treatment(motor_test_1$ambient)
motor_test_1$torque <- out_treatment(motor_test_1$torque)
motor_test_1$i_q <- out_treatment(motor_test_1$i_q)

#------------------------------------------- Feature selection ------------------------------------------#
set.seed(111)
motor_train_1$profile_id <- as.integer(motor_train_1$profile_id)
importance <- rfe(x=motor_train_1[,-9],y = motor_train_1[,9],rfeControl = rfeControl(functions=lmFuncs,method = 'repeatedcv',number = 10),metric = "RMSE")
importance$optVariables
plot(importance,type=c('g','o'))
plot.rfe(importance)

# I'm getting better RMSE with considering Profile_id 
# Profile_id  variable is  not eliminated

#------------------------------------------- Standardising the data ------------------------------------#
motor_train_1 <- as.data.frame(scale(motor_train_1))
motor_test_1 <- as.data.frame(scale(motor_test_1))

#------------------------------------------- Splitting data to test and train -------------------------------#
set.seed(100)
train_test <- createDataPartition(motor_train_1$pm,p=0.75,list = F)
train_data <- as.data.frame(motor_train_1[train_test,])
test_data <- as.data.frame(motor_train_1[-train_test,])
dim(train_data)
dim(test_data)

#------------------------------------------ model building -------------------------------------------------#
#------------------#Linear model
linear <- lm(pm~.,data=train_data)
summary(linear)
predictlinear_test <- predict(linear,test_data)
predictlinear_train <- predict(linear,train_data)
acc_lm_test <- cor(predictlinear_test,test_data$pm)
acc_lm_test
acc_lm_train <- cor(predictlinear_train,train_data$pm)
acc_lm_train
sqrt(mean((test_data$pm-predictlinear_test)^2)) 

#-----------------#Ridge model
colnames(train_data)
trainX <-as.matrix(train_data[,-9])
trainY <- (train_data[,9])
testX <- as.matrix(test_data[,-9])
testY <- test_data[,9]
ridge <- cv.glmnet(trainX,trainY,alpha=0,type.measure = 'mse',family='gaussian')
predridge <- predict(ridge,s=ridge$lambda.1se,testX)
rmse(testY,predridge)
cor(predridge,testY)

#----------------#Lasso model
lasso <- cv.glmnet(trainX,trainY,alpha=1,type.measure = 'mse',family='gaussian')
predlasso <- predict(lasso,s=lasso$lambda.1se,testX)
cor(predlasso,testY)
rmse(predlasso,testY)

#----------------#Elastic-net
listofEnet <- list()
for(i in 0:10){
  name <- paste0("alpha",i/10)
  listofEnet[[name]] <- cv.glmnet(trainX,trainY,type.measure = 'mse',alpha=i/10,family='gaussian')
}
resultEnet <- data.frame()
for(i in 0:10){
  name <- paste0("alpha",i/10)
  predEnet <- predict(listofEnet[[name]],s=listofEnet[[name]]$lambda.1se,testX)
  RMSE=rmse(testY,predEnet)
  accEnet = cor(predEnet,testY)
  temp <- data.frame(alpha=i/10,RootMeanSqrError=RMSE,Name=name,Acc=accEnet)
  resultEnet <- rbind(resultEnet,temp)
}
resultEnet
plot(resultEnet$alpha,resultEnet$RootMeanSqrError,type = 'o')
plot(resultEnet$alpha,resultEnet$Acc,type = c('o'))

#----------------#decision tree 
set.seed(100)
dectree <- rpart(pm~.,data = train_data,method = 'anova',control = rpart.control(cp=0.01 ))
rpart.plot(dectree)
preddectree <- predict(dectree,test_data)
cor(preddectree,test_data$pm)
rmse(test_data$pm,preddectree)

#---------------#Random forest
set.seed(100)
randfor <- ranger(pm~.,data = train_data, num.trees = 100,mtry = 4,importance = 'none')
randfor
predrandfor <- predict(randfor,test_data)
predrandfor_test <- predict(randfor,motor_test)
test_prediction <- as.data.frame(predrandfor_test$predictions)
names(test_prediction)[names(test_prediction) == "predrandfor_test$predictions"] <- "prediction1"


cor(predrandfor$predictions,test_data$pm)                  
rmse(test_data$pm,predrandfor$predictions)
plot(test_data$pm,predrandfor$predictions)
write.table(test_prediction,"test_prediction.csv",sep=",")

#---------------#xgboost
library(xgboost)
set.seed(100)
xg <- xgboost(data = as.matrix(train_data[,-9]),label = as.matrix(train_data$pm ),nrounds = 20)
predxg <- predict(xg,newdata = as.matrix(test_data[,-9]))
cor(predxg,test_data$pm)    
rmse(predxg,test_data$pm)   
plot(predxg,test_data$pm)

#--------------#Earth
library(earth)
set.seed(100)
earthmodel <- earth(train_data[,-9],train_data[,9],nk=200,degree = 2)
plot(earthmodel)
earth_plotmodsel(earthmodel)
summary(earthmodel,decomp="none")
predmars <- predict(earthmodel,test_data)
rmse(predmars,test_data$pm)
cor(predmars,test_data$pm)

#-------------#GBM
library(gbm)
gbmmodel <- gbm(pm~.,data = train_data,n.trees = 100)
predgbm <- predict.gbm(gbmmodel,test_data,single.tree = F,n.trees = 100)
rmse(predgbm,test_data$pm)
cor(predgbm,test_data$pm)

#------------#H2o NN model
library(h2o)
h2o.init()
h2o.train <- as.h2o(train_data)
h2o.test <- as.h2o(test_data)
h2o.model <- h2o.deeplearning(x = setdiff(names(train_data),c("pm")),y = "pm",training_frame=h2o.train,nesterov_accelerated_gradient = T, epochs = 10,
                              standardize = FALSE,hidden = c(50, 50), rate = 0.05,seed = 1)
h2o.prediction <- h2o.predict(h2o.model,h2o.test)%>%as.data.frame()
cor(h2o.prediction$predict,test_data$pm)
rmse(h2o.prediction$predict,test_data$pm)

#------------#H2o random forest model
h2o.rf <- h2o.randomForest(x=setdiff(names(train_data),c("pm")),y="pm",training_frame = h2o.train,ntrees = 50,seed = 1,mtries = 4)
h2o.rfprediction <- h2o.predict(h2o.rf,h2o.test)%>%as.data.frame()
cor(h2o.rfprediction$predict,test_data$pm)
rmse(h2o.rfprediction$predict,test_data$pm)

#------------#H2o GBM
h2o.gbmodel <- h2o.gbm(x=setdiff(names(train_data),c("pm")),y="pm",training_frame = h2o.train,seed = 1,learn_rate = 0.05,ntrees = 100)
h2o.gbmprediction <- h2o.predict(h2o.gbmodel,h2o.test)%>%as.data.frame()
cor(h2o.gbmprediction$predict,test_data$pm)
rmse(h2o.gbmprediction$predict,test_data$pm)

#-----------#H2o automl
h2oautoml <- h2o.automl(x=setdiff(names(train_data),c("pm")),y="pm",training_frame = h2o.train,stopping_metric = "RMSE",max_runtime_secs = 30)
h2oautoleader <- h2oautoml@leader
h2o.performance(h2oautoleader,h2o.test)
predh2oautoml <- predict(h2oautoleader,h2o.test)%>%as.data.frame()
cor(predh2oautoml$predict,test_data$pm)
rmse(predh2oautoml$predict,test_data$pm)


save(linear,file = "linear.rda")
save(xg,file = "xg.rda")

