#Load libraries
library(dplyr)
library(psych)
library(glmnet)  
library(caret)
library(vip)
library(randomForest)
library(stringr)
library(mgsub)

#Data cleaning
#Missing values
sapply(laptop_price,function(x) sum(is.na(x)))

#Remove laptop_ID column
laptop_price=subset(laptop_price, select=-c(laptop_ID))

#Number of duplicate rows
sum(duplicated(laptop_price))

#Remove duplicates
lap=distinct(laptop_price)
View(lap)

#Feature engineering & preprocessing
#1.ScreenResolution column
#1.1.Extract IPS column
for(i in 1:length(lap$ScreenResolution)){
  if(str_detect(lap$ScreenResolution[i], "IPS")){
    lap$IPS[i]='Yes'
  }else{
    lap$IPS[i]='No'
  }
}

#1.2.Extract Touch screen information
for(i in 1:length(lap$ScreenResolution)){
  if(str_detect(lap$ScreenResolution[i], "Touchscreen")){
    lap$TouchScreen[i]='Yes'
  }else{
    lap$TouchScreen[i]='No'
  }
}

#1.3.Extract X-axis and Y-axis screen resolution dimensions
lap$Xres=as.integer(substr(word(lap$ScreenResolution,-1),1,4))
lap$Yres=as.integer(substr(word(lap$ScreenResolution,-1),6,9))

#1.4.Calculate PPI- pixels per inch  
lap$PPI=sqrt((lap$Xres^2)+(lap$Yres^2))/lap$Inches

#2.CPU column
#2.1.Extract CPU brand name
for(i in 1:length(lap$Cpu)){
  if(str_detect(lap$Cpu[i], "AMD")){
    lap$CpuBrand[i]='AMD Processor'
  }else if (str_detect(lap$Cpu[i], "i3")){
    lap$CpuBrand[i]='Intel Core i3'
  }else if (str_detect(lap$Cpu[i], "i5")){
    lap$CpuBrand[i]='Intel Core i5'
  }else if (str_detect(lap$Cpu[i], "i7")){
    lap$CpuBrand[i]='Intel Core i7'
  }else{
    lap$CpuBrand[i]='Other Intel Processor'
  }
}

#2.2.Extract clock speed
lap$ClockSpeed=word(lap$Cpu,-1)
lap$ClockSpeed=as.numeric(gsub('GHz','',as.character(lap$ClockSpeed)))

#3.Ram column 
#3.1.Remove units in Ram & convert to integer
lap$Ram=as.integer(gsub('GB','',as.character(lap$Ram)))

#4.Memory column 
lap$M= strsplit(lap$Memory, split = " ")

#4.1.Create SSD column
for(i in 1:length(lap$Memory)){
  if((str_count(lap$Memory[i], "SSD")==1) & 
     (str_count(lap$Memory[i], "HDD")==1)){
    lap$SSD[i]= as.integer(mgsub::mgsub(as.character(lap$M[[i]][1]), c("GB", "TB"), 
                                        c(" ", "000"))) 
  }else if(str_count(lap$Memory[i], "SSD")==1){
    lap$SSD[i]= as.integer(mgsub::mgsub(as.character(lap$M[[i]][1]), c("GB", "TB"), 
                                        c(" ", "000"))) 
  }else if(str_count(lap$Memory[i], "SSD")==2){
    lap$SSD[i]= as.integer(mgsub::mgsub(as.character(lap$M[[i]][1]), c("GB", "TB"), 
                                        c(" ", "000")))*2
  }else{
    lap$SSD[i]=0
  }
}

#4.2.Create HDD column
for(i in 1:length(lap$Memory)){
  if((str_detect(lap$Memory[i], "SSD")) & (str_detect(lap$Memory[i], "HDD"))){
    lap$HDD[i]=  as.integer(mgsub::mgsub(as.character(lap$M[[i]][5]), 
                                         c("GB", "TB"), c(" ", "000")))
  }else if(str_detect(lap$Memory[i], "HDD")){
    lap$HDD[i]=  as.integer(mgsub::mgsub(as.character(lap$M[[i]][1]), 
                                         c("GB", "TB"), c(" ", "000")))
  }else{
    lap$HDD[i]=0
  }
}


#4.3.Create FlashStorage column
for(i in 1:length(lap$Memory)){
  if(str_detect(lap$Memory[i], "Flash")){
    lap$FlashStorage[i]=  as.integer(mgsub::mgsub(as.character(lap$M[[i]][1]), 
                                                  c("GB", "TB"), c(" ", "000")))
  }else{
    lap$FlashStorage[i]=0
  }
}

#4.4.Create Hybrid column
for(i in 1:length(lap$Memory)){
  if((str_detect(lap$Memory[i], "SSD")) & (str_detect(lap$Memory[i], "Hybrid"))){
    lap$Hybrid[i]=  as.integer(mgsub::mgsub(as.character(lap$M[[i]][5]), 
                                            c("GB", ".0TB"), c(" ", "000")))
  }else if(str_detect(lap$Memory[i], "Hybrid")){
    lap$Hybrid[i]= as.integer(mgsub::mgsub(as.character(lap$M[[i]][1]), 
                                           c("GB", ".0TB"), c(" ", "000"))) 
  }else{
    lap$Hybrid[i]=0
  }
}

#5.GPU column
#5.1.Exract Gpu Brand
lap$GpuBrand=word(lap$Gpu,1)

#6.OpSys column 
#6.1.Transform opSys
for(i in 1:length(lap$OpSys)){
  if(str_detect(lap$OpSys[i], "Windows")){
    lap$OpSys[i]='Windows'
  }else if (str_detect(lap$OpSys[i], "macOS")){
    lap$OpSys[i]='Mac'
  }else if (str_detect(lap$OpSys[i], "Mac OS X")){
    lap$OpSys[i]='Mac'
  }else if(str_detect(lap$OpSys[i], "Linux")){
    lap$OpSys[i]='Linux'
  }else if(str_detect(lap$OpSys[i], "No OS")){
    lap$OpSys[i]='No OS'
  }else{
    lap$OpSys[i]='Others'
  } 
}

#7.Weight Column
#7.1.Remove units in Weight & convert to numeric
lap$Weight=as.numeric(gsub('kg','',as.character(lap$Weight)))

#Remove unnecessary variables
lap=subset(lap, select=-c(ScreenResolution,Cpu,Memory,
                          Gpu,M,Xres,Yres,Product,Inches))

#Convert into Factors
lap$OpSys = as.factor(lap$OpSys)
lap$IPS = as.factor(lap$IPS)
lap$TouchScreen = as.factor(lap$TouchScreen)
lap$CpuBrand = as.factor(lap$CpuBrand)
lap$Company = as.factor(lap$Company)
lap$TypeName = as.factor(lap$TypeName)
lap$GpuBrand = as.factor(lap$GpuBrand)

str(lap)

#Divide data set into training and testing
set.seed(100)
indexes = sample(1:nrow(lap),size=0.2*nrow(lap))
testset=lap[indexes,]
trainset=lap[-indexes,]

#Analysis
#Univariate Analysis
# 1) Distribution of price
ggplot(trainset, aes(x=Price_euros)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white",bins=30)+
  geom_density(alpha=0.2, fill="blue") +
  labs(x="Price(Euros)", y = "Density")+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))+
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background=element_blank(), axis.line = element_line(colour = "black")
  )
  

# 2) Bar Plot - Company
ggplot(trainset, aes(x=factor(Company)))+
  geom_bar(stat="count", width=0.7, alpha=0.3, fill="blue")+
  geom_text(aes(label = ..count..), stat = "count",vjust = -0.2)+
  labs(x="Company", y = "Count")+
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), axis.line = element_line(colour = "black")
  )
  
#Bivariate Analysis
# 1) Scatter Plot- PPI vs Price
ggplot(trainset, aes(x = PPI, y = Price_euros)) +
  geom_point(alpha=0.3,colour = "blue")+
  labs(x="PPI", y = "Price(Euros)")+
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background=element_blank(), axis.line = element_line(colour = "black")
  )

# 2) Bar Plot- Memory vs Average Price
avg_price_SSD=mean(trainset$Price_euros[trainset$SSD!=0])
avg_price_HDD=mean(trainset$Price_euros[trainset$HDD!=0])
avg_price_FS=mean(trainset$Price_euros[trainset$FlashStorage!=0])
avg_price_HB=mean(trainset$Price_euros[trainset$Hybrid!=0])

mem=data.frame(Memory=c('SSD','HDD','FlashStorage','Hybrid'),
               AvgPrice=c(avg_price_SSD,avg_price_HDD,avg_price_FS,avg_price_HB))

ggplot(mem, aes(x = Memory, y =AvgPrice))+
  geom_bar(stat= 'identity' , alpha=0.2, fill="blue")+
  geom_text(aes(label = round(AvgPrice,2), vjust = -0.2))+
  labs( x="Memory", y = "Average Price(Euros)")+
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), axis.line = element_line(colour = "black")
  )
   
# 3) Boxplot- Company vs Price
ggplot(trainset, aes(x=Company, y=Price_euros)) + 
  geom_boxplot(alpha=0.2,fill='blue')+
  stat_summary(fun.y=mean, geom="point", shape=20, 
               size=4,col='dark blue',fill='dark blue')+
  labs(x="Company Name", y = "Price(Euros)")+
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), axis.line = element_line(colour = "black"),
  )
 
# 4) Boxplot-TypeName vs Price
ggplot(trainset, aes(x=TypeName, y=Price_euros)) + 
  geom_boxplot(alpha=0.2,fill='blue')+
  stat_summary(fun.y=mean, geom="point", shape=20, size=4, col='blue',fill='blue')+
  labs(x="Type Name", y = "Price(Euros)")+
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), axis.line = element_line(colour = "black")
  )

# 5) Bar Plot- Touch Screen vs Average Price
TS=c('No','Yes')
avg_price_TS=numeric(0)
for(i in 1:length(TS)){
  avg_price_TS[i]=mean(trainset$Price_euros[trainset$TouchScreen==TS[i]])
}
touchScreen=data.frame(TS,avg_price_TS)

ggplot(touchScreen, aes(x = TS, y =avg_price_TS))+
  geom_bar(stat= 'identity' , alpha=0.2, fill="blue")+
  geom_text(aes(label = round(avg_price_TS,2), vjust = -0.2))+
  labs(x="Touch Screen", y = "Average Price(Euros)")+
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), axis.line = element_line(colour = "black")
  )

# 6) Box Plot- CPU vs price
ggplot(trainset, aes(x=CpuBrand, y=Price_euros)) + 
  geom_boxplot(alpha= 0.2, fill='blue')+
  stat_summary(fun.y=mean, geom="point", shape=20, size=4, col='blue',fill='blue')+
  labs(x="Cpu", y = "Price(Euros)")+
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), axis.line = element_line(colour = "black")
  )

# 7) Boxplot- RAM vs price
ggplot(trainset, aes(x=as.character(Ram), y=Price_euros)) + 
  geom_boxplot(alpha=0.2, fill='blue')+
  stat_summary(fun.y=mean, geom="point", shape=20, size=4, col='blue',fill='blue')+
  labs(x="Ram(GB)", y = "Price(Euros)")+
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), axis.line = element_line(colour = "black")
  )
 
# 8) Boxplot- Operating system vs Price
ggplot(trainset, aes(x=OpSys, y=Price_euros)) + 
  geom_boxplot(alpha=0.2,fill='blue')+
  stat_summary(fun.y=mean, geom="point", shape=20, size=4, col='blue',fill='blue')+
  labs(x="Operating System", y = "Price(Euros)")+
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(), axis.line = element_line(colour = "black"),
  )
  
#Multivariate Analysis
#Advanced Analysis Techniques
#Extract only numeric variables
numericVar=subset(trainset, select=c(Price_euros,PPI,ClockSpeed,
                                     Ram,SSD,HDD,FlashStorage,
                                     Hybrid,Weight))
View(numericVar)
#corr matrix
corrplot(cor(numericVar),method='number',diag = FALSE, type='lower')
 
#log-price
ggplot(trainset, aes(x=log(Price_euros))) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white",bins=30)+
  geom_density(alpha=.2, fill="blue") +
  labs(x="Price(Euros)", y = "Density")+
  scale_x_continuous(expand = c(0, 0)) + scale_y_continuous(expand = c(0, 0))+
  theme(
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background=element_blank(), axis.line = element_line(colour = "black")
  )


#Check what are the rows containing outliers
#Extract only numeric variables
numericVar=subset(trainset, select=c(Price_euros,PPI,ClockSpeed,
                                     Ram,SSD,HDD,FlashStorage,
                                     Hybrid,Weight))
md = mahalanobis(numericVar, center = colMeans(numericVar), 
                 cov = cov(numericVar))
cutoff = (qchisq(p = 0.999, df = ncol(numericVar)))
names_outliers = which(md > cutoff)
names_outliers 
length(names_outliers)
 
##########################################
# Advanced analysis techniques

# omit outliers
excluded = names_outliers
data_clean = trainset[-excluded, ]

# Y4 = log(Price_euros) without outlier data points
# Center Y, X will be standardized in the modelling function
X = model.matrix(Price_euros ~.-1, data = data_clean)
testX = model.matrix(Price_euros ~.-1, data = testset)

# transform y with log transformation
Y = log(data_clean$Price_euros)
testY= log(testset$Price_euros)

# Ridge Regression
# Fitting
ridge <- glmnet(x = X, y = Y, alpha = 0)
#  Doing cross validation to select the best lambda
set.seed(1234)
ridgeCV <- cv.glmnet(x = X, y = Y,alpha = 0)
bestlam = ridgeCV$lambda.1se
bestlam
# coefficients
predict(ridge,type = "coefficients",s = bestlam)
# compute RMSE values
pred_train=predict(ridge, s=bestlam,newx=X)
RMSE(exp(pred_train), exp(Y))
pred_test=predict(ridge, s=bestlam,newx=testX)
RMSE(exp(pred_test), exp(testY))

# Lasso
# fitting
lasso = glmnet(x = X, y = Y, alpha = 1)
#  Doing cross validation to select the best lambda
# set.seed(1234)
lassoCV =  cv.glmnet(x = X, y = Y, alpha = 1)
lam.best=lassoCV$lambda.1se
lam.best
# coefficients
predict(lasso,type = "coefficients",s = lam.best)
# compute RMSE values
pred_train=predict(lasso, s=lam.best,newx=X)
RMSE(exp(pred_train), exp(Y))
pred_test=predict(lasso, s=lam.best,newx=testX)
RMSE(exp(pred_test), exp(testY))

#Enet
cv_glmnet <- train(
  x = X,
  y = Y,
  method = "glmnet",
  preProc = c("zv", "center", "scale"),
  trControl = trainControl(method = "cv", number = 10),
  tuneLength = 10
)

# model with lowest RMSE
cv_glmnet$bestTune
# results for model with lowest RMSE
cv_glmnet$results %>%
  filter(alpha == cv_glmnet$bestTune$alpha, 
         lambda == cv_glmnet$bestTune$lambda)
enet_model=glmnet(X,Y,alpha = cv_glmnet$bestTune$alpha,
                  lambda=cv_glmnet$bestTune$lambda,standardize = T)
# coefficients
predict(enet_model,type = "coefficients")

# compute RMSE values
pred_train=predict(enet_model,newx=X)
RMSE(exp(pred_train), exp(Y))
pred_test=predict(enet_model,newx=testX)
RMSE(exp(pred_test), exp(testY))


#random forest
set.seed(1234)
modelRF1=train( log(Price_euros) ~ ., data = data_clean, method= 'rf',
                ntree = 500,  trControl=trainControl(method = 'cv',number=10), 
                importance = TRUE )
modelRF1
#evaluate the model performance on the train set
sqrt(mean((data_clean$Price_euros - exp(predict(modelRF1, data_clean)))^2))
#evaluate the model performance on the test set
sqrt(mean((testset$Price_euros - exp(predict(modelRF1, testset)))^2))


# Y1 = Price_euros with outlier data points
# Center Y, X will be standardized in the modelling function
X1 = model.matrix(Price_euros ~.-1, data = trainset)
testX1 = model.matrix(Price_euros ~.-1, data = testset)

# Y
Y1 = trainset$Price_euros
testY1= testset$Price_euros

# Ridge Regression
# Fitting
ridge1 <- glmnet(x = X1, y = Y1, alpha = 0)
#  Doing cross validation to select the best lambda
set.seed(1234)
ridgeCV1 <- cv.glmnet(x = X1, y = Y1,alpha = 0)
bestlam1 = ridgeCV1$lambda.min
bestlam1
# coefficients
predict(ridge1,type = "coefficients",s = bestlam1)

# compute RMSE values
pred_train1=predict(ridge1, s=bestlam1,newx=X1)
RMSE( pred_train1,  Y1)
pred_test1=predict(ridge1, s=bestlam1,newx=testX1)
RMSE( pred_test1,  testY1)

# Lasso
# fitting
lasso1 = glmnet(x = X1, y = Y1, alpha = 1)
#  Doing cross validation to select the best lambda
# set.seed(1234)
lassoCV1 =  cv.glmnet(x = X1, y = Y1, alpha = 1)
lam.best1=lassoCV1$lambda.min
lam.best1
# coefficients
predict(lasso1,type = "coefficients",s = lam.best1)
# compute RMSE values
pred_train1=predict(lasso1, s=lam.best1,newx=X1)
RMSE( pred_train1, Y1)
pred_test1=predict(ridge1, s=bestlam1,newx=testX1)
RMSE(pred_test1, testY1)

#Enet
cv_glmnet1 <- train(
  x = X1,
  y = Y1,
  method = "glmnet",
  preProc = c("zv", "center", "scale"),
  trControl = trainControl(method = "cv", number = 10),
  tuneLength = 10
)
# model with lowest RMSE
cv_glmnet1$bestTune
# results for model with lowest RMSE
cv_glmnet1$results %>%
  filter(alpha == cv_glmnet1$bestTune$alpha,
         lambda == cv_glmnet1$bestTune$lambda)
enet_model1=glmnet(X1,Y1,alpha = cv_glmnet1$bestTune$alpha,
                   lambda=cv_glmnet1$bestTune$lambda,standardize = T)
# coefficients
predict(enet_model1,type = "coefficients")
# compute RMSE values
pred_train1=predict(enet_model1,newx=X1)
RMSE(pred_train1,Y1)
pred_test1=predict(enet_model1,newx=testX1)
RMSE(pred_test1, testY1)

#random forest
set.seed(1234)
modelRF2=train( Price_euros ~ ., data = trainset, method= 'rf',
                ntree = 500,  trControl=trainControl(method = 'cv',number=10),
                importance = TRUE )
modelRF2 
#evaluate the model performance on the train set
sqrt(mean((trainset$Price_euros - predict(modelRF2, trainset))^2))
#evaluate the model performance on the test set
sqrt(mean((testset$Price_euros -  predict(modelRF2, testset))^2))


# Y2 = Price_euros without outlier data points
# Center Y, X will be standardized in the modelling function
X2 = model.matrix(Price_euros ~.-1, data = data_clean)
testX2 = model.matrix(Price_euros ~.-1, data = testset)

# transform y with log transformation
Y2 = data_clean$Price_euros
testY2= testset$Price_euros

# Ridge Regression
# Fitting
ridge2 <- glmnet(x = X2, y = Y2, alpha = 0)
#  Doing cross validation to select the best lambda
set.seed(1234)
ridgeCV2 <- cv.glmnet(x = X2, y = Y2,alpha = 0)
bestlam2 = ridgeCV2$lambda.min
bestlam2
# coefficients
predict(ridge2,type = "coefficients",s = bestlam2)

# compute RMSE values
pred_train2=predict(ridge2, s=bestlam2,newx=X2)
RMSE( pred_train2,  Y2)
pred_test2=predict(ridge2, s=bestlam2,newx=testX2)
RMSE( pred_test2,  testY2)

# Lasso
# fitting
lasso2 = glmnet(x = X2, y = Y2, alpha = 1)
#  Doing cross validation to select the best lambda
# set.seed(1234)
lassoCV2 =  cv.glmnet(x = X2, y = Y2, alpha = 1)
lam.best2=lassoCV2$lambda.min
lam.best2
# coefficients
predict(lasso2,type = "coefficients",s = lam.best2)
# compute RMSE values
pred_train2=predict(lasso2, s=lam.best2,newx=X2)
RMSE( pred_train2, Y2)
pred_test2=predict(ridge2, s=bestlam2,newx=testX2)
RMSE(pred_test2, testY2)

#Enet
cv_glmnet2 <- train(
  x = X2,
  y = Y2,
  method = "glmnet",
  preProc = c("zv", "center", "scale"),
  trControl = trainControl(method = "cv", number = 10),
  tuneLength = 10
)

# model with lowest RMSE
cv_glmnet2$bestTune
# results for model with lowest RMSE
cv_glmnet2$results %>%
  filter(alpha == cv_glmnet2$bestTune$alpha, 
         lambda == cv_glmnet2$bestTune$lambda)
enet_model2=glmnet(X2,Y2,alpha = cv_glmnet2$bestTune$alpha,
                   lambda=cv_glmnet2$bestTune$lambda,standardize = T)
# coefficients
predict(enet_model2,type = "coefficients")
# compute RMSE values
pred_train2=predict(enet_model2,newx=X2)
RMSE(pred_train2,Y2)
pred_test2=predict(enet_model2,newx=testX2)
RMSE(pred_test2, testY2)

#random forest
set.seed(1234)
modelRF3=train( Price_euros ~ ., data = data_clean, method= 'rf',
                ntree = 500,  trControl=trainControl(method = 'cv',number=10),
                importance = TRUE )
modelRF3
#evaluate the model performance on the train set
sqrt(mean((data_clean$Price_euros - predict(modelRF3, data_clean))^2))
#evaluate the model performance on the test set
sqrt(mean((testset$Price_euros -  predict(modelRF3, testset))^2))

# Y3 = log(Price_euros) with outlier data points 
# Center Y, X will be standardized in the modelling function
X3 = model.matrix(log(Price_euros) ~.-1, data = trainset)
testX3 = model.matrix(log(Price_euros) ~.-1 + ., data = testset)

# transform y with log transformation
Y3 =  log(trainset$Price_euros)
testY3=  log(testset$Price_euros)

# Ridge Regression
# Fitting
ridge3 <- glmnet(x = X3, y = Y3, alpha = 0)
#  Doing cross validation to select the best lambda
ridgeCV3 <- cv.glmnet(x = X3, y = Y3,alpha = 0)
bestlam3 = ridgeCV3$lambda.min
bestlam3
# coefficients
predict(ridge3,type = "coefficients",s = bestlam3)

# compute RMSE values
pred_train3=predict(ridge3, s=bestlam3,newx=X3)
RMSE(exp(pred_train3), exp(Y3))
pred_test3=predict(ridge3, s=bestlam3,newx=testX3)
RMSE(exp(pred_test3), exp(testY3))

# Lasso
# fitting
lasso3 = glmnet(x = X3, y = Y3, alpha = 1)
#  Doing cross validation to select the best lambda
lassoCV3 =  cv.glmnet(x = X3, y = Y3, alpha = 1)
lam.best3=lassoCV3$lambda.min
lam.best3
# coefficients
predict(lasso3,type = "coefficients",s = lam.best3)
# compute RMSE values
pred_train3=predict(lasso3, s=lam.best3,newx=X3)
RMSE(exp(pred_train3), exp(Y3))
pred_test3=predict(ridge3, s=bestlam3,newx=testX3)
RMSE(exp(pred_test3), exp(testY3))

#Enet
cv_glmnet3 <- train(
  x = X3,
  y = Y3,
  method = "glmnet",
  preProc = c("zv", "center", "scale"),
  trControl = trainControl(method = "cv", number = 10),
  tuneLength = 10
)

# model with lowest RMSE
cv_glmnet3$bestTune
# results for model with lowest RMSE
cv_glmnet3$results %>%
  filter(alpha == cv_glmnet3$bestTune$alpha, 
         lambda == cv_glmnet3$bestTune$lambda)
enet_model3=glmnet(X3,Y3,alpha = cv_glmnet3$bestTune$alpha,
                   lambda=cv_glmnet3$bestTune$lambda,standardize = T)
# coefficients
predict(enet_model3,type = "coefficients")
# compute RMSE values
pred_train3=predict(enet_model3,newx=X3)
RMSE(exp(pred_train3), exp(Y3))
pred_test3=predict(enet_model3,newx=testX3)
RMSE(exp(pred_test3), exp(testY3))

#random forest
set.seed(1234)
modelRF4=train( log(Price_euros) ~ ., data = trainset, method= 'rf',
                ntree = 500,  trControl=trainControl(method = 'cv',number=10), 
                importance = TRUE )
modelRF4
#evaluate the model performance on the train set
sqrt(mean((trainset$Price_euros - exp(predict(modelRF4, trainset)))^2))
#evaluate the model performance on the test set
sqrt(mean((testset$Price_euros - exp(predict(modelRF4, testset)))^2))
 
selectedModel=modelRF2
#MAPE
mean(abs((testset$Price_euros- predict(selectedModel,testset))/testset$Price_euros))
# Important Variables
vip(selectedModel,num_features = 14,aesthetics = list(color = "blue"))
# Save model to RDS file
saveRDS(selectedModel, "model.rds")
