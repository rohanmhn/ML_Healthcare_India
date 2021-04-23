getwd()
Xtrain<-read.csv("X_train.csv")
Ytrain<-read.csv("Y_train.csv")
Xtrain$State <- as.factor(Xtrain$State)
str(Xtrain$State)
summary(Xtrain)

Xtest<-read.csv("X_test.csv")
Ytest<-read.csv("Y_test.csv")
Xtest$State <- as.factor(Xtest$State)


############################################################
boxplot(Xtrain[c(136:144)], horizontal = T , cex.axis=.5)

hist(Xtrain$Population)

#setting parameters for above boxplot
axis(side = 2, labels = F)
axis(side=2, las = 2, mgp = c(3, 2, 0))
par("usr")
text(x = 1:length(Xtrain[3:8]),
     y = par("usr")[1] ,
     labels = names(Xtrain[3:8]),
     xpd = NA,
     ## Rotate the labels by 90 degrees.
     srt = 90,
     cex = 1)
########################################################

#breaking data into relevant connected features
Xtrain1<-Xtrain[3:8]
Xtrain2<-Xtrain[9:14]
Xtrain3<-Xtrain[15:17]
Xtrain4<-Xtrain[c(18,19,20,27:32)]
Xtrain5<-Xtrain[21:26]
Xtrain6<-Xtrain[33:38]
Xtrain7<-Xtrain[39:50]
Xtrain8<-Xtrain[51:63]
Xtrain9<-Xtrain[64:69]
Xtrain10<-Xtrain[70:84]
Xtrain11<-Xtrain[85:111]
Xtrain12<-Xtrain[112:123]
Xtrain13<-Xtrain[124:135]
Xtrain14<-Xtrain[136:144]


str(Xtrain5)
library(Hmisc)
describe(Xtrain7)

#########################################################

### Checking correlations
library(corrplot)
corr<-cor(Xtrain14)

corrplot.mixed(corr,tl.pos = "lt" ,tl.cex=.8, number.cex=.6)

#?corrplot.mixed

#########################################################

XtrainLRFever<-cbind(Xtrain,Ytrain$Acute.Fever.Tot)
## subset of features
XtrainLR<-Xtrain[c(1,5,8,10:12,13,15,17,19,20,21:26,
                   28,29,31,32,34,35,37,38,40,41,43,44,46,47,49,50,
                   51,54:58,60,61,64:68,71:77,79:81,83:88,91:93,95,
                   97:105,110,111,112,113,116:123,124:126,128,129,
                   132:135,136:142)]


### dummy Linear models

trainLR<- lm(formula=XtrainLRFever$`Ytrain$Acute.Fever.Tot`~., 
             data=XtrainLRFever)
summary(trainLR)

trainLR1.0<-lm(formula=XtrainLRFevertot$`Ytrain$Acute.Fever.Tot` ~., 
               data=XtrainLRFevertot)
summary(trainLR1.0)

#########################################################

# Extract Variable  importance VI scores from VIP package
install.packages("vip")
library(vip)
library(ggplot2)

backward <- step(trainLR, direction = "backward", trace = 0)
View(as.data.frame(vi(backward )))

## reduced subset of features
modifiedset <- Xtrain[c(5,11:13,17,23,25,26,34,43,60,100,110,112,121,124,135,136,
141,84,113,77,128,134,21,73,55,123,67,29,39,44,22,32,38,49,
15,31,79,54,41,105)]
XtrainLRFevertot<-cbind(modifiedset,Ytrain$Acute.Fever.Tot)

XtrainLRnew3<-Xtrain[c(5,11:12,17,23,25,26,100,110,112,135,136,
                       141,84,113,134,21,73,55,29,22,32,38)]
XtrainLRFevernew3<-cbind(XtrainLRnew3,Ytrain$Acute.Fever.Tot)


XtrainLRnew2<-Xtrain[c(5,11:13,17,23,25,26,34,43,60,100,110,112,121,124,135,136,
                      141,84,113,77,128,134,21,73,55,123,67,29,39,44,22,32,38,49,
                      15,31,79,54,41,105)]
XtrainLRFevernew2<-cbind(XtrainLRnew2,Ytrain$Acute.Fever.Tot)
describe(XtrainLRFevernew2)

XtrainLRnew<-Xtrain[c(5,11:13,17,23,25,26,
                   34,43,60,100,110,112,121,124,135,136,141)]
XtrainLRFevernew<-cbind(XtrainLRnew,Ytrain$Acute.Fever.Tot)
summary(XtrainLRFevernew)
str(XtrainLRFevernew)    

### dummy Linear model 2.0/3.0

trainLRnew3Norm<- lm(formula=XtrainLRFevernew3Norm$Fever ~., 
                 data=XtrainLRFevernew3Norm)
summary(trainLRnew3Norm)

trainLRnew2<- lm(formula=XtrainLRFevernew2$`Ytrain$Acute.Fever.Tot`~., 
                 data=XtrainLRFevernew2)
summary(trainLRnew2)

trainLRnew<- lm(formula=XtrainLRFevernew$`Ytrain$Acute.Fever.Tot`~., data=XtrainLRFevernew)
summary(trainLRnew)

##### Normalizing the Xlabel data
XtrainLRnew3Norm<-scale(XtrainLRnew3)
XtrainLRFevernew3Norm<-cbind(XtrainLRnew3Norm, "Fever"=Ytrain$Acute.Fever.Tot)
XtrainLRFevernew3Norm<-as.data.frame(XtrainLRFevernew3Norm)
is.null(XtrainLRFevernew3Norm)
describe(XtrainLRFevernew3Norm)
XtrainLRFevernew3Norm<-is.data.frame(XtrainLRFevernew3Norm)


XtrainLRnew2Norm<-scale(XtrainLRnew2)
XtrainLRFevernew2Norm<-cbind(XtrainLRnew2Norm, "Fever"=Ytrain$Acute.Fever.Tot)
XtrainLRFevernew2Norm<-as.data.frame(XtrainLRFevernew2Norm)
is.null(XtrainLRFevernew2Norm)
describe(XtrainLRFevernew2Norm)


##### Finding multi-collinearity####
install.packages("tidyverse")
library(tidyverse)
library(caret)
install.packages("car")
library(car)
vif(trainLRnew3Norm)
vif(trainLRnew2Norm)


#### finalizing data as per VIf
XtrainLRnew4<-Xtrain[c(5,11,17,23,25,100,110,112,135,136,
                       141,84,113,134,73,38)]
str(XtrainLRnew4)
XtrainLRFevernew4<-cbind(XtrainLRnew4,Ytrain$Acute.Fever.Tot)
#scaling
XtrainLRnew4Norm<-scale(XtrainLRnew4)
XtrainLRFevernew4Norm<-cbind(XtrainLRnew4Norm, "Fever"=Ytrain$Acute.Fever.Tot)
XtrainLRFevernew4Norm<-as.data.frame(XtrainLRFevernew4Norm)
is.null(XtrainLRFevernew4Norm)
describe(XtrainLRFevernew4Norm)
XtrainLRFevernew4Norm<-is.data.frame(XtrainLRFevernew4Norm)
#linear model 4.0
trainLRnew4Norm<- lm(formula=XtrainLRFevernew4Norm$Fever~., 
                     data=XtrainLRFevernew4Norm)
summary(trainLRnew4Norm)
predictions<-trainLRnew4Norm%>%predict(XtrainLRFevernew4Norm)
R2 = R2(predictions, XtrainLRFevernew4Norm$Fever)
RMSE = RMSE(predictions, XtrainLRFevernew4Norm$Fever)
print(R2)
print( RMSE)
#rechecking VIF
vif(trainLRnew4Norm)

i=1
for(i in 1:ncol(XtrainLRnew4Norm))
     {
     boxplot(XtrainLRnew4Norm[,i:(i+2)], horizontal = T )
     #jpeg(file = paste("plot", i, ".jpeg", sep=""))
     i=i+3
     dev.off()
}        


#######Outlier treatment required testing



################################ Building the model
####Model 4.0################
set.seed(800)
XtestLR<-Xtest[c(5,11,17,23,25,100,110,112,135,136,
                       141,84,113,134,73,38)]
str(XtestLR)
#XtestLR<-cbind(XtestLR,Ytest$Acute.Fever.Tot)
#scaling
XtestLRNorm<-scale(XtestLR)
XtestLRNorm<-cbind(XtestLR, "Fever"=Ytest$ACUTE.ILLNESS...Persons.suffering.from.Acute.Illness..Per.100.000.Population....Fever..All.Types....Person...Total)
XtestLRNorm<-as.data.frame(XtestLRNorm)
is.null(XtestLRNorm)
summary(XtestLRNorm)
str(XtestLRNorm)

install.packages("dplyr")
library(dplyr)
#linear model  predictions
predictions4.1<- trainLRnew4Norm%>%predict(XtestLRNorm)
predictions4.2<-trainLRnew4Norm%>%predict(XtrainLRFevernew4Norm)
# Model performance
data.frame(
        RMSE = RMSE(predictions4.1, XtestLRNorm$Fever),
        R2 = R2(predictions, XtestLRNorm$Fever)
)
data.frame(
        RMSE = RMSE(predictions4.2, XtrainLRFevernew4Norm$Fever),
        R2 = R2(predictions, XtrainLRFevernew4Norm$Fever)
)
str(XtrainLRFevernew4Norm)

####Model 3.0#####################################
#linear model  predictions
XtestLR3<-Xtest[c(5,11:12,17,23,25,26,100,110,112,135,136,
                 141,84,113,134,21,73,55,29,22,32,38)]
str(XtestLR3)
 
#scaling
XtestLRNorm3<-scale(XtestLR3)
XtestLRNorm3<-cbind(XtestLR3, "Fever"=Ytest$ACUTE.ILLNESS...Persons.suffering.from.Acute.Illness..Per.100.000.Population....Fever..All.Types....Person...Total)
XtestLRNorm3<-as.data.frame(XtestLRNorm3)
is.null(XtestLRNorm3)
summary(XtestLRNorm3)
str(XtestLRNorm3)
##model 3
#trainLRnew3Norm<- lm(formula=XtestLRNorm3$Fever ~., 
                     data=XtestLRNorm3)
summary(trainLRnew3Norm)

predictions3.1<- trainLRnew3Norm%>%predict(XtestLRNorm3)
predictions3.2<-trainLRnew3Norm%>%predict(XtrainLRFevernew3Norm)

# Model performance
data.frame(
        RMSE = RMSE(predictions3.1, XtestLRNorm3$Fever),
        R2 = R2(predictions3.1, XtestLRNorm3$Fever)
)


####Model 2.0#####################################
trainLRnew2<- lm(formula=XtrainLRFevernew2$`Ytrain$Acute.Fever.Tot`~., 
                 data=XtrainLRFevernew2)
summary(trainLRnew2)

####Model 2.0#####################################
trainLRnew<- lm(formula=XtrainLRFevernew$`Ytrain$Acute.Fever.Tot`~., data=XtrainLRFevernew)
summary(trainLRnew)

####Model 1.0#####################################
#linear model  predictions
XtestLR3<-Xtest[c(5,11:12,17,23,25,26,100,110,112,135,136,
                  141,84,113,134,21,73,55,29,22,32,38)]
str(XtestLR3)

#scaling
XtestLRNorm3<-scale(XtestLR3)
XtestLRNorm3<-cbind(XtestLR3, "Fever"=Ytest$ACUTE.ILLNESS...Persons.suffering.from.Acute.Illness..Per.100.000.Population....Fever..All.Types....Person...Total)
XtestLRNorm3<-as.data.frame(XtestLRNorm3)
is.null(XtestLRNorm3)
summary(XtestLRNorm3)
str(XtestLRNorm3)
##model 3
#trainLRnew3Norm<- lm(formula=XtestLRNorm3$Fever ~., 
data=XtestLRNorm3)
summary(trainLRnew3Norm)

predictions3.1<- trainLRnew3Norm%>%predict(XtestLRNorm3)
predictions3.2<-trainLRnew3Norm%>%predict(XtrainLRFevernew3Norm)

# Model performance
data.frame(
        RMSE = RMSE(predictions3.1, XtestLRNorm3$Fever),
        R2 = R2(predictions3.1, XtestLRNorm3$Fever)
)
############################### Outlier treatment between 5-95% ###

XtrainOutlr<-Xtrain
YtrainOutlr<-Ytrain
class(XtrainOutlr$State)
XtestOutlr<-Xtest
YtestOutlr<-Ytest
summary(XtrainOutlr)

#trying with one column
XtrainOutlr$Samp.Units[which(XtrainOutlr$Samp.Units 
                             >quantile(XtrainOutlr$Samp.Units, c(0.95)))]=quantile(XtrainOutlr$Samp.Units,c(0.95))
XtrainOutlr$Samp.Units[which(XtrainOutlr$Samp.Units 
                             <quantile(XtrainOutlr$Samp.Units, c(0.05)))]=quantile(XtrainOutlr$Samp.Units,c(0.05))

boxplot(XtrainOutlr$Households, horizontal = T, cex.axis=.9)
######
library(data.table)
outlierReplace = function(dataframe, cols, rows, newValue = NA) {
        if (any(rows)) {
                set(dataframe, rows, cols, newValue)
        }
}
outlierReplace(XtrainOutlr, 
               XtrainOutlr[c(3:144)], 
               which(XtrainOutlr[c(3:144)>quantile(XtrainOutlr[c(3:144)], c(0.95))]), 
               quantile(XtrainOutlr[c(3:144)],c(0.95))
               )
#######
for(i in range(ncol(XtrainOutlr))){
        if(class(XtrainOutlr[[i]])=="factor"){
                print("ok")
                i=i+1 
        }else{
                XtrainOutlr[[i]][which(XtrainOutlr[[i]]
                                             >quantile(XtrainOutlr[[i]], c(0.95)))]=quantile(XtrainOutlr[[i]],c(0.95))
                i=i+1
                }
        }
