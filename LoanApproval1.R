rm(list=ls())
df=read.csv("./loan_prediction.csv")

summary(df)
table(is.na(df))
str(df)


getmode = function(v) {
  uniqv = unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

columns=list('Gender','Married','Education','Self_Employed','Dependents')
for(i in columns){
  for( j in 1:nrow(df)){
    if(df[j,i]==""){
      df[j,i]=getmode(df[,i])
    }
  }
}


################################Replacing NA with mean##########################
#################################For numeric variables##########################


for(i in 7:9) 
{
  for(j in 1:nrow(df)) 
  {
    if(is.na(df[j,i])==TRUE)
    {
      t = mean(df[,i],na.rm = TRUE) 
      df[j,i] = t 
    }
  }
  
}


for(i in 10:11) 
{
  for(j in 1:nrow(df)) 
  {
    if(is.na(df[j,i])==TRUE)
    {
      t = median(df[,i],na.rm = TRUE) 
      df[j,i] = t 
    }
  }
  
}

table(is.na(df))

#install.packages("mice")
#install.packages("vim")
#install.packages("plyr")

#################################DATA Exploratory Analysis######################
library(plyr)
df$Dependents=revalue(df$Dependents,c("3+"="3"))


df$Gender=ifelse(df$Gender=='Male',1,0)
df$Married=ifelse(df$Married=='Yes',1,0)
df$Education=ifelse(df$Education=='Graduate',1,0)
df$Self_Employed=ifelse(df$Self_Employed=='Yes',1,0)
df$Property_Area=as.factor(df$Property_Area)
df$Property_Area=unclass(df$Property_Area)
df$Dependents=as.factor(df$Dependents)
df$Dependents=unclass(df$Dependents)
df$Dependents=as.numeric(df$Dependents)
df$Property_Area=as.numeric(df$Property_Area)
df$Loan_Status=ifelse(df$Loan_Status=='Y',1,0)

#####################COrrelation plot
library(corrplot)
M=cor(df[2:12])
corrplot(M,method="number")
str(df)

####################Scatter of all predictors
plot(df[c(-1,-13)])

###########################BOX & Histogram

par(mfrow=c(3,2))
for(i in 7:9)
{
  boxplot(df[,i],ylab=colnames(df[i]),horizontal=TRUE,col="green",notch = TRUE) 
  abline(h=mean(df[,i]))
  hist(df[,i],breaks = 100,col="red",xlab=colnames(df[i]),main=paste(names(df[i])))
}

######################################BAR PLOTS#################################


par(mfrow=c(2,3))
counts = table(df$Loan_Status, df$Gender)
barplot(counts, main="Loan Status by Gender",
        xlab="Gender", col=c("grey","blue"),
        legend = rownames(counts))
counts2 = table(df$Loan_Status, df$Education)
barplot(counts2, main="Loan Status by Education",
        xlab="Education", col=c("grey","blue"),
        legend = rownames(counts2))
counts3 = table(df$Loan_Status, df$Married)
barplot(counts3, main="Loan Status by Married",
        xlab="Married", col=c("grey","blue"),
        legend = rownames(counts3))
counts4 = table(df$Loan_Status, df$Self_Employed)
barplot(counts4, main="Loan Status by Self Employed",
        xlab="Self_Employed", col=c("grey","blue"),
        legend = rownames(counts4))
counts5 = table(df$Loan_Status, df$Property_Area)
barplot(counts5, main="Loan Status by Property_Area",
        xlab="Property_Area", col=c("grey","blue"),
        legend = rownames(counts5))
counts6 = table(df$Loan_Status, df$Credit_History)
barplot(counts6, main="Loan Status by Credit_History",
        xlab="Credit_History", col=c("grey","blue"),
        legend = rownames(counts5))

counts6 = table(df$Loan_Status==1, df$Loan_Status==0)
barplot(counts6, main="Loan Status",
        xlab="Loan Approvals",  ylab = "Count",col=c("grey","blue"),beside = TRUE
)
legend("topright",
       c("Rejected","Approved"),
       fill = c("grey","blue"),cex = 1,bty= "n"
)


#############################Data Splitting for CV##############################


flag_fn=function(percent,rows)
{
  s=c()
  for(i in 1:rows)
  {
    if(runif(1)<=percent)
    {
      s[i]=1
    }
    else
    {
      s[i]=0
    }
  }
  s
}

holdout=0.75
df$Property_Area=as.factor(df$Property_Area)
F=flag_fn(holdout,nrow(df))
tempdata=cbind(df,F)

train=subset(tempdata,F==1)[,1:length(df)]
test=subset(tempdata,F==0)[,1:length(df)]

X.train=subset(train,select=-c(Loan_Status))
X.test=subset(test,select=-c(Loan_Status))

Y.train=train$Loan_Status
Y.test=test$Loan_Status

##############################Logistic Regression###############################
set.seed(1)
model1=glm(Loan_Status~Gender+Married+Dependents+Education+Self_Employed+ApplicantIncome+CoapplicantIncome+
  LoanAmount+Loan_Amount_Term+Credit_History+Property_Area, data = train, family = "binomial")

summary(model1)

predict.mod1.train=predict(model1,X.train,type="response")
model1.bin.train=ifelse(predict.mod1.train>0.60,1,0)
table(Y.train,model1.bin.train)

predict.mod1.test=predict(model1,X.test,type="response").
model1.bin.test=ifelse(predict.mod1.test>0.6,1,0)
table(Y.test,model1.bin.test)

step.model1=step(model1,direction ="backward",k=3.84,family="binomial")
summary(step.model1)


predict.smod1.train=predict(step.model1,X.train,type="response")
model1.bin.train=ifelse(predict.smod1.train>0.6,1,0)
table(Y.train,model1.bin.train)

predict.smod1.test=predict(step.model1,X.test,type="response")
model1.bin.test=ifelse(predict.smod1.test>0.6,1,0)
table(Y.test,model1.bin.test)



#####################################Decision Tree##############################
#install.packages("tree")
library(tree)

model2=tree(formula = Loan_Status~.-Loan_ID,data=train)
summary(model2)
plot(model2)

predict.mod2.train=predict(model2,X.train)
model2.bin.train=ifelse(predict.mod2.train>0.6,1,0)
table(Y.train,model2.bin.train)

predict.mod2.test=predict(model2,X.test)
model2.bin.test=ifelse(predict.mod2.test>0.6,1,0)
table(Y.test,model2.bin.test)

cv.model2=cv.tree(model2)
summary(cv.model2)
plot(cv.model2)
cv.model2$dev
cv.model2$size

plot(cv.model2$size,cv.model2$dev)
plot(cv.model2$k,cv.model2$dev)

prune.model2=prune.tree(model2,best = 3)
plot(prune.model2)
summary(prune.model2)

predict.mod2.train=predict(model2,X.train)
model2.bin.train=ifelse(predict.mod2.train>0.6,1,0)
table(Y.train,model2.bin.train)

predict.mod2.test=predict(model2,X.test)
model2.bin.test=ifelse(predict.mod2.test>0.6,1,0)
table(Y.test,model2.bin.test)


################################Random Forest###################################
install.packages("tidyverse")
install.packages("ISLR")
install.packages("randomForest")
install.packages("caret")
install.packages("rpart")
install.packages("rpart.plot")

library(tidyverse)
library(ISLR)
library(randomForest)
library(caret)
library(rpart)
library(rpart.plot)
head(df)
summary(train[-13,-1])
head(X.train[-13,-1])
rf_model1 = randomForest(x=X.train[-1],
                          y=Y.train,
                          importance=TRUE,
                          na.action=na.exclude, ntree = 500, mtry = 3)
summary(rf_model1)
?randomForest
varImpPlot(rf_model1)
validate_preds = predict(rf_model1, newdata=X.test)
head()

validate_preds
length(validate_preds)
pred_y = vector()

length(validate_preds)


for(i in 1:length(validate_preds)){
  if  (validate_preds[i] >= 0.6){
    
    pred_y[i] = 1
  } else {
    pred_y[i] = 0
  }
}

pred_y
pred_y = factor(pred_y) 
Y.test = factor(Y.test)
# build confusion matrix from predictions
confusionMatrix(pred_y, Y.test)

predict.mod3.train=predict(rf_model1,X.train)
model3.bin.train=ifelse(predict.mod3.train>0.6,1,0)
table(Y.train,model3.bin.train)
