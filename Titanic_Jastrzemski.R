##################################################################################################################
# Kaggle code created by Robert Jastrzemski
# adopted from the styles referenced in the tutorials 
#provided by Trevor Stephens and Kurt Wehlery

##################################################################################################################
# R Libraries

require(Amelia)
require(plyr)
require(dplyr)
require(graphics)
require(rpart)
require(plot.rpart)
require(tree)
require(pROC)
require(randomForest)
require(pryr)
require(robustbase)
require(caret)
require(FNN)
library(kknn)
library(QTLRel)
require(ggplot2)
require("MASS")
library(aod)
library(mice)
require(VIM)
library(lattice)
library(ggplot2)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
require(party)
require(nnet)
require(dummy)

###################################################################################################################
# To import csv data sources

Titanic.train<-read.csv("C:/Users/robj_01/Desktop/PRED-453/project1_Titanic/train.csv", header=T, dec=".", 
                        na.strings="n/a", stringsAsFactors=FALSE)
Titanic.test<-read.csv("C:/Users/robj_01/Desktop/PRED-453/project1_Titanic/test.csv", header=T, dec=".", 
                       na.strings="n/a", stringsAsFactors=FALSE)

typeof(Titanic.train)
is.data.frame(Titanic.train)

typeof(Titanic.test)
is.data.frame(Titanic.test)

###################################################################################################################
#Exploratory Data Analysis

  ##string details
    str(Titanic.train)
    str(Titanic.test)
  
  ##data summary
    summary(Titanic.train)
    summary(Titanic.test)
    
  ##view missing attributes
     missmap(Titanic.train, main="Titanic Training Data - Missing Values Mapped", 
            col=c("red", "black"), legend=FALSE)
     
  ##View survival rate per variable
     
     barplot(table(Titanic.train$Survived),
             names.arg = c("Fatality", "Survived"),
             main="Passenger Fate", col="blue")
     barplot(table(Titanic.train$Pclass), 
             names.arg = c("first", "second", "third"),
             main="Pclass (passenger class)", col="red")
     barplot(table(Titanic.train$Sex), main="Sex (fate by gender)", col="darkviolet")
     hist(Titanic.train$Age, main="Age", xlab = NULL, col="yellow")
     barplot(table(Titanic.train$SibSp), main="SibSp (siblings and spouse aboard)", 
             col="blue")
     barplot(table(Titanic.train$Parch), main="Parch (parents and children aboard)", 
             col="gray50")
     hist(Titanic.train$Fare, main="Fare", xlab = NULL, 
          col="darkgreen")
     
     mosaicplot(Titanic.train$Pclass ~ Titanic.train$Survived, 
                main="Passenger Fate by Traveling Class", shade=FALSE, 
                color=TRUE, xlab="Pclass", ylab="Survived")
     
     mosaicplot(Titanic.train$Sex ~ Titanic.train$Survived, 
                main="Passenger Fate by Gender", shade=FALSE, color=TRUE, 
                xlab="Sex", ylab="Survived")

     boxplot(Titanic.train$Age ~ Titanic.train$Survived, 
             main="Passenger Fate by Age",
             xlab="Survived", ylab="Age")
  
  ##view survived vs perished count
    table(Titanic.train$Survived)
  
  ##view survived vs perished proportion
    prop.table(table(Titanic.train$Survived))
  
  ##view gender of survivors vs fatalities
    prop.table(table(Titanic.train$Sex, Titanic.train$Survived),1)
    
  ##create child variable
    Titanic.train$Child <- 0
    Titanic.train$Child[Titanic.train$Age < 18] <- 1

  ## view child survival rate
    aggregate(Survived ~ Child + Sex, data=Titanic.train, FUN=function(x) {sum(x)/length(x)})

  ##create fare range variable
    Titanic.train$Fare2 <- '30+'
    Titanic.train$Fare2[Titanic.train$Fare < 30 & Titanic.train$Fare >= 20] <- '20-30'
    Titanic.train$Fare2[Titanic.train$Fare < 20 & Titanic.train$Fare >= 10] <- '10-20'
    Titanic.train$Fare2[Titanic.train$Fare < 10] <- '<10'

  ## view survival rate based on fares paid
    aggregate(Survived ~ Fare2 + Pclass + Sex, data=Titanic.train, FUN=function(x) {sum(x)/length(x)})

  
##################################################################################################################

# Missing Attributes Imputation

  ##rpart

      ###age

          ####indicator

            Titanic.train$Age[is.na(Titanic.train$Age)]
    
          ###relationship between missing values
            cor(Age)

          ####fill in missing age

            predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked,
                                   data = Titanic.train[!is.na(Titanic.train$Age),], method = "anova")
            Titanic.train$Age[is.na(Titanic.train$Age)] <- predict(predicted_age, Titanic.train[is.na(Titanic.train$Age),])

      ###number of family members with
            
          ####indicator
            
            Titanic.train$SibSp[is.na(Titanic.train$SibSp)]
            
          ####

            predicted_SibSp <- rpart(SibSp ~ Pclass + Sex + Age + Parch + Fare + Embarked + Ticket,
                                   data = Titanic.train[!is.na(Titanic.train$SibSp),], method = "anova")
            Titanic.train$SibSp[is.na(Titanic.train$SibSp)] <- predict(predicted_SibSp, Titanic.train[is.na(Titanic.train$SibSp),])

      ###parchments
      
            predicted_Parch <- rpart(Parch ~ Pclass + Sex + Age + SibSp + Fare + Embarked + Ticket,
                                     data = Titanic.train[!is.na(Titanic.train$Parch),], method = "anova")
            Titanic.train$Parch[is.na(Titanic.train$Parch)] <- predict(predicted_Parch, Titanic.train[is.na(Titanic.train$Parch),])
            
      ###fare
      
            predicted_Fare <- rpart(Fare ~ Pclass + Sex + Age + SibSp + Parch + Embarked + Ticket,
                                     data = Titanic.train[!is.na(Titanic.train$Fare),], method = "anova")
            Titanic.train$Fare[is.na(Titanic.train$Fare)] <- predict(predicted_Fare, Titanic.train[is.na(Titanic.train$Fare),])

      ###embarked
            which(Titanic.train$Embarked == '')

            summary(Titanic.train)

            Titanic.train$Embarked[c(62,830)] = "S"
            Titanic.train$Embarked <- factor(Titanic.train$Embarked)
            
            summary(Titanic.train$Embarked == '')
            
      ###title

            Titanic.train$Name <- as.character(Titanic.train$Name)
            
             Titanic.train$title <- sapply(Titanic.train$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]}) 
             Titanic.train$title <- sub(' ', '', Titanic.train$title) 
             # Inspect new feature 
             table(Titanic.train$title) 
             # Combine small title groups 
             Titanic.train$title[Titanic.train$title %like% c('Don')] <- 'Sir' 
             Titanic.train$title[Titanic.train$title %like% c('Dr')] <- 'Sir'
             Titanic.train$title[Titanic.train$title %like% c('Rev')] <- 'Sir'
             Titanic.train$title[Titanic.train$title %like% c('Col')] <- 'Sir'
             Titanic.train$title[Titanic.train$title %like% c('Major')] <- 'Sir'
             Titanic.train$title[Titanic.train$title %like% c('the Countess')] <- 'Lady' 
             Titanic.train$title[Titanic.train$title %like% c('Jonkheer')] <- 'Lady'
             Titanic.train$title[Titanic.train$title %like% c('Mme')] <- 'Lady'
             Titanic.train$title[Titanic.train$title %like% c('Mlle')] <- 'Lady'
             Titanic.train$title[Titanic.train$title %like% c('Ms')] <- 'Mrs'
             # Convert to a factor 
             Titanic.train$title <- factor(Titanic.train$title) 
             table(Titanic.train$title)
     
      ###family size       
            
            Titanic.train$FamilySize <- Titanic.train$SibSp + Titanic.train$Parch + 1
            
            table(Titanic.train$FamilySize)


      summary(Titanic.train)


# Missing Attributes Imputation (test)

##rpart

###age

####indicator

AgeParse <-Titanic.test$Age
ageDummy <-as.integer(complete.cases(AgeParse))
table(ageDummy)

####

summary(Titanic.test$Age)

predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked,
                       data = Titanic.train[!is.na(Titanic.test$Age),], method = "anova")
Titanic.test$Age[is.na(Titanic.test$Age)] <- predict(predicted_age, Titanic.train[is.na(Titanic.test$Age),])

summary(Titanic.test$Age)
####

summary(Titanic.test$SibSp)

predicted_SibSp <- rpart(SibSp ~ Pclass + Sex + Age + Parch + Fare + Embarked,
                         data = Titanic.train[!is.na(Titanic.test$SibSp),], method = "anova")
Titanic.test$SibSp[is.na(Titanic.test$SibSp)] <- predict(predicted_SibSp, Titanic.train[is.na(Titanic.test$SibSp),])

summary(Titanic.test$SibSp)

###parchments

summary(Titanic.test$Parch)

predicted_Parch <- rpart(Parch ~ Pclass + Sex + Age + SibSp + Fare + Embarked,
                         data = Titanic.train[!is.na(Titanic.test$Parch),], method = "anova")
Titanic.test$Parch[is.na(Titanic.test$Parch)] <- predict(predicted_Parch, Titanic.train[is.na(Titanic.test$Parch),])

summary(Titanic.test$Parch)

###fare

summary(Titanic.test$Fare)

predicted_Fare <- rpart(Fare ~ Pclass + Sex + Age + SibSp + Parch + Embarked,
                        data = Titanic.train[!is.na(Titanic.test$Fare),], method = "anova")
Titanic.test$Fare[is.na(Titanic.test$Fare)] <- predict(predicted_Fare, Titanic.train[is.na(Titanic.test$Fare),])

summary(Titanic.test$Fare)

which(Titanic.test$Embarked == '')

table(Titanic.test$Ticket)
Titanic.test$ticketPrefix <- sapply(Titanic.test$Ticket, FUN=function(x) {strsplit(x, split='/ ')[1]})
summary(Titanic.test$ticketPrefix)

summary(Titanic.train)

summary(Titanic.test$Embarked == '')

Titanic.test$Embarked[c(62,830)] = "S"
Titanic.test$Embarked <- factor(Titanic.test$Embarked)

Titanic.test$Name <- as.character(Titanic.test$Name)

Titanic.test$title <- sapply(Titanic.test$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]}) 
Titanic.test$title <- sub(' ', '', Titanic.test$title) 
# Inspect new feature 
table(Titanic.test$title) 
# Combine small title groups 
Titanic.test$title[Titanic.test$title %like% c('Don')] <- 'Sir' 
Titanic.test$title[Titanic.test$title %like% c('Dr')] <- 'Sir'
Titanic.test$title[Titanic.test$title %like% c('Rev')] <- 'Sir'
Titanic.test$title[Titanic.test$title %like% c('Col')] <- 'Sir'
Titanic.test$title[Titanic.test$title %like% c('Major')] <- 'Sir'
Titanic.test$title[Titanic.test$title %like% c('the Countess')] <- 'Lady' 
Titanic.test$title[Titanic.test$title %like% c('Jonkheer')] <- 'Lady'
Titanic.test$title[Titanic.test$title %like% c('Mme')] <- 'Lady'
Titanic.test$title[Titanic.test$title %like% c('Mlle')] <- 'Lady'
Titanic.test$title[Titanic.test$title %like% c('Ms')] <- 'Miss'
# Convert to a factor 
Titanic.test$title <- factor(Titanic.test$title) 
table(Titanic.test$title)


Titanic.test$FamilySize <- Titanic.test$SibSp + Titanic.test$Parch + 1

table(Titanic.test$FamilySize)

set.seed(415)

################################################################################################
#Linear Regression Model
reg<- lm(Survived ~ Pclass + factor(Sex) + SibSp + Parch + Fare + 
           Embarked + Age + title + Child + FamilySize, 
           data=Titanic.train)
summary(reg) 

## Stepwise Selection

stepit=step(reg, direction="backward")

################################################################################################
#Performing logistic regression

# factor if necessary mydata$rank <- factor(mydata$rank)
logit <- glm(Survived ~ Pclass + factor(Sex) + SibSp + Parch + Fare + Embarked + 
               Age + ageDummy +title +Child, data = Titanic.train, family = binomial("logit"))

stepit=step(logit, direction="backward")

summary(logit)

logit <- glm(Survived ~ Pclass + SibSp + Parch +  
  Age + title + FamilySize, data = Titanic.train, family = binomial("logit"))

stepit=step(logit, direction="backward")

summary(logit)

logit <- glm(Survived ~ Pclass + factor(Sex) + SibSp + Parch + Fare + Age + 
               title, data = Titanic.train, family = binomial("logit"))

stepit=step(logit, direction="backward")

summary(logit)

#Validating logistic model

## CIs using standard errors
confint.default(logit)

##Wald test

wald.test(b = coef(logit), Sigma = vcov(logit), Terms = 4:6)

l <- cbind(0,0,0,1,-1,0)
wald.test(b = coef(logit), Sigma = vcov(logit), L = l)

## odds ratios and 95% CI
exp(cbind(OR = coef(logit), confint(logit)))

Prediction <- predict(logit, Titanic.test, type="response")
submit <- data.frame(PassengerId = Titanic.test$PassengerId, Survived = Prediction)
write.csv(submit, file = "C:/Users/robj_01/Desktop/PRED-453/project1_Titanic/glm.csv", 
          row.names = FALSE)

################################################################################################
#Preforming recursive partitioning

rpart <- rpart( Survived~ Pclass + Age +Sex + SibSp + Parch + Fare + Embarked,
                       data = Titanic.train, method="class", control=rpart.control(xvals=200))

rpart2 <- rpart( Survived~ Pclass + factor(Sex) + SibSp + Parch + Fare + Age + 
                  title,
                data = Titanic.train, method="class", control=rpart.control(xvals=200))

set.seed(415)

summary(rpart)

summary(rpart2)

Prediction <- predict(rpart, Titanic.test, type="class")
submit <- data.frame(PassengerId = Titanic.test$PassengerId, Survived = Prediction)
write.csv(submit, file = "C:/Users/robj_01/Desktop/PRED-453/project1_Titanic/rpart.csv", 
          row.names = FALSE)

Prediction <- predict(rpart2, Titanic.test, type="class")
submit <- data.frame(PassengerId = Titanic.test$PassengerId, Survived = Prediction)
write.csv(submit, file = "C:/Users/robj_01/Desktop/PRED-453/project1_Titanic/rpart2.csv", 
          row.names = FALSE)

#################################################################################################
# Preforming decision tree

tree <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare 
              + Embarked, data=Titanic.train, method="class", 
              control=rpart.control(minisplit =50, cp=0))

plot(tree, asp=4.5)
text(tree, cex=0.7)

fancyRpartPlot(tree)

set.seed(415)

Prediction <- predict(tree, Titanic.test, type = "class")
submit <- data.frame(PassengerId = Titanic.test$PassengerId, Survived = Prediction)
write.csv(submit, file = "C:/Users/robj_01/Desktop/PRED-453/project1_Titanic/tree.csv", 
          row.names = FALSE)

tree2 <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked + title + FamilySize,
             data=Titanic.train, method="class")

plot(tree, asp=4.5)
text(tree, cex=0.7)

fancyRpartPlot(tree)

#############################################################################################
# Performing random forest

summary(Titanic.train)

set.seed(415)

rforest <- randomForest(Survived ~ Pclass + Age + SibSp 
                    + Parch + Fare + FamilySize +title, data=Titanic.train, 
                    importance=TRUE, ntree=2000)

varImpPlot(rforest)

summary(rforest)

rforest2 = randomForest(Survived~Pclass + factor(Sex) + SibSp + Parch + Fare + Age + 
                title, data=Titanic.train, 
                importance=TRUE, ntree=2000)

varImpPlot(rforest2)


Prediction <- predict(fit, Titanic.test, type="class")
submit <- data.frame(PassengerId = Titanic.test$PassengerId, Survived = Prediction)
write.csv(submit, file = "C:/Users/robj_01/Desktop/PRED-453/project1_Titanic/rforest.csv", 
          row.names = FALSE)

#############################################################################################
# Performing cforest

set.seed(415)

cf = cforest(as.factor(Survived) ~Pclass + factor(Sex) + SibSp + Parch + Fare + factor(Embarked) + 
               Age + title, data = Titanic.train, 
             controls = cforest_control(ntree = 10000))

cf2 = cforest(Survived~Pclass + factor(Sex) + SibSp + Parch + Fare + Age + 
                title, data = Titanic.train, 
             controls = cforest_control(ntree = 10000))


Prediction <- predict(cf, Titanic.test, OOB=TRUE, type="response")
submit <- data.frame(PassengerId = Titanic.test$PassengerId, Survived = Prediction)
write.csv(submit, file = "C:/Users/robj_01/Desktop/PRED-453/project1_Titanic/cforest.csv", 
          row.names = FALSE)

Prediction <- predict(cf2, Titanic.test, OOB=TRUE, type="response")
submit <- data.frame(PassengerId = Titanic.test$PassengerId, Survived = Prediction)
write.csv(submit, file = "C:/Users/robj_01/Desktop/PRED-453/project1_Titanic/cforest2.csv", 
          row.names = FALSE)

############################################################################################
#Neural Networks

set.seed(415)

neuralnet = nnet(Survived~Pclass + factor(Sex) + SibSp + Parch + Fare + Embarked + 
                   Age + title, data=Titanic.train, size=20, maxit=10000, decay=.001, linout=T)

neuralnet2 = nnet(Survived~Pclass + SibSp + Parch + Age, 
                 data=Titanic.train, size=20, maxit=10000, decay=.001, linout=T)

table(Titanic.train$Survived,predict(neuralnet,newdata=Titanic.test,type="class"))

Prediction <- predict(neuralnet, Titanic.test, OOB=TRUE, type="class")
submit <- data.frame(PassengerId = Titanic.test$PassengerId, Survived = Prediction)
write.csv(submit, file = "C:/Users/robj_01/Desktop/PRED-453/project1_Titanic/nnet.csv", 
          row.names = FALSE)

Prediction <- predict(neuralnet2, Titanic.test, OOB=TRUE, type="class")
submit <- data.frame(PassengerId = Titanic.test$PassengerId, Survived = Prediction)
write.csv(submit, file = "C:/Users/robj_01/Desktop/PRED-453/project1_Titanic/nnet2.csv", 
          row.names = FALSE)


############################################################################################
