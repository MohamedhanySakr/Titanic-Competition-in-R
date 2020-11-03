titanictrain <- read.csv(file="data/train (1).csv",stringsAsFactors=FALSE,header=TRUE)
titanictest <- read.csv(file="data/test (1).csv",stringsAsFactors=FALSE,header=TRUE)
median(titanictest$Age,na.rm = TRUE)
titanictrain$IsTrainSet <-TRUE 
titanictest$IsTrainSet <- FALSE 
titanictest$Survived <- NA
titanicfull <- rbind(titanictrain,titanictest) #rbind to merge two columns
table(titanicfull$IsTrainSet) 
table(titanicfull$Embarked)
# handle null values
#names(titanic.full)
titanicfull[titanicfull$Embarked =='',"Embarked"] <- 'S'
age.median <- median(titanicfull$Age ,na.rm=TRUE)
titanicfull[is.na(titanicfull$Age),"Age"]<-age.median
table(is.na(titanicfull$Fare))
fare.median <- median(titanicfull$Fare,na.rm=TRUE)
titanicfull[is.na(titanicfull$Fare),"Fare"] <- fare.median
# categorical Casting
titanicfull$Pclass <- as.factor(titanicfull$Pclass)
titanicfull$Sex<- as.factor(titanicfull$Sex)
titanicfull$Embarked <- as.factor(titanicfull$Embarked)

# Split dataset backout into train and test
titanictrain <- titanicfull[titanicfull$IsTrainSet==TRUE,]
titanictest <- titanicfull[titanicfull$IsTrainSet==FALSE,]

titanictrain$Survived <- as.factor(titanictrain$Survived)
survived.equation <- "Survived~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
survived.formula <- as.formula(survived.equation)
install.packages("randomForest")
library(randomForest)
titanic.model <- randomForest(formula = survived.formula,data=titanictrain,ntree = 500,mtry=3,nodesize=0.01*nrow(titanic.test))
features.equation <- "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
Survived <- predict(titanic.model,newdata=titanictest)
Survived 
PassengerId <-titanictest$PassengerId
output.df <- as.data.frame(PassengerId)
output.df$survived <- Survived
write.csv(output.df,file="kaggle_submission.csv",row.names = FALSE)
