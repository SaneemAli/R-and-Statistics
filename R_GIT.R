options(scipen=1000000)

# Step-1
#Loading the raw Data
InputData=read.csv('C:/Users/HP/Documents/R&Stats/Regression Datasets/ZomatoData.csv')
str(InputData)

# Step-2
# Exploring the dataset
head(InputData,10)
str(InputData)
summary(InputData)

# Finding unique values for each column
# Defining the function to find unique values
FunctionFindUnique=function(inpColumn){
	return(length(unique(inpColumn)))
}

# Applying the function on every column using sapply
# Understand categorical/continuous columns
sapply(InputData, FunctionFindUnique)


# Removing useless columns
InputData[, c("Restaurant.ID", "Restaurant.Name","City", "Address", "Locality", "Locality.Verbose",
"Cuisines")] = NULL
head(InputData)

# Identify the Problem Statement, What are you trying to solve?

# Identify the Target variable, What value will be predicted?

# Whether it is a Regression problem or Classification?

# Explore each "Potential" predictor for distribution and Quality

# Exploring MULTIPLE CONTINUOUS features
ColsForHist=c("Longitude","Latitude","Votes","Average.Cost.for.two", "Rating")

par(mfrow=c(3,2))
library(RColorBrewer)
for (contCol in ColsForHist){
	hist(InputData[,c(contCol)], main=paste('Histogram of:', contCol), 
	col=brewer.pal(8,"Paired"))
}

############################################################
# Exploring MULTIPLE CATEGORICAL features
ColsForBar=c("Country.Code","Currency","Has.Table.booking","Has.Online.delivery", "Is.delivering.now", "Switch.to.order.menu","Price.range")

par(mfrow=c(4,2))
for (catCol in ColsForBar){
	barplot(table(InputData[,c(catCol)]), main=paste('Barplot of:', catCol), 
	col=brewer.pal(8,"Paired"))
}

# Removing columns with highly skewed distribution
InputData[, c("Country.Code","Currency","Has.Online.delivery","Is.delivering.now","Switch.to.order.menu")] =NULL
head(InputData)

# Converting numeric categorical variables to factor
# Choosing all the columns as they have fair distribution
for (catCol in c("Has.Table.booking","Price.range")){
	InputData[,c(catCol)] = as.factor(InputData[,c(catCol)])
}
str(InputData)
############################################################
plot(x= InputData$Rating, y= InputData$Votes, col='blue')

# For multiple columns at once
ContinuousCols = c("Longitude","Latitude","Votes","Average.Cost.for.two", "Rating") 
plot(InputData[, ContinuousCols], col='blue')

# Correlation for multiple columns at once
cor(InputData[, ContinuousCols], use = "complete.obs")

CorrData=cor(InputData[, ContinuousCols], use = "complete.obs")
CorrData

# Final columns which has high correlation with the target variable
names(CorrData[,'Rating'][abs(CorrData[,'Rating'])>0.2])

# Selecting "Votes" continuous columns for Model

############################################################
boxplot(Rating ~ Price.range, data = InputData, col=brewer.pal(8,"Paired"))

# Analysis of Variance(ANOVA)
# Using a for-loop to perform ANOVA on multiple columns
ColsForANOVA=c("Has.Table.booking","Price.range")
for (catCol in ColsForANOVA){
	anovaData= InputData[, c("Rating", catCol)]
	print(summary(aov(Rating ~., data= anovaData)))
}

# Selecting below columns based on ANOVA results
# "Has.Table.booking","Price.range"

# Checking and treating missing values
colSums(is.na(InputData))
# No Missing values, hence proceeding ahead

# Generating the Data frame for machine learning
TargetVariableName=c('Rating')

# Choosing multiple Predictors which may have relation with Target Variable
# Based on the exploratory data analysis
BestPredictorName= c("Has.Table.booking","Price.range","Votes")

# Extracting Target and predictor variables from data to create a generic dataset
TargetVariable=InputData[, c(TargetVariableName)]
str(TargetVariable)

PredictorVariable=InputData[, BestPredictorName]
str(PredictorVariable)

DataForML=data.frame(TargetVariable,PredictorVariable)
str(DataForML)
head(DataForML)

#########################################################################
# Sampling | Splitting data into 70% for training 30% for testing
TrainingSampleIndex=sample(1:nrow(DataForML), size=0.7 * nrow(DataForML) )
DataForMLTrain=DataForML[TrainingSampleIndex, ]
DataForMLTest=DataForML[-TrainingSampleIndex, ]
dim(DataForMLTrain)
dim(DataForMLTest)

########################################################################
# Creating Predictive models on training data to check the accuracy of each algorithm
###### Linear Regression #######
startTime=Sys.time()
Model_Reg=lm(TargetVariable~.,data=DataForMLTrain)
summary(Model_Reg)
endTime=Sys.time()
endTime-startTime

# Checking Accuracy of model on Testing data
DataForMLTest$Pred_LM=predict(Model_Reg, DataForMLTest)
head(DataForMLTest)

# Calculating the Absolute Percentage Error for each prediction
LM_APE= 100 *(abs(DataForMLTest$Pred_LM - DataForMLTest$TargetVariable)/DataForMLTest$TargetVariable)
print(paste('### Mean Accuracy of Linear Regression Model is: ', 100 - mean(LM_APE)))
print(paste('### Median Accuracy of Linear Regression Model is: ', 100 - median(LM_APE)))


########################################################################
###### Decision Tree #######
library(party)
startTime=Sys.time()
Model_CTREE=ctree(TargetVariable ~ . , data=DataForMLTrain)
#plot(Model_CTREE)
endTime=Sys.time()
endTime-startTime

# Checking Accuracy of model on Testing data
DataForMLTest$Pred_CTREE=as.numeric(predict(Model_CTREE, DataForMLTest))
head(DataForMLTest)

# Calculating the Absolute Percentage Error for each prediction
CTREE_APE= 100 *(abs(DataForMLTest$Pred_CTREE - DataForMLTest$TargetVariable)/DataForMLTest$TargetVariable)
print(paste('### Mean Accuracy of ctree Model is: ', 100 - mean(CTREE_APE)))
print(paste('### Median Accuracy of ctree  Model is: ', 100 - median(CTREE_APE)))

