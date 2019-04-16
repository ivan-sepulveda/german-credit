library(caTools)
library(psych)
relevantData <- read.csv('GermanCredit.csv')

# The variables below were removed because they either improved accuracy
#   or once removed they tydid not affect accuracy rate of the matrix 
relevantData <- relevantData[ , -which(names(relevantData) %in% c("Telephone"))]
relevantData <- relevantData[ , -which(names(relevantData) %in% c("ForeignWorker"))]
relevantData <- relevantData[ , -which(names(relevantData) %in% c("Property.Unknown"))]
relevantData <- relevantData[ , -which(names(relevantData) %in% c("SavingsAccountBonds.Unknown"))]
relevantData <- relevantData[ , -which(names(relevantData) %in% c("Personal.Male.Married.Widowed"))]
relevantData <- relevantData[ , -which(names(relevantData) %in% c("Personal.Female.NotSingle"))]
relevantData <- relevantData[ , -which(names(relevantData) %in% c("OtherInstallmentPlans.None"))]
relevantData <- relevantData[ , -which(names(relevantData) %in% c("Purpose.Other"))]
relevantData <- relevantData[ , -which(names(relevantData) %in% c("Personal.Male.Divorced.Seperated"))]
relevantData <- relevantData[ , -which(names(relevantData) %in% c("Personal.Female.Single"))]
relevantData <- relevantData[ , -which(names(relevantData) %in% c("OtherDebtorsGuarantors.None"))]
relevantData <- relevantData[ , -which(names(relevantData) %in% c("Purpose.Vacation"))]
relevantData <- relevantData[ , -which(names(relevantData) %in% c("Housing.ForFree"))]
relevantData <- relevantData[ , -which(names(relevantData) %in% c("Job.UnskilledResident"))]
relevantData <- relevantData[ , -which(names(relevantData) %in% c("Job.SkilledEmployee"))]
relevantData <- relevantData[ , -which(names(relevantData) %in% c("Job.UnemployedUnskilled"))]
relevantData <- relevantData[ , -which(names(relevantData) %in% c("EmploymentDuration.lt.1"))]
relevantData <- relevantData[ , -which(names(relevantData) %in% c("Purpose.UsedCar"))]
relevantData <- relevantData[ , -which(names(relevantData) %in% c("CreditHistory.PaidDuly"))]
relevantData <- relevantData[ , -which(names(relevantData) %in% c("ResidenceDuration"))]
relevantData <- relevantData[ , -which(names(relevantData) %in% c("NumberPeopleMaintenance"))]
relevantData <- relevantData[ , -which(names(relevantData) %in% c("CheckingAccountStatus.0.to.200"))]
relevantData <- relevantData[ , -which(names(relevantData) %in% c("Job.Management.SelfEmp.HighlyQualified"))]

relevantData$Class = factor(relevantData$Class, levels=c('Bad', 'Good'), labels = c(0, 1))
targetVarColNum = as.numeric(which( colnames(relevantData)=='Class' ))
set.seed(42)
split = sample.split(relevantData$Class, 0.75)
training_set = subset(relevantData, split= TRUE)
test_set = subset(relevantData, split = FALSE)
classifier = glm(formula = Class ~ ., family = binomial, data=training_set)
prob_predict = predict(classifier, type = 'response', relevantData1 = test_set[-targetVarColNum])
Class_predict = ifelse(prob_predict > 0.5, 1, 0)
con_matrix = table(test_set[, targetVarColNum], Class_predict)
con_matrix
results_matrix = data.matrix(con_matrix)
true_zero = as.numeric(results_matrix[1, 1])
false_zero = as.numeric(results_matrix[1, 2])
true_one = as.numeric(results_matrix[2, 2])
false_one = as.numeric(results_matrix[2, 1])
accuracy = (true_one + true_zero)/(true_one + true_zero + false_one + false_zero)
print(paste("Accuracy: ", toString(accuracy*100) ))

# The variabvles below hurt the accuracy rate once removed, so they must be kept
# NumberExistingCredits # NumberPeopleMaintenance # OtherInstallmentPlans.Stores
# SavingsAccountBonds.500.to.1000 # SavingsAccountBonds.gt.1000 # Duration # Age
# OtherInstallmentPlans.Bank # SavingsAccountBonds.100.to.500 # Amount
# CheckingAccountStatus.0.to.200 # CreditHistory.PaidDuly

# Plotting Variable Importance
control <- trainControl(method='repeatedcv', number = 10, repeats = 3)
# Train the model
model <- train(Class~., data=creditData, method = "lvq", preProcess = "scale", trControl = control)
importance <- varImp(model, scale=FALSE)
print(importance)
plot(importance)