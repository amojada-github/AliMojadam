#importing the dataset >> impute >> write file for Tableau use >> 
# manually divide data into training and testing >> read dataset into R >> 
# drop unnecessary columns >> analyze in R
install.packages('xlsx')
library('xlsx')
covid19_df <- read.xlsx('COVID19_2.xlsx', 1)

nrow(covid19_df)

#back up dataset for controls
original <- covid19_df

#Exploring the data
str(covid19_df)
View(covid19_df)


### Adding the dependent variable : outcome2
covid19_df$outcome2 <- ifelse(covid19_df$outcome == "Dead", 1, 
                              ifelse(covid19_df$outcome == "NA", "missing", 0))
covid19_df$outcome2 <- as.factor(covid19_df$outcome2)

View(covid19_df)

#CONTROL
write.csv(covid19_df,"/Users/alimojadam/Documents/R Directory/covid_19_test1.csv",row.names = F )
### Converting Numerical Data to Factors ###


### Imputing missing data using MICE ###

# how much of our data is missing?
missing_data_percentage <- function (x) {sum(is.na(x))/length(x)*100}
apply(covid19_df, 2, missing_data_percentage)

#age
#manual imputation
#impute(covid19_df$age, median)
#manual imputation with median: age
#covid19_df$age[is.na(covid19_df$age)] <- median(covid19_df$age, na.rm = T)

## Using the mice "Multivariate Imputations by Chained Equations" packages to impute age, sex, travel_history_binary 
library("mice")
install.packages("VIM")
library(VIM)
str(covid19_df)

#sex
impute <- mice(covid19_df[, c("age", "sex", "travel_history_binary" )], m =3, seed = 123)
print(impute)
impute$imp$age
impute$imp$sex
impute$imp$travel_history_binary

# Adding imputation results to dataset
covid19_df_new <- complete(impute,2)
View(covid19_df_new)

### Completed dataset (by inserting the imputed values for missing data points) ###

covid19_df$age <- covid19_df_new$age
covid19_df$sex <- covid19_df_new$sex
covid19_df$travel_history_binary <- covid19_df_new$travel_history_binary

View(covid19_df)

# CONTROL
write.csv(covid19_df,"/Users/alimojadam/Documents/R Directory/covid_19_control.csv",row.names = F )

# CONTROL: how much of our data is missing?
missing_data_percentage <- function (x) {sum(is.na(x))/length(x)*100}
apply(covid19_df, 2, missing_data_percentage)

str(covid19_df)

### converting categorical variables into factors ### 
#outcome
levels(covid19_df$outcome)
covid19_df$outcome <- factor(covid19_df$outcome, levels = c("Alive", "Critical Condition", "Dead", "Discharged", "Hospitalized", "Receiving Treatment"), labels =c(1,2,3,4,5,6))
levels(covid19_df$outcome)


#chronic_disease_binary
levels(covid19_df$chronic_disease_binary)
covid19_df$chronic_disease_binary <- factor(covid19_df$chronic_disease_binary, levels = c("TRUE", "FALSE"), labels =c(1,0))
View(covid19_df)

### Extract completed dataset after imputation for Tableau Use ###
write.csv(covid19_df,"/Users/alimojadam/Documents/R Directory/covid_19_tableau.csv",row.names = F)

#sex
covid19_df$sex_binary <- NULL
levels(covid19_df$sex)
#covid19_df$sex <- factor(covid19_df$sex, levels = c("male", "female"), labels =c("male",0))
#View(covid19_df)



#travel_history_binary
#levels(covid19_df$travel_history_binary)
#covid19_df$travel_history_binary <- factor(covid19_df$travel_history_binary, levels = c("TRUE", "FALSE"), labels =c(1,0))
#levels(covid19_df$travel_history_binary)
#View(covid19_df)


### Reading the new complete dataset ###
install.packages("xlsx")
library("xlsx")
covid19_df_train1 <- read.xlsx("covid_19_tableau_2.xlsx", 1)
str(covid19_df_train1)
View(covid19_df_train1)

# CONTROL: how much of our data is missing?
missing_data_percentage <- function (x) {sum(is.na(x))/length(x)*100}
apply(covid19_df_train1, 2, missing_data_percentage)

# making sex binary
levels(covid19_df_train1$sex)
covid19_df_train1$sex <- factor(covid19_df_train1$sex, levels = c("male", "female"), labels =c(1,0))

### Dropping unnecessary columns ###
#covid19_df_train$ID <- NULL
#covid19_df_train$latitude <- NULL
#covid19_df_train$longitude <- NULL
#covid19_df_train$city <- NULL
#covid19_df_train$travel_history_dates <- NULL
#covid19_df_train$travel_history_location <- NULL

str(covid19_df_train1)

### Creating Factors ###
covid19_df_train1$chronic_disease_binary <- as.factor(covid19_df_train1$chronic_disease_binary)
covid19_df_train1$outcome <- as.factor(covid19_df_train1$outcome)
covid19_df_train1$travel_history_binary <- as.factor(covid19_df_train1$travel_history_binary)
covid19_df_train1$outcome2 <- as.factor(covid19_df_train1$outcome2)
str(covid19_df_train1)

### Creating Dummy variable for factors ###
# country
install.packages("dummies")

library(dummies)

#str(covid19_df_train1)
# covid19_df_analysis$country >> dummy
# covid19_df_analysis <- cbind(covid19_df_analysis, dummy(covid19_df_analysis$country, covid19_df_analysis, sep = "_"))

# covid19_df_analysis$outcome >> dummy
#covid19_df_train <- cbind(covid19_df_train, dummy(covid19_df_train$outcome, sep = "_"))

# covid19_df_train$sex >> dummy
#covid19_df_train1 <- cbind(covid19_df_train1, dummy(covid19_df_train1$sex, sep = "_"))

str(covid19_df_train)
View(covid19_df_train)
rm(covid19_df_training)
rm(covid19_df_train)
### Running Linear Regression ###

## Partioning ##

install.packages ("caTools")
library(caTools)

Partition <- sample.split(covid19_df_train1$outcome2, SplitRatio = 0.8)

Train_set <- subset(covid19_df_train1, Partition == TRUE)
Test_set <- subset(covid19_df_train1, Partition == FALSE)

nrow(Train_set)
nrow(Test_set)

str(Train_set)
View(Train_set)

covid19_logreg_1 <- glm(outcome2 ~ age + sex + travel_history_dates+ travel_history_location+
                        travel_history_binary + chronic_disease_binary, data = Train_set, family= "binomial")

summary(covid19_logreg_1)

# Refining the model by taking out insignificant variables
covid19_logreg_0 <- glm(outcome2 ~ age + sex + travel_history_binary + chronic_disease_binary, 
                        data = Train_set, family= "binomial")

summary(covid19_logreg_0)

# To automatically enhance the model, I'll be using the MASS package
#install.packages("MASS")
#library("MASS")
#model_select <- step(covid19_logreg_0)

# Final Logistic Regression Model
str(Train_set)
covid19_logreg_final <- glm(outcome2 ~ age  + travel_history_binary + chronic_disease_binary, data = Train_set, family= "binomial")

summary(covid19_logreg_final)

### Prediction Equation based on the fitted training model
coef(covid19_logreg_final)

####
probability_of_death = -7.43649085 + 0.08256971*age + 0.44079519*travel_history_binary + 3.24575925*chronic_disease_binary
####

### Prediction using the training dataset ###
predict_train_covid19 <- predict(covid19_logreg_final, data = Train_set, type = 'response')
predictions_train_covid19 <- ifelse(predict_train_covid19>0.65,1,0)

head(predictions_train_covid19)
head(Train_set$outcome2)

nrow(predictions_train_covid19)

## confusion matrix: traininig dataset ##
mean(predictions_train_covid19 == Train_set$outcome2)
confusionmatrix_train_covid19 <- table(Predicted = predictions_train_covid19, Actual = Train_set$outcome2)
confusionmatrix_train_covid19

# Error percentage for predictions
(100 + 13) / nrow(Train_set)*100

### Prediction using the training model for the testing dataset ###

predict_test_covid19 <- predict(covid19_logreg_final, newdata = Test_set, type = 'response')
predictions_test_covid19 <- ifelse(predict_test_covid19>0.65,1,0)

head(predictions_test_covid19)
head(Test_set$outcome2)

## confusion matrix: traininig dataset ##
mean(predictions_test_covid19 == Test_set$outcome2)
confusionmatrix_test_covid19 <- table(Predicted = predictions_test_covid19, Actual = Test_set$outcome2)
confusionmatrix_test_covid19

# Error percentage for predictions
(26 + 3) / nrow(Test_set)*100

### Reading and predicting test dataset ###
install.packages("xlsx")
library("xlsx")
covid19_test_dataset <- read.xlsx("covid_19_tableau_2.xlsx",2)
str(covid19_test_dataset)

# Converting categorical variables to numerical fators
# making sex binary
levels(covid19_test_dataset$sex)
covid19_test_dataset$sex <- factor(covid19_test_dataset$sex, levels = c("male", "female"), labels =c(1,0))
str(covid19_test_dataset)

# converting to factors
covid19_test_dataset$chronic_disease_binary <- as.factor(covid19_test_dataset$chronic_disease_binary)
covid19_test_dataset$travel_history_binary <- as.factor(covid19_test_dataset$travel_history_binary)
str(covid19_test_dataset)
nrow(covid19_test_dataset)

### Final prediction for unknown outcome 2 --testing dataset ###
predict_test_dataset <- predict(covid19_logreg_final, newdata = covid19_test_dataset, type = 'response')
predictions_test_dataset <- ifelse(predict_test_dataset>0.65,1,0)

head(predictions_test_dataset)
head(covid19_test_dataset$outcome2)

### Replacing "Unknown" outcome2's with "prdicted" values
covid19_test_dataset$outcome2 <- predictions_test_dataset
View(covid19_test_dataset)

### extracting predictions
write.csv(covid19_test_dataset,"/Users/alimojadam/Documents/R Directory/covid_19_final_predictions.csv",row.names = T)


### Create graphs using the training dataset ###
#correlations (my variables are ALL binary but let's explore the correlation function see what we get--correlcation should be explored for any relationship among numerical variables)
install.packages("corrplot")
library(corrplot)
correlations <- cor(covid19_df_train1[,c("age", "latitude", "longitude")])
corrplot(correlations, method = "circle",bg = "grey10",
         addgrid.col = "gray50", tl.cex=1,
         tl.col = "black", 
         col = colorRampPalette(c("yellow","green","navyblue"))(100))
#Let's explore the dataset in a scatterplot matrix
pairs(covid19_df_train1, col=covid19_df_train1$outcome)


#Density Distribution of variables
install.packages("caret")
library(caret)
x <- covid19_df_train1[,2:4]
y <- covid19_df_train1[,10]
scales <- list(x=list(relation="free"),y=list(relation="free"))
featurePlot(x=x,y=y, plot = "density",scales = scales)
### The model diagnostics I would run to further control the model accuracy

#VIF : Checking multicollinearity
#Misclassification Error: percentage of mismatch of predicted vs actuals, irrespective of 1's and 0's
#ROC: Receiver Operating Characteristics Curve traces the percentage of true positives accurately predicted by a given logit model as the prediction probability cutoff is lowered from 1 to 0.
#Concordance: Ideally, the model-calculated-probability-scores of all actuals positives (aka one's) should be greater than the model-calculated-probability-scores for ALL the negatives (aka zeroes)
#Specifity and Sensitivity: 
      # Sensitivity: (True positive rate) is hte percentage of 1's (actuals) correctly predicted by the model.
      # Specifity: is the percentage of 0's (actuals) correctly predictetd.
#confusion matrix -- I did check.

