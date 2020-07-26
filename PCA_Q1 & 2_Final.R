rm(list = ls()) 
options(scipen = 99,digits = 10,max.print = 9999) 

gc()

## Check if you have universal installer package, install if not
if("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} 

## Check, and if needed install the necessary packages
pacman::p_load("na.tools", "tidyverse", "caret", "dplyr", "DMwR", "ROCR", "lift", "FactoMineR", "dummies") 

training <- read.csv("C:\\Users\\Alo-Ai day-Toi day\\Desktop\\MMA COURSE\\8. MMA 831 Marketing Analytics\\GRADED ASSIGNMENT\\Midterm Assignment_25Jul\\Data\\Training.csv")
validation <- read.csv("C:\\Users\\Alo-Ai day-Toi day\\Desktop\\MMA COURSE\\8. MMA 831 Marketing Analytics\\GRADED ASSIGNMENT\\Midterm Assignment_25Jul\\Data\\Validation.csv")

## Join datasets for data cleansing
dataset <- rbind(training, validation)

## Address missing values - NAs
na.cols <- which(colSums(is.na(dataset))>0)
sort(colSums(sapply(dataset[na.cols],is.na)), decreasing = TRUE)
paste('There are', length(na.cols), 'columns with missing values')

#  Drop complete NAs columns - 3
#  gu_ind_vmw_major_lookup & gu_ind_vmw_sub_category & ftr_first_date_seminar_page_view
vmware = subset(dataset, select = 
                  -c(gu_ind_vmw_major_lookup, gu_ind_vmw_sub_category, ftr_first_date_seminar_page_view))

#  Drop mostly NAs columns (that has over 95% NAs) - 8
vmware = subset(vmware, select = 
                  -c(ftr_first_date_webinar_page_view, ftr_first_date_eval_page_view, ftr_first_date_whitepaper_download, 
                     ftr_first_date_any_download, ftr_first_date_hol_page_view, hyperthreading_active_flag, 
                     hv_replay_capable_flag, db_accountwatch))

## numeric NAs imputation
#  check numeric features and categorical features with NAs
vmware_numeric <- select_if(vmware, is.numeric)
num_na.cols <- which(colSums(is.na(vmware_numeric))>0)
sort(colSums(sapply(vmware_numeric[num_na.cols],is.na)), decreasing = TRUE)
paste('There are', length(num_na.cols), 'numeric columns with missing values')

## integer NAs imputation
#  check integer features with NAs
vmware_integer <- select_if(vmware, is.integer)
int_na.cols <- which(colSums(is.na(vmware_integer))>0)
sort(colSums(sapply(vmware_integer[int_na.cols],is.na)), decreasing = TRUE)

#  fix integer NAs accordingly
vmware$gu_num_of_employees <- as.numeric(vmware$gu_num_of_employees)
vmware$highest_prodA_edition <- as.factor(vmware$highest_prodA_edition)
paste('There are', length(int_na.cols), 'integer columns with missing values')

## factor NAs imputation
#  check factor features with NAs
vmware_factor <- select_if(vmware, is.factor)
fac_na.cols <- which(colSums(is.na(vmware_factor))>0)
sort(colSums(sapply(vmware_factor[fac_na.cols],is.na)), decreasing = TRUE)
paste('There are', length(fac_na.cols), 'factor columns with missing values')

#  check categorical features with NAs
#paste('There are still', length(na.cols)-3-8-length(num_na.cols), 'categorical columns with missing values')

## Create a custom function to fix the rest of missing values ("NAs") and preserve the NA info as surrogate variables
fixNAs<-function(data_frame){
  # Define reactions to NAs
  integer_reac<-0
  factor_reac<-"Missing"
  character_reac<-"Missing"
  date_reac<-as.Date("1900-01-01")
  # Loop through columns in the data frame and depending on which class the variable is, apply the defined reaction and create a surrogate
  
  for (i in 1 : ncol(data_frame)){
    
    #  create an additional column to capture whether or not the value was imputed for numeric columns
    if (class(data_frame[,i]) %in% c("numeric")) {
      if (any(is.na(data_frame[,i]))){
        data_frame[,paste0(colnames(data_frame)[i],"_impute")]<-
          as.integer(is.na(data_frame[,i]))
        data_frame[is.na(data_frame[,i]),i] <- mean(data_frame[,i], na.rm = TRUE)
      }
    } else
      if (class(data_frame[,i]) %in% c("integer")) {
        if (any(is.na(data_frame[,i]))){
          data_frame[,paste0(colnames(data_frame)[i],"_impute")]<-
            as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
          data_frame[is.na(data_frame[,i]),i]<-integer_reac
        }
      } else
        if (class(data_frame[,i]) %in% c("factor")) {
          if (any(is.na(data_frame[,i]))){
            data_frame[,i]<-as.character(data_frame[,i])
            data_frame[,paste0(colnames(data_frame)[i],"_impute")]<-
              as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
            data_frame[is.na(data_frame[,i]),i]<-factor_reac
            data_frame[,i]<-as.factor(data_frame[,i])
          } 
        } else {
          if (class(data_frame[,i]) %in% c("character")) {
            if (any(is.na(data_frame[,i]))){
              data_frame[,paste0(colnames(data_frame)[i],"_impute")]<-
                as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
              data_frame[is.na(data_frame[,i]),i]<-character_reac
            }  
          } else {
            if (class(data_frame[,i]) %in% c("Date")) {
              if (any(is.na(data_frame[,i]))){
                data_frame[,paste0(colnames(data_frame)[i],"_impute")]<-
                  as.factor(ifelse(is.na(data_frame[,i]),"1","0"))
                data_frame[is.na(data_frame[,i]),i]<-date_reac
              }
            }  
          }       
        }
  } 
  return(data_frame) 
}

#  Apply fixNAs function to the data to fix missing values
vmware <- fixNAs(vmware) 

#  check numeric features no longer has NAs and mean remains unchanged as expected per FAQ
summary(vmware$db_annualsales)

#  impute flag equals NAs proportion
table(vmware$db_annualsales_impute)

#  check for rare categorical features
table(vmware$gu_state)
table(vmware$gu_state_impute)

## Create another a custom function to combine rare categories into "Other."+the name of the original variable (e.g., Other.State)
#  This function has two arguments: the name of the data frame and the count of observation in a category to define "rare"
combinerarecategories<-function(data_frame,mincount){ 
  for (i in 1 : ncol(data_frame)){
    a<-data_frame[,i]
    replace <- names(which(table(a) < mincount))
    levels(a)[levels(a) %in% replace] <-paste("Other",colnames(data_frame)[i],sep=".")
    data_frame[,i]<-a }
  return(data_frame) 
}


#  Apply combinerarecategories function to the data and then split it into testing and training data.
#  combine categories with <20 values in vmware dataset into "Other"
vmware <- combinerarecategories(vmware,20) 

## convert character to factor so PCA will work
vmware_cha <- select_if(vmware, is.character)
colnames(vmware_cha)
vmware <- vmware %>% dplyr::mutate_if(is.character,as.factor)

## cut cleansed dataset into half for training and validation
clean.training <- vmware[1:50006,]
clean.validation <- vmware[50007:100012,]

## rename clean.validation to vmware_ to be consistent on naming convention
vmware_validation <- clean.validation


########################
######################## DATA PROCESSING BEFORE FED INTO MODEL

## Create binary target for the entire dataset
vmware_df <- clean.training
vmware_df$dummy_target <- ifelse(clean.training$target >= 1, "1","0")
vmware_df$dummy_target <- as.factor(vmware_df$dummy_target)
vmware_df$target <- NULL

table(vmware_df$dummy_target)
# 0     1 
# 48670  1336 

## REMOVE variables that can cause data leakeage: all variables start with "tgt"
vmware_df <-  vmware_df %>% dplyr:: select(!starts_with("tgt")) 

######################
###################### CORRELATION (Just on the train set)
## set a random number generation seed to ensure that the holdout split is the same every time
set.seed(1000) 
inTrain <- createDataPartition(y = vmware_df$dummy_target,
                               p = 0.8, list = FALSE)
vmware_train <- vmware_df[ inTrain,]
vmware_test <- vmware_df[ -inTrain,]

## Correlation Maxtrix
# Identifying numeric variables
vmware_train_num <- vmware_train[sapply(vmware_train, is.numeric)] 

# Remove numeric variables that have zero or near zero variance
vmware_train_num <- vmware_train_num[ , which(apply(vmware_train_num, 2, var) != 0)] 

# Calculate correlation matrix
numCor <- cor(vmware_train_num)

# find attributes that are highly corrected
highlyCorrelated_num <- findCorrelation(numCor, cutoff = 0.7)

# Indentifying Variable Names of Highly Correlated Variables
highlyCorCol <- colnames(vmware_train_num)[highlyCorrelated_num]
print(highlyCorCol) 


##########################
########################## MODELLING: CATERGORICAL INCLUDED

# Remove the respond variable: dummy target
vmware_df2 <- subset(vmware_df, select = -c(dummy_target)) 

# Only select catergorical variables with less than 15 levels for easy one hot encoding
sapply(vmware_df2, function(col) length(unique(col)))

cat <- sapply(vmware_df2, is.factor) # Select categorical variables
vmware_df_cat <- Filter(function(x) nlevels(x)<15, vmware_df2[,cat]) # 34 vars
names <- colnames(vmware_df_cat)

# Identify numeric variables
vmware_df_num <- vmware_df2[sapply(vmware_df2, is.numeric)] 

# Remove highly correlated variables and create a new dataset. 
vmware_df_num <- vmware_df_num[,-c(highlyCorrelated_num)]
str(vmware_df_num)

# Combine numeric variables and less than 15 levels catergorical variables
vmware_df2 <- cbind(vmware_df_num, vmware_df_cat) 
str(vmware_df2)

# One hot econding on categorical variables in the new dataset
vmware_df3 <- dummy.data.frame(vmware_df2, names = c(names)) 
dummy_target <- vmware_df$dummy_target
vmware_df4 <- cbind(vmware_df3, dummy_target)
str(vmware_df4)

table(vmware_df4$dummy_target)
# 0     1 
# 48670  1336 

# Remove numeric variables that have zero or near zero variance
vmware_df4 <- vmware_df4[ , which(apply(vmware_df4, 2, var) != 0)] 
str(vmware_df4)

## set a random number generation seed to ensure that the holdout split is the same every time
set.seed(1000) 
inTrain <- createDataPartition(y = vmware_df4$dummy_target,
                               p = 0.8, list = FALSE)
vmware_train_2 <- vmware_df4[ inTrain,]
vmware_test_2 <- vmware_df4[ -inTrain,]

dummy_target_train <- vmware_train_2$dummy_target
dummy_target_test <- vmware_test_2$dummy_target

vmware_train_2$dummy_target <- NULL
vmware_test_2$dummy_target <- NULL


# Transform features of Train Dataset into Principal Components. Apply PCA
pca = prcomp(vmware_train_2, center = TRUE, scale. = TRUE) # TOTAL: 488 PCs
summary(pca)

# Variance explained by each Principal Component
std_dev <- pca$sdev
pr_comp_var <- std_dev^2
pr_comp_var

# Ratio of Variance explained by each component
prop_var_ex <- pr_comp_var/sum(pr_comp_var)
prop_var_ex

# PCA Chart
plot(cumsum(prop_var_ex), xlab = "Principal Component",ylab = "Proportion of Variance Explained")
abline(h=0.9, col = "red", lwd=2) # 160 PCs
abline(h=0.95, col = "blue", lwd=2) # 196 PCs
text(34.86014235,0.98, "95 % Mark")
text(34.86014235,0.93, "90 % Mark")


# Concatenate Dependent variable and Principal Components
loadings <- as.data.frame(pca$x)
dummy_target <- dummy_target_train
pca_train <- cbind(loadings,dummy_target)
pca_train <- as.data.frame(pca_train)

# Creating Dataset having Principal Components
loadings2 <- loadings[1:196]
pca_train2 <- cbind(loadings2,dummy_target)

# Transform features of Test Dataset into Principal Components
# Create a full dataset with all PCs
pca_test <- predict(pca, newdata = vmware_test_2)
pca_test <- as.data.frame(pca_test)

# Create a test set with only 196 PCs 
pca_test2 <- pca_test[1:196]
dummy_target <- dummy_target_test
pca_test3 <- cbind(pca_test2,dummy_target)

str(pca_train2) 
str(pca_test3) 


## SMOTE to handle imbalance dataset in Binary Classification
smote_pca_train <- SMOTE(dummy_target ~., pca_train2, perc.over = 1000 , k = 5, perc.under = 300)
summary(smote_pca_train$dummy_target)
# 0     1 
# 32070 11759

### Initializing and Fitting Logistic Regression Model
model_logistic <-glm(dummy_target ~., data=smote_pca_train, binomial("logit"))
summary(model_logistic) 

# Looking at the Variable Importance table
varImp(model_logistic, scale = TRUE)

# Make predictions on the test data
logistic_probabilities <- predict(model_logistic, newdata= pca_test3, type="response")

# Translate probabilities to predictions
mean(pca_test3$dummy_target == "1")
logistic_classification <- ifelse(logistic_probabilities > 0.02669733, "1", "0")
logistic_classification <- as.factor(logistic_classification)
pca_test3$dummy_target <- as.factor(pca_test3$dummy_target)

# Model Accuracy
observed_classes <- pca_test3$dummy_target
mean(logistic_classification == observed_classes) # 0.8927107289

###Confusion matrix 
confusionMatrix(logistic_classification,pca_test3$dummy_target,positive = "1")
#           Reference
# Prediction    0     1
#           0 8669    8
#           1 1065    259
# Sensitivity : 0.97003745             
# Specificity : 0.89058969


####ROC Curve
logistic_ROC_prediction <- prediction(logistic_probabilities, pca_test3$dummy_target)
logistic_ROC <- performance(logistic_ROC_prediction,"tpr","fpr") #Create ROC curve data
plot(logistic_ROC) #Plot ROC curve

####AUC (area under curve)
auc.tmp.logit <- performance(logistic_ROC_prediction,"auc") #Create AUC data
logistic_auc_testing <- as.numeric(auc.tmp.logit@y.values) #Calculate AUC
logistic_auc_testing # AUC value 0.9303135694

#### Lift chart
plotLift(logistic_probabilities, pca_test3$dummy_target, cumulative = TRUE, n.buckets = 10) # Plot Lift chart


###############################
############################### Fit the model into Validation dataset
vmware_validation <- clean.validation

## Create binary target for the entire dataset
vmware_validation$dummy_target <- ifelse(vmware_validation$target >= 1, "1","0")
vmware_validation$dummy_target <- as.factor(vmware_validation$dummy_target)
dummy_target <- vmware_validation$dummy_target
vmware_validation$target <- NULL

table(vmware_validation$dummy_target)
# 0     1 
# 48601  1405 

## RESHAPE THE VALIDATION SET BEFORE FED TO THE MODEL
# Create a list of column names from pre-one hot encoding training data
names1 <- colnames(vmware_df2)

# Match the column names with the validation set to get the list of index for column order
idx <- match(names1, names(vmware_validation))

# Create a new validation set with same column names as the training set
vmware_validation2 <- vmware_validation[,idx]  # 449 vars
str(vmware_validation2)

# Identify categorical variables
vmware_val_cat <- vmware_validation2[sapply(vmware_validation2, is.factor)] # 34 vars
names2 <- colnames(vmware_val_cat)

# One hot econding on categorical variables in the new dataset
vmware_validation3 <- dummy.data.frame(vmware_validation2, names = c(names2)) # 551 vars

# Create a list of column names from post-one hot encoding training data
names3 <- colnames(vmware_test_2)

# Match the column names with the validation set to get the list of index for column order
idx1 <- match(names3, names(vmware_validation3))

# Create a new validation set with same column names as the training set
vmware_validation4 <- vmware_validation3[,idx1] 
str(vmware_validation4) # 488 vars

# Transform features of Validation Dataset into Principal Components
# Create a full dataset with all PCs
pca_val <- predict(pca, newdata = vmware_validation4)
pca_val <- as.data.frame(pca_val)

# Create a validation set with only 196 PCs 
pca_val2 <- pca_val[1:196]
dummy_target <- vmware_validation$dummy_target
pca_val3 <- cbind(pca_val2,dummy_target)

str(pca_val3)

# Make predictions on the test data
logistic_probabilities_val <- predict(model_logistic, newdata= pca_val3, type="response")

# Translate probabilities to predictions
logistic_classification_val <- ifelse(logistic_probabilities_val > 0.02669733, "1", "0")
logistic_classification_val <- as.factor(logistic_classification_val)
pca_val3$dummy_target <- as.factor(pca_val3$dummy_target)

# Model Accuracy
observed_classes_val <- pca_val3$dummy_target
mean(logistic_classification_val == observed_classes_val) # 0.8936327641

###Confusion matrix 
confusionMatrix(logistic_classification_val, pca_val3$dummy_target,positive = "1")
#           Reference
# Prediction    0      1
#           0 43322   40
#           1 5279    1365
# Sensitivity : 0.97153025            
# Specificity : 0.89138084 

####ROC Curve
logistic_ROC_prediction_val <- prediction(logistic_probabilities_val, pca_val3$dummy_target)
logistic_ROC_val <- performance(logistic_ROC_prediction_val,"tpr","fpr") #Create ROC curve data
plot(logistic_ROC_val) #Plot ROC curve

####AUC (area under curve)
auc.tmp.logit_val <- performance(logistic_ROC_prediction_val,"auc") #Create AUC data
logistic_auc_testing_val <- as.numeric(auc.tmp.logit_val@y.values) #Calculate AUC
logistic_auc_testing_val # AUC value 0.9314555424

#### Lift chart
plotLift(logistic_probabilities_val, pca_val3$dummy_target, cumulative = TRUE, n.buckets = 10) # Plot Lift chart
