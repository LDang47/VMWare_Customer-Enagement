df_set.seed(100)  #Setting Seed to 100
rm(list = ls())  # Removing all current Object
x <- c('data.table','dplyr','FactoMineR','Gifi','DMwR') 
lapply(x,require,character.only=TRUE) # loading packages
memory.size(max = TRUE) # Setting memory to max
memory.limit(size=56000) 
options(scipen = 99,digits = 10,max.print = 9999) 

df_train <- readRDS("E:\\MMA2020\\8 MMA 831 Marketing Analytics\\MidTerm Assignment\\Data_cleansing\\vmware_train.RDS")
df_test <- readRDS("E:\\MMA2020\\8 MMA 831 Marketing Analytics\\MidTerm Assignment\\Data_cleansing\\vmware_test.RDS")
df_valid <- readRDS("E:\\MMA2020\\8 MMA 831 Marketing Analytics\\MidTerm Assignment\\Data_cleansing\\vmware_validation.RDS")

df_train <- df_train %>% dplyr:: select(!starts_with("tgt"))
df_train <- df_train %>% dplyr:: select(!c(X))
df_train$target[df_train$target>0] <- 1
df_train$target <- as.factor(df_train$target)

df_test <- df_test %>% dplyr:: select(!starts_with("tgt"))
df_test <- df_test %>% dplyr:: select(!c(X))
df_test$target[df_test$target>0] <- 1
df_test$target <- as.factor(df_test$target)

df_valid <- df_valid %>% dplyr:: select(!starts_with("tgt"))
df_valid <- df_valid %>% dplyr:: select(!c(X))
df_valid$target[df_valid$target>0] <- 1
df_valid$target <- as.factor(df_valid$target)


df_train_SMOTE <- DMwR::SMOTE(target~.,df_train,  perc.over = 1000 , k = 5, perc.under = 300)
prop.table(table(df_train_SMOTE$target))

### Had waiting for long, I switched to H2o
detach("package:h2o", unload = TRUE)
library(h2o)
# Initiallizing H2O with 2 threads and 10 gigs of memory
h2o.shutdown()
h2o.init(nthreads = 2,max_mem_size = "10g")

df_train_h2o <- as.h2o(as.data.frame(df_train_SMOTE))
df_test_h2o <- as.h2o(as.data.frame(df_test))
df_valid_h2o <- as.h2o(as.data.frame(df_valid))

# Making list of all the predictors
colnames(df_train_h2o)
# predictors <- c(colnames(df_train_h2o)[1:679],colnames(df_train_h2o)[681:730])
predictors <- c(colnames(df_train_h2o)[1:680],colnames(df_train_h2o)[682:731])
predictors

response <- "target"

## JUST DOING RIDGE ALPHA ==0
vmware_glm_ridge <- h2o.glm(x = predictors, y = response, training_frame = df_train_h2o,validation_frame = df_valid_h2o
                            ,family = 'binomial',link='logit',lambda_search = T,seed = 1,alpha=0,nfolds = 5)

vmware_glm_ridge
summary(vmware_glm_ridge)
print(h2o.auc(vmware_glm_ridge))
print(h2o.auc(vmware_glm_ridge,valid=T))
# Checking Accuracy on the validation frame
h2o.confusionMatrix(vmware_glm_ridge)
h2o.confusionMatrix(vmware_glm_ridge,valid=T)
h2o.confusionMatrix(vmware_glm_ridge, df_test_h2o)
h2o.confusionMatrix(vmware_glm_ridge, df_valid_h2o)

# Plotting the coefficients of variables
x <- h2o.varimp(vmware_glm_ridge)

h2o.performance(vmware_glm_ridge)


## JUST DOING LASSO Alpha=1
vmware_glm_lasso <- h2o.glm(x = predictors, y = response, training_frame = df_train_h2o,validation_frame = df_valid_h2o
                            ,family = 'binomial',link='logit',lambda_search = T,seed = 1,alpha=1,nfolds = 5)

vmware_glm_lasso
summary(vmware_glm_lasso)
print(h2o.auc(vmware_glm_lasso))
print(h2o.auc(vmware_glm_lasso,valid=T))
# Checking Accuracy on the validation frame
h2o.confusionMatrix(vmware_glm_lasso)
h2o.confusionMatrix(vmware_glm_lasso,valid=T)
h2o.confusionMatrix(vmware_glm_lasso, df_test_h2o)
h2o.confusionMatrix(vmware_glm_lasso, df_valid_h2o,thresholds=0.001082)

# Plotting the coefficients of variables
x <- h2o.varimp(vmware_glm_lasso)

h2o.performance(vmware_glm_lasso)

## JUST DOING ELASTIC NET ALPHA == 0.5
vmware_glm_elasticnet <- h2o.glm(x = predictors, y = response, training_frame = df_train_h2o,
                                 validation_frame = df_valid_h2o,family = 'binomial',link='logit',
                                 lambda_search = T,seed = 1,alpha=0.5,nfolds = 5)

vmware_glm_elasticnet
summary(vmware_glm_elasticnet)
print(h2o.auc(vmware_glm_elasticnet))
print(h2o.auc(vmware_glm_elasticnet,valid=T))
# Checking Accuracy on the validation frame
h2o.confusionMatrix(vmware_glm_elasticnet)
h2o.confusionMatrix(vmware_glm_elasticnet,valid=T)
h2o.confusionMatrix(vmware_glm_elasticnet, df_test_h2o)
h2o.confusionMatrix(vmware_glm_elasticnet, df_valid_h2o)

# Plotting the coefficients of variables
x <- h2o.varimp(vmware_glm_elasticnet)
h2o.performance(vmware_glm_elasticnet)
