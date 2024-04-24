library(aod)
library(caret)
library(caTools)
library(dplyr)
library(ggplot2)
library(ISLR)
library(mlbench)
library(plyr)
library(readr)
library(stats)
library(MASS)

###Explore data set###

#Initial Observations
#12 car0 and 337 car - Changed car0 to car
#no other NA values found
Credit <- credit_2_
dim(Credit)
summary(Credit)

#Initially did not exclude any columns as I am not sure which will effect the deafulters and which will not

###Split data###
set.seed(1000)
Credit$default <- as.factor(Credit$default)

Training <- createDataPartition(Credit$default, p=0.8, list=FALSE)
training <- Credit[ Training, ]
testing <- Credit[ -Training, ]

preproc <- c("center", "scale")
control <- trainControl(method = "cv", number = 10, savePredictions = TRUE)   



########################## LOGISTIC MODEL ##########################



mod_log_t1 <- train(default ~ checking_balance + months_loan_duration + credit_history 
                    + amount +  employment_duration + percent_of_income
                    + age + other_credit + housing + existing_loans_count
                    + job + dependents + phone,
                    data=training, method="glm", family="binomial",
                    metric = "Accuracy",
                    trControl = control, preProcess = preproc)
summary(mod_log_t1)
pred_log_t1 = predict(mod_log_t1, newdata=testing)

confusionMatrix(data=pred_log_t1, testing$default, mode = 'prec_recall')

mod_log_t2 <- train(default ~ checking_balance + months_loan_duration + credit_history + purpose
                    + amount + savings_balance + employment_duration + percent_of_income
                    + years_at_residence + age  +  existing_loans_count
                    + dependents + phone,
                    data=training, method="glm", family="binomial",
                    metric = "Accuracy",
                    trControl = control, preProcess = preproc)
summary(mod_log_t2)
pred_log_t2 = predict(mod_log_t2, newdata=testing)

confusionMatrix(data=pred_log_t2, testing$default, mode = 'prec_recall')

mod_log_t3 <- train(default ~ checking_balance + months_loan_duration + credit_history + purpose
                    + amount + savings_balance + employment_duration + percent_of_income
                    + years_at_residence + age + existing_loans_count,
            
                    data=training, method="glm", family="binomial",
                    metric = "Accuracy",
                    trControl = control, preProcess = preproc)
summary(mod_log_t3)
pred_log_t3 = predict(mod_log_t3, newdata=testing)

confusionMatrix(data=pred_log_t3, testing$default, mode = 'prec_recall')

mod_log_t4 <- train(default ~ + months_loan_duration + credit_history 
                     + employment_duration + percent_of_income
                    + years_at_residence  + existing_loans_count,
                    
                    data=training, method="glm", family="binomial",
                    metric = "Accuracy",
                    trControl = control, preProcess = preproc)
summary(mod_log_t4)
pred_log_t4 = predict(mod_log_t4, newdata=testing)

confusionMatrix(data=pred_log_t4, testing$default, mode = 'prec_recall')

mod_log_t5 <- train(default ~  + months_loan_duration + credit_history + purpose
                    + amount + savings_balance + employment_duration + percent_of_income
                    + years_at_residence + age + existing_loans_count,
                    
                    data=training, method="glm", family="binomial",
                    metric = "Accuracy",
                    trControl = control, preProcess = preproc)
summary(mod_log_t5)
pred_log_t5 = predict(mod_log_t5, newdata=testing)

confusionMatrix(data=pred_log_t5, testing$default, mode = 'prec_recall')




########################## LDA MODEL ##########################

mod_lda_t1 <- train(default ~ checking_balance + months_loan_duration + credit_history + purpose
                        + amount + savings_balance + employment_duration + percent_of_income
                        + years_at_residence + age + other_credit + housing + existing_loans_count
                        + job + dependents + phone,  
                        data = training, method = "lda", family="binomial",                 
                        metric = "Accuracy",   
                        trControl = control, preProcess = preproc)                 

mod_lda_t1                                                
pred_lda_t1 = predict(mod_lda_t1, newdata=testing)

confusionMatrix(data=pred_lda_t1, testing$default, mode = 'prec_recall') 

mod_lda_t2 <- train(default ~ checking_balance + months_loan_duration + credit_history + purpose
                    + amount + savings_balance + employment_duration + percent_of_income
                    + years_at_residence + age  +  existing_loans_count
                    + dependents + phone,
                    data=training, method="lda", family="binomial",
                    metric = "Accuracy",
                    trControl = control, preProcess = preproc)
summary(mod_lda_t2)
pred_lda_t2 = predict(mod_lda_t2, newdata=testing)

confusionMatrix(data=pred_lda_t2, testing$default, mode = 'prec_recall')

mod_lda_t3 <- train(default ~ checking_balance + months_loan_duration + credit_history + purpose
                    + amount + savings_balance + employment_duration + percent_of_income
                    + years_at_residence + age + existing_loans_count,
                    
                    data=training, method="lda", family="binomial",
                    metric = "Accuracy",
                    trControl = control, preProcess = preproc)
summary(mod_lda_t3)
pred_lda_t3 = predict(mod_lda_t3, newdata=testing)

confusionMatrix(data=pred_lda_t3, testing$default, mode = 'prec_recall')

mod_lda_t4 <- train(default ~ + months_loan_duration + credit_history 
                    + employment_duration + percent_of_income
                    + years_at_residence  + existing_loans_count,
                    
                    data=training, method="lda", family="binomial",
                    metric = "Accuracy",
                    trControl = control, preProcess = preproc)
summary(mod_lda_t4)
pred_lda_t4 = predict(mod_lda_t4, newdata=testing)

confusionMatrix(data=pred_lda_t4, testing$default, mode = 'prec_recall')

mod_lda_t5 <- train(default ~  + months_loan_duration + credit_history + purpose
                    + amount + savings_balance + employment_duration + percent_of_income
                    + years_at_residence + age + existing_loans_count,
                    
                    data=training, method="lda", family="binomial",
                    metric = "Accuracy",
                    trControl = control, preProcess = preproc)
summary(mod_lda_t5)
pred_lda_t5 = predict(mod_lda_t5, newdata=testing)

confusionMatrix(data=pred_lda_t5, testing$default, mode = 'prec_recall')




########################## QDA MODEL ##########################

mod_qda_t1 <- train(default ~ checking_balance + months_loan_duration + credit_history + purpose
                    + amount + savings_balance + employment_duration + percent_of_income
                    + years_at_residence + age + other_credit + housing + existing_loans_count
                    + job + dependents + phone,     
                        data = training, method = "qda", family="binomial",  
                        metric = "Accuracy",                          
                        trControl = control, preProcess = preproc)      

mod_qda_t1
pred_qda_t1 = predict(mod_qda_t1, newdata=testing)

confusionMatrix(data=pred_qda_t1, testing$default, mode = 'prec_recall')

mod_qda_t2 <- train(default ~ checking_balance + months_loan_duration + credit_history + purpose
                    + amount + savings_balance + employment_duration + percent_of_income
                    + years_at_residence + age  +  existing_loans_count
                    + dependents + phone,
                    data=training, method="qda", family="binomial",
                    metric = "Accuracy",
                    trControl = control, preProcess = preproc)
summary(mod_qda_t2)
pred_qda_t2 = predict(mod_qda_t2, newdata=testing)

confusionMatrix(data=pred_qda_t2, testing$default, mode = 'prec_recall')

mod_qda_t3 <- train(default ~ checking_balance + months_loan_duration + credit_history + purpose
                    + amount + savings_balance + employment_duration + percent_of_income
                    + years_at_residence + age + existing_loans_count,
                    
                    data=training, method="qda", family="binomial",
                    metric = "Accuracy",
                    trControl = control, preProcess = preproc)
summary(mod_qda_t3)
pred_qda_t3 = predict(mod_qda_t3, newdata=testing)

confusionMatrix(data=pred_qda_t3, testing$default, mode = 'prec_recall')

mod_qda_t4 <- train(default ~ + months_loan_duration + credit_history 
                    + employment_duration + percent_of_income
                    + years_at_residence  + existing_loans_count,
                    
                    data=training, method="qda", family="binomial",
                    metric = "Accuracy",
                    trControl = control, preProcess = preproc)
summary(mod_qda_t4)
pred_qda_t4 = predict(mod_qda_t4, newdata=testing)

confusionMatrix(data=pred_qda_t4, testing$default, mode = 'prec_recall')

mod_qda_t5 <- train(default ~  + months_loan_duration + credit_history + purpose
                    + amount + savings_balance + employment_duration + percent_of_income
                    + years_at_residence + age + existing_loans_count,
                    
                    data=training, method="qda", family="binomial",
                    metric = "Accuracy",
                    trControl = control, preProcess = preproc)
summary(mod_qda_t5)
pred_qda_t5 = predict(mod_qda_t5, newdata=testing)

confusionMatrix(data=pred_qda_t5, testing$default, mode = 'prec_recall')

########################## KNN ##########################

mod_knn_t1 <- train(default ~ checking_balance + months_loan_duration + credit_history + purpose
                    + amount + savings_balance + employment_duration + percent_of_income
                    + years_at_residence + age + other_credit + housing + existing_loans_count
                    + job + dependents + phone,     
                    data = training, method = "knn",  
                    metric = "Accuracy",                          
                    trControl = control, preProcess = preproc, tuneLength = 20)      

mod_knn_t1
pred_knn_t1 = predict(mod_knn_t1, newdata=testing)

confusionMatrix(data=pred_knn_t1, testing$default, mode = 'prec_recall')


mod_knn_t2 <- train(default ~ checking_balance + months_loan_duration + credit_history + purpose
                    + amount + savings_balance + employment_duration + percent_of_income
                    + years_at_residence + age  +  existing_loans_count
                    + dependents + phone,    
                    data = training, method = "knn",  
                    metric = "Accuracy",                          
                    trControl = control, preProcess = preproc, tuneLength = 20)      

mod_knn_t2
pred_knn_t2 = predict(mod_knn_t2, newdata=testing)

confusionMatrix(data=pred_knn_t2, testing$default, mode = 'prec_recall')

mod_knn_t3 <- train( default ~ checking_balance + months_loan_duration + credit_history + purpose
                     + amount + savings_balance + employment_duration + percent_of_income
                     + years_at_residence + age + existing_loans_count,   
                    data = training, method = "knn",  
                    metric = "Accuracy",                          
                    trControl = control, preProcess = preproc, tuneLength = 20)      

mod_knn_t3
pred_knn_t3 = predict(mod_knn_t3, newdata=testing)

confusionMatrix(data=pred_knn_t3, testing$default, mode = 'prec_recall')

mod_knn_t4 <- train(default ~ + months_loan_duration + credit_history 
                    + employment_duration + percent_of_income
                    + years_at_residence  + existing_loans_count,    
                    data = training, method = "knn",  
                    metric = "Accuracy",                          
                    trControl = control, preProcess = preproc, tuneLength = 20)      

mod_knn_t4
pred_knn_t4 = predict(mod_knn_t4, newdata=testing)

confusionMatrix(data=pred_knn_t4, testing$default, mode = 'prec_recall')

mod_knn_t5 <- train( default ~  + months_loan_duration + credit_history + purpose
                     + amount + savings_balance + employment_duration + percent_of_income
                     + years_at_residence + age + existing_loans_count,   
                    data = training, method = "knn",  
                    metric = "Accuracy",                          
                    trControl = control, preProcess = preproc, tuneLength = 20)      

mod_knn_t5
pred_knn_t5 = predict(mod_knn_t5, newdata=testing)

confusionMatrix(data=pred_knn_t5, testing$default, mode = 'prec_recall')







                   


########################## TREE ##########################

mod_tree_t1 = train(default ~ checking_balance + months_loan_duration + credit_history + purpose
              + amount + savings_balance + employment_duration + percent_of_income
              + years_at_residence + age + other_credit + housing + existing_loans_count
              + job + dependents + phone,            
              data=training,                 
              method="rpart",                 
              parms = list(split="gini"),     
              metric = "Accuracy",       
              trControl = control,    
              tuneLength = 20)  

mod_tree_t1
pred_tree_t1 = predict(mod_tree_t1, newdata=testing)

confusionMatrix(data=pred_tree_t1, testing$default, mode = 'prec_recall')

mod_tree_t2 = train( default ~ checking_balance + months_loan_duration + credit_history + purpose
                     + amount + savings_balance + employment_duration + percent_of_income
                     + years_at_residence + age  +  existing_loans_count
                     + dependents + phone,           
                    data=training,                 
                    method="rpart",                 
                    parms = list(split="gini"),     
                    metric = "Accuracy",       
                    trControl = control,    
                    tuneLength = 20)  

mod_tree_t2
pred_tree_t2 = predict(mod_tree_t2, newdata=testing)

confusionMatrix(data=pred_tree_t2, testing$default, mode = 'prec_recall')

mod_tree_t3 = train( default ~ checking_balance + months_loan_duration + credit_history + purpose
                     + amount + savings_balance + employment_duration + percent_of_income
                     + years_at_residence + age + existing_loans_count,
                     data=training,                 
                     method="rpart",                 
                     parms = list(split="gini"),     
                     metric = "Accuracy",       
                     trControl = control,    
                     tuneLength = 20)  

mod_tree_t3
pred_tree_t3 = predict(mod_tree_t3, newdata=testing)

confusionMatrix(data=pred_tree_t3, testing$default, mode = 'prec_recall')


mod_tree_t4 = train( default ~ + months_loan_duration + credit_history 
                     + employment_duration + percent_of_income
                     + years_at_residence  + existing_loans_count,
                     data=training,                 
                     method="rpart",                 
                     parms = list(split="gini"),     
                     metric = "Accuracy",       
                     trControl = control,    
                     tuneLength = 20)  

mod_tree_t4
pred_tree_t4 = predict(mod_tree_t4, newdata=testing)

confusionMatrix(data=pred_tree_t4, testing$default, mode = 'prec_recall')


mod_tree_t5 = train( default ~  + months_loan_duration + credit_history + purpose
                     + amount + savings_balance + employment_duration + percent_of_income
                     + years_at_residence + age + existing_loans_count,
                     data=training,                 
                     method="rpart",                 
                     parms = list(split="gini"),     
                     metric = "Accuracy",       
                     trControl = control,    
                     tuneLength = 20)  

mod_tree_t5
pred_tree_t5 = predict(mod_tree_t5, newdata=testing)

confusionMatrix(data=pred_tree_t5, testing$default, mode = 'prec_recall')