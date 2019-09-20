install.packages("mlr", repos='http://cran.us.r-project.org')

library(mlr)
library(stringr)

train_1 <-read.delim('train.txt', header=TRUE, sep=",",  na.strings=c("","NA"))
test_1<-read.delim('test.txt', header=TRUE, sep = ",",  na.strings=c("","NA"))


colSums(is.na(train_1))
colSums(is.na(test_1))
typeof(train_1$Loan_Amount_Term)

imputed_data<-impute(train_1, classes = list(factor=imputeMode()))
train_1<-imputed_data$data


imputed_data_test<-impute(test_1, classes = list(factor=imputeMode()))
test_1<-imputed_data_test$data


train_1$Dependents<-str_replace(
  train_1$Dependents,
  pattern = '\\+',
  replacement = ''
)

test_1$Dependents<-str_replace(
  test_1$Dependents,
  pattern = '\\+',
  replacement = ''
)

train_1$LoanAmount<-ifelse(is.na(train_1$LoanAmount), mean(train_1$LoanAmount, na.rm = TRUE), train_1$LoanAmount)

test_1$LoanAmount<-ifelse(is.na(test_1$LoanAmount), mean(test_1$LoanAmount, na.rm = TRUE), test_1$LoanAmount)

names(which.max(table(train_1$Loan_Amount_Term, useNA = "no")))

train_1$Loan_Amount_Term[is.na(train_1$Loan_Amount_Term)]<-'360'

names(which.max(table(train_1$Credit_History , useNA = "no")))

train_1$Credit_History[is.na(train_1$Credit_History )]<-'1'


names(which.max(table(test_1$Loan_Amount_Term, useNA = "no")))

test_1$Loan_Amount_Term[is.na(test_1$Loan_Amount_Term)]<-'360'

names(which.max(table(test_1$Credit_History , useNA = "no")))

test_1$Credit_History[is.na(test_1$Credit_History )]<-'1'


train<-train_1
test<-test_1

colSums(is.na(train))
colSums(is.na(test))


#sapply(train[,c("ApplicantIncome", "CoapplicantIncome", "LoanAmount")], function(x) quantile(x, seq(0,1,.01)))

#sapply(test[,c("ApplicantIncome", "CoapplicantIncome", "LoanAmount")], function(x) quantile(x, seq(0,1,.01)))

#Noramlising the continous variables

train$ApplicantIncome<-scale(train$ApplicantIncome)
train$CoapplicantIncome<-scale(train$CoapplicantIncome)
train$LoanAmount<-scale(train$LoanAmount)

test$ApplicantIncome<-scale(test$ApplicantIncome)
test$CoapplicantIncome<-scale(test$CoapplicantIncome)
test$LoanAmount<-scale(test$LoanAmount)

train$Gender<-ifelse(train$Gender=="Male", 1,0)
train$Married<-ifelse(train$Married=="Yes",1,0)
train$Education<-ifelse(train$Education=="Graduate",1,0)
train$Self_Employed<-ifelse(train$Self_Employed=="Yes",1,0)
train$Loan_Status<-ifelse(train$Loan_Status=="Y",1,0)
train$Loan_ID<-NULL

test$Gender<-ifelse(test$Gender=="Male", 1,0)
test$Married<-ifelse(test$Married=="Yes",1,0)
test$Education<-ifelse(test$Education=="Graduate",1,0)
test$Self_Employed<-ifelse(test$Self_Employed=="Yes",1,0)
test$Loan_ID<-NULL


loan<-sum(train$Loan_Status)/nrow(train)

train_c<-train[,-c(6,7,8)]

train_c<-data.frame(sapply(train_c, function(x) factor(x)))
str(train_c)

dummies_train<-data.frame(sapply(train_c, function(x) data.frame(model.matrix(~x-1, data=train_c))[,-1]))



test_c<-test[,-c(6,7,8)]

test_c<-data.frame(sapply(test_c, function(x) factor(x)))
str(test_c)

dummies_test<-data.frame(sapply(test_c, function(x) data.frame(model.matrix(~x-1, data=test_c))[,-1]))

train_final<-cbind(dummies_train, train[,c(6,7,8)])

test_final<-cbind(dummies_test, test[,c(6,7,8)])


model1=glm(Loan_Status~., data = train_final, family = "binomial")

summary(model1)

library("MASS")
model2<- stepAIC(model1, direction="both")

library(car)
vif(model2)

model3<-glm(Loan_Status ~  Dependents.x1 + Loan_Amount_Term.x36 + 
              Loan_Amount_Term.x480 + Credit_History + Property_Area.xSemiurban + 
              CoapplicantIncome, family = "binomial", data = train_final)


summary(model3)
vif(model3)


model4<-glm(Loan_Status ~  Dependents.x1 + Loan_Amount_Term.x480 + Credit_History + Property_Area.xSemiurban + 
              CoapplicantIncome, family = "binomial", data = train_final)


summary(model4)
vif(model4)


model5<-glm(Loan_Status ~ Loan_Amount_Term.x480 + Credit_History + Property_Area.xSemiurban + 
              CoapplicantIncome, family = "binomial", data = train_final)


summary(model5)
vif(model5)

model6<-glm(Loan_Status ~ Loan_Amount_Term.x480 + Credit_History + Property_Area.xSemiurban,family = "binomial", data = train_final)

summary(model6)
vif(model6)

model7<-glm(Loan_Status ~ Credit_History + Property_Area.xSemiurban,family = "binomial", data = train_final)

summary(model7)
vif(model7)


final_model<-model7

### Model Evaluation

### Test Data ####

#predicted probabilities of Churn 1 for test data

test_pred = predict(final_model, type = "response", 
                    newdata = test_final)

# Let's see the summary 

summary(test_pred)

test_final$prob <- test_pred
View(test_final)


test_final$Loan_Status<- factor(ifelse(test_pred >=0.5, 1, 0))


test$Loan_Status<- factor(ifelse(test_pred >=0.5, 1, 0))

test_1$Loan_Status<- factor(ifelse(test_pred >=0.5, 1, 0))

test_f<-test_1[,c("Loan_ID", "Loan_Status")]

write.csv(test_f, file = "Loan_Predicted_Data.csv", row.names = FALSE)
