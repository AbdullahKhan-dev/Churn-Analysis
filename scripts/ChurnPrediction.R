library(dplyr)
library(tidyr)
library(ggplot2)
library(car) 
library(broom) 
library(purrr) 
library(readxl)
library(GGally)
library(pROC)
library(ggpubr)
library(caret) #for k-fold cross-validation


churn_data = read.csv('./Churn_Data.csv')
str(churn_data)

#### 0: Understanding the dataset ####

## the dataset contians information about the following attributes 
# -Customers who left within the last month – the column is called Churn
# -Services that each customer has signed up for – phone, multiple lines, internet, online security, online backup, device protection, tech support, and streaming TV and movies
# -Customer account information – how long they’ve been a customer, contract, payment method, paperless billing, monthly charges, and total charges
# -Demographic info about customers – gender, age range, and if they have partners and dependents


## Here is a breakdown of the important columns in this dataset
# CustomerID: Unique identifier for each customer
# Gender: Customer's gender (Male/Female)
# SeniorCitizen: Whether the customer is a senior citizen (1 if yes, 0 if no)
# Partner: Whether the customer has a partner (Yes/No)
# Dependents: Whether the customer has dependents (Yes/No)
# Tenure: Number of months the customer has stayed with the company
# PhoneService: Whether the customer has phone service (Yes/No)
# MultipleLines: Whether the customer has multiple lines (Yes/No/No phone service)
# InternetService: Type of internet service (DSL/Fiber optic/No)
# OnlineSecurity: Whether the customer has online security (Yes/No/No internet service)
# OnlineBackup: Whether the customer has online backup (Yes/No/No internet service)
# DeviceProtection: Whether the customer has device protection (Yes/No/No internet service)
# TechSupport: Whether the customer has tech support (Yes/No/No internet service)
# StreamingTV: Whether the customer has streaming TV (Yes/No/No internet service)
# StreamingMovies: Whether the customer has streaming movies (Yes/No/No internet service)
# Contract: Type of contract (Month-to-month/One year/Two years)
# PaperlessBilling: Whether the customer uses paperless billing (Yes/No)
# PaymentMethod: Payment method (Electronic check/Mailed check/Bank transfer (automatic)/Credit card (automatic))
# MonthlyCharges: Monthly charges for the customer
# TotalCharges: Total charges for the customer
# Churn: Target variable indicating whether the customer churned (Yes/No)




#### 1: Data Vizualizations to understand the data ####
str(churn_data)
# Senior Citizen variable is numeric, so i'll recode it to Yes and No, which will also make it categorical
churn_data <- churn_data %>%
  mutate(SeniorCitizen = ifelse(SeniorCitizen == 1, 'Yes','No')
  )

# Visualizing data for some varibales
ggduo(
  data = churn_data,
  columnsX = "Churn",
  columnsY = c("gender", "SeniorCitizen", "Partner", "Dependents", "PhoneService","MultipleLines"),
  types = list(discrete = "rowbar"),
  legend = 1)

# The findings are interesting and corroborate my initial thoughts that Gender has no significant 
# impact on predicting churn, and that Senior Citizens are almost 20% more likely to churn, and if you have
# a partner or dependents, you are less likely to churn as well, which also makes sense
# Interestingly, however, the variable PhoneServices and it's derived variable 'MultipleLines' show no significant 
# difference in churn rates between their various values or factors

# More visualization
ggduo(
  data = churn_data,
  columnsX = "Churn",
  columnsY = c("InternetService", "OnlineSecurity", "OnlineBackup", "DeviceProtection","TechSupport"),
  types = list(discrete = "rowbar"),
  legend = 1)

# Interestingly, people who have Fiber Optics as their internet service, are more likely to churn, which possibly indicates 
# that users are having a bad experience with fiber optics 
# Unsurprisingly, people who subscribe to OnlineBackups, OnlineSecurity, or other internet support
# are less likely to churn

# Even more visualizations
ggduo(
  data = churn_data,
  columnsX = "Churn",
  columnsY = c("StreamingTV", "StreamingMovies", "Contract", "PaperlessBilling"),
  types = list(discrete = "rowbar"),
  legend = 1)

# People who don't have StreamingTV services are more likely to churn
# There is not a significant difference in churn rates cause by StreamingMovies variable
# People with monthly contracts are more likely to churn, which is understandable
# Interestingly however, people with paperless billing have churned more. I was expecting
# no relation between churn %age and PaperlessBilling, but the data indicates otherwise

#Visualizing numerical variables
ggduo(
  data = churn_data,
  columnsX = "Churn",
  columnsY = c( "PaymentMethod","tenure", "MonthlyCharges", "TotalCharges"),
  types = list(discrete = "rowbar"),
  legend = 1)

# PaymentMethod does have an impact on churn rate (which is not what I had expected)
# The avg tenure for people who churned is significantly less than the avg tenure for those who 
# haven't churned, which is understandable, as long-time users are expected to stick with the company
# The avg monthly rate for people who churned is significantly greater than those who 
# haven't churned


#### 2: Data Wrangling ####


#First of all, let's remove any missing values
#are there missing values or nas?
#missing value
churn_data[!complete.cases(churn_data), ]

#seems like we do have some missing values. Lets see what columns do these belong to
#sum of nas by column
colSums(is.na(churn_data))

# all the missing values are in column TotalCharges (total 11). let's remove those rows or observations
churn_data <- churn_data %>% filter(!is.na(TotalCharges))


## We have many variables whose factors can be binary Yes and No but also contain a third variable named either "No Phone Service"
## Or "No Internet Service", which we can simply mutate as "No", for the sake of this model.
## I found an easy way to do that on the internet 
churn_data <- data.frame(lapply(churn_data, function(x) {
  gsub("No internet service", "No", x)}))

churn_data <- data.frame(lapply(churn_data, function(x) {
  gsub("No phone service", "No", x)}))




churn_data <- churn_data %>% 
              mutate(ChurnNumeric = ifelse(Churn == 'Yes', 1, 0),
                     InternetService = as.factor(InternetService),
                     tenure = as.numeric(tenure),
                     MonthlyCharges = as.numeric(MonthlyCharges),
                     TotalCharges = as.numeric(TotalCharges))
levels(churn_data$InternetService) = c("No","DSL","Fiber optic")
                     
str(churn_data) 



#### Splitting the data between training and testing sets (70% for training data, 30% for test) ####

set.seed(123) 
churn_train <- churn_data %>%
  slice_sample(prop = 0.7, replace = FALSE)

#test set; rest of full data
churn_test <- churn_data %>%
  anti_join(churn_train)

#### 3: Start building a model. I will go with a 10% LOS ####

# First draft model. I will use all the variables which I believe are significant, judging from the visualizations that I performed earlier

model1 <- glm(ChurnNumeric ~ SeniorCitizen + Partner + Dependents + tenure + InternetService + OnlineSecurity + OnlineBackup + TechSupport + StreamingTV + Contract + PaperlessBilling + PaymentMethod + MonthlyCharges + TotalCharges, data =churn_train, family = "binomial")
summary(model1)

#Removing Partner variable (p-value = 0.64)
model2 <- glm(ChurnNumeric ~ SeniorCitizen + Dependents + tenure + InternetService + OnlineSecurity + OnlineBackup + TechSupport + StreamingTV + Contract + PaperlessBilling + PaymentMethod + MonthlyCharges + TotalCharges, data =churn_train , family = "binomial")
summary(model2)

#Removing SeniorCitizen variable (p-value = 0.263)
model3 <- glm(ChurnNumeric ~ Dependents + tenure + InternetService + OnlineSecurity + OnlineBackup + TechSupport + StreamingTV + Contract + PaperlessBilling + PaymentMethod + MonthlyCharges + TotalCharges, data =churn_train, family = "binomial")
summary(model3)

#Removing OnlineBackup variable (p-value = 0.193805)
model_final <- glm(ChurnNumeric ~ Dependents + tenure + InternetService + OnlineSecurity + TechSupport + StreamingTV + Contract + PaperlessBilling + PaymentMethod + MonthlyCharges + TotalCharges, data =churn_train, family = "binomial")

summary(model_final)


#### 4: Making Predictions and Checking for accuracy ####
train_pred <- augment(model_final) %>%
  mutate(pred_prob = predict(model_final, type = "response"))
train_pred


#ROC curve
final_model_roc <- roc(as.factor(train_pred$ChurnNumeric), train_pred$pred_prob)
roc_plot <- ggroc(final_model_roc, legacy.axes = TRUE) +
  labs(x = 'False-positives', y = 'True-positives',
       title = 'ROC curve - training set:  model churn predictions') +
  annotate('text', x = 0.5, y = 0.5, label = paste0('AUC: ', round(auc(final_model_roc), digits = 2)))
roc_plot

# AUC = 0.84

## let's check confusion matrices
calculateConfusionMatrix <- function(predictions, actuals, cutoff) {
  predicted_classes <- ifelse(predictions > cutoff, 1, 0)
  confusion_matrix <- table(Predicted = predicted_classes, Actual = actuals) #manually generating the confusion matrix with a table  
  return(confusion_matrix)
}

cutoffs <- seq(0.1, 0.9, by = 0.025)

confusion_matrices <- lapply(cutoffs, function(cutoff) {
  calculateConfusionMatrix(train_pred$pred_prob, as.factor(train_pred$ChurnNumeric), cutoff)
})

#Print confusion matrices for different cutoffs
for (i in 1:length(cutoffs)) {
  print(paste("cutoff:", cutoffs[i]))
  print(confusion_matrices[[i]])
  print("----------")
}

sensitivity_rates <- c()   #true positives 
specificity_rates <- c()    #true negatives
for (i in 1:28){
  sensitivity_rates[i] = confusion_matrices[[i]][[4]]/sum(train_pred$ChurnNumeric == 1)
  specificity_rates[i] = confusion_matrices[[i]][[1]]/sum(train_pred$ChurnNumeric == 0)
}

# drawing a graph of specificity vs sensitivity to find the optimal cutoff rate
df_sensitivity_specificity <- data.frame(cutoffs=cutoffs[1:28],sensitivity_rates, specificity_rates)

ggplot(df_sensitivity_specificity, aes(x = cutoffs)) +
  geom_line(aes(y = sensitivity_rates, color = "Sensitivity"), size = 1) +
  geom_line(aes(y = specificity_rates, color = "Specificity"), size = 1) +
  geom_vline(xintercept = df_sensitivity_specificity$cutoffs[which.min(abs(df_sensitivity_specificity$sensitivity_rates - df_sensitivity_specificity$specificity_rates))],
             linetype = "dotted", color = "black") +
  labs(title = "Sensitivity and Specificity vs. Cutoffs",
       x = "Cutoffs",
       y = "Sensitivity/Specificity Rates") +
  scale_color_manual(values = c("Sensitivity" = "blue", "Specificity" = "red"),
                     labels = c("Sensitivity", "Specificity"))


## from this graph, the best cutoff value is around 0.32, so we'll pick that as our cutoff

test_pred_table <- churn_test %>%
  mutate(pred_prob = predict(model_final, newdata = churn_test, type = "response"),
         class=ifelse(pred_prob >= 0.32, 1, 0 ))
test_pred_table[,'pred_prob'] <- round(test_pred_table[,'pred_prob'],6)



test_cm <- confusionMatrix(as.factor(test_pred_table$class), as.factor(churn_test$ChurnNumeric), positive = "1", mode = "everything")
test_cm

## The model is performing well, with a Sensitivity=0.7679, Specificity=0.7852 and Accuracy=0.7765.
## These are acceptable numbers, and I am happy with this model 

####5: Lets dig deep into the results to tell a story that would be helpful for TelCo ####
summary(model_final)

  ## Variable                             ##Co-efficient(logodds)     ##Interpretation 
#  DependentsYes                            -2.369e-01               Having a dependent decreases the odds of a customer churning
#  tenure                                   -5.485e-02               Every 1 month increase in tenure decreases the log odds of churning by 0.0548
#. InternetServiceFiber optic                7.608e-01               Base-case = No internet service.This means that customers who used fiber optics have increased log(odds) of churning than customers who do not have internet service by a factor of 0.76. This indicates that Telco's fiber optics service is bad and is making customers churn
#  InternetServiceDSL                        -9.431e-01              Base-case = No internet service.Customers who use DSL as their internet service have fewer odds of churning. This means that DSL service is good, and is keeping people happy.
#  OnlineSecurityYes                        -4.281e-01               Customers who have Online Security are less likely to churn. This indicates a satisfactory online security provided by Telco  
#  TechSupportYes                           -3.029e-01               Customers who get TechSupport are less likely to churn. This makes sense, and also indicates are customers are satisfied with the tech support that they are getting
#  StreamingTVYes                            3.450e-01               Again, this indicates a bad TV streaming experience provided to customers, which is making them churn 
#  ContractOne year                         -7.974e-01               Base Case = Monthly contract. Those who have signed a yearly contract have fewer odds of churning than those who sign monthly contracts. This makes sense because a customer is less likely to cancel his contract if they have already paid for a year.
#  ContractTwo year                        -1.441e+00                Base Case = Monthly contract. Understandably, people who have signed two years' contract are even less likely to churn than those who have signed one year contract. These people probably love Telco's services very much to have committed to a two-year's contract in the first place.
#  PaperlessBillingYes                      3.327e-01                People who have paperless billing have greater odds of churning.
#  PaymentMethodCredit card (automatic)    -1.382e-01                Base Case = Bank Transfer. People who pay via Credit card (automatic) have fewer chances of churning than the base case  
#. PaymentMethodElectronic check            2.799e-01                Base Case = Bank Transfer. People who pay via electronic check have greater chances of churning than the base case  
#  PaymentMethodMailed check               -1.841e-01               Base Case = Bank Transfer. People who pay via Mailed check have greater fewer of churning than the base case 
#. MonthlyCharges                          -5.203e-03                With every unit increase in monthly charges, log odds of churning decrease by 0.005203 units. This variable has a p-value higher than 0.1 whihc means this is not significant, but I decided to keep it in the model anyway becuase it would make sense if people who have higher monthly charges are more likely to churn. But this figure tells a different story, so i'll attribute it to model inaccuracies.
#  TotalCharges                             3.130e-04                As expected, with every unit increase in total charges, log odds of churning increase by 0.000313. Telco needs to be cautious of their pricing model, if they wish to retain customers.




#### What can Telco Learn from this scenario? ####
#1 - Telco needs to improve their Fiber Optics service, as it is increasingly causing users to churn 
#2 - They should also improve their TVStreaming Services.
#3 - Telco needs to do some further surveying or analyses on customers who pay with Electronic Check, as they are more likely to churn. They need to better understand this customer segment. It is also likely that the electronic check method is causing frustration for customers due to inscrutable processes or maybe the system is broken altogether 
#4 - 