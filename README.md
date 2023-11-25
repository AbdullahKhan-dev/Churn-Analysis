# Telco Churn Analysis

## Overview
This project focuses on analyzing and predicting customer churn for a telecommunications company (Telco). Customer churn, the rate at which customers leave a service, is a critical metric for businesses, especially in subscription-based services like telecommunications. Identifying factors influencing churn and building predictive models can help businesses take proactive measures to retain customers and improve overall customer satisfaction.

## Dataset
The dataset used in this analysis contains information about Telco customers, including demographic details, services subscribed to, contract information, and churn status. Key variables include customer ID, gender, senior citizen status, partner and dependent status, tenure, types of services subscribed to (phone, internet, streaming), contract details, and payment-related information.

## Methodology
The analysis follows a structured process:

### 1. Data Exploration and Visualization
Exploratory data analysis (EDA) was conducted to understand the distribution of variables and identify potential patterns. Visualizations were created to explore the impact of various factors on churn, such as senior citizen status, partner and dependent relationships, internet services, contract types, and payment methods.

### 2. Data Wrangling
Missing values, particularly in the "TotalCharges" column, were addressed by removing corresponding rows. Categorical variables with additional levels like "No Phone Service" or "No Internet Service" were simplified to "No" for modeling consistency.

### 3. Model Building
Logistic regression was chosen as the predictive modeling technique. The model was iteratively refined by removing variables with high p-values, indicating low significance. The final model included significant predictors such as tenure, internet service type, online security, tech support, streaming TV, contract type, paperless billing, payment method, and total charges.

### 4. Model Evaluation
The model's performance was assessed using a Receiver Operating Characteristic (ROC) curve. Sensitivity and specificity rates were calculated across various cutoff values to identify an optimal threshold for predicting churn.

### 5. Interpretation and Insights
The final model's coefficients were interpreted to derive actionable insights for Telco. Factors such as the impact of having dependents, tenure, internet service type, online security, contract duration, paperless billing, payment methods, and total charges were analyzed to provide strategic recommendations.

## Key Findings and Recommendations
1. **Service Quality:** Customers using Fiber Optics for internet services are more likely to churn. Telco should focus on improving the quality of its Fiber Optics service to retain customers.

2. **Contract Duration:** Customers with longer contract durations (one year or two years) are less likely to churn. Telco should consider incentivizing longer-term contracts to enhance customer loyalty.

3. **Payment Methods:** Customers using electronic checks are more likely to churn. Telco should investigate and address any issues related to this payment method to improve customer satisfaction.

4. **Total Charges:** An increase in total charges is associated with higher odds of churning. Telco should carefully review its pricing model and consider adjustments to retain customers.

## Conclusion
This analysis provides Telco with valuable insights into factors influencing customer churn. By addressing specific areas highlighted in the analysis, Telco can implement targeted strategies to reduce churn, enhance customer satisfaction, and improve overall business performance.

**Note:** The project code and detailed analysis can be found in the associated GitHub repository.
