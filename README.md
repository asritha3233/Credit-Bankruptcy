This project focuses on predicting credit default using various machine learning models and evaluating their performance. Here's a breakdown of the project components:

Project Overview:
Objective: Predict credit default using machine learning models and assess their effectiveness.
Models Used: Logistic Regression, LDA, QDA, Decision Trees, KNN.
Dataset: "Credit" dataset containing attributes related to credit applications.

Dataset Contents:
checking_balance, months_loan_duration, credit_history, purpose, amount, savings_balance, employment_duration, percent_of_income, years_at_residence, age, other_credit, housing, existing_loans_count, job, dependents, phone, default.

Project Workflow

Data Preparation:
- Load Dataset:** Retrieve the "Credit" dataset.
- Handling Missing Values:** Address any missing data points through imputation or removal.
- Encoding Categorical Variables:Convert categorical attributes into numerical representations for model compatibility.
- Scaling Numerical Features:Normalize numerical attributes to ensure uniformity in model training.

Model Training:
Split Dataset: Divide the dataset into training and testing subsets, typically using an 80-20 or 70-30 ratio.
- Train Multiple Models:
 - Logistic Regression:Fit a logistic regression model to capture linear relationships between features and the target variable.
 - LDA (Linear Discriminant Analysis): Implement LDA to find the linear combination of features that best separates classes.
 - QDA (Quadratic Discriminant Analysis): Employ QDA for more flexible separation boundaries between classes.
 - Decision Trees:** Construct decision trees to recursively partition the feature space based on attribute values.
 - KNN (K-Nearest Neighbors): Train a KNN model to classify data points based on the majority class among their k nearest neighbors.

Model Evaluation:
-Performance Metrics: Assess each model's effectiveness using key evaluation metrics:
  - Accuracy: Proportion of correctly classified instances.
  - Precision: Proportion of true positives among all positive predictions.
  - Recall: Proportion of true positives correctly identified.
  - F1 Score: Harmonic mean of precision and recall, providing a balanced measure of model performance.
- Comparative Analysis: Compare the results of different models to identify the most effective approach for predicting credit default.

 Benefit Calculation:
- Model Benefit Analysis:
  - Utilize Recall & Accuracy, Precision & Recall to evaluate the benefit of each model across 100 iterations.
  - Select the best-performing model based on inputs from banking stakeholders.
- Bank Benefit Estimation:
  - Calculate the projected benefits for the bank, including improvements in client retention and default rate.
  - Adjust payment and average default values to reflect varying banking scenarios and requirements.

Dictionary:
Credit.xlsx: Dataset
Credit Bankruptcy.xlsx: Excel file that allows to enter variables based on the bank's situation and show instant outputs. It also has the underlying calculations used for the final benefit of model.
Credit Bankruptcy.R : R file has the all the models run for each other method used for accuracy,recall and precision respectively.
 
