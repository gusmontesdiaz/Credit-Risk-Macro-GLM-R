# Credit Risk Modeling: GLM for Macroeconomic Impact on Delinquency

## 🎯 Project Objective

This project demonstrates the application of **Generalized Linear Models (GLM)** in R for credit risk analysis, specifically focusing on how macroeconomic variables influence credit card delinquency rates (IMORA TDC). The objective is to build a robust predictive model that identifies and quantifies the statistical significance of external economic factors on portfolio performance.

This analysis is critical for forecasting portfolio losses, setting provisions, and informing strategic risk mitigation efforts.

## 🛠️ Key Methodologies and R Packages Utilized

The analysis was executed entirely in R, showcasing advanced proficiency in statistical programming:

* **Generalized Linear Models (GLM):** Implementation of the GLM framework using the `quasibinomial` family and `logit` link function, the industry standard for modeling binary/rate outcomes like Probability of Default (PD) or delinquency.
* **Variable Selection & Statistical Rigor:** Techniques include initial model fitting, assessment of variable significance (p-values), and formal comparison of nested models (Full vs. Null) using **ANOVA**.
* **Data Handling:** Utilization of R packages (`readxl`, `DescTools`) for efficient ingestion and manipulation of macroeconomic time series data.

## 📊 Core Variables Modeled

The model integrates external economic factors to predict credit performance:

* **Delinquency Metric (Dependent Variable):** Credit Card Delinquency (Morosidad) for a financial institution.
* **Macro Predictors (Independent Variables):** Key macroeconomic indicators like TIIE (Interbank Interest Rate), Unemployment, GDP, Inflation, and VaR on the IGAE (Global Economic Activity Index).

## ✨ Value Proposition for the Analyst Role

* **R Proficiency for Statistical Modeling:** Proven mastery of R for complex statistical inference, demonstrating versatility alongside Python skills.
* **Credit Risk Expertise:** Direct experience in building foundational PD models essential for regulatory compliance and financial reporting.
* **Quantitative Forecasting:** Ability to link macroeconomic trends with portfolio risk metrics, essential for stress testing and forward-looking analysis.
