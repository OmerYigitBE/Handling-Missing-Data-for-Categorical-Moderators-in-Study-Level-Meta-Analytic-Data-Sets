# Handling Missing Data for Categorical Moderators in Study Level Meta Analytic Data Sets

This repository includes the codes and raw data for the simulation study of my master thesis. If you want to read the full thesis, you can request it via omer1yigit@hotmail.com. Below is a brief summary:

### Problem Statement
Missing data is almost inevitable in any statistical analysis. In meta-analysis, sample size (number of individual studies) is extremely small (<50) in most cases, although the literature on missing data doesn't deal with this small datasets. The aim is to understand how the common missing data handling methods perform with extremely small datasets. Meta-analytic datasets are examples of such datasets. For the scope of the thesis, only one categorical moderator is considered as a variable with missing data.
  
### Methodology
A simulation study is conducted. Here are the steps:
  - Artificial datasets are generated (based on linear models).
  - Performance measures (bias, variance and MSE) are calculated on the full data.
  - Missingness is simulated (based on different mechanisms).
  - Missing data are recovered (based on different methods).
  - Performance measures are recalculated on the recovered data.
  - Performances are compared to see which methods work better in which conditions.

Different conditions are simulated. Similar to experimental design approach, there are 4 factors with 3 levels each. In total, there are 3^4 = 81 different conditions. For each condition, 1000 artificial datasets are generated.
  - Moderator settings: 1 categorical, 1 categorical + 1 continuous, 2 categorical.
  - Sample size (number of individual studies): 10, 25, 50.
  - Missingness mechanism: MCAR, MAR, MNAR.
  - Missingness percentage: 10%, 25%, 40%.

There are 4 different missing data handling methods used.
  - Complete case analysis (CC).
  - Multinomial logistic regression (MLR).
  - Multiple imputation with chained equations (MICE).
  - Joint modelling multiple imputation (JOMO).

### Key Insights
- Missingness mechanisms have no effect, possible due to low sample size (n≤50) and low number of moderators (1 or 2). So, MCAR or MAR (ignorable missingness mechanisms) can be assumed.
- Coding as much information as possible during meta-analysis is important, because higher number of individual studies and lower percentage of missingness are better. Also, additional moderators increase power drastically.
- Ultimate recommendation is MICE. It shows the best performance overall, it has much less bias than MLR and it has higher power than full data sets.

### Further Suggestions
- There were so many assumptions. Thus, replications are suggested.
- The results should also be applied to real meta-analytic datasets for validation.
- Wider range of (or different) conditions can be simulated. Hyperparameter tuning can be done to achieve so.
