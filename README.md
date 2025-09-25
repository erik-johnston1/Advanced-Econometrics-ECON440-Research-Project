# Advanced-Econometrics-ECON440-Research-Project
This repository contains the files used to encode a propensity score matching analysis to isolate the causal effect of U.S. veteran status on annual earnings. This project was coauthored by Tori Farris.

## Overview
This project investigates the causal relationship between military service (veteran status) and personal income in the United States. Using data from the IPUMS USA dataset, we apply propensity score methods to address selection bias and estimate the causal effect of military service on earnings.

The analysis was completed as part of a research project for ECON 440: Advanced Econometrics at Cal Poly San Luis Obispo instructed by Dr. Joseph Kuehn.

## Data
**Source**: IPUMS USA  
**Years included**: 2016, 2021, 2022, 2023 (2017-2020 excluded to do approximation methods implemented by IPUMS)  
**Key variables**:  
• Veteran status (VETSTAT): 1 = Veteran/Active Duty, 0 = Non-veteran  
• Income (INCTOT): Total personal income  
• Demographics: Age (AGE), sex (SEX), marital status (MARST), race (RACE), family income (FTOTINC)

Data cleaning in this project includes:
1. Recoding categorical variables on the basis fo the IPUMS code book to prepare an appropriate format for our analysis (e.g., veteran status, marital status, race)
2. Filtering out missing/invalid observations in income variables
3. Restricting sample to individuals aged 18+


## Modeling Methodology
• OLS Regression – Baseline estimate of the effect of veteran status on income.  
• Propensity Score Matching (PSM) – Logistic regression model predicting veteran status based on covariates: sex, age, marital status, race, and family income.  
• Inverse Probability Weighting – Balances treatment (veterans) and control (non-veterans) groups using estimated propensity scores.  
• Trimming – Restricts analysis to observations with propensity scores between 0.05 and 0.95 to ensure common support.

## Key Results
• OLS model: Suggested a small negative effect of veteran status on income.  
• PSM (unweighted): Indicated veterans earn on average $7,371 more than non-veterans.  
• Weighted model: Estimated a smaller but positive effect ($4,676).  
• Trimmed model: Found the most conservative estimate, showing veterans earn on average $1,789 more than non-veterans

## Limitations
1. Inability to bootstrap due to large sample size (>9 million observations).
2. Assumption that all confounding factors are observable and included.  
3. Results reflect short-run impacts; long-term career trajectories may differ.

## Code Features
• Data loading via ipumsr  
• Data cleaning and transformation using tidyverse  
• Propensity score estimation with glm()  
• Visualizations of score distributions and overlap using ggplot2  
• Inverse probability weighting and trimming implementation  
• Output tables formatted with knitr::kable and kableExtra  
