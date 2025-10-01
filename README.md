# Global Pricing of Antibiotics per DDD and Sustainable Access

## About this Paper  
This repository accompanies our study on the **global pricing of antibiotics** using the WHO **AWaRe classification** (Access, Watch, Reserve). The analysis is motivated by the **2024 UNGA High-level Meeting on AMR**, which committed to the target that **70% of global human antibiotic use (ABU) should come from the Access group**.  

- **Data**  
  - Source: IQVIA MIDAS® 2019 global sales data (value and volume).  
  - We calculated **average ex-manufacturer prices per defined daily dose (DDD)** across AWaRe groups and countries.  

- **Scenarios**  
  - Estimated potential **pharmaceutical expenditure savings** under two scenarios:  
    1. Countries achieve the **70% Access target**.  
    2. Antibiotic use aligns with the **WHO Essential Medicines List (EML)**.  

- **Treatment Prices**  
  - Compared **7-day treatment costs** for common oral and parenteral antibiotics across AWaRe groups.  

- **Affordability**  
  - For **middle-income countries**, we measured the proportion of the population at risk of falling below national poverty lines if paying out-of-pocket, using income distribution models and national thresholds.  

## CONTENT of this repository

- R code. HEA_midas_financial.R. This contains the script used to clean data and compute all analyses
- Shinny app: A tool for visualisation of a country’s potential pharmaceutical expenditure savings to account for different pricing strategies and improvement in antibiotic usage around the 70% Access target (https://bit.ly/3WN1wZR)
