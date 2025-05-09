# Replication Package: Nutrition-Mental Health Mediation Analysis

Replication package for the paper Thalia M. Sparling, Cesar Cornejo, Suneetha Kadiyala et al. **Nutrition-sensitive agriculture programme impacts mental health via food security in rural Bangladesh**, 30 April 2025, PREPRINT (Version 1) available at Research Square [https://doi.org/10.21203/rs.3.rs-6556946/v1] 
Submitted to BMJ Global Health

## Study Summary

This repository provides all materials needed to reproduce the analyses from our study examining how a homestead food production (HFP) programme affected women's mental health in rural Bangladesh.

Our research found that:
- The intervention reduced the odds of depressive symptoms by 23% (OR 0.77, p=0.03) 
- Food security mediated approximately one-third of the total effect on mental health 
- At baseline, 39% of households were severely food insecure, and 69% of women did not have minimally diverse diets 

## Repository Contents

This replication package includes:

### /data
- FAARM trial data is not publicly available. Please contact Sabine Gabrysch <sabine.gabrysch@pik-potsdam.de> for information about the data.

### /code
- R/Stata scripts for all analyses reported in the paper
- Sequential mediation analysis with cluster-bootstrapped standard errors
- Data cleaning and preparation steps

### /outputs
- Figures and tables as they appear in the manuscript
- Additional supplementary analyses

## How to Use This Repository

1. Dependencies: Install required R/Stata packages listed in `dependencies.R`
2. Data Preparation: Run `1_data_cleaning.R` to process raw data files
3. Main Analysis: Execute `2_main_analysis.R` to reproduce primary findings
4. Mediation Analysis: Run `3_mediation_models.R` to reproduce pathway analyses
5. Generate Figures: Execute `4_create_figures.R` to reproduce all visualizations

## Research Context

This study is part of the Food and Agricultural Approaches to Reducing Malnutrition (FAARM) cluster-randomized trial, which allocated 96 settlements in northeastern Bangladesh to either a homestead food production programme (implemented 2015-2018) or control group.

Key measures included:
- Mental health: Edinburgh Postpartum Depression Scale (EPDS)
- Food security: Household Food Insecurity Access Scale
- Women's empowerment: Adapted measures of agency and decision-making
- Dietary diversity: Women's Dietary Diversity Score

## Contact Information

For questions regarding this replication package, please contact:
- cesar.cornejo@lshtm.ac.uk

For questions about the paper, please contact: 
- thalia.sparling@lshtm.ac.uk

For questions about the data, please contact: 
- sabine.gabrysch@pik-potsdam.de
