#==============================================================================
# TITLE: MULTIPLE IMPUTATION OF FOOD SECURITY AND DIETARY DIVERSITY DATA
# ORGANIZED BY: CESAR CORNEJO 
# AUTHOR:
# DATE CREATED:  2025-03-21
# LAST MODIFIED: 2025-04-22
# 
# DESCRIPTION:
#    This script performs multiple imputation on the dataset "7_Food_sec_and_dd_labeled.dta" 
#    using the MICE package in R. The script:
#
#    1) Loads and prepares the dataset for imputation
#    2) Identifies missing data patterns and tests assumptions
#    3) Applies appropriate imputation methods for different variable types:
#       - Random forest for continuous variables
#       - Proportional odds models (polr) for ordinal categorical variables
#       - Logistic regression for nominal categorical variables
#    4) Performs multiple imputation with parameters m=5 (number of imputed datasets) 
#       and maxit=30 (maximum number of iterations)
#    5) Produces diagnostic plots to assess imputation quality
#    6) Exports five complete imputed datasets as CSV files (FSN_MH_ImpData_1_2024.csv 
#       through FSN_MH_ImpData_5_2024.csv:)
#
#   Input files:
#   - 7_Food_sec_and_dd_labeled.dta: Prepared dataset from Stata with labeled variables
#
#   Output file:
#   - FSN_MH_ImpData_1_2024.csv through FSN_MH_ImpData_5_2024.csv
#
# NOTES:
#    - Uses random seed 123 to ensure reproducibility of results
#    - Assumes data are Missing At Random (MAR)
#    - Special handling is provided for ordinal variables (using polr method) and 
#      nominal variables (using logreg method)
#==============================================================================



# RESOURCES
# https://www.kaggle.com/questions-and-answers/105010
# https://search.r-project.org/CRAN/refmans/misty/html/na.test.html
# https://www.kdnuggets.com/2017/09/missing-data-imputation-using-r.html
####
# https://www.gerkovink.com/miceVignettes/Ad_hoc_and_mice/Ad_hoc_methods.html
# https://www.gerkovink.com/miceVignettes/Convergence_pooling/Convergence_and_pooling.html


# Check if there are linked packages; if not, skip step "Detach"
sessionInfo()$otherPkgs

# Detach
# lapply(paste('package:',names(sessionInfo()$otherPkgs),sep=""),detach,character.only=TRUE,unload=TRUE)


# List of packages needed
packages_needed <- c("readstata13", "mice", "VIM", "dplyr", "misty", "mvnmle")

# Check which packages are not installed
packages_to_install <- packages_needed[!(packages_needed %in% installed.packages()[,"Package"])]

# Install missing packages
if(length(packages_to_install) > 0) {
  cat("Installing the following packages:", paste(packages_to_install, collapse=", "), "\n")
  install.packages(packages_to_install)
} else {
  cat("All required packages are already installed.\n")
}


# Load Packages
library(readstata13)
library(mice)
library(VIM)
library(dplyr)
library(misty)

# Set working Directory to where your data is
setwd('/Users/cesarm3/Downloads/Replication package')


# Load Data
df <- read.dta13("Data/7_Food_sec_and_dd_labeled.dta")


#### 0. Data Cleaning ####
set.seed(123) # ensure results are replicable 

# Select raw variables to be imputed
# NOTE: Order of variable imputation matters (previous imputations feed into next columns as predictor)

data <- df %>% dplyr::select(wcode, c_code, treat,
                             # BL & EL
                             religion_bl, fam_type_bl, dep_ratio, quint_bl, age_bl, woman_edu_cat_bl,lit_cat, impgarden_bl,
                             num_crops_bl, lefthome_bl, lefthome_el, ss_score_cont_bl, ss_score_cont_el, huscomm_score_cont_bl,
                             huscomm_score_cont_el, extcomm_score_cont_bl, extcomm_score_cont_el, dec_score_cont_bl, dec_score_cont_el,
                             noincome_bl, noincome_el, prod_e_y14, el_selfefficacy, el_gen238_ew, el_network_score_cont,
                             # BL - key variables
                             hfias_score_bl, md_score_bl, preg_bl,  dd10r_score_m_bl, dd10r_min_m_bl,  hfias_bl,
                             # Surv & EL (DD)
                             dd10r_score_m_r1, dd10r_score_m_r2, dd10r_score_m_r3, dd10r_score_m_r4, dd10r_score_m_r5, 
                             dd10r_score_m_r6, dd10r_score_m_r7, dd10r_score_m_r8, dd10r_score_m_el, 
                             # Surv & EL (HFIAS)
                             hfias_score_r1314, hfias_score_r2223, hfias_r1314, hfias_r2223, hfias_r2223, hfias_el, hfias_score_el,
                             # Other
                             month_0, month_1, month_2, month_3, month_4, month_5, month_6, month_7, month_8, month_9, ramadan_0, ramadan_1, 
                             ramadan_2, ramadan_3, ramadan_4, ramadan_5, ramadan_6, ramadan_7, ramadan_8, ramadan_9, md_score_el)

## !!! Ensure that Data is disaggregated and does not include sum scores !!!
summary(data)
str(data)

#### 1. Explore Missing Data ####
#### Check missing data 
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(data,2,pMiss) # (if >5%, variable (column) should be left from analysis)
apply(data,1,pMiss) # (if >50%, sample (row) should be left from analysis)

missing_summary <- data.frame(
  Variable = names(data),
  Missing_Values = colSums(is.na(data)),
  Missing_Percentage = apply(data, 2, pMiss)
)

# Check missing
missing_summary <- missing_summary[order(-missing_summary$Missing_Values), ]
print(missing_summary)


#### Check pattern
md.pattern(data)
# LEFT: number of samples with pattern
# RIGHT: number of variables missing for those samples
# BOTTOM: number of missing values for variable

#### Test for MCAR
marginplot(data[, c("dd10r_score_m_r1", "md_score_el")], col = mdc(1:2), cex = 1.2,
           cex.lab = 1.2, cex.numbers = 1.3, pch = 19)
# For MCAR values, the red and blue boxes will be identical.

# Little's Test of Complete Randomness (can only test MCAR, not MNAR/MAR)
#na.test(data, digits = 2, p.digits = 3, as.na = NULL, check = TRUE, output = TRUE)
# NOTE: MICE assumes that data is MAR


# Check normal distribution to select imputation method
densityplot(data$dd10r_score_m_r1)
densityplot(data$md_score_el)
# NOTE:  no applicable method for 'densityplot' applied to an object of class "factor" 

#### 2.1 Preliminary/Crude Regression (Reference) ####
fit <- with(data, lm(md_score_el ~ dd10r_score_m_el))
summary(fit)

#### 2.2 Imputation ####

#### A. Conduct basic imputation (select method you want to apply to continuous data)
imp <- mice(data, method="rf", m=1, maxit=1, seed=123)
imp$loggedEvents


#### B. Change method for specific columns (i.e. binary & continuous)
meth <- imp$meth
ord_cat <- c('hfias_bl', 'hfias_el', 'quint_bl', 'ss_score_cont_bl', 'huscomm_score_cont_bl',
             'extcomm_score_cont_bl', 'dec_score_cont_bl', 'noincome_bl')
nom_cat <- c('fam_type_bl')

for (i in ord_cat) { # ordinal data
  data[, i] <- factor(data[, i])
  meth[i] <- "polr"
}
for (i in nom_cat) { # nominal data
  data[, i] <- factor(data[, i])
  meth[i] <- "logreg"
}

pred <- quickpred(data)
print("Number of predictors for fam_type_bl:")
print(sum(pred["fam_type_bl", ]))
print("Active predictors for fam_type_bl:")
print(names(pred["fam_type_bl", ])[pred["fam_type_bl", ] == 1])


#setdiff(nom_cat, colnames(data))
imp <- mice(data, method=meth, m=5, maxit=30, seed=123)
imp$imp


#### 3. Diagnostic Checking ####
plot(imp)

# Check regression Model
#fit <- with(imp, lm(md_score_el ~ dd10r_score_m_el))
#pool.fit <- pool(fit) # With pooled estimates
#summary(pool.fit)

#fit <- with(imp, lm(md_score_el ~ dd10r_score_m_el + dd10r_min_m_el + hfias_el + preg_el))
#pool.fit <- pool(fit)
#summary(pool.fit)


#### Export Imputation ####
for (i in 1:5) {
  result <- complete(imp, i)  # Extrae la i-ésima imputación
  file_name <- paste0("Data/Imputation/FSN_MH_ImpData_", i,"_2024.csv")  
  write.csv(result, file_name, row.names = FALSE)  
}

