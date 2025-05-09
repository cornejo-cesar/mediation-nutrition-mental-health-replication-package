#==============================================================================
# SEQUENTIAL MEDIATION ANALYSIS: FOOD SECURITY, EMPOWERMENT, AND 
# DIETARY DIVERSITY PATHWAYS BETWEEN HFP INTERVENTION AND DEPRESSION 
# AUTHOR: 
# DATE CREATED:  2025-04-22
# LAST MODIFIED: 2025-04-22
#
# DESCRIPTION:
#
#   This script analyzes the sequential mediation pathways between the HFP 
#   intervention and depression outcomes, with food security, empowerment 
#   and dietary diversity as a mediating variables
#
#   SAMPLE:
#   - 2,513 women with complete endline observations
#   - Cluster-randomized at village level
#
#   VARIABLES:
#   Primary Outcome:
#   - Depression (EPDS score, cutoff ≥12)
#   
#   Sequential Mediators:
#   1. Food Security (HFIAS categorical measure)
#   2. Empowerment (ss_score_cont_el, extcomm_score_cont_el, 
#      huscomm_score_cont_el, dec_score_cont_el, noincome_el, lefthome_el)
#   3. Dietary Diversity (DD10r score, Ramadan-adjusted)
#
#   Covariates:
#   - Demographic: age, religion, family type
#   - Socioeconomic: dependency ratio, wealth quintiles, education
#   - Agricultural: prod_e_y14
#   - Food security baseline: HFIAS, dietary diversity
#   - Women's agency: no income, mobility, social support, communication, lefthome, el_gen238_ew
#   - Mental health: baseline depression score
#
#   Sensitivity Analysis:
#   - Women's agency: no income, mobility, social support, communication, lefthome, 
#     el_gen238_ew, el_selfefficacy, el_network_score_cont
#
#==============================================================================

#------------------------------------------------------------------------------
# SETUP
#------------------------------------------------------------------------------

# Clear environment
rm(list = ls())

# Install required packages
packages <- c("readstata13", "medflex", "Matrix", "arm", "glmnet", 
              "mice", "epiDisplay", "dplyr", "openxlsx")
install.packages(packages[!packages %in% installed.packages()[,"Package"]])

# Detach all loaded packages
if (!is.null(sessionInfo()$otherPkgs)) {
  lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=""),
         detach, character.only=TRUE, unload=TRUE)
}

# Load required packages
library(readstata13)
library(medflex)
library(Matrix)
library(arm)
library(glmnet)
library(mice)
library(epiDisplay)
library(dplyr)
library(openxlsx)

#------------------------------------------------------------------------------
# DATA PREPARATION
#------------------------------------------------------------------------------

# Load and prepare dataset for analysis
# Select relevant variables and create analysis sample

# Set working directory 
setwd('/Users/cesarm3/Downloads/Replication package')


# Load Data
df <- read.dta13("Data/8_Master_data.dta")

# Set seed for reproducibility
set.seed(123) 


sub <- df %>% dplyr::select(age_bl, age_bl_i, anemic_all_bl, avg_hfias_cat, avg_hfias_cat_sandEL, 
                            avg_hfias_cat_sandEL_i_rev, avg_hfias_cat_sandEL_rev, avg_hfias_sandEL_categorized,
                            c_code, com_score_bl, DD10r_score_avg_sandELr, DD10r_score_avg_2017r_i, DD10r_score_avg_2017r,
                            DD10r_score_avg_sandELr_i, dd10r_min_el, dd10r_min_m_el, dd10r_score_bl, dd10r_score_el, 
                            dd10r_score_m_noramadan_bl, dd10r_score_m_noramadan_bl_i, dd10r_score_m_el, dec_bl, 
                            dec_score_cont_bl, dec_score_cont_bl_i, dec_score_cont_el,dep_ratio, dep_ratio_i, 
                            extcomm_score_cont_bl, extcomm_score_cont_bl_i, extcomm_score_cont_el, fam_type_bl, 
                            fam_type_bl_i, hfias_bl, hfias_bl_rev, hfias_bl_i_rev, huscomm_score_cont_bl, DD10r_score_avg_2017e_i,
                            huscomm_score_cont_bl_i, huscomm_score_cont_el, impgarden_bl, lefthome_bl, lefthome_bl_i, lefthome_el, 
                            lefthome_el_i, lit_cat, md_d11_bl, md_d11_el, md_d12_bl, md_d12_el, md_d13_bl, md_d13_el, md_score_bl,
                            md_score_el, md_score_bl_i, md_scale_bl, md_scale_el, prod_e_y14, prod_e_y14_i, quint_bl, 
                            quint_bl_i, religion_bl, religion_bl_i, ss_score_cont_bl, ss_score_cont_bl_i, ss_score_cont_el,  
                            treat, wcode, woman_edu_cat_bl, woman_edu_cat_bl_i, noincome_bl, noincome_bl_i, noincome_el,
                            ss_score_cont_el_i, extcomm_score_cont_el_i, huscomm_score_cont_el_i, dec_score_cont_el_i, 
                            noincome_el_i, lefthome_el_i, el_gen238_ew, el_selfefficacy, el_network_score_cont)

#------------------------------------------------------------------------------
# 1. SEQUENTIAL MEDIATION ANALYSIS - IMPUTATION
#------------------------------------------------------------------------------
# Conduct mediation analysis with two sequential mediators:
# M1: Food security  (avg_hfias_cat_sandEL_i_rev)
# M2: Empowerment  (ss_score extcomm_score huscomm_score dec_score noincome lefthome el_gen238)
# M3: Dietary diversity (DD10r_score_avg_2017r)
# 
# Steps:
# A. Estimate direct effect and combined mediation effect (M1 + M2 + M3)
# B. Estimate direct effect and M1 + M2 only mediation effect
# C. Estimate direct effect and M1 only mediation effect
# D. Calculate total effects and decompose pathways
# 

# A. Get direct effect & effect mediated via M1, M2 & M3

impData <- neImpute(md_d12_el ~ factor(treat)+   
                      avg_hfias_cat_sandEL_i_rev + ss_score_cont_el_i + extcomm_score_cont_el_i + huscomm_score_cont_el_i +
                      dec_score_cont_el_i + noincome_el_i + lefthome_el_i + el_gen238_ew + DD10r_score_avg_2017r_i + I(avg_hfias_cat_sandEL_i_rev^2) +
                      age_bl_i + religion_bl_i + fam_type_bl_i + dep_ratio_i + quint_bl_i + woman_edu_cat_bl_i + prod_e_y14_i +
                      hfias_bl_i_rev + dd10r_score_m_noramadan_bl_i + noincome_bl_i + lefthome_bl_i + ss_score_cont_bl_i +
                      huscomm_score_cont_bl_i + extcomm_score_cont_bl_i + dec_score_cont_bl_i +  md_score_bl_i,
                    family = binomial, data = sub, nMed=9)

neModC <- neModel(md_d12_el ~ treat0 + treat1 +
                    age_bl_i + religion_bl_i + fam_type_bl_i + dep_ratio_i + quint_bl_i + woman_edu_cat_bl_i + prod_e_y14_i +
                    hfias_bl_i_rev + dd10r_score_m_noramadan_bl_i + noincome_bl_i + lefthome_bl_i + ss_score_cont_bl_i +
                    huscomm_score_cont_bl_i + extcomm_score_cont_bl_i + dec_score_cont_bl_i +  md_score_bl_i,
                  family = binomial, expData = impData, se = "bootstrap")

summary(neModC)
neModC$neModelFit

# B. Get direct effect & effect mediated via M1 & M2 only
impData <- neImpute(md_d12_el ~ factor(treat) + 
                      avg_hfias_cat_sandEL_i_rev + ss_score_cont_el_i + extcomm_score_cont_el_i + huscomm_score_cont_el_i +
                      dec_score_cont_el_i + noincome_el_i + lefthome_el_i + el_gen238_ew + I(avg_hfias_cat_sandEL_i_rev^2) +
                      age_bl_i + religion_bl_i + fam_type_bl_i + dep_ratio_i + quint_bl_i + woman_edu_cat_bl_i + prod_e_y14_i +
                      hfias_bl_i_rev + dd10r_score_m_noramadan_bl_i + noincome_bl_i + lefthome_bl_i + ss_score_cont_bl_i +
                      huscomm_score_cont_bl_i + extcomm_score_cont_bl_i + dec_score_cont_bl_i +  md_score_bl_i,
                    family = binomial, data = sub, nMed=8)

neModB <- neModel(md_d12_el ~ treat0 + treat1 +
                    age_bl_i + religion_bl_i + fam_type_bl_i + dep_ratio_i + quint_bl_i + woman_edu_cat_bl_i + prod_e_y14_i +
                    hfias_bl_i_rev + dd10r_score_m_noramadan_bl_i + noincome_bl_i + lefthome_bl_i + ss_score_cont_bl_i +
                    huscomm_score_cont_bl_i + extcomm_score_cont_bl_i + dec_score_cont_bl_i +  md_score_bl_i,
                  family = binomial, expData = impData, se = "bootstrap")

summary(neModB)
neModB$neModelFit


# C. Get direct effect & effect mediated via M1 only
impData <- neImpute(md_d12_el ~ factor(treat) + 
                      avg_hfias_cat_sandEL_i_rev + I(avg_hfias_cat_sandEL_i_rev^2) +
                      age_bl_i + religion_bl_i + fam_type_bl_i + dep_ratio_i + quint_bl_i + woman_edu_cat_bl_i + prod_e_y14_i +
                      hfias_bl_i_rev + dd10r_score_m_noramadan_bl_i + noincome_bl_i + lefthome_bl_i + ss_score_cont_bl_i +
                      huscomm_score_cont_bl_i + extcomm_score_cont_bl_i + dec_score_cont_bl_i +  md_score_bl_i,
                    family = binomial, data = sub, nMed=1)


neModA <- neModel(md_d12_el ~ treat0 + treat1 +
                    age_bl_i + religion_bl_i + fam_type_bl_i + dep_ratio_i + quint_bl_i + woman_edu_cat_bl_i + prod_e_y14_i +
                    hfias_bl_i_rev + dd10r_score_m_noramadan_bl_i + noincome_bl_i + lefthome_bl_i + ss_score_cont_bl_i +
                    huscomm_score_cont_bl_i + extcomm_score_cont_bl_i + dec_score_cont_bl_i +  md_score_bl_i,
                  family = binomial, expData = impData, se = "bootstrap")

summary(neModA)
neModC$neModelFit

# Get overall indirect effect of ALL mediators (en bloc)
total_eff <- neEffdecomp(neModC)
summary(total_eff)

# Create result dataframe
result <- rbind(summary(total_eff)$coefficients[1,],  # Direct effect
                summary(total_eff)$coefficients[2,],  # Indirect effect
                summary(total_eff)$coefficients[3,],  # Total effect
                summary(neModA)$coefficients[3,],     # M1 effect (HFIAS)
                summary(neModB)$coefficients[3,] - summary(neModA)$coefficients[3,],  # M2 effect (Empowerment)
                summary(neModC)$coefficients[3,] - summary(neModB)$coefficients[3,])  # M3 effect (DD)

row.names(result) <- c("direct_effect", "indirect_effect", "total_effect", 
                       "med1_effect", "med2_effect", "med3_effect")
result <- as.data.frame(result)
colnames(result) <- c("LogOdds_Scale", "Std_Error", "Z_score", "P_Value")

# Get CI's & p-values
result$LogOdds_LowerCI <- result$LogOdds_Scale - 1.96 * result$Std_Error
result$LogOdds_UpperCI <- result$LogOdds_Scale + 1.96 * result$Std_Error
result$Odds_Ratio <- exp(result$LogOdds_Scale)
result$Odds_LowerCI <- exp(result$LogOdds_LowerCI)
result$Odds_UpperCI <- exp(result$LogOdds_UpperCI)

result <- result %>% 
  select(Odds_Ratio, Odds_LowerCI, Odds_UpperCI, 
         LogOdds_Scale, Std_Error, Z_score, P_Value, 
         LogOdds_LowerCI, LogOdds_UpperCI)

result <- cbind(Effect = rownames(result), result)
rownames(result) <- NULL

noBoot_Imp_results_2017 <- result
noBoot_Imp_results_2017 

#------------------------------------------------------------------------------
# BOOTSTRAP ANALYSIS
#------------------------------------------------------------------------------

# Set Variables
data=sub
cluster = sub$c_code
reps=1000


# Initial models for all three mediator combinations
basic3 <- glm(md_d12_el ~ factor(treat) + 
                avg_hfias_cat_sandEL_i_rev + ss_score_cont_el_i + extcomm_score_cont_el_i + 
                huscomm_score_cont_el_i + dec_score_cont_el_i + noincome_el_i + lefthome_el_i + 
                el_gen238_ew + DD10r_score_avg_2017r_i + I(avg_hfias_cat_sandEL_i_rev^2), 
              family = binomial, data = data)

basic2 <- glm(md_d12_el ~ factor(treat) + 
                avg_hfias_cat_sandEL_i_rev + ss_score_cont_el_i + extcomm_score_cont_el_i + 
                huscomm_score_cont_el_i + dec_score_cont_el_i + noincome_el_i + lefthome_el_i +  
                el_gen238_ew + I(avg_hfias_cat_sandEL_i_rev^2),
              family = binomial, data = data)

basic1 <- glm(md_d12_el ~ factor(treat) + 
                avg_hfias_cat_sandEL_i_rev + I(avg_hfias_cat_sandEL_i_rev^2),
              family = binomial, data = data)

# Bootstrap preparation
clusters <- names(table(cluster))

impData3 <- neImpute(basic3, nMed=9)
impData2 <- neImpute(basic2, nMed=8)
impData1 <- neImpute(basic1, nMed=1)

neMod3 <- neModel(md_d12_el ~ treat0 + treat1, family = binomial, 
                  expData = impData3, se = "bootstrap", nBoot = 1)
neMod2 <- neModel(md_d12_el ~ treat0 + treat1, family = binomial, 
                  expData = impData2, se = "bootstrap", nBoot = 1)
neMod1 <- neModel(md_d12_el ~ treat0 + treat1, family = binomial, 
                  expData = impData1, se = "bootstrap", nBoot = 1)

# Initialize matrix for storing bootstrap results
sterrs <- matrix(NA, nrow=reps, ncol=6)

# Bootstrap loop
for(i in 1:reps) {
  # Cluster bootstrap sampling
  index <- sample(1:length(clusters), length(clusters), replace=TRUE)
  aa <- clusters[index]
  bb <- table(aa)
  bootdat <- NULL
  
  for(j in 1:max(bb)) {
    cc <- data[cluster %in% names(bb[bb %in% j]),]
    for(k in 1:j) {
      bootdat <- rbind(bootdat, cc)
    }
  }
  
  # Fit models on bootstrap sample
  basic3 <- glm(md_d12_el ~ factor(treat) + avg_hfias_cat_sandEL_i_rev + 
                  ss_score_cont_el_i + extcomm_score_cont_el_i + huscomm_score_cont_el_i + 
                  dec_score_cont_el_i + noincome_el_i + lefthome_el_i + el_gen238_ew + DD10r_score_avg_2017r_i + 
                  I(avg_hfias_cat_sandEL_i_rev^2), 
                family = binomial, data = bootdat)
  
  basic2 <- glm(md_d12_el ~ factor(treat) + avg_hfias_cat_sandEL_i_rev + 
                  ss_score_cont_el_i + extcomm_score_cont_el_i + huscomm_score_cont_el_i + 
                  dec_score_cont_el_i + noincome_el_i + lefthome_el_i + el_gen238_ew + I(avg_hfias_cat_sandEL_i_rev^2),
                family = binomial, data = bootdat)
  
  basic1 <- glm(md_d12_el ~ factor(treat) + avg_hfias_cat_sandEL_i_rev + 
                  I(avg_hfias_cat_sandEL_i_rev^2),
                family = binomial, data = bootdat)
  
  # Get natural effects for each model
  impData3 <- neImpute(basic3, nMed=9)
  impData2 <- neImpute(basic2, nMed=8)
  impData1 <- neImpute(basic1, nMed=1)
  
  neMod3b <- neModel(md_d12_el ~ treat0 + treat1, family = binomial, 
                     expData = impData3, se = "bootstrap", nBoot = 1)
  neMod2b <- neModel(md_d12_el ~ treat0 + treat1, family = binomial, 
                     expData = impData2, se = "bootstrap", nBoot = 1)
  neMod1b <- neModel(md_d12_el ~ treat0 + treat1, family = binomial, 
                     expData = impData1, se = "bootstrap", nBoot = 1)
  
  # Store results
  sterrs[i,1] <- coef(neMod3b$neModelFit)[2]                               # direct
  sterrs[i,2] <- coef(neMod3b$neModelFit)[3]                               # indirect
  sterrs[i,3] <- coef(neMod3b$neModelFit)[2] + coef(neMod3b$neModelFit)[3] # total
  sterrs[i,4] <- coef(neMod1b$neModelFit)[3]                               # M1
  sterrs[i,5] <- coef(neMod2b$neModelFit)[3] - coef(neMod1b$neModelFit)[3] # M2
  sterrs[i,6] <- coef(neMod3b$neModelFit)[3] - coef(neMod2b$neModelFit)[3] # M3
}

# Process bootstrap results
d <- apply(sterrs, 2, sd)
result <- rbind(
  cbind(coef(neMod3$neModelFit)[2], d[1]),                              # direct
  cbind(coef(neMod3$neModelFit)[3], d[2]),                              # indirect
  cbind(coef(neMod3$neModelFit)[2] + coef(neMod3$neModelFit)[3], d[3]), # total
  cbind(coef(neMod1$neModelFit)[3], d[4]),                              # M1
  cbind(coef(neMod2$neModelFit)[3] - coef(neMod1$neModelFit)[3], d[5]), # M2
  cbind(coef(neMod3$neModelFit)[3] - coef(neMod2$neModelFit)[3], d[6])  # M3
)

row.names(result) <- c("direct_effect", "indirect_effect", "total_effect",
                       "med1_effect", "med2_effect", "med3_effect")
result <- as.data.frame(result)

# Calculate statistics
z <- result[,1]/result[,2]
p <- 2 * (1-pnorm(abs(z)))
LowerCI <- result[,1] - 1.96*result[,2]
UpperCI <- result[,1] + 1.96*result[,2]

result <- cbind(result, z, LowerCI, UpperCI, p)
result$Odds_Ratio <- exp(result[,1])

colnames(result) <- c("LogOdds_Scale", "Std_Error", "Z_score",
                      "LogOdds_LowerCI", "LogOdds_UpperCI", "P_Value", "Odds_Ratio")
result$Odds_LowerCI <- exp(result$LogOdds_LowerCI)
result$Odds_UpperCI <- exp(result$LogOdds_UpperCI)

boot_Imp_results_2017 <- result %>%
  select(Odds_Ratio, Odds_LowerCI, Odds_UpperCI,
         LogOdds_Scale, Std_Error, Z_score, P_Value,
         LogOdds_LowerCI, LogOdds_UpperCI)

boot_Imp_results_2017 <- cbind(Effect = rownames(boot_Imp_results_2017), boot_Imp_results_2017)
rownames(boot_Imp_results_2017) <- NULL

print(boot_Imp_results_2017)


#------------------------------------------------------------------------------
# 3. SENSITIVITY ANALYSIS
#------------------------------------------------------------------------------

# Dietary diversity measured as the average across survey rounds 
#  onwards, including the endline DD10r_score_avg_2017r_i


#------------------------------------------------------------------------------
# 1. SEQUENTIAL MEDIATION ANALYSIS - IMPUTATION
#------------------------------------------------------------------------------
# Conduct mediation analysis with two sequential mediators:
# M1: Food security  (avg_hfias_cat_sandEL_i)
# M2: Empowerment  (ss_score extcomm_score huscomm_score dec_score noincome lefthome el_gen238)
# M3: Dietary diversity (DD10r_score_avg_2017r)
# 
# Steps:
# A. Estimate direct effect and combined mediation effect (M1 + M2 + M3)
# B. Estimate direct effect and M1 + M2 only mediation effect
# C. Estimate direct effect and M1 only mediation effect
# D. Calculate total effects and decompose pathways

# A. Get direct effect & effect mediated via M1, M2 & M3

impData <- neImpute(md_d12_el ~ factor(treat)+   
                      avg_hfias_cat_sandEL_i_rev + ss_score_cont_el_i + extcomm_score_cont_el_i + huscomm_score_cont_el_i +
                      dec_score_cont_el_i + noincome_el_i + lefthome_el_i + el_gen238_ew + el_selfefficacy + 
                      el_network_score_cont + DD10r_score_avg_2017r_i + I(avg_hfias_cat_sandEL_i_rev^2) +
                      age_bl_i + religion_bl_i + fam_type_bl_i + dep_ratio_i + quint_bl_i + woman_edu_cat_bl_i + prod_e_y14_i +
                      hfias_bl_i_rev + dd10r_score_m_noramadan_bl_i + noincome_bl_i + lefthome_bl_i + ss_score_cont_bl_i +
                      huscomm_score_cont_bl_i + extcomm_score_cont_bl_i + dec_score_cont_bl_i +  md_score_bl_i,
                    family = binomial, data = sub, nMed=11)

neModC <- neModel(md_d12_el ~ treat0 + treat1 +
                    age_bl_i + religion_bl_i + fam_type_bl_i + dep_ratio_i + quint_bl_i + woman_edu_cat_bl_i + prod_e_y14_i +
                    hfias_bl_i_rev + dd10r_score_m_noramadan_bl_i + noincome_bl_i + lefthome_bl_i + ss_score_cont_bl_i +
                    huscomm_score_cont_bl_i + extcomm_score_cont_bl_i + dec_score_cont_bl_i +  md_score_bl_i,
                  family = binomial, expData = impData, se = "bootstrap")

summary(neModC)
neModC$neModelFit

# B. Get direct effect & effect mediated via M1 & M2 only
impData <- neImpute(md_d12_el ~ factor(treat) + 
                      avg_hfias_cat_sandEL_i_rev + ss_score_cont_el_i + extcomm_score_cont_el_i + huscomm_score_cont_el_i +
                      dec_score_cont_el_i + noincome_el_i + lefthome_el_i + el_gen238_ew + el_selfefficacy + 
                      el_network_score_cont + I(avg_hfias_cat_sandEL_i_rev^2) +
                      age_bl_i + religion_bl_i + fam_type_bl_i + dep_ratio_i + quint_bl_i + woman_edu_cat_bl_i + prod_e_y14_i +
                      hfias_bl_i_rev + dd10r_score_m_noramadan_bl_i + noincome_bl_i + lefthome_bl_i + ss_score_cont_bl_i +
                      huscomm_score_cont_bl_i + extcomm_score_cont_bl_i + dec_score_cont_bl_i +  md_score_bl_i,
                    family = binomial, data = sub, nMed=10)

neModB <- neModel(md_d12_el ~ treat0 + treat1 +
                    age_bl_i + religion_bl_i + fam_type_bl_i + dep_ratio_i + quint_bl_i + woman_edu_cat_bl_i + prod_e_y14_i +
                    hfias_bl_i_rev + dd10r_score_m_noramadan_bl_i + noincome_bl_i + lefthome_bl_i + ss_score_cont_bl_i +
                    huscomm_score_cont_bl_i + extcomm_score_cont_bl_i + dec_score_cont_bl_i +  md_score_bl_i,
                  family = binomial, expData = impData, se = "bootstrap")

summary(neModB)
neModB$neModelFit


# C. Get direct effect & effect mediated via M1 only
impData <- neImpute(md_d12_el ~ factor(treat) + 
                      avg_hfias_cat_sandEL_i_rev + I(avg_hfias_cat_sandEL_i_rev^2) +
                      age_bl_i + religion_bl_i + fam_type_bl_i + dep_ratio_i + quint_bl_i + woman_edu_cat_bl_i + prod_e_y14_i +
                      hfias_bl_i_rev + dd10r_score_m_noramadan_bl_i + noincome_bl_i + lefthome_bl_i + ss_score_cont_bl_i +
                      huscomm_score_cont_bl_i + extcomm_score_cont_bl_i + dec_score_cont_bl_i +  md_score_bl_i,
                    family = binomial, data = sub, nMed=1)


neModA <- neModel(md_d12_el ~ treat0 + treat1 +
                    age_bl_i + religion_bl_i + fam_type_bl_i + dep_ratio_i + quint_bl_i + woman_edu_cat_bl_i + prod_e_y14_i +
                    hfias_bl_i_rev + dd10r_score_m_noramadan_bl_i + noincome_bl_i + lefthome_bl_i + ss_score_cont_bl_i +
                    huscomm_score_cont_bl_i + extcomm_score_cont_bl_i + dec_score_cont_bl_i +  md_score_bl_i,
                  family = binomial, expData = impData, se = "bootstrap")

summary(neModA)
neModC$neModelFit

# Get overall indirect effect of ALL mediators (en bloc)
total_eff <- neEffdecomp(neModC)
summary(total_eff)

# Create result dataframe
result <- rbind(summary(total_eff)$coefficients[1,],  # Direct effect
                summary(total_eff)$coefficients[2,],  # Indirect effect
                summary(total_eff)$coefficients[3,],  # Total effect
                summary(neModA)$coefficients[3,],     # M1 effect (HFIAS)
                summary(neModB)$coefficients[3,] - summary(neModA)$coefficients[3,],  # M2 effect (Empowerment)
                summary(neModC)$coefficients[3,] - summary(neModB)$coefficients[3,])  # M3 effect (DD)

row.names(result) <- c("direct_effect", "indirect_effect", "total_effect", 
                       "med1_effect", "med2_effect", "med3_effect")
result <- as.data.frame(result)
colnames(result) <- c("LogOdds_Scale", "Std_Error", "Z_score", "P_Value")

# Get CI's & p-values
result$LogOdds_LowerCI <- result$LogOdds_Scale - 1.96 * result$Std_Error
result$LogOdds_UpperCI <- result$LogOdds_Scale + 1.96 * result$Std_Error
result$Odds_Ratio <- exp(result$LogOdds_Scale)
result$Odds_LowerCI <- exp(result$LogOdds_LowerCI)
result$Odds_UpperCI <- exp(result$LogOdds_UpperCI)

result <- result %>% 
  select(Odds_Ratio, Odds_LowerCI, Odds_UpperCI, 
         LogOdds_Scale, Std_Error, Z_score, P_Value, 
         LogOdds_LowerCI, LogOdds_UpperCI)

result <- cbind(Effect = rownames(result), result)
rownames(result) <- NULL

noBoot_Imp_results <- result
noBoot_Imp_results 

#------------------------------------------------------------------------------
# BOOTSTRAP ANALYSIS
#------------------------------------------------------------------------------

# Set Variables
data=sub
cluster = sub$c_code
reps=1000


# Initial models for all three mediator combinations
basic3 <- glm(md_d12_el ~ factor(treat) + 
                avg_hfias_cat_sandEL_i_rev + ss_score_cont_el_i + extcomm_score_cont_el_i + 
                huscomm_score_cont_el_i + dec_score_cont_el_i + noincome_el_i + lefthome_el_i + 
                el_gen238_ew + el_selfefficacy + el_network_score_cont + 
                DD10r_score_avg_2017r_i + I(avg_hfias_cat_sandEL_i_rev^2), 
              family = binomial, data = data)

basic2 <- glm(md_d12_el ~ factor(treat) + 
                avg_hfias_cat_sandEL_i_rev + ss_score_cont_el_i + extcomm_score_cont_el_i + 
                huscomm_score_cont_el_i + dec_score_cont_el_i + noincome_el_i + lefthome_el_i +  
                el_gen238_ew + el_selfefficacy + el_network_score_cont + I(avg_hfias_cat_sandEL_i_rev^2),
              family = binomial, data = data)

basic1 <- glm(md_d12_el ~ factor(treat) + 
                avg_hfias_cat_sandEL_i_rev + I(avg_hfias_cat_sandEL_i_rev^2),
              family = binomial, data = data)

# Bootstrap preparation
clusters <- names(table(cluster))

impData3 <- neImpute(basic3, nMed=11)
impData2 <- neImpute(basic2, nMed=10)
impData1 <- neImpute(basic1, nMed=1)

neMod3 <- neModel(md_d12_el ~ treat0 + treat1, family = binomial, 
                  expData = impData3, se = "bootstrap", nBoot = 1)
neMod2 <- neModel(md_d12_el ~ treat0 + treat1, family = binomial, 
                  expData = impData2, se = "bootstrap", nBoot = 1)
neMod1 <- neModel(md_d12_el ~ treat0 + treat1, family = binomial, 
                  expData = impData1, se = "bootstrap", nBoot = 1)

# Initialize matrix for storing bootstrap results
sterrs <- matrix(NA, nrow=reps, ncol=6)

# Bootstrap loop
for(i in 1:reps) {
  # Cluster bootstrap sampling
  index <- sample(1:length(clusters), length(clusters), replace=TRUE)
  aa <- clusters[index]
  bb <- table(aa)
  bootdat <- NULL
  
  for(j in 1:max(bb)) {
    cc <- data[cluster %in% names(bb[bb %in% j]),]
    for(k in 1:j) {
      bootdat <- rbind(bootdat, cc)
    }
  }
  
  # Fit models on bootstrap sample
  basic3 <- glm(md_d12_el ~ factor(treat) + avg_hfias_cat_sandEL_i_rev + 
                  ss_score_cont_el_i + extcomm_score_cont_el_i + huscomm_score_cont_el_i + 
                  dec_score_cont_el_i + noincome_el_i + lefthome_el_i + el_gen238_ew + el_selfefficacy + 
                  el_network_score_cont + DD10r_score_avg_2017r_i + I(avg_hfias_cat_sandEL_i_rev^2), 
                family = binomial, data = bootdat)
  
  basic2 <- glm(md_d12_el ~ factor(treat) + avg_hfias_cat_sandEL_i_rev + 
                  ss_score_cont_el_i + extcomm_score_cont_el_i + huscomm_score_cont_el_i + 
                  dec_score_cont_el_i + noincome_el_i + lefthome_el_i + el_gen238_ew + 
                  el_selfefficacy + el_network_score_cont + I(avg_hfias_cat_sandEL_i_rev^2),
                family = binomial, data = bootdat)
  
  basic1 <- glm(md_d12_el ~ factor(treat) + avg_hfias_cat_sandEL_i_rev + 
                  I(avg_hfias_cat_sandEL_i_rev^2),
                family = binomial, data = bootdat)
  
  # Get natural effects for each model
  impData3 <- neImpute(basic3, nMed=11)
  impData2 <- neImpute(basic2, nMed=10)
  impData1 <- neImpute(basic1, nMed=1)
  
  neMod3b <- neModel(md_d12_el ~ treat0 + treat1, family = binomial, 
                     expData = impData3, se = "bootstrap", nBoot = 1)
  neMod2b <- neModel(md_d12_el ~ treat0 + treat1, family = binomial, 
                     expData = impData2, se = "bootstrap", nBoot = 1)
  neMod1b <- neModel(md_d12_el ~ treat0 + treat1, family = binomial, 
                     expData = impData1, se = "bootstrap", nBoot = 1)
  
  # Store results
  sterrs[i,1] <- coef(neMod3b$neModelFit)[2]                               # direct
  sterrs[i,2] <- coef(neMod3b$neModelFit)[3]                               # indirect
  sterrs[i,3] <- coef(neMod3b$neModelFit)[2] + coef(neMod3b$neModelFit)[3] # total
  sterrs[i,4] <- coef(neMod1b$neModelFit)[3]                               # M1
  sterrs[i,5] <- coef(neMod2b$neModelFit)[3] - coef(neMod1b$neModelFit)[3] # M2
  sterrs[i,6] <- coef(neMod3b$neModelFit)[3] - coef(neMod2b$neModelFit)[3] # M3
}

# Process bootstrap results
d <- apply(sterrs, 2, sd)
result <- rbind(
  cbind(coef(neMod3$neModelFit)[2], d[1]),                              # direct
  cbind(coef(neMod3$neModelFit)[3], d[2]),                              # indirect
  cbind(coef(neMod3$neModelFit)[2] + coef(neMod3$neModelFit)[3], d[3]), # total
  cbind(coef(neMod1$neModelFit)[3], d[4]),                              # M1
  cbind(coef(neMod2$neModelFit)[3] - coef(neMod1$neModelFit)[3], d[5]), # M2
  cbind(coef(neMod3$neModelFit)[3] - coef(neMod2$neModelFit)[3], d[6])  # M3
)

row.names(result) <- c("direct_effect", "indirect_effect", "total_effect",
                       "med1_effect", "med2_effect", "med3_effect")
result <- as.data.frame(result)

# Calculate statistics
z <- result[,1]/result[,2]
p <- 2 * (1-pnorm(abs(z)))
LowerCI <- result[,1] - 1.96*result[,2]
UpperCI <- result[,1] + 1.96*result[,2]

result <- cbind(result, z, LowerCI, UpperCI, p)
result$Odds_Ratio <- exp(result[,1])

colnames(result) <- c("LogOdds_Scale", "Std_Error", "Z_score",
                      "LogOdds_LowerCI", "LogOdds_UpperCI", "P_Value", "Odds_Ratio")
result$Odds_LowerCI <- exp(result$LogOdds_LowerCI)
result$Odds_UpperCI <- exp(result$LogOdds_UpperCI)

boot_Imp_results <- result %>%
  select(Odds_Ratio, Odds_LowerCI, Odds_UpperCI,
         LogOdds_Scale, Std_Error, Z_score, P_Value,
         LogOdds_LowerCI, LogOdds_UpperCI)

boot_Imp_results <- cbind(Effect = rownames(boot_Imp_results), boot_Imp_results)
rownames(boot_Imp_results) <- NULL

print(boot_Imp_results)


#------------------------------------------------------------------------------
# 5. SENSITIVITY ANALYSIS - NO IMPUTATION
#------------------------------------------------------------------------------

# Dietary diversity measured as the average across survey rounds 
#  onwards, including the endline DD10r_score_avg_2017r


#------------------------------------------------------------------------------
# 1. SEQUENTIAL MEDIATION ANALYSIS - NO IMPUTATION
#------------------------------------------------------------------------------
# Conduct mediation analysis with two sequential mediators:
# M1: Food security  (avg_hfias_cat_sandEL)
# M2: Empowerment  (ss_score extcomm_score huscomm_score dec_score noincome lefthome el_gen238)
# M3: Dietary diversity (DD10r_score_avg_2017r)
# 
# Steps:
# A. Estimate direct effect and combined mediation effect (M1 + M2 + M3)
# B. Estimate direct effect and M1 + M2 only mediation effect
# C. Estimate direct effect and M1 only mediation effect
# D. Calculate total effects and decompose pathways

# A. Get direct effect & effect mediated via M1, M2 & M3

impData <- neImpute(md_d12_el ~ factor(treat)+   
                      avg_hfias_cat_sandEL_rev + ss_score_cont_el + extcomm_score_cont_el + huscomm_score_cont_el +
                      dec_score_cont_el + noincome_el + lefthome_el + el_gen238_ew + el_selfefficacy + 
                      el_network_score_cont + DD10r_score_avg_2017r + I(avg_hfias_cat_sandEL_rev^2) +
                      age_bl + religion_bl + fam_type_bl + dep_ratio + quint_bl + woman_edu_cat_bl + prod_e_y14 +
                      hfias_bl_rev + dd10r_score_m_noramadan_bl + noincome_bl + lefthome_bl + ss_score_cont_bl +
                      huscomm_score_cont_bl + extcomm_score_cont_bl + dec_score_cont_bl +  md_score_bl,
                    family = binomial, data = sub, nMed=11)

neModC <- neModel(md_d12_el ~ treat0 + treat1 +
                    age_bl + religion_bl + fam_type_bl + dep_ratio + quint_bl + woman_edu_cat_bl + prod_e_y14 +
                    hfias_bl_rev + dd10r_score_m_noramadan_bl + noincome_bl + lefthome_bl + ss_score_cont_bl +
                    huscomm_score_cont_bl + extcomm_score_cont_bl + dec_score_cont_bl +  md_score_bl,
                  family = binomial, expData = impData, se = "bootstrap")

summary(neModC)
neModC$neModelFit

# B. Get direct effect & effect mediated via M1 & M2 only
impData <- neImpute(md_d12_el ~ factor(treat) + 
                      avg_hfias_cat_sandEL_rev + ss_score_cont_el + extcomm_score_cont_el + huscomm_score_cont_el +
                      dec_score_cont_el + noincome_el + lefthome_el + el_gen238_ew + el_selfefficacy + 
                      el_network_score_cont + I(avg_hfias_cat_sandEL_rev^2) +
                      age_bl + religion_bl + fam_type_bl + dep_ratio + quint_bl + woman_edu_cat_bl + prod_e_y14 +
                      hfias_bl_rev + dd10r_score_m_noramadan_bl + noincome_bl + lefthome_bl + ss_score_cont_bl +
                      huscomm_score_cont_bl + extcomm_score_cont_bl + dec_score_cont_bl +  md_score_bl,
                    family = binomial, data = sub, nMed=10)

neModB <- neModel(md_d12_el ~ treat0 + treat1 +
                    age_bl + religion_bl + fam_type_bl + dep_ratio + quint_bl + woman_edu_cat_bl + prod_e_y14 +
                    hfias_bl_rev + dd10r_score_m_noramadan_bl + noincome_bl + lefthome_bl + ss_score_cont_bl +
                    huscomm_score_cont_bl + extcomm_score_cont_bl + dec_score_cont_bl +  md_score_bl,
                  family = binomial, expData = impData, se = "bootstrap")

summary(neModB)
neModB$neModelFit


# C. Get direct effect & effect mediated via M1 only
impData <- neImpute(md_d12_el ~ factor(treat) + 
                      avg_hfias_cat_sandEL_rev + I(avg_hfias_cat_sandEL_rev^2) +
                      age_bl + religion_bl + fam_type_bl + dep_ratio + quint_bl + woman_edu_cat_bl + prod_e_y14 +
                      hfias_bl_rev + dd10r_score_m_noramadan_bl + noincome_bl + lefthome_bl + ss_score_cont_bl +
                      huscomm_score_cont_bl + extcomm_score_cont_bl + dec_score_cont_bl +  md_score_bl,
                    family = binomial, data = sub, nMed=1)


neModA <- neModel(md_d12_el ~ treat0 + treat1 +
                    age_bl + religion_bl + fam_type_bl + dep_ratio + quint_bl + woman_edu_cat_bl + prod_e_y14 +
                    hfias_bl_rev + dd10r_score_m_noramadan_bl + noincome_bl + lefthome_bl + ss_score_cont_bl +
                    huscomm_score_cont_bl + extcomm_score_cont_bl + dec_score_cont_bl +  md_score_bl,
                  family = binomial, expData = impData, se = "bootstrap")

summary(neModA)
neModC$neModelFit

# Get overall indirect effect of ALL mediators (en bloc)
total_eff <- neEffdecomp(neModC)
summary(total_eff)

# Create result dataframe
result <- rbind(summary(total_eff)$coefficients[1,],  # Direct effect
                summary(total_eff)$coefficients[2,],  # Indirect effect
                summary(total_eff)$coefficients[3,],  # Total effect
                summary(neModA)$coefficients[3,],     # M1 effect (HFIAS)
                summary(neModB)$coefficients[3,] - summary(neModA)$coefficients[3,],  # M2 effect (Empowerment)
                summary(neModC)$coefficients[3,] - summary(neModB)$coefficients[3,])  # M3 effect (DD)

row.names(result) <- c("direct_effect", "indirect_effect", "total_effect", 
                       "med1_effect", "med2_effect", "med3_effect")
result <- as.data.frame(result)
colnames(result) <- c("LogOdds_Scale", "Std_Error", "Z_score", "P_Value")

# Get CI's & p-values
result$LogOdds_LowerCI <- result$LogOdds_Scale - 1.96 * result$Std_Error
result$LogOdds_UpperCI <- result$LogOdds_Scale + 1.96 * result$Std_Error
result$Odds_Ratio <- exp(result$LogOdds_Scale)
result$Odds_LowerCI <- exp(result$LogOdds_LowerCI)
result$Odds_UpperCI <- exp(result$LogOdds_UpperCI)

result <- result %>% 
  select(Odds_Ratio, Odds_LowerCI, Odds_UpperCI, 
         LogOdds_Scale, Std_Error, Z_score, P_Value, 
         LogOdds_LowerCI, LogOdds_UpperCI)

result <- cbind(Effect = rownames(result), result)
rownames(result) <- NULL

noBoot_noimp_results <- result
noBoot_noimp_results 

#------------------------------------------------------------------------------
# BOOTSTRAP ANALYSIS
#------------------------------------------------------------------------------

# Set Variables
data=sub
cluster = sub$c_code
reps=1000


# Initial models for all three mediator combinations
basic3 <- glm(md_d12_el ~ factor(treat) + 
                avg_hfias_cat_sandEL_rev + ss_score_cont_el + extcomm_score_cont_el + 
                huscomm_score_cont_el + dec_score_cont_el + noincome_el + lefthome_el + 
                el_gen238_ew + el_selfefficacy + el_network_score_cont + 
                DD10r_score_avg_2017r + I(avg_hfias_cat_sandEL_rev^2), 
              family = binomial, data = data)

basic2 <- glm(md_d12_el ~ factor(treat) + 
                avg_hfias_cat_sandEL_rev + ss_score_cont_el + extcomm_score_cont_el + 
                huscomm_score_cont_el + dec_score_cont_el + noincome_el + lefthome_el +  
                el_gen238_ew + el_selfefficacy + el_network_score_cont + I(avg_hfias_cat_sandEL_rev^2),
              family = binomial, data = data)

basic1 <- glm(md_d12_el ~ factor(treat) + 
                avg_hfias_cat_sandEL_rev + I(avg_hfias_cat_sandEL_rev^2),
              family = binomial, data = data)

# Bootstrap preparation
clusters <- names(table(cluster))

impData3 <- neImpute(basic3, nMed=11)
impData2 <- neImpute(basic2, nMed=10)
impData1 <- neImpute(basic1, nMed=1)

neMod3 <- neModel(md_d12_el ~ treat0 + treat1, family = binomial, 
                  expData = impData3, se = "bootstrap", nBoot = 1)
neMod2 <- neModel(md_d12_el ~ treat0 + treat1, family = binomial, 
                  expData = impData2, se = "bootstrap", nBoot = 1)
neMod1 <- neModel(md_d12_el ~ treat0 + treat1, family = binomial, 
                  expData = impData1, se = "bootstrap", nBoot = 1)

# Initialize matrix for storing bootstrap results
sterrs <- matrix(NA, nrow=reps, ncol=6)

# Bootstrap loop
for(i in 1:reps) {
  # Cluster bootstrap sampling
  index <- sample(1:length(clusters), length(clusters), replace=TRUE)
  aa <- clusters[index]
  bb <- table(aa)
  bootdat <- NULL
  
  for(j in 1:max(bb)) {
    cc <- data[cluster %in% names(bb[bb %in% j]),]
    for(k in 1:j) {
      bootdat <- rbind(bootdat, cc)
    }
  }
  
  # Fit models on bootstrap sample
  basic3 <- glm(md_d12_el ~ factor(treat) + avg_hfias_cat_sandEL_rev + 
                  ss_score_cont_el + extcomm_score_cont_el + huscomm_score_cont_el + 
                  dec_score_cont_el + noincome_el + lefthome_el + el_gen238_ew + el_selfefficacy + 
                  el_network_score_cont + DD10r_score_avg_2017r + I(avg_hfias_cat_sandEL_rev^2), 
                family = binomial, data = bootdat)
  
  basic2 <- glm(md_d12_el ~ factor(treat) + avg_hfias_cat_sandEL_rev + 
                  ss_score_cont_el + extcomm_score_cont_el + huscomm_score_cont_el + 
                  dec_score_cont_el + noincome_el + lefthome_el + el_gen238_ew + 
                  el_selfefficacy + el_network_score_cont + I(avg_hfias_cat_sandEL_rev^2),
                family = binomial, data = bootdat)
  
  basic1 <- glm(md_d12_el ~ factor(treat) + avg_hfias_cat_sandEL_rev + 
                  I(avg_hfias_cat_sandEL_rev^2),
                family = binomial, data = bootdat)
  
  # Get natural effects for each model
  impData3 <- neImpute(basic3, nMed=11)
  impData2 <- neImpute(basic2, nMed=10)
  impData1 <- neImpute(basic1, nMed=1)
  
  neMod3b <- neModel(md_d12_el ~ treat0 + treat1, family = binomial, 
                     expData = impData3, se = "bootstrap", nBoot = 1)
  neMod2b <- neModel(md_d12_el ~ treat0 + treat1, family = binomial, 
                     expData = impData2, se = "bootstrap", nBoot = 1)
  neMod1b <- neModel(md_d12_el ~ treat0 + treat1, family = binomial, 
                     expData = impData1, se = "bootstrap", nBoot = 1)
  
  # Store results
  sterrs[i,1] <- coef(neMod3b$neModelFit)[2]                               # direct
  sterrs[i,2] <- coef(neMod3b$neModelFit)[3]                               # indirect
  sterrs[i,3] <- coef(neMod3b$neModelFit)[2] + coef(neMod3b$neModelFit)[3] # total
  sterrs[i,4] <- coef(neMod1b$neModelFit)[3]                               # M1
  sterrs[i,5] <- coef(neMod2b$neModelFit)[3] - coef(neMod1b$neModelFit)[3] # M2
  sterrs[i,6] <- coef(neMod3b$neModelFit)[3] - coef(neMod2b$neModelFit)[3] # M3
}

# Process bootstrap results
d <- apply(sterrs, 2, sd)
result <- rbind(
  cbind(coef(neMod3$neModelFit)[2], d[1]),                              # direct
  cbind(coef(neMod3$neModelFit)[3], d[2]),                              # indirect
  cbind(coef(neMod3$neModelFit)[2] + coef(neMod3$neModelFit)[3], d[3]), # total
  cbind(coef(neMod1$neModelFit)[3], d[4]),                              # M1
  cbind(coef(neMod2$neModelFit)[3] - coef(neMod1$neModelFit)[3], d[5]), # M2
  cbind(coef(neMod3$neModelFit)[3] - coef(neMod2$neModelFit)[3], d[6])  # M3
)

row.names(result) <- c("direct_effect", "indirect_effect", "total_effect",
                       "med1_effect", "med2_effect", "med3_effect")
result <- as.data.frame(result)

# Calculate statistics
z <- result[,1]/result[,2]
p <- 2 * (1-pnorm(abs(z)))
LowerCI <- result[,1] - 1.96*result[,2]
UpperCI <- result[,1] + 1.96*result[,2]

result <- cbind(result, z, LowerCI, UpperCI, p)
result$Odds_Ratio <- exp(result[,1])

colnames(result) <- c("LogOdds_Scale", "Std_Error", "Z_score",
                      "LogOdds_LowerCI", "LogOdds_UpperCI", "P_Value", "Odds_Ratio")
result$Odds_LowerCI <- exp(result$LogOdds_LowerCI)
result$Odds_UpperCI <- exp(result$LogOdds_UpperCI)

boot_noimp_results <- result %>%
  select(Odds_Ratio, Odds_LowerCI, Odds_UpperCI,
         LogOdds_Scale, Std_Error, Z_score, P_Value,
         LogOdds_LowerCI, LogOdds_UpperCI)

boot_noimp_results <- cbind(Effect = rownames(boot_noimp_results), boot_noimp_results)
rownames(boot_noimp_results) <- NULL

print(boot_noimp_results)


#------------------------------------------------------------------------------
# 5. EXPORT RESULTS
#------------------------------------------------------------------------------

# Export to Excel with formatted sheets
write.xlsx(list("1. Imputed Bootstrap ew" = boot_Imp_results_2017,
                "2. Imputed Non-Bootstrap ew" = noBoot_Imp_results_2017,
                "3. Imputed Bootstrap " = boot_Imp_results,
                "4. Imputed Non-Bootstrap" = noBoot_Imp_results,
                "5. Non-Imputed Bootstrap" = boot_noimp_results,
                "6. Non-Imputed Non-Bootstrap" = noBoot_noimp_results),
           file = "Output/Mediation_analysis_HFIAS_Empowement_DD_all.xlsx")