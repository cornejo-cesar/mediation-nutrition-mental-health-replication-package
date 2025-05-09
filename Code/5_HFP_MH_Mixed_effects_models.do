/*==============================================================================
TITLE: Multilevel mixed-effects logistic regression models
AUTHOR: Cesar Cornejo
DATE CREATED:  2025-04-14
LAST MODIFIED: 2025-04-14

DESCRIPTION:

    This script analyzes the effect of the FAARM trial intervention on depression 
    outcomes using multilevel mixed-effects logistic regression models. The analysis 
    includes:

    1. Treatment distribution analysis for depression outcomes
    2. Three progressive regression models:
       - Model 1: Unadjusted multilevel analysis
       - Model 2: Fully adjusted analysis with non-imputed data
       - Model 3: Fully adjusted analysis with imputed data  

NOTES:
    - Analysis uses cluster-level random effects (c_code)
    - Depression measured using EPDS ≥12 cut-off
    - Results presented as odds ratios
	
==============================================================================*/

use "${DATA}/8_Master_data.dta", clear

* Check treatment distribution before removing missing outcomes
display as text _newline "Treatment distribution before removing missing outcomes..."
tab treat if md_d12_el != .
tab md_d12_el treat, col

* Simple multilevel mixed-effects logistic regression (unadjusted)
* Examining the overall treatment effect without covariates
display as text _newline "Running Model 1: unadjusted multilevel mixed-effects logistic regression..."
meqrlogit md_d12_el treat || c_code:, or

* Compare depression prevalence by treatment group
tab md_d12_el treat, col

*Fully adjusted multilevel mixed-effects logistic regression
* Controlling for demographic, socioeconomic, nutrition, agency, and baseline depression
display as text _newline "Running Model 2: fully adjusted multilevel mixed-effects logistic regression..."
meqrlogit md_d12_el treat age_bl religion_bl fam_type_bl dep_ratio i.quint_bl   ///
          i.woman_edu_cat_bl prod_e_y14 i.hfias_bl_rev dd10r_score_m_noramadan_bl ///
          noincome_bl lefthome_bl ss_score_cont_bl huscomm_score_cont_bl        /// 
		  extcomm_score_cont_bl dec_score_cont_bl md_score_bl                   ///
          || c_code:, or		  
		  		  		  
display as text _newline "Running Model 3: fully adjusted multilevel mixed-effects logistic regression - imputed..."
meqrlogit md_d12_el_i treat age_bl_i religion_bl_i fam_type_bl_i dep_ratio_i i.quint_bl_i  ///
          i.woman_edu_cat_bl_i  prod_e_y14_i i.hfias_bl_i_rev dd10r_score_m_noramadan_bl_i    ///
          noincome_bl_i lefthome_bl_i ss_score_cont_bl_i huscomm_score_cont_bl_i           ///
          extcomm_score_cont_bl_i dec_score_cont_bl_i md_score_bl_i || c_code:, or
		  
		  