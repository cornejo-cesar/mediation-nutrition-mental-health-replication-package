/*==============================================================================
TITLE: Descriptive Statistics
AUTHOR: Cesar Cornejo
DATE CREATED:  2025-03-12
LAST MODIFIED: 2025-04-14

DESCRIPTION:
    This script generates descriptive statistics tables for the Food and Agricultural 
    Approaches to Reducing Malnutrition (FAARM) trial, including:
    
    1) Baseline characteristics by treatment arm (n=2513):
       - Continuous variables (means)
       - Categorical variables (percentages)
       - Missing values report
       
    2) Depression prevalence at endline using different EPDS cut-off points
    
    3) Mediating variables comparison between non-imputed and imputed data:
       - Food insecurity (HFIAS categories)
       - Dietary diversity scores

INPUT DATA:
    - 8_Master_data.dta
    
OUTPUT FILES:
    - Baseline_characteristics_scores.xlsx
    - Baseline_characteristics_categories.xlsx
    - Missing_values.xlsx
    - Depression_prevalence.xlsx
	
==============================================================================*/


use "${DATA}/8_Master_data.dta", clear

/*------------------------------------------------------------------------------
TABLE 1: Baseline characteristics of the FAARM trial by arm, 
for women measured at endline  (n=2513) - SCORES (mean)
------------------------------------------------------------------------------*/

tab md_d12_bl treat, col 

*---------------------------------------------------------
  * 1. Prepare data and variables
*---------------------------------------------------------
preserve
label variable dd10r_score_bl "Women's Dietary Diversity Score (WDDS, 1-10 food groups)"
label variable age_bl "Age in years"
label variable ss_score_cont_bl "Social support score (0=none to 2=most)"
label variable dec_score_cont_bl "Decision-making capacity category (0=none to 2=most)"
label variable huscomm_score_cont_bl "Husband communication score (0=none to 2=most)"
label variable extcomm_score_cont_bl "External communication score (0=none to 2=most)"
label variable dep_ratio "Dependency ratio (0=ratio of children/elderly to adults to 4=children/elderly to adults)"
label variable prod_e_y14 "Number of garden crop species harvested (0-32)"

local vars md_d12_bl dd10r_score_bl age_bl ss_score_cont_bl dec_score_cont_bl /// 
      huscomm_score_cont_bl extcomm_score_cont_bl dep_ratio prod_e_y14   
   
  
*---------------------------------------------------------
* 2. Create a temporary dataset with group means
*---------------------------------------------------------
tempfile results
capture postclose handle
postfile handle str200 varlabel float mean0 float mean1 using `results', replace

foreach var of local vars {
    quietly summarize `var' if treat == 0
local m0 = r(mean)

quietly summarize `var' if treat== 1
    local m1 = r(mean)
    
    local mylabel: variable label `var'
if `"`mylabel'"' == "" {
  
  local mylabel "`var'"
}

post handle ("`mylabel'") (`m0') (`m1')
}

postclose handle

use `results', clear

label variable varlabel "Variable"
label variable mean0  "Control (mean)"
label variable mean1  "Intervention (mean)"

*---------------------------------------------------------
  * 3. Display the table and export table
*---------------------------------------------------------
tabdisp varlabel, c(mean0 mean1) format(%9.2f)
format mean0 mean1 %9.2f

export excel using "${OUTPUT}/Baseline_characteristics_scores.xlsx", ///
  sheet("Mean") ///
  firstrow(varlabels) ///
  replace

restore


/*------------------------------------------------------------------------------
  TABLE 1: Baseline characteristics of the FAARM trial by arm, 
for women measured at endline  (n=2513) - CATERORY (%)
------------------------------------------------------------------------------*/
  
*---------------------------------------------------------
* 1. Prepare data and variables
*---------------------------------------------------------
preserve
label variable hfias_bl_rev "Household food insecurity category"
label variable religion_bl "Religion"
label variable fam_type_bl "Family type"
label variable woman_edu_cat_bl "Educational category (0=none to 5=HSC+)"
label variable quint_bl "Wealth (household asset score; 0=poorest to 5=richest quintile)"
label variable noincome_bl "No income"
label variable lefthome_bl "Lefthome category (0=unable to 2=able)"
local cat_vars hfias_bl_rev religion_bl fam_type_bl woman_edu_cat_bl quint_bl noincome_bl lefthome_bl

*---------------------------------------------------------
  * 2. Create a temporary dataset with group means
*---------------------------------------------------------
  
tempfile results_cat
capture postclose handle_cat
postfile handle_cat str32 varlabel str20 category float pct0 float pct1 using `results_cat', replace

foreach var of local cat_vars {
    quietly count if treat== 0 & !missing(`var')
local total0 = r(N)
quietly count if treat == 1 & !missing(`var')
    local total1 = r(N)
    
    levelsof `var', local(levels) clean
                                        
    foreach lvl of local levels {
                                          
    local lvl_str: label (`var') `lvl'
        
        quietly count if `var' == `lvl' & treat == 0
        local n0 = r(N)
        quietly count if `var' == `lvl' & treat == 1
        local n1 = r(N)
        
        if (`total0' > 0) {
		    local pct0 = 100 * `n0' / `total0'
        }
        else {
            local pct0 = 0
        }
        
        if (`total1' > 0) {
  local pct1 = 100 * `n1' / `total1'
        }
        else {
            local pct1 = 0
        }
        
        post handle_cat ("`var'") ("`lvl_str'") (`pct0') (`pct1')
}
}
                                          
postclose handle_cat
                                          
use `results_cat', clear
sort varlabel category

label variable varlabel   "Variable"              // superrow
label variable category  "Category"              // subfilas
label variable pct0      "Control (%)"
label variable pct1      "Intervención (%)"

*---------------------------------------------------------
* 3. Display the table and export table
*---------------------------------------------------------

gen id = varlabel + ": " + category

tabdisp id, c(pct0 pct1) format(%9.2f) 

format pct0 pct1 %9.2f
export excel using "${OUTPUT}/Baseline_characteristics_categories.xlsx", ///
    sheet("percentage") ///
    firstrow(varlabels) ///
    replace
	
restore


/*------------------------------------------------------------------------------
FOOTNOTE TABLE 1: Report of missing values 
------------------------------------------------------------------------------*/

*---------------------------------------------------------
* 1. Prepare data and variables
*---------------------------------------------------------

preserve 

local vars hfias_bl_rev dd10r_score_m_bl dd10r_score_m_noramadan_bl age_bl           ///
           woman_edu_cat_bl ss_score_cont_bl dec_score_cont_bl huscomm_score_cont_bl ///
           extcomm_score_cont_bl lefthome_bl quint_bl religion_bl dep_ratio          ///
		   fam_type_bl prod_y14 noincome_bl
			
*---------------------------------------------------------
* 2. Create a temporary dataset with group missing
*--------------------------------------------------------

tempfile missings
postfile handle str32 varname float missing0 float missing1 using `missings', replace
                                          
    foreach var of local vars {
    quietly count if treat == 0 & missing(`var')
    local m0 = r(N)
    quietly count if treat == 1 & missing(`var')
    local m1 = r(N)
    post handle ("`var'") (`m0') (`m1')
}

postclose handle

use `missings', clear
                                          
label variable varname  "Variable"
label variable missing0 "Control"
label variable missing1 "Intervention"
                                          
*---------------------------------------------------------
* 3. Display the table and export table
*---------------------------------------------------------
                                            
tabdisp varname, c(missing0 missing1) format(%9.0f)
                                          
export excel using "${OUTPUT}/Missing_values.xlsx", ///
sheet("percentage") ///
firstrow(varlabels) ///
replace
                                          
restore
                                          
                                          
                                          /*------------------------------------------------------------------------------
Supplementary Table 3: Prevalence for likely major depression based on three cut-off points on the Edinburgh Postpartum Depression Score among FAARM trial participants at endline, with rationale for each
                                          ------------------------------------------------------------------------------*/
                                            
*---------------------------------------------------------
* 1. Prepare data and variables
*---------------------------------------------------------
preserve
                                          
label variable md_d11_el_rev  "≥11"
label variable md_d12_el_rev  "≥12"
label variable md_d13_el_rev  "≥13"
                                          
*---------------------------------------------------------
* 2. Create a temporary dataset with prevalence rates
*---------------------------------------------------------
tempfile results
capture postclose handle
postfile handle str200 varlabel float prevalence using `results', replace

local vars "md_d11_el md_d12_el md_d13_el"

foreach var of local vars {
    quietly count if !missing(`var')
                                            local total = r(N)
                                            
                                            levelsof `var', local(levels) clean
    
    foreach lvl of local levels {
        
        local lvl_str: label (`var') `lvl'
        
        quietly count if `var' != `lvl'
        local n = r(N)
        
        if (`total' > 0) {
          local pct = 100 * `n' / `total'
        }
        else {
            local pct = 0
        }
        
        post handle ("`var'") (`pct')
    }
}

postclose handle

use `results', clear

label variable varlabel "Cut-off point"
label variable prevalence  "Prevalence (%)"

*---------------------------------------------------------
* 3. Display the table and export table
*---------------------------------------------------------
tabdisp varlabel, c(prevalence) format(%9.0f)

export excel using "${OUTPUT}/Depression_prevalence.xlsx", ///
    sheet("Prevalence") ///
    firstrow(varlabels) ///
    replace

restore

display as text _newline "Descriptive statistics completed!"
