/*==============================================================================
TITLE: Descriptive Statistics
AUTHOR: Cesar Cornejo
DATE CREATED:  2025-03-25
LAST MODIFIED: 2025-04-25

DESCRIPTION:
    This script generates Suplementary table 2 

INPUT DATA:
    - 8_Master_data.dta
    
==============================================================================*/

use "${DATA}/8_Master_data.dta", clear

/*------------------------------------------------------------------------------

TABLE X: Descriptive Statistics of Mediating Variables (Non-Imputed vs. Imputed) 
  of the FAARM trial by arm, for women with EPDS measured at endline (n=2513) 

------------------------------------------------------------------------------*/
  
*---------------------------------------------------------
* 1. Prepare data and variables : Average HFIAS category
*---------------------------------------------------------
preserve

label variable avg_hfias_cat_sandEL_rev   "Average HFIAS category, surv + endline"
label variable avg_hfias_cat_sandEL_i_rev "Average HFIAS category, surv + endline - Imputed "
label variable el_selfefficacy "Self-efficacy score (0/1)"
label variable el_selfefficacy_i "Self-efficacy score (0/1) - Imputed"
label variable lefthome_el "Mobility outside homestead in last month (0/1)"
label variable lefthome_el_i "Mobility outside homestead in last month (0/1) - Imputed"
label variable noincome_el "Woman earned any income in last month (0/1)"
label variable noincome_el_i "Woman earned any income in last month (0/1) - Imputed"
label variable el_gen238_ew "Woman can decide on her own income (0/1)"
label variable el_gen238_ew_i "Woman can decide on her own income (0/1) - Imputed"

local cat_vars avg_hfias_cat_sandEL_rev avg_hfias_cat_sandEL_i_rev el_selfefficacy el_selfefficacy_i lefthome_el lefthome_el_i noincome_el noincome_el_i el_gen238_ew el_gen238_ew_i 

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

restore


*---------------------------------------------------------
  * 1. Prepare data and variables : Average Dietary Diversity score
*---------------------------------------------------------
preserve

label variable DD10r_score_avg_2017r    "Average Dietary Diversity score, surv + endline - detrended by ramadan"
label variable DD10r_score_avg_2017r_i  "Average Dietary Diversity score, surv + endline - detrended by ramadan - Imputed" 
label variable dec_score_cont_el "Decision-making capacity score (0-2)"
label variable dec_score_cont_el_i "Decision-making capacity score (0-2) - Imputed"
label variable extcomm_score_cont_el "External communication score (0-2)"
label variable extcomm_score_cont_el_i "External communication score (0-2) - Imputed"
label variable huscomm_score_cont_el "Communication with husbands score (0-2)"
label variable huscomm_score_cont_el_i "Communication with husbands score (0-2) - Imputed"
label variable ss_score_cont_el "Social support score (0-2)"
label variable ss_score_cont_el_i "Social support score (0-2) - Imputed"
label variable el_network_score_cont "Social network score (0-2)"
label variable el_network_score_cont_i "Social network score (0-2) - Imputed"

local vars DD10r_score_avg_2017r DD10r_score_avg_2017r_i  dec_score_cont_el dec_score_cont_el_i extcomm_score_cont_el extcomm_score_cont_el_i huscomm_score_cont_el huscomm_score_cont_el_i ss_score_cont_el ss_score_cont_el_i el_network_score_cont el_network_score_cont_i 
  
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


restore

/*

bysort treat: sum DD10r_score_avg_2017r dec_score_cont_el extcomm_score_cont_el huscomm_score_cont_el ss_score_cont_el el_network_score_cont 

******************************************************

bysort treat: sum DD10r_score_avg_2017r_i dec_score_cont_el_i extcomm_score_cont_el_i huscomm_score_cont_el_i ss_score_cont_el_i el_network_score_cont_i 

*/









