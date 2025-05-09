/*==============================================================================
TITLE: Food Security and Dietary Diversity Data Preparation
AUTHOR: Cesar Cornejo
DATE CREATED:  2025-03-12
LAST MODIFIED: 2025-04-22

DESCRIPTION:
    This dofile creates the dataset "7_Food_sec_and_dd_labeled.dta" which will be 
    used for imputation in R Studio using the MICE package. The script:

	1) Performs detrending of dietary diversity scores to adjust for Ramadan effects
    2) Merges multiple data sources into a comprehensive dataset 
    3) Labels variables for analysis
    4) Standardizes variable names for consistency
    5) Removes observations with missing endline depression scores
	
    Input files:
    - 0_All DD - women (3 Jan 2025).dta - Contains DD information for all women
    - 1_FSN_MH_data_test_2_220314.csv - Original primary data
    - 2_Background_data_by_wcode.dta - Background characteristics
    - 3_empowerment (20 Feb 2025).dta - Women's empowerment variables
    - 4_Food_Sec_and_DD_before_imputing.dta - Categorical HFIAS variables
    - 5_DD10r_score_detrending.dta - DD scores with Ramadan detrending adjustments
    - 6_species richness reduced lists (20 Sep 2024) - count wide.dta - Contains prod_e_y14
	
	Output file:
	- 7_Food_sec_and_dd_labeled.dta
	
NOTES:
    - HFIAS variables should be in categorical form (1-4) instead of raw scores (0-27).
==============================================================================*/


/*------------------------------------------------------------------------------
SECTION 0: DETRENDING RAMADAN
------------------------------------------------------------------------------*/

* Load women's data, excluding interviews during abnormal periods
use "${DATA}/0_All DD - women ( 3 Jan 2025).dta", replace
drop if rd==25 | rd>25.5 
drop *dd6* *dd13* *dd9* *dd10_*
drop if dd_elig==0

* Run mixed-effects regression to estimate Ramadan effect
* Using month 3 (MARCH) as the reference category
fvset base 3 month
mixed dd10r_score_m i.month ramadan treatment || wcode:

* Create adjusted dietary diversity score that removes the Ramadan effect
gen dd10r_score_m_noramadan = dd10r_score_m 
replace dd10r_score_m_noramadan = dd10r_score_m_noramadan - _b[ramadan] if ramadan == 1 & dd10r_score_m > 0

*Keep relevant variables and reshape from long to wide format
keep wcode dd_rotation dd10r_score_m dd10r_score_m_noramadan dd10r_min_m ramadan preg month year 
order wcode dd10r_score_m dd10r_score_m_noramadan ramadan preg month year
reshape wide dd10r_score_m dd10r_score_m_noramadan dd10r_min_m ramadan preg month year, i(wcode) j(dd_rotation)   
    
* Rename variables for clarity and add labels       
foreach q of numlist 0/9 {
	rename preg`q' preg_`q'
	rename year`q' year_`q' 
	rename month`q' month_`q'
	rename ramadan`q' ramadan_`q'
	rename dd10r_min_m`q' dd10r_min_m_`q'
	rename dd10r_score_m`q' dd10r_score_m_r`q'
	rename dd10r_score_m_noramadan`q' dd10r_score_m_noramadan_`q'
	
	la var preg_`q' "woman pregnant in R`q'"
	la var dd10r_score_m_r`q' "G: # food grps 15+ g (prev day, WDDS, 10 grp, dk=0, R`q')"
	la var dd10r_min_m_`q' "G: Inadequate diet - <5 food grps 15+ g (prev day, WDDS, 10 grp, dk=0, R`q')"
	la var dd10r_score_m_noramadan_`q' "G: Diet score adj. excl. Ramadan (prev day, WDDS, 10 grp, dk=0, R`q')"
	
}

* Special handling for baseline and endline variables
local bl_el 0 9
foreach i of local bl_el {
	rename preg_`i' preg_`=cond(`i'==0,"bl","el")'
	rename dd10r_min_m_`i' dd10r_min_m_`=cond(`i'==0,"bl","el")'
	rename dd10r_score_m_r`i' dd10r_score_m_`=cond(`i'==0,"bl","el")'
	rename dd10r_score_m_noramadan_`i' dd10r_score_m_noramadan_`=cond(`i'==0,"bl","el")'
}

save "${DATA}/5_DD10r_score_detrending.dta", replace


/*------------------------------------------------------------------------------
SECTION 1: DATA IMPORT AND MERGING
------------------------------------------------------------------------------*/

* Import the original dataset
import delimited "${DATA}/1_FSN_MH_data_test_2_220314.csv", clear

* Drop variables that will be replaced with better versions from other datasets
drop treatment hfias_bl hfias_r1314 hfias_r2223 hfias_el dd10r_score_m_bl       ///
     dd10r_min_m_bl dd10r_score_m_el dd10r_min_m_el preg_el *_hfias_cat *_rev   ///
	 hfias_cat_* dd10r_score_m_y* dd10r_score_r* dd10r_score_m_r* dd10r_min_m_*     
	   	  
* Merge with background characteristics dataset
* This dataset contains manually corrected values for demographic variables
* using multiple sources (endline, surveillance, and other corrections)
merge 1:1 wcode using "${DATA}/2_Background_data_by_wcode.dta",                 ///
      keepusing(treatment bl_age religion bl_fam_type bl_quint                  ///
	  bl_wom_edu_cat bl_mobility bl_support bl_communication bl_decision        ///
	  bl_ss_score bl_com_score bl_dec_score) nogen                            
	  
* Merge with women's empowerment variables 	  
merge 1:1 wcode using "${DATA}/3_Empowerment (20 Feb 2025).dta",                ///
      keepusing(bl_lefthome el_lefthome bl_ss_score_cont el_ss_score_cont       ///
      bl_huscomm_score_cont el_huscomm_score_cont bl_extcomm_score_cont         ///
	  el_extcomm_score_cont bl_dec_score_cont el_dec_score_cont bl_noincome     ///
	  el_noincome el_selfefficacy el_gen238_ew bl_gen238_ew                     ///
	  el_network_score_cont) nogen	  
	  
* Merge with food security and dietary diversity dataset
* Contains categorical HFIAS variables
merge 1:1 wcode using  "${DATA}/4_Food_Sec_and_DD_before_imputing.dta",         ///
keepusing(c_code hfias_bl hfias_r1314 hfias_r2223 hfias_el dd_cycle) nogen

* Merge with dietary diversity score detrending for Ramadan
merge 1:1 wcode using  "${DATA}/5_DD10r_score_detrending.dta", nogen

* Merge with species richness data
* Contains prod_e_y14 (number of crops harvested variable)
merge 1:1 wcode using "${DATA}/6_species richness reduced lists (20 Sep 2024) - count wide.dta", keepusing(prod_e_y14) nogen

/*------------------------------------------------------------------------------
SECTION 2: CHECKING MISSING
------------------------------------------------------------------------------*/

* Create tables to compare missing values in key variables
* Verify if the background characteristics are indeed complete
display as text _newline "Checking for missing values in background characteristics..."
local vars treat treatment age_3_bl bl_age g_2h_bl religion fam_type_bl               ///
      bl_fam_type quint_bl bl_quint woman_edu_cat__bl bl_wom_edu_cat mobility_bl      ///
      bl_mobility bl_lefthome support_bl bl_support communication_bl bl_communication ///
      decision_bl bl_decision com_score_bl bl_com_score bl_huscomm_score_cont         /// 
      bl_extcomm_score_cont dec_score_bl bl_dec_score bl_dec_score_cont ss_score_bl   /// 
      bl_ss_score bl_ss_score_cont 


foreach var in `vars' {
	display as text _newline "Variable: `var'"
    tab `var', m
}

* We keep the variables from the background characteristics dataset
display as text _newline "Removing redundant variables..."
drop treatment age_3_bl g_2h_bl fam_type_bl quint_bl woman_edu_cat__bl          ///
     mobility_bl support_bl communication_bl decision_bl com_score_bl           ///
	 dec_score_bl bl_ss_score 
	 
* Fix a specific value for one woman
replace dd10r_score_bl = dd10r_score_m_bl if wcode==3504
	 
/*------------------------------------------------------------------------------
SECTION 3: STANDARDIZING VARIABLE NAMES
------------------------------------------------------------------------------*/

* Standardize variable names for consistency
* Using the background characteristics versions (bl_*) as the primary source

local source_vars bl_age bl_quint bl_wom_edu_cat religion bl_support            ///
      bl_mobility bl_communication bl_com_score bl_decision bl_dec_score        /// 
	  bl_fam_type bl_lefthome el_lefthome bl_ss_score_cont el_ss_score_cont     ///
	  bl_huscomm_score_cont el_huscomm_score_cont bl_extcomm_score_cont         ///
	  el_extcomm_score_cont bl_dec_score_cont el_dec_score_cont bl_noincome el_noincome
	  
local target_vars age_bl quint_bl woman_edu_cat_bl religion_bl support_bl       ///
      mobility_bl communication_bl com_score_bl decision_bl dec_score_bl        /// 
	  fam_type_bl lefthome_bl lefthome_el ss_score_cont_bl ss_score_cont_el     ///
	  huscomm_score_cont_bl huscomm_score_cont_el extcomm_score_cont_bl         /// 
	  extcomm_score_cont_el dec_score_cont_bl dec_score_cont_el noincome_bl noincome_el

local n : word count `source_vars'

forval i = 1/`n' {
    local source : word `i' of `source_vars'
    local target : word `i' of `target_vars'
    rename `source' `target'
}

* Remove subjects with missing endline depression scores
display as text _newline "Removing subjects with missing endline depression scores..."
drop if md_score_el == .

* Save the final prepared dataset
display as text _newline "Saving final dataset..."
save "${DATA}/7_Food_sec_and_dd_labeled.dta", replace

display as text _newline"Data preparation completed successfully!"
