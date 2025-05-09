/*==============================================================================
TITLE: Food Security and Mental Health Mediation Analysis Variable Preparation
Data Processing
AUTHOR:
REORDER BY: Cesar Cornejo 
DATE CREATED:  2025-03-07
LAST MODIFIED: 2025-04-14

DESCRIPTION:
    This dofile prepares variables for mediation analysis exploring the relationship  
    between food security, dietary diversity, and mental health outcomes. It: 
	
    1) Processes both imputed and non-imputed data in parallel workflows
	2) Creates consistent variables across both datasets for comparative analysis
    3) Generates key derived variables needed for mediation analysis
	
    Input files:
    - FSN_MH_ImpData_4_2024.csv: Dataset with imputed values from MICE in R
    - 7_Food_sec_and_dd_labeled.dta: Original dataset with labeled variables
    
    Output file:
    - 8_Master_data.dta: Comprehensive dataset containing both imputed and 
      non-imputed variables for all models
	  
NOTES:
    - Variables with "_i" suffix indicate imputed variables
    - HFIAS was reversed (higher = better food security) to match dietary diversity direction
    - Food security categories: 1=severe food insecure, 4=food secure (after reversal)
    - Binary depression variables created with cutoffs at 11, 12, and 13	  
==============================================================================*/

/*==============================================================================
* IMPUTATION DATA
==============================================================================*/

/*------------------------------------------------------------------------------
SECTION 0: DETRENDING RAMADAN
------------------------------------------------------------------------------*/

* Import imputed data directly from csv 
import delimited "${DATA}/Imputation/FSN_MH_ImpData_4_2024.csv", clear

keep wcode treat dd10r_score_m_r* dd10r_score_m_bl dd10r_score_m_el month_* ramadan_*
rename (dd10r_score_m_bl dd10r_score_m_el) (dd10r_score_m_r0 dd10r_score_m_r9)

foreach var of varlist ramadan_0 ramadan_1 ramadan_3 ramadan_5 ramadan_7 {
    replace `var' = "0" if `var' == "NA"
	destring `var', replace
}

reshape long month_ ramadan_ dd10r_score_m_r, i(wcode) j(time)

* Run mixed-effects regression to estimate Ramadan effect
* Using month 3 (MARCH) as the reference category
fvset base 3 month_
mixed dd10r_score_m i.month_ ramadan treat || wcode:

* Create adjusted dietary diversity score that removes the Ramadan effect
gen dd10r_score_m_noramadan = dd10r_score_m_r 
replace dd10r_score_m_noramadan = dd10r_score_m_noramadan - _b[ramadan] if ramadan == 1 & dd10r_score_m_r > 0

keep wcode dd10r_score_m_r dd10r_score_m_noramadan month_ ramadan_  time
reshape wide dd10r_score_m_r dd10r_score_m_noramadan month_ ramadan_, i(wcode) j(time)

* Rename variables for clarity and add labels  
foreach q of numlist 0/9 {
	rename dd10r_score_m_noramadan`q' dd10r_score_m_noramadan_`q'
	la var dd10r_score_m_r`q' "G: # food grps 15+ g (prev day, WDDS, 10 grp, dk=0, R`q') - imputed"
	la var dd10r_score_m_noramadan_`q' "G: Diet score adj. excl. Ramadan (prev day, WDDS, 10 grp, dk=0, R`q')- imputed"
	
}

* Special handling for baseline and endline variables
local bl_el 0 9
foreach i of local bl_el {
	rename dd10r_score_m_r`i' dd10r_score_m_`=cond(`i'==0,"bl","el")'
	rename dd10r_score_m_noramadan_`i' dd10r_score_m_noramadan_`=cond(`i'==0,"bl","el")'
}

save "${DATA}/temp.dta", replace 

/*------------------------------------------------------------------------------
SECTION 1: IMPORT IMPUTED DATA
------------------------------------------------------------------------------*/

* Import imputed data directly from csv 
import delimited "${DATA}/Imputation/FSN_MH_ImpData_4_2024.csv", clear
drop ramadan*
merge 1:1 wcode using "${DATA}/temp.dta"

local vars fam_type_bl dd10r_min_m_bl hfias_bl hfias_r1314 hfias_r2223 hfias_el
foreach var of local vars {
    encode `var', generate(`var'_num)
    drop `var'
    rename `var'_num `var'
}

* Add "_i" tag to all imputed variables to clearly identify them
foreach var of varlist religion_bl-hfias_el {
rename `var' `var'_i  
}


/*------------------------------------------------------------------------------
SECTION 2: VARIABLE LABELING
------------------------------------------------------------------------------*/

*  Mental health variables
label var md_score_el_i "Major depressive symptoms score - EL, imputed"
label value md_score_el_i md_score_el_i_label

label var md_score_bl_i "Major depressive symptoms score - BL, imputed"
label value md_score_bl_i md_score_bl_i_label

* Demographic variables
label var religion_bl_i "Religion - BL, imputed"
label define religion_bl_i_label 1 "Muslim" 2 "Hindu"
label value religion_bl_i religion_bl_i_label

label var fam_type_bl_i "Family type - BL, imputed"
label define fam_type_bl_i_label 0 "Joint" 1 "Nuclear"
label value fam_type_bl_i fam_type_bl_i_label

label var dep_ratio_i "Ratio of kids/elderly to prime working age adults - BL, imputed"
label value dep_ratio_i dep_ratio_i_label

label var age_bl_i "Age of index woman - BL, imputed"
label value age_bl_i age_bl_i_label

label var woman_edu_cat_bl_i "Woman's edu category - BL, imputed"
label value woman_edu_cat_bl_i woman_edu_cat_bl_i_label

label var lit_cat_i "Woman's literacy"
label define lit_cat_i_label 2 "Able to read" 1 "Able to read partial" 0 "Unable to read", replace
label value lit_cat_i lit_cat_i_label

* Agricultural variables
label var impgarden_bl_i "Number of improved practices observed in garden - BL, imputed"
label value impgarden_bl_i impgarden_bl_i_label

label var num_crops_bl_i "Number of crop species in garden - BL, imputed"
label value num_crops_bl_i num_crops_bl_i_label

* Dietary diversity and Food security variables  
label var dd10r_score_m_bl_i "Dietary diversity of W - BL, imputed"
label value dd10r_score_m_bl_i dd10r_score_m_bl_i_label

label var hfias_score_bl_i "HFIAS score - BL, imputed"
label value hfias_score_bl_i hfias_score_bl_i_label

label var hfias_score_bl_i "HFIAS score - BL, imputed"
label value hfias_score_bl_i hfias_score_bl_i_label

label var hfias_bl_i "HFIAS - BL, imputed"
label value hfias_bl_i hfias_bl_i

* Define HFIAS category labels
label define hfias_label 1"food secure"  2"mild food insecure" 3"moderate food insecure" 4"severe food insecure"   
label values hfias_r1314_i hfias_label
label values hfias_r2223_i hfias_label
label values hfias_el_i hfias_label

label define hfias_label_rev 4"food secure"  3"mild food insecure" 2"moderate food insecure" 1"severe"

label define woman_edu_cat 5"HSC+"  4"Complete secondary"  3"Partial secondary" 2"Complete primary" 1"Partial primary"  0"None"
label values woman_edu_cat_bl_i woman_edu_cat

label define quintilcat 5"Highest" 4"High" 3"Middle" 2"Low" 1"Lowest" 

label value quint_bl_i quintilcat

/*------------------------------------------------------------------------------
SECTION 3: BINARY DEPRESSION VARIABLES
------------------------------------------------------------------------------*/

* Create binary depression variable for endline (cutoff ≥12)
tab md_score_el_i, m
gen md_d12_el_i = .
replace md_d12_el_i = 0 if md_score_el_i <12
replace md_d12_el_i = 1 if md_score_el_i >=12
replace md_d12_el_i = . if md_score_el_i == .
label var md_d12_el_i "Major depressive symptoms binary 11/12  - EL, imputed"
label define md_d12_el_i_label 0 "No" 1 "Yes"
label value md_d12_el_i md_d12_el_i_label


* Create binary depression variable for baseline (cutoff ≥12) 
tab md_score_bl_i, m
gen md_d12_bl_i = .
replace md_d12_bl_i = 0 if md_score_bl_i <12
replace md_d12_bl_i = 1 if md_score_bl_i >=12
replace md_d12_bl_i = . if md_score_bl_i == .
label var md_d12_bl_i "Major depressive symptoms binary 11/12  - BL"
label define md_d12_bl_i_label 0 "No" 1 "Yes"
label value md_d12_bl_i md_d12_bl_i_label


/*------------------------------------------------------------------------------
SECTION 4: HFIAS SCORE VARIABLES (ORIGINAL SCALE)
------------------------------------------------------------------------------*/

*HFIAS average of surveillance measures using IMPUTED data (0 is food secure, 25 is severe food insecurity)
tab hfias_score_r1314_i 
tab hfias_score_r2223_i, m
gen tot_hfias_surv_i = hfias_score_r1314_i + hfias_score_r2223_i
tab tot_hfias_surv_i, m

gen avg_hfias_surv_i = tot_hfias_surv_i/2
label var avg_hfias_surv_i "Average HFIAS score, surv r1314 + r2223, imputed"
label value avg_hfias_surv_i avg_hfias_surv_i_label
tab avg_hfias_surv_i


*Avg HFIAS surveillance, endline - IMPUTED 
gen tot_hfias_sandEL_i = hfias_score_r1314_i + hfias_score_r2223_i + hfias_score_el_i
tab tot_hfias_sandEL_i, m
gen avg_hfias_sandEL_i = tot_hfias_sandEL_i/3
tab avg_hfias_sandEL_i, m
label var avg_hfias_sandEL_i "Average HFIAS score, surv + endline, imputed"
label value avg_hfias_sandEL_i avg_hfias_sandEL_i_label


/*------------------------------------------------------------------------------
SECTION 5: HFIAS CATEGORICAL VARIABLES
------------------------------------------------------------------------------*/

* Verify HFIAS categorical variables (original scale: 1-4)
* 1=food secure, 4=severe food insecure
tab hfias_r1314_i, m 
tab hfias_r2223_i, m

* Create total and average HFIAS category from surveillance rounds
egen tot_hfias_cat_surv_i = rowtotal(hfias_r1314_i hfias_r2223_i)
tab tot_hfias_cat_surv_i, m

* Compute average HFIAS category (only surveillance rounds)
egen avg_hfias_cat_surv_i = rowmean(hfias_r1314_i hfias_r2223_i)
tab avg_hfias_cat_surv_i, m
label var avg_hfias_cat_surv_i "Average HFIAS category, surv r1314+r2223, imputed"
label value avg_hfias_cat_surv_i avg_hfias_cat_surv_i_label

* Categorize the average HFIAS category (surveillance rounds)
gen avg_hfias_cat_surv_i_categorized = .
replace avg_hfias_cat_surv_i_categorized = 1 if avg_hfias_cat_surv_i < 1.5
replace avg_hfias_cat_surv_i_categorized = 2 if avg_hfias_cat_surv_i >= 1.5 & avg_hfias_cat_surv_i < 2.5
replace avg_hfias_cat_surv_i_categorized = 3 if avg_hfias_cat_surv_i >= 2.5 & avg_hfias_cat_surv_i < 3.5
replace avg_hfias_cat_surv_i_categorized = 4 if avg_hfias_cat_surv_i >= 3.5
tab avg_hfias_cat_surv_i_categorized, m

* Create total and average HFIAS category including endline
egen tot_hfias_cat_sandEL_i = rowtotal(hfias_r1314_i hfias_r2223_i hfias_el_i)
tab tot_hfias_cat_sandEL_i
egen avg_hfias_cat_sandEL_i = rowmean(hfias_r1314_i hfias_r2223_i hfias_el_i)
tab avg_hfias_cat_sandEL_i, m
label var avg_hfias_cat_sandEL_i "Average HFIAS category, surv + endline, imputed"
label value avg_hfias_cat_sandEL_i avg_hfias_cat_sandEL_i_label

*  Categorize the average HFIAS category (surveillance + endline)            
gen avg_hfias_sandEL_i_categorized = .
replace avg_hfias_sandEL_i_categorized = 1 if avg_hfias_cat_sandEL_i < 1.5
replace avg_hfias_sandEL_i_categorized = 2 if avg_hfias_cat_sandEL_i >= 1.5 & avg_hfias_cat_sandEL_i < 2.5
replace avg_hfias_sandEL_i_categorized = 3 if avg_hfias_cat_sandEL_i >= 2.5 & avg_hfias_cat_sandEL_i < 3.5
replace avg_hfias_sandEL_i_categorized = 4 if avg_hfias_cat_sandEL_i >= 3.5
tab avg_hfias_sandEL_i_categorized, m

* Compare the distributions
tab1 avg_hfias_cat_sandEL_i avg_hfias_sandEL_i_categorized


/*------------------------------------------------------------------------------
SECTION 6: REVERSE CODING HFIAS VARIABLES
------------------------------------------------------------------------------*/

revv hfias_bl_i, gen(hfias_bl_i_rev)
label var hfias_bl_i_rev "Baseline HFIAS category, reversed"
label value hfias_bl_i_rev hfias_label_rev

revv  hfias_score_bl_i, gen(hfias_score_bl_i_rev)
label var hfias_score_bl_i_rev "HFIAS score - BL, imputed, reversed"
label value hfias_score_bl_i_rev hfias_label_rev

revv avg_hfias_surv_i, gen(avg_hfias_surv_i_rev)
label var avg_hfias_surv_i_rev "Average HFIAS score - surv, imputed, reversed"
label value avg_hfias_surv_i_rev hfias_label_rev

revv avg_hfias_sandEL_i, gen(avg_hfias_sandEL_i_rev)
label var avg_hfias_sandEL_i_rev "Average HFIAS score - surv+EL, imputed, reversed"
label value avg_hfias_sandEL_i_rev hfias_label_rev

revv avg_hfias_cat_surv_i_categorized, gen(avg_hfias_cat_surv_i_rev)
label var avg_hfias_cat_surv_i_rev "Average HFIAS category - surv, imputed, reversed"
label value avg_hfias_cat_surv_i_rev hfias_label_rev
                              
revv avg_hfias_sandEL_i_categorized, gen(avg_hfias_cat_sandEL_i_rev)
label var avg_hfias_cat_sandEL_i_rev "Average HFIAS category, surv+EL, imputed, reversed"
label value avg_hfias_cat_sandEL_i_rev hfias_label_rev

tab1  avg_hfias_cat_surv_i_rev  avg_hfias_cat_sandEL_i_rev


/*------------------------------------------------------------------------------
SECTION 7: DIETARY DIVERSITY
------------------------------------------------------------------------------*/

*DD average of surveillance measures using imputed data
*Total average over all points, excluding baseline (excluding endline) 
egen DD10r_score_avg_surv_i = rowmean(dd10r_score_m_r1_i  dd10r_score_m_r2_i dd10r_score_m_r3_i  dd10r_score_m_r4_i ///
                              dd10r_score_m_r5_i dd10r_score_m_r6_i dd10r_score_m_r7_i dd10r_score_m_r8_i)
						   
*Total average over all points, excluding baseline (including endline) 
egen DD10r_score_avg_sandEL_i = rowmean(dd10r_score_m_r1_i  dd10r_score_m_r2_i dd10r_score_m_r3_i dd10r_score_m_r4_i /// 
                                dd10r_score_m_r5_i dd10r_score_m_r6_i dd10r_score_m_r7_i dd10r_score_m_r8_i          ///
								dd10r_score_m_el_i)		
								
*Total average over all points, excluding baseline (including endline) - detrending
egen DD10r_score_avg_sandELr_i = rowmean(dd10r_score_m_noramadan_1_i  dd10r_score_m_noramadan_2_i dd10r_score_m_noramadan_3_i ///
                                 dd10r_score_m_noramadan_4_i dd10r_score_m_noramadan_5_i dd10r_score_m_noramadan_6_i          ///
								 dd10r_score_m_noramadan_7_i dd10r_score_m_noramadan_8_i dd10r_score_m_noramadan_el_i)

** 2017 onwards **
egen DD10r_score_avg_2017_i = rowmean(dd10r_score_m_r5_i dd10r_score_m_r6_i dd10r_score_m_r7_i dd10r_score_m_r8_i dd10r_score_m_el_i)

egen DD10r_score_avg_2017r_i = rowmean(dd10r_score_m_noramadan_5_i dd10r_score_m_noramadan_6_i dd10r_score_m_noramadan_7_i ///
                              dd10r_score_m_noramadan_8_i dd10r_score_m_noramadan_el_i)
							  
egen DD10r_score_avg_2017e_i = rowmean(dd10r_score_m_noramadan_5_i dd10r_score_m_noramadan_6_i dd10r_score_m_noramadan_7_i ///
                              dd10r_score_m_noramadan_8_i)
							  
label var DD10r_score_avg_surv_i    "Average DD score, surv, imputed"
label var DD10r_score_avg_sandEL_i  "Average DD score, surv+EL, imputed"
label var DD10r_score_avg_sandELr_i "Average DD score, surv+EL, imputed - detrending by ramadan"
label var DD10r_score_avg_2017_i    "Average DD score, surv+EL, imputed 2017"
label var DD10r_score_avg_2017e_i   "Average DD score, surv - detrending by ramadan"


display as text _newline "Saving final cleaned and imputed dataset..."
save "${DATA}/8_FNS_MH_cleaned_imputed_data.dta", replace

/*==============================================================================
* NO IMPUTATION DATA
==============================================================================*/

* Load the prepared dataset
use "${DATA}/7_Food_sec_and_dd_labeled.dta", clear

* The previous code uses a input a dataset we no longer have.
* I temporarilly drop the following variables to be able to run the code and reproduce the analysis
drop *r_bl *_sandel num_crops_* lit_cat  impgarden_bl anemic_all_bl avg_hfias* tot_hfias* avg_hfias_bin md_d*


/*------------------------------------------------------------------------------
  SECTION 1: VARIABLE RECODING AND PREPARATION
------------------------------------------------------------------------------*/
  display as text _newline "FAARM INTERVENTION ON MH, MEDIATED BY CROP DIVERSITY, FOOD INSECURITY, DIETARY DIVERSITY"

* Recode special missing values (-7777) to system missing (.a)
mvdecode _all, mv(-7777=.a)

clonevar ag_1100r_bl = ag_1100_bl
clonevar ag_1101r_bl = ag_1101_bl
clonevar ag_1102r_bl = ag_1102_bl
clonevar ag_1103r_bl = ag_1103_bl
clonevar ag_1104r_bl = ag_1104_bl
clonevar ag_1105r_bl = ag_1105_bl
clonevar ag_1106r_bl = ag_1106_bl
clonevar ag_1107r_bl = ag_1107_bl
clonevar ag_1108r_bl = ag_1108_bl
clonevar ag_1109r_bl = ag_1109_bl
clonevar ag_11010r_bl = ag_11010_bl
clonevar ag_11011r_bl = ag_11011_bl
clonevar ag_11012r_bl = ag_11012_bl
clonevar ag_11013r_bl = ag_11013_bl
clonevar ag_11014r_bl = ag_11014_bl
clonevar ag_11015r_bl = ag_11015_bl
clonevar ag_11077r_bl = ag_11077_bl

clonevar pb_62= pb_62_bl
clonevar pb_621= pb_621_bl 

* Replace missing values with 0 for agricultural practice variables
foreach var of varlist ag_1100r_bl ag_1101r_bl ag_1102r_bl ag_1103r_bl ag_1104r_bl ag_1105r_bl ag_1106r_bl ag_1107r_bl ag_1108r_bl ag_1109r_bl ag_11010r_bl ag_11011r_bl ag_11012r_bl ag_11013r_bl ag_11014r_bl ag_11015r_bl ag_11077r_bl {
  replace `var'  = 0 if `var' == . | `var'== .a
}

gen impgarden_bl = ag_1100r_bl + ag_1101r_bl + ag_1102r_bl + ag_1103r_bl + ag_1104r_bl + ag_1105r_bl + ag_1106r_bl + ag_1107r_bl + ag_1108r_bl + ag_1109r_bl + ag_11010r_bl + ag_11011r_bl + ag_11012r_bl + ag_11013r_bl + ag_11014r_bl + ag_11015r_bl + ag_11077r_bl
tab impgarden_bl, m

/*------------------------------------------------------------------------------
  SECTION 2: CREATING HEALTH AND DEMOGRAPHIC INDICATORS
------------------------------------------------------------------------------*/
  
* Anemia at BL
*Anemic whether NP or Pregnant variable
tab anemia_np_bl, m
tab anemia_p_bl, m
gen anemic_all_bl = .
replace anemic_all_bl = 1 if anemia_np_bl == 1 | anemia_p_bl == 1
replace anemic_all_bl = 0 if anemia_np_bl == 0 | anemia_p_bl == 0
replace anemic_all_bl = . if anemia_np_bl == . & anemia_p_bl == .
label var anemic_all_bl "Anemia status, np+p at BL"
*label define anemic_all_bl_label 1 "Anemic" 0 "Not anemic"
label value anemic_all_bl anemic_all_bl_label
tab anemic_all_bl, missing

*Literacy: Not able to read, partially able to read, able to read*
  tab hhl_9a, missing
gen lit_cat = .
replace lit_cat = 2 if hhl_9a == 1 | hhl_9a == 4
replace lit_cat = 1 if hhl_9a == 2
replace lit_cat = 0 if hhl_9a == 3 | hhl_9a == .a
label var lit_cat "Woman's literacy"
label define lit_cat_label 2 "Able to read" 1 "Able to read partial" 0 "Unable to read", replace
label value lit_cat lit_cat_label
tab hhl_9a lit_cat, missing
tab lit_cat, m


*Parity: None, 1, 2, 3+ * Should we use live born or total living?
* Live births categorical variable (0, 1, 2, 3+)
tab pb_62, m
tab pb_621, m

gen livebirth = .
replace livebirth = 0 if pb_621 == 0 
replace livebirth = 1 if pb_621 ==1
replace livebirth = 2 if pb_621 ==2
replace livebirth= 3 if pb_621 >=3
label var livebirth "Total live births"
label define livebirth_label 0 "None" 1 "One" 2 "Two" 3 "Three or more", replace
label value livebirth livebirth_label
tab pb_621 livebirth, missing

* Living children: None, 1, 2, 3+ * Should we use live born or total living?
  tab pb_62, m
tab pb_621, m
gen livingch = .
replace livingch = 0 if pb_62 == 0 
replace livingch = 1 if pb_62 ==1
replace livingch = 2 if pb_62 ==2
replace livingch= 3 if pb_62 >=3
label var livingch "Total living children"
label define livingch_label 0 "None" 1 "One" 2 "Two" 3 "Three or more"
label value livingch livingch_label
tab pb_62 livingch, missing


/*------------------------------------------------------------------------------
  SECTION 3: HFIAS CATEGORICAL VARIABLES
------------------------------------------------------------------------------*/
  
* HFIAS CATEGORICAL VARIABLES - SURVEILLANCE ROUNDS
* Verify HFIAS categorical variables (original scale: 1-4)
* 1=food secure, 4=severe food insecure
tab hfias_r1314, m 
tab hfias_r2223, m

* Create total and average HFIAS category from surveillance rounds
egen tot_hfias_cat = rowtotal(hfias_r1314 hfias_r2223)
tab tot_hfias_cat, m

egen avg_hfias_cat = rowmean(hfias_r1314 hfias_r2223)
tab avg_hfias_cat, m
label var avg_hfias_cat "Average HFIAS category, surv r1314+r2223"
label value avg_hfias_cat avg_hfias_cat_label

* Create categorized version of average HFIAS
gen avg_hfias_cat_surv_categorized = .
replace avg_hfias_cat_surv_categorized = 1 if avg_hfias_cat < 1.5
replace avg_hfias_cat_surv_categorized = 2 if avg_hfias_cat >= 1.5 & avg_hfias_cat < 2.5
replace avg_hfias_cat_surv_categorized = 3 if avg_hfias_cat >= 2.5 & avg_hfias_cat < 3.5
replace avg_hfias_cat_surv_categorized = 4 if avg_hfias_cat >= 3.5
label var avg_hfias_cat_surv_categorized "HFIAS category, surveillance rounds (1=secure, 4=severe)"

* HFIAS CATEGORICAL VARIABLES - WITH ENDLINE
display as text _newline "Processing HFIAS categorical variables including endline data..."
tab hfias_el, m

egen tot_hfias_cat_sandEL = rowtotal(hfias_r1314 hfias_r2223 hfias_el)
tab tot_hfias_cat_sandEL, m

egen avg_hfias_cat_sandEL = rowmean(hfias_r1314 hfias_r2223 hfias_el)
tab avg_hfias_cat_sandEL, m
label var avg_hfias_cat_sandEL "Average HFIAS category, surv + endline"
label value avg_hfias_cat_sandEL avg_hfias_cat_sandEL_label

* Create categorized version of average HFIAS including endline
gen avg_hfias_sandEL_categorized = .
replace avg_hfias_sandEL_categorized = 1 if avg_hfias_cat_sandEL < 1.5
replace avg_hfias_sandEL_categorized = 2 if avg_hfias_cat_sandEL >= 1.5 & avg_hfias_cat_sandEL < 2.5
replace avg_hfias_sandEL_categorized = 3 if avg_hfias_cat_sandEL >= 2.5 & avg_hfias_cat_sandEL < 3.5
replace avg_hfias_sandEL_categorized = 4 if avg_hfias_cat_sandEL >= 3.5
label var avg_hfias_sandEL_categorized "HFIAS category, surv+endline (1=secure, 4=severe)"
tab avg_hfias_sandEL_categorized, m


/*------------------------------------------------------------------------------
  SECTION 4: REVERSING HFIAS VARIABLES (HIGHER = BETTER FOOD SECURITY)
------------------------------------------------------------------------------*/
  
  display as text _newline "Reversing HFIAS variables (higher values = better food security)..."

* Reverse baseline HFIAS category
revv hfias_bl, gen(hfias_bl_rev)
label var hfias_bl_rev "Baseline HFIAS category, reversed"
label value hfias_bl_rev avg_hfias_cat_rev_label
tab hfias_bl hfias_bl_rev

* Reverse average HFIAS category (surveillance rounds)

revv avg_hfias_cat_surv_categorized, gen(avg_hfias_cat_surv_rev)
label var avg_hfias_cat_surv_rev "Average HFIAS category, surv r1314+r2223, reversed"
label define avg_hfias_cat_rev_label 4 "food secure" 3 "mild food insecure" ///
  2 "moderate food insecure" 1 "severe food insecure"
label value avg_hfias_cat_surv_rev avg_hfias_cat_rev_label
tab avg_hfias_cat_surv_rev

* Reverse average HFIAS category (with endline)
revv avg_hfias_sandEL_categorized, gen(avg_hfias_cat_sandEL_rev)
label var avg_hfias_cat_sandEL_rev "Average HFIAS category, surv+endline, reversed"
label value avg_hfias_cat_sandEL_rev avg_hfias_cat_rev_label
tab avg_hfias_cat_sandEL_rev


/*------------------------------------------------------------------------------
  SECTION 5: DIETARY DIVERSITY
------------------------------------------------------------------------------*/
  
display as text _newline "Processing dietary diversity variables from surveillance rounds..."

* Create average dietary diversity score across all surveillance rounds
egen DD10r_score_avg_surv = rowmean(dd10r_score_m_r1 dd10r_score_m_r2 dd10r_score_m_r3 ///
                                      dd10r_score_m_r4 dd10r_score_m_r5 dd10r_score_m_r6 ///
                                      dd10r_score_m_r7 dd10r_score_m_r8)
									  
label var DD10r_score_avg_surv "Average dietary diversity score, surveillance rounds"
tab DD10r_score_avg_surv, m

display as text _newline "Processing dietary diversity variables including endline data..."
egen DD10r_score_tot_sandEL = rowtotal(dd10r_score_m_r1 dd10r_score_m_r2 dd10r_score_m_r3 ///
                                         dd10r_score_m_r4 dd10r_score_m_r5 dd10r_score_m_r6 ///
                                         dd10r_score_m_r7 dd10r_score_m_r8 dd10r_score_m_el)

egen DD10r_score_avg_sandEL = rowmean(dd10r_score_m_r1 dd10r_score_m_r2 dd10r_score_m_r3 ///
                                        dd10r_score_m_r4 dd10r_score_m_r5 dd10r_score_m_r6 ///
                                        dd10r_score_m_r7 dd10r_score_m_r8 dd10r_score_m_el)

egen DD10r_score_avg_sandELr = rowmean(dd10r_score_m_noramadan_1 dd10r_score_m_noramadan_2 dd10r_score_m_noramadan_3 ///
                                       dd10r_score_m_noramadan_4 dd10r_score_m_noramadan_5 dd10r_score_m_noramadan_6 ///
                                       dd10r_score_m_noramadan_7 dd10r_score_m_noramadan_8 dd10r_score_m_noramadan_el)

label var DD10r_score_tot_sandEL "Total dietary diversity score, surveillance + endline"
label var DD10r_score_avg_sandEL "Average dietary diversity score, surveillance + endline"									   
label var DD10r_score_avg_sandELr "Average dietary diversity score, surveillance + endline - detrending by ramadan"


** 2017 onwards **
egen DD10r_score_avg_2017 = rowmean(dd10r_score_m_r5 dd10r_score_m_r6 dd10r_score_m_r7 dd10r_score_m_r8 dd10r_score_m_el)
                                        
egen DD10r_score_avg_2017r = rowmean(dd10r_score_m_noramadan_5 dd10r_score_m_noramadan_6 dd10r_score_m_noramadan_7 ///
                                     dd10r_score_m_noramadan_8 dd10r_score_m_noramadan_el)
									 
egen DD10r_score_avg_2017e = rowmean(dd10r_score_m_noramadan_5 dd10r_score_m_noramadan_6 dd10r_score_m_noramadan_7 ///
                                     dd10r_score_m_noramadan_8)
									 
label var DD10r_score_avg_2017 "Average DD score, surv+EL 2017"									 
label var DD10r_score_avg_2017r "Average DD score, surv+EL 2017 - detrending by ramadan"
label var DD10r_score_avg_2017e "Average DD score, surv - detrending by ramadan"

**DIETARY DIVERSITY**
  
*Minimum DD of women - all surveillance points + endline
tab DD10r_score_tot_sandEL
tab DD10r_score_avg_sandEL
gen DD_min_sandEl = .
replace DD_min_sandEl = 0 if DD10r_score_avg_sandEL >0 &  DD10r_score_avg_sandEL <5
replace DD_min_sandEl = 1 if DD10r_score_avg_sandEL >=5 & DD10r_score_avg_sandEL <11
label var DD_min_sandEl "Average DD binary, surv + endline"
label define DD_min_sandEl_label 0 "Not diverse diet" 1 "Diet diverse"  
label value DD_min_sandEl DD_min_sandEl_label


/*------------------------------------------------------------------------------
  SECTION 6: DEPRESSION
  ------------------------------------------------------------------------------*/
  
  *EPDS binary at 12: creation of major depression binary variable*
  * from raw depression score
*Baseline
tab md_score_bl, m
gen md_d_bl = .
replace md_d_bl = 0 if md_score_bl <12
replace md_d_bl = 1 if md_score_bl >=12
replace md_d_bl = . if md_score_bl == .
label var md_d_bl "Major depressive symptoms binary 11/12  - BL"
label define md_d_bl_label 0 "No" 1 "Yes"
label value md_d_bl md_d_bl_label
tab md_score_bl md_d_bl, missing

*EPDS binary at 12: creation of major depression binary variable*
  * from raw depression score
*Endline
tab md_score_el, m
gen md_d_el = .
replace md_d_el = 0 if md_score_el <12
replace md_d_el = 1 if md_score_el >=12
replace md_d_el = . if md_score_el == .
label var md_d_el "Major depressive symptoms binary 11/12  - EL"
label define md_d_el_label 0 "No" 1 "Yes"
label value md_d_el md_d_el_label
tab md_score_el md_d_el, missing

*Reverse code depression so highest (30) is no depressive symptoms, 0/1 is worst
revv md_d_bl, gen(md_d_bl_rev)
revv md_d_el, gen(md_d_el_rev)


* EPDS binary at "cutoffs": creation of major depression binary variable*
  local cutoffs 13 11 12
local times bl el

foreach cutoff in `cutoffs' {
    foreach time in `times' {
      local score_var md_score_`time'
        local bin_var md_d`cutoff'_`time'
        local label_desc "Major depressive symptoms binary `cutoff'-1/`cutoff' - `time'"
        local rev_var md_d`cutoff'_`time'_rev

        * Crear variable binaria
        gen `bin_var' = .
        replace `bin_var' = 0 if `score_var' < `cutoff'
        replace `bin_var' = 1 if `score_var' >= `cutoff'
        replace `bin_var' = . if `score_var' == .

        * Etiquetar variable
        label var `bin_var' "`label_desc'"
        label define `bin_var'_label 0 "No" 1 "Yes", replace
        label value `bin_var' `bin_var'_label

        * Tabular resultados
        tab `score_var' `bin_var', missing

        * Crear variable invertida
        revv `bin_var', gen(`rev_var')
    }
}

*Change score in EPDS from BL to EL
tab md_score_bl md_score_el, m
gen dep_change = md_score_el - md_score_bl
replace dep_change = . if md_score_bl == . & md_score_el ==.
tab dep_change, m
gen dep_change_cat = .
replace dep_change_cat = -3 if dep_change <= -10
replace dep_change_cat = -2 if (dep_change <= -3 & dep_change >= -10)

replace dep_change_cat = 3 if dep_change >= 10
replace dep_change_cat = 0 if dep_change == 0

replace dep_change_cat = 2 if dep_change >=9 
replace dep_change_cat = -1 if dep_change == -1 | -2 
replace dep_change_cat = -2 if dep_change == -3 |-4|-5|-6 |-7 |-8 |-9
replace dep_change_cat = -3 if dep_change == -10|-11|-12|-13|-14|-15|-16|-17|-18|-19|-20|-21|-22|-23|-24|-25|-26|-27|-28|-29
replace dep_change_cat = 1 if dep_change == 1|2
replace dep_change_cat = 2 if dep_change == 3 | 4 | 5| 6 | 7 |8 |9
*replace dep_change_cat = 3 if dep_change ==10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27|28|29
tab dep_change dep_change_cat, m

**assess relationship between intervention, mediators, outcome
*graph twoway scatter x y

*Add dataset non imputed
merge 1:1 wcode using "${DATA}/8_FNS_MH_cleaned_imputed_data.dta", nogen

erase "${DATA}/8_FNS_MH_cleaned_imputed_data.dta"
erase "${DATA}/temp.dta"

/*------------------------------------------------------------------------------
SECTION 10: EMPOWERMENT AVERAGE
------------------------------------------------------------------------------*/

label value quint_bl quintilcat  
label values woman_edu_cat_bl woman_edu_cat

foreach var in ss_score_cont extcomm_score_cont huscomm_score_cont dec_score_cont noincome lefthome {
    egen `var'_average = rowmean(`var'_bl `var'_el)
    egen `var'_average_i = rowmean(`var'_bl_i `var'_el_i)
}

/*------------------------------------------------------------------------------
SECTION 11: SAVE FINAL DATASET
------------------------------------------------------------------------------*/

* Save the master dataset 
display as text _newline "Saving master dataset with all variables and imputed data..."
save "${DATA}/8_Master_data.dta", replace

display as text _newline "Master data set completed!"
