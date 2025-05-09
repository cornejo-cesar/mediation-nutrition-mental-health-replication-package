/*-----------------------------------------------------------------------------
                             MAIN PREPARATION DATA
                                                              
** Program written by Cesar Cornejo 
** Created: 2025.03.12                              
** Last revised: 2024.04.15                                      
-------------------------------------------------------------------------------*/

clear
set more off	

* Install packag

    net install revv, from(http://fmwww.bc.edu/RePEc/bocode/r)
	ssc install fre

* Set global directories for all data 

	gl ROOT "/Users/cesarm3/Downloads/Replication package"

	* Set specific directories for each created data

	gl CODE      "$ROOT/Code"
	gl DATA      "$ROOT/Data"
	gl OUTPUT    "$ROOT/Output"

**************************************************


	* Run data cleaning and prepare data for imputation
	
	do "${CODE}/1_Data_cleaning.do"
	
	* Preparing mediators and Effect of Homestead Food Production 
	* intervention on women's depressive symptoms at endline 

	do "${CODE}/3_Mediaton_variables_preparation.do"
	
	* Run Descriptive Statistics
	
	do "${CODE}/4_HFP_MH_Descriptive_statistics.do"

	* Run Regression: Mixed effects
	
	do "${CODE}/5_HFP_MH_Mixed_effects_models.do"


















* Check frequency distributions of HFIAS categorical variables
* These should have values 1-4 (1=food secure, 4=severe food insecure)
fre hfias_BL hfias_r1314 hfias_r2223 hfias_EL

/*
Check values: hfias_BL, hfias_r1314, hfias_r2223, hfias_EL

        1 food secure            
        2 mild food insecure     
        3 moderate food insecure 
        4 severe food insecure   
*/