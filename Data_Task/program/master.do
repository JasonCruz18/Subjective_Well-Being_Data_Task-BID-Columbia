*****************************************************************************	*			   BID-COLUMBIA: Subjective Well-Being Data Task				*
*****************************************************************************

*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%
*Program: 		master program
*first created: 28/09/2021
*last updated:  29/09/2021
*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%	

*------------------------------------
*	Code script along with my answers to the questions
*	By Jason Cruz (Universidad Nacional de San Antonio Abad del Cusco)
* 	GitHub: JasonCruz18
*------------------------------------

clear all
set more off
set matsize 10000
set maxvar 10000


*--------------------------
* We define the working directory
*--------------------------

global path "C:/Users/PC/Documents/BID-Columbia/Data Task"
	global data "$path/data"
	global figures "$path/figures"                                    
	global program "$path/program" 

**********************************
* 			Question 1			 *
**********************************	
	
*---------------------------------------------------------------------------
*	a) Load demographics.csv
*---------------------------------------------------------------------------
	
import delimited using "${data}/ratings.csv", clear 

*---------------------------------------------------------------------------
*	b) Number of unique respondents and the number of unique aspects
*---------------------------------------------------------------------------

codebook, problems // Checking for database problems

local var aspect worker

foreach x of local var{
	codebook `x', compact
}

*---------------------------------------------------------------------------
*	c) We check to see if each respondent has only rated each aspect once
*---------------------------------------------------------------------------

bysort worker aspect (time) : gen newid = 1 if _n==1	
egen newid_NA = total(missing(newid))
count if newid == .	
drop if newid == .	

local var aspect worker

foreach x of local var{
	codebook `x', compact
}

*---------------------------------------------------------------------------
*	d) Average rating for each respondent
*---------------------------------------------------------------------------

* Here, calculate the average rating for each respondent.

egen subjective_riches = mean(rating), by(worker) // We named "subjective_riches" to rating mean as question ask
label var subjective_riches "subjective riches"

* Here, some descriptive statistics as: min, max, percentile 25, 50 and 75 for subjective riches value as a new variables.

egen min_subjective_riches= min(subjective_riches) // min
egen max_subjective_riches = max(subjective_riches) // max
egen p_25_subjective_riches = pctile(subjective_riches)  , p(25) // percentile25
egen p_50_subjective_riches = pctile(subjective_riches)  , p(50) // percentile50
egen p_75_subjective_riches = pctile(subjective_riches)  , p(75) // percentile 75

* This command is for summirize while the statistics in a unique table

tabstat subjective_riches, s(min q max) 


/// Finally, we save the data modified acording to questions

save "${data}/ratings.dta", replace
restore

**********************************
* 			Question 2			 *
**********************************

*---------------------------------------------------------------------------
*	a) Load demographics.csv
*---------------------------------------------------------------------------

* Importamos la data recientemente guardada, esta data está limpia de acuerdo a los requerimientos exigidos por los items de la pregunta 1

import delimited using "${data}/demographics.csv", clear 
