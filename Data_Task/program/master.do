*****************************************************************************		      BID-COLUMBIA: Subjective Well-Being Data Task				   *
****************************************************************************

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

*---------------------------------------------------------------------------
*	b) number of rows and check to see if it is the same as the number of unique respondents in question 1
*---------------------------------------------------------------------------
scalar number_rows = c(N)
scalar list  number_rows

local mean1 = c(N)
di `mean1'

foreach x of varlist worker `$unique_respondents'{
	codebook `x', compact
}

	
forval i=1/10 {
		di "looping over number: `i'"
	}

codebook worker, compact
	
*---------------------------------------------------------------------------
*	c) Merging the subjective riches data with the demographics data
*---------------------------------------------------------------------------
	
merge 1:m worker using  "${data}/ratings.dta", nogen keepusing(time rating subjective_riches aspect)

*---------------------------------------------------------------------------
*	d) Regress (OLS) subjective riches on income (Model 1)
*---------------------------------------------------------------------------

describe

*** Descriptive statistics of some variables

summ income subjective_riches

*** Regression using OLS

reg income subjective_riches, robust // I use just one option to deal with the heteroscedasticity problem: "robust"
vif // just to confirm the suspicion of not having multicollinearity problem
estimates store m1 // estimates is post estimation command and save results

*** Plotting the regression to make interpretation more intuitive

* To use the color palette it was previously installed:
	/// ssc install palettes, replace
	/// ssc install colrspace, replace
	
colorpalette ///
 "17 32 49" ///
 "21 45 53" ///
 "52 91 99" ///
 "212 236 221" ///
  , n(4) nograph

twoway (scatter income subjective_riches, yaxis(1) yscale(range(0(500)500) axis(1)) msize(tiny) mcolor("`r(p1)'")) ///
( lfit income subjective_riches, lcolor("`r(p2)'")) ///
  ,xtitle("Subjective Riches", size(*0.6) color("`r(p3)'")) ///
  xlabel(0(25)100, labsize(*0.6)) ///
  ytitle("Income", size(*0.6) color("`r(p3)'")) ///
  ylabel(0(50000)300000, labsize(*0.6)) ///
  graphregion(color(white)) ///
  legend(cols(2) size(*0.4) region(lcolor("`r(p4)'"))) ///
  title("Regress OLS: Subjective Riches on Income", size(*.7) box bcolor("`r(p4)'") color(white)) ///
  bgcolor(white) ylabel(, nogrid) ///
  text(250000 15 "predicted {it:life} = 50.36 + 2.45{it:school}", place(se) box  width(46) size(vsmall) color(white) bcolor("`r(p2)'") fcolor("`r(p2)'")) ///
  name(model_1, replace)
  
graph export "${figuras}\model_1.pdf", as(pdf) replace

reg subjective_riches log_income, robust 
twoway (scatter subjective_riches log_income) ///
(lfit subjective_riches log_income)
 
/// Interpreting the results. What is the relationship between income and subjective riches? 
 
 /*
 
 */
 
 
 *---------------------------------------------------------------------------
*	e) Regress (OLS) subjective riches on income with controls (Model 2)
*---------------------------------------------------------------------------

*** It will be more helpful to make transformations to the variables education and race, which are originally of the string type

* We check how many unique values ​​each variable has

codebook education race, compact
tab education race // tab command displays labels for unique values

local var education_new race_new

foreach x of local var{
	gen `x' = .
}


*gen education_new = .
replace education_new = 1 if education == "Less than high school"
replace education_new = 2 if education == "High school" 
replace education_new = 3 if education == "Some college"
replace education_new = 4 if education == "Graduate degree" 
replace education_new = 5 if education == "Bachelor's degree" 
replace education_new = 6 if education == "Master's degree" 
replace education_new = 7 if education == "Doctoral degree" 


label define education_lab 1 "Less than high school" 2 "High school" 3 "Some college" 4 "Graduate degree" 5 "Bachelor's degree" 6 "Master's degree" 7 "Doctoral degree"
label values education_new education_lab

tab race

*gen race_new = .
replace race_new = 1 if race == "White (non-Hispanic)"
replace race_new = 2 if race == "Multiracial"
replace race_new = 3 if race == "Hispanic (any race)"
replace race_new = 4 if race == "Black (non-Hispanic)"
replace race_new = 5 if race == "Asian (non-Hispanic)"
replace race_new = 6 if race == "Other"

label define race_lab 1 "White (non-Hispanic)" 2 "Multiracial" 3 "Hispanic (any race)" 4 "Black (non-Hispanic)" 5 "Asian (non-Hispanic)" 6 "Other"
label values race_new race_lab

*** Generating a new control variable containing age squared

gen age_squared = age*age

*** Regression 2 

reg income subjective_riches age age_squared i.male i.education_new i.race_new, robust
vif // just to confirm the suspicion of not having multicollinearity problem
estimates store m2 // save Model 2

estout m1 m2 // it displays both models in a single table

* Editing the final table

estout, drop(0.male 1.education_new 1.race_new) rename(1.male Male 2.education_new Master's_degree 3.education_new Less_than_high_school 4.education_new High_school 5.education_new Graduate_degree 6.education_new Doctoral_degree 7.education_new Bachelor's_degree 2.race_new Multiracial 3.race_new Hispanic 4.race_new Black 5.race_new Asian 6.race_new Other _cons Const) ///
mlabels("(1)" "(2)") title("Tabla 1") legend ///
cells("b( star label(Coef.) fmt(2)) p(fmt(2) label(p-value))" ///
 se(fmt(2) label(S.E.)))  stats(r2 N, labels("R-cuadrado" "N. de obs."))

*** producing the table to save it in tex format that compiles in latex
 
estout using "C:/Users/PC/Documents/BID-Columbia/tex/first.tex", rename(_cons Const) ///
title("Tabla 1") legend ///
cells(b( star label(Coef.) fmt(2)) p(fmt(2) label(p-value)) ///
 se(fmt(2) label(S.E.)))  stats(r2 N, labels("R-cuadrado" "N. de obs.")) replace 

*** An alternative optimal regression given that we have categorical control variables (add interactions between variables in the regression)

reg income c.subjective_riches##i.male c.subjective_riches##i.education_new c.subjective_riches##i.race_new age age_squared, robust

/// Interpreting the results.

/*

*/
 
*---------------------------------------------------------------------------
*	f) Analysis about in light of each respondent’s household size (Model 3)
*---------------------------------------------------------------------------

*** Assuming that I have the information on the respondent’s household size, I set an assumption: at most, a family can have 5 members and at least 1.

set seed 1506

gen household_size = runiformint(1, 5) // With this command I generate a random variable of integer values ​​between the ad hoc range (1 to 5 assumed)

su household_size // The mean is not far from what is seen empirically (for example in ENAHO)

*** Regression 3 

reg income subjective_riches household_size age age_squared i.male i.education_new i.race_new, robust
vif // just to confirm the suspicion of not having multicollinearity problem
estimates store m3 // save Model 3

*** How would you change your analysis above in light of this new information?

/*

*/


**********************************
* 			Question 3			 *
**********************************

*---------------------------------------------------------------------------
*	a) List the steps you would take to produce the scatterplot
*---------------------------------------------------------------------------

* Step 1
*_________________

gen health_aspect = .

replace health_aspect = 1 if aspect == "the quality of your sleep" 
replace health_aspect = 2 if aspect == "you not feeling anxious" 
replace health_aspect = 3 if aspect == "your emotional stability" 
replace health_aspect = 4 if aspect == "your health"
replace health_aspect = 5 if aspect == "your mental health"
replace health_aspect = 6 if aspect == "your physical fitness"
replace health_aspect = 7 if aspect == "your physical safety and security"

label define aspect_l 1 "aspect1" 2 "aspect2" 3 "aspect3" 4 "aspect4" 5 "aspect5" 6 "aspect6" 7 "aspect7"
label values health_aspect aspect_l

drop if health_aspect == .

collapse (firstnm) income age subjective_riches, by(worker health_aspect)

* Step 2
*_________________

sort age
seperate age , by(health_aspect)
tw scatter age1-age7 mean_rating [fw= income] || scatter age mean_rating, m(none) legend(off) 

* Step 3
*_________________



* Step 4
*_________________




* Step 5
*_________________



* Step 6
*_________________




*---------------------------------------------------------------------------
*	b) Produce and save the scatterplot
*---------------------------------------------------------------------------





*---------------------------------------------------------------------------
*	c) Describe the ways in which your regressions in the previous question and your scatterplot(s) help or do not help answer this question.
*---------------------------------------------------------------------------

*** Think about your proxy for well-being as well as the specification of your regressions

* For this purpose, I will test the exogeneity and relevance tests for a regression where I evaluate endogeneity (this to check if it is a good instrument)
	
	



*----------------------------------THE END----------------------------------

/// Visit JasonCruz18 on GitHub

