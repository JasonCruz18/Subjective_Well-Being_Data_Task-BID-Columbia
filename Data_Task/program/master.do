*****************************************************************************		      BID-COLUMBIA: Subjective Well-Being Data Task				   *
****************************************************************************

*------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------%7
/*
This dofile contains all of the commands for answer questions 1-3 from "BID-COLUMBIA: Subjective Well-Being Data Task"
Created by: Jason Cruz (any questions to 164468@unsaac.edu.pe)
*/

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

*--------------------------
* We define the working directory
*--------------------------

global path "C:\Users\PC\Documents\GitHub\Subjective_Well-Being_Data_Task-BID-Columbia\Data_Task"
	global data "$path/data"
	global figures "$path/figures"   
	global tables "$path/tables" 
	global program "$path/program" // $ symbol is useful for

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
egen newid_NA = total(missing(newid)) //
count if newid == .	
drop if newid == .	

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

* local mean1 = c(N) // This is an otherway to get number_rows (using local)
* di `mean1'

foreach x of varlist worker `$unique_respondents'{
	codebook `x', compact
}

	
*---------------------------------------------------------------------------
*	c) Merging the subjective riches data with the demographics data
*---------------------------------------------------------------------------
	
merge 1:m worker using  "${data}/ratings.dta", nogen keepusing(time rating subjective_riches aspect)

*---------------------------------------------------------------------------
*	d) Regress (OLS) subjective riches on income (Model 1)
*---------------------------------------------------------------------------

describe

*** Descriptive statistics of some variables

summ subjective_riches income

*** Regression using OLS

eststo clear
eststo: reg subjective_riches income, robust // I use just one option to deal with the heteroscedasticity problem: "robust"
vif // just to confirm the suspicion of not having multicollinearity problem
estout // estout is post estimation command and save results

*** Editing the final table

esttab, nomtitle legend ///
cells("b( star label(Coef.) fmt(2)) p(fmt(2) label(p-value))" ///
 se(fmt(2) label(S.E.)))  stats(r2 N, labels("R-cuadrado" "N. de obs."))

*** producing the table to save it in tex format that compiles in latex
 
esttab using "${tables}/tab1.tex", rename(_cons Const) ///
nomtitle legend ///
cells("b( star label(Coef.) fmt(2)) p(fmt(2) label(p-value))" ///
 se(fmt(2) label(S.E.)))  stats(r2 N, labels("R-cuadrado" "N. de obs.")) replace

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

twoway (scatter subjective_riches income, yaxis(1) yscale(range(0(5)100) axis(1)) msize(tiny) mcolor("`r(p1)'")) ///
( lfit subjective_riches income, lcolor("`r(p4)'")) ///
  ,ytitle("Subjective Riches", size(*0.6) color("`r(p2)'")) ///
  ylabel(0(25)100, labsize(*0.6)) ///
  xtitle("Income", size(*0.6) color("`r(p2)'")) ///
  xlabel(0(50000)300000, labsize(*0.6)) ///
  graphregion(color(white)) ///
  legend(cols(2) size(*0.7) region(lcolor("`r(p1)'"))) ///
  title("Regress OLS: Subjective Riches vs Income", size(*.7) box bcolor("`r(p2)'") color(white)) ///
  bgcolor(white) ylabel(, nogrid) ///
  text(250000 15 "predicted {it:Subjective Riches} =  56.28 + 0.0000927{it:Income}", place(se) box  width(58) size(vsmall) color(white) bcolor("`r(p1)'") fcolor("`r(1)'")) ///
  name(model_1, replace)
  
graph export "${figures}\model_1.pdf", as(pdf) replace

/// Interpreting the results. What is the relationship between income and subjective riches? 
 
 /*
There is a very weak positive relationship. The coefficient 0.0000927 (significant by p-value) is interpreted as: for each additional monetary unit that the respondent receives, his or her score in aspects of well-being increases by 0.0000927 points. This low value means that there are other determinants other than income that better explain the differences in aspects of well-being (such as health, happiness, etc.). In addition, being a bivariate model without controls, the correlation is weak with a very low R squared, although this is not a dazzling indicator in this analysis, we are practically facing an ad hoc model.
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

reg subjective_riches income age age_squared i.male i.education_new i.race_new, robust
vif // just to confirm the suspicion of not having multicollinearity problem

*** Both models (it´s useful to interpret the results)

eststo clear
eststo: reg subjective_riches income, robust
eststo: reg subjective_riches income age age_squared i.male i.education_new i.race_new, robust
estout // it displays both models in a single table

*** Editing the final table

esttab, nomtitle legend ///
cells("b( star label(Coef.) fmt(2)) p(fmt(2) label(p-value))" ///
 se(fmt(2) label(S.E.)))  stats(r2 N, labels("R-cuadrado" "N. de obs."))

*** producing the table to save it in tex format that compiles in latex
 
esttab using "${tables}/tab2.tex", rename(_cons Const) ///
nomtitle legend ///
cells("b( star label(Coef.) fmt(2)) p(fmt(2) label(p-value))" ///
 se(fmt(2) label(S.E.)))  stats(r2 N, labels("R-cuadrado" "N. de obs.")) replace

*** An alternative optimal regression given that we have categorical control variables (add interactions between variables in the regression)

reg income c.subjective_riches##i.male c.subjective_riches##i.education_new c.subjective_riches##i.race_new age age_squared, robust

/// Interpreting the results.

/*
The model has improved since the specification, it is more interesting the relationships of the control variables with subjective riches according to the results. For example, the negative coefficient of age (significant) shows that each additional year represents -0.35 points on the aspects of well-being, while the coefficient of age_squared shows that at a certain age (many years of life) the respondents value more the aspects of well-being, older adults probably rate well-being aspects higher than young people. Likewise, being a male respondent means scoring the aspects of well-being with 2.57 points more than women. On the other hand, for the most part, the categories of the variables education and race score the aspects of well-being less.
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

*** I display the results for the 3 models

eststo clear
eststo: reg subjective_riches income, robust
eststo: reg subjective_riches income age age_squared i.male i.education_new i.race_new, robust
eststo: reg income subjective_riches household_size age age_squared i.male i.education_new i.race_new, robust
estout // it displays 3 models in a single table

*** Editing the final table

esttab, nomtitle legend ///
cells("b( star label(Coef.) fmt(2)) p(fmt(2) label(p-value))" ///
 se(fmt(2) label(S.E.)))  stats(r2 N, labels("R-cuadrado" "N. de obs."))

*** producing the table to save it in tex format that compiles in latex
 
esttab using "${tables}/tab3.tex", rename(_cons Const) ///
nomtitle legend ///
cells("b( star label(Coef.) fmt(2)) p(fmt(2) label(p-value))" ///
 se(fmt(2) label(S.E.)))  stats(r2 N, labels("R-cuadrado" "N. de obs.")) replace

*** How would you change your analysis above in light of this new information?

/*
Coefficients of the continuous and categorical controls have changed abruptly, even changed sign. It is that to say, the "household size" would significantly influence on model. If we had "household size" in data, the results would change as this simulation shows.
As the questions (so far) asked us for specific tasks, I were unable to carry out a more rigorous analysis. To improve analysis, I should set a better specification and  work more on the proxy. Then the estimation would be simpler. On the other hand, the R squared is not reliable. Basically, we cannot see causality but only correlation.
*/

**********************************
* 			Question 3			 *
**********************************

*---------------------------------------------------------------------------
*	a) List the steps you would take to produce the scatterplot
*---------------------------------------------------------------------------

* Step 1
*_________________

*** I generate a new variable that contains only health aspects

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

drop if health_aspect == . // I drop those that do not correspond to health aspects or are NA

*** I collapse the main variables per respondent and health aspects

collapse (firstnm) income age subjective_riches, by(worker health_aspect)

* Step 2
*_________________

*** Sort by age

sort age // 

* Step 3
*_________________

*** Seperate age variable, by health_aspect

seperate age , by(health_aspect)

* Step 4
*_________________

*** Use twoway command to include several options in a unique graph, this means, of course, include three variables where one of them represents the size of the bubbles.

tw scatter age1-age7 subjective_riches [fw= income] || scatter age subjective_riches, m(none) legend(off) 
 
*---------------------------------------------------------------------------
*	b) Produce and save the scatterplot
*---------------------------------------------------------------------------

graph export "${figures}\model_1.pdf", as(pdf) replace


*---------------------------------------------------------------------------
*	c) Describe the ways in which your regressions in the previous question and your scatterplot(s) help or do not help answer this question.
*---------------------------------------------------------------------------

*** Think about your proxy for well-being as well as the specification of your regressions

* Perhaps it is better to find another more specific proxy variable. Then, I could test exogeneity and relevance (this to verify if it is a good instrument) and avoid endogeneity.
	
/// My answer (Max)	

/*
In my opinion, the analysis previously developed is not enough to answer such an important question as “determinants of well-being”.
Firstly, I performed a bivariate regression between subjective riches and income. This regression is not rigorous since causality is not possible; however, for this to make sense, I chose the subjective riches as a response variable and income as explanatory; doing it the other way around had no background since my variable of interest is subjective riches. It was interesting to ask the question: Will the welfare of respondents increase with higher income? But this question cannot be answered with such a simple specification.
Secondly, when regress with continuous and categorical control variables, they latter looked more interesting since their coefficients showed higher magnitude marginal effects on the main variable. However, from my point of view, I should have modeled differently than OLS because the response variable in this case, although continuous, is limited; it is that to say, the OLS estimate does not fit the data very well. My alternative would be to model with logit or use other multinomial models.
Finally, it may be useful to think of a welfare proxy as a more specific dependent variable (using another estimation method), as requested in question 3 f) (only for health aspects) because it is better to isolate effects for modeling.
*/

*----------------------------------THE END----------------------------------

/// Visit JasonCruz18 on GitHub
/// visit Network graph (GitHub) "https://github.com/JasonCruz18/Subjective_Well-Being_Data_Task-BID-Columbia/network" to know the sequence of my work to answer the questions of this great task
