***************************************************************************************
****** EFFECT OF FREE TUITIION ON PARENTAL EXPECTATIONS *******************************
***************************************************************************************

******************** SECTION 1 - LONGUTIDUINAL DID ************************************

{
clear all
set more off
set maxvar 120000

*Import the data 
use "C:\Users\JoaquinFarina\OneDrive - Teachers College, Columbia University\Desktop\TC\IP\data_cohorte_parental_exp.dta"

* Create a quintile variable for family_income_4b
egen family_income_4b_quintile=cut(family_income_4b), group(5)

generate SES_quintiles_pre =  SES_quintiles  if agno == 2013
bysort mrun (SES_quintiles_pre): replace SES_quintiles_pre = SES_quintiles_pre[1]

* Set student as longitudinal unit 
xtset mrun agno

* Pre - post policy announcement 
gen T = 0
replace T = 1 if agno >=2015

* Treatment status 
gen TREAT = .
replace TREAT = 1 if inlist(SES_quintiles_pre,1,2) 
replace TREAT = 0 if inlist(SES_quintiles_pre,4,5) 

* Interaction 
gen TREATxT = TREAT*T

* keep population
keep if inlist(TREAT, 0, 1)

* DiD
gen d2013=agno==2013
gen treat_2013=TREAT*d2013

gen d2015=agno==2015
gen treat_2015=TREAT*d2015

gen d2017=agno==2017
gen treat_2017=TREAT*d2017

gen d2011=agno==2011
gen treat_2011=TREAT*d2011


reghdfe parental_exp_college_or_more treat_2011 treat_2015 treat_2017, absorb(mrun agno) cl(rbd) 
outreg2 using pretrends_2008cohort_parental_expectations.doc, replace  dec(3) 

reghdfe parental_exp_college_or_more TREATxT TREAT T, absorb(mrun) cl(rbd)
outreg2 using REG_2008cohort_parental_expectations.doc, replace  dec(3) 

reghdfe parental_exp_college_or_more  TREATxT T TREAT i.gen_alu i.cod_depe i.rural_rbd GPA_percentil, absorb(curso_numeric mrun) 
outreg2 using REG_2008cohort_parental_expectations.doc, append  dec(3) 







* ATTENDANCE 
reghdfe GPA_percentil treat_2011 treat_2015 treat_2017, absorb(mrun agno) cl(rbd) 



* Run the regression with fixed effects agno##i.cod_com_rbd
*reghdfe parental_exp_college_or_more T##TREAT, absorb(i.mrun)
* Use honestdid for sensitivity analysis
*honestdid, pre(1/2) post(3/4) mvec(0.5 (0.5)2)

* Parallel Trends Plot
drop mean_parental_exp
bysort agno TREAT: egen mean_parental_exp = mean(GPA_percentil)

twoway (line mean_parental_exp agno if TREAT == 0, sort lcolor(red) lwidth(thick) lpattern(solid) legend(label(1 "Control"))) ///
       (line mean_parental_exp agno if TREAT == 1, sort lcolor(blue) lwidth(thick) lpattern(solid) legend(label(2 "Treated"))) ///
       (scatter mean_parental_exp agno if TREAT == 0, mcolor(red) msize(large)) ///
       (scatter mean_parental_exp agno if TREAT == 1, mcolor(blue) msize(large)), ///
       legend(order(2 1)) ///
       xline(2014.5) ///
       title("Mean Parental Experience Over Time") ///
       xtitle("Year") ///
       ytitle("Mean Parental Experience")
	   
* Parallel trends using regression
reg parental_exp TREAT##ibn.agno i.curso_numeric i.gen_alu i.cod_depe i.rural_rbd GPA_percentil if T == 0, hascons

}


******************** SECTION 2 - LONGUTIDUINAL DID GPA PERCENTILE ************************************

{
    
clear all
set more off
set maxvar 120000

*Import the data 
use "C:\Users\JoaquinFarina\OneDrive - Teachers College, Columbia University\Desktop\TC\IP\data_cohorte_parental_exp.dta"

* Create a quintile variable for family_income_4b
egen family_income_4b_quintile=cut(family_income_4b), group(5)

generate SES_quintiles_pre =  SES_quintiles  if agno == 2013
bysort mrun (SES_quintiles_pre): replace SES_quintiles_pre = SES_quintiles_pre[1]

* Set student as longitudinal unit 
xtset mrun agno

* Pre - post policy announcement 
gen T = 0
replace T = 1 if agno >2015

* Treatment status 
gen TREAT = .
replace TREAT = 1 if inlist(SES_quintiles_pre,1,2) 
replace TREAT = 0 if inlist(SES_quintiles_pre,4,5) 

* Interaction 
gen TREATxT = TREAT*T

* keep population
keep if inlist(TREAT, 0, 1)

* DiD
gen d2013=agno==2013
gen treat_2013=TREAT*d2013

gen d2015=agno==2015
gen treat_2015=TREAT*d2015

gen d2017=agno==2017
gen treat_2017=TREAT*d2017

gen d2011=agno==2011
gen treat_2011=TREAT*d2011

* GPA 
reghdfe GPA_percentil treat_2011 treat_2015 treat_2017, absorb(mrun agno) cl(rbd) 

reghdfe GPA_percentil treat_2011 treat_2013 treat_2017 i.gen_alu i.cod_depe i.rural_rbd, absorb(mrun agno) cl(rbd) 

*reghdfe GPA_percentil  TREATxT T TREAT i.gen_alu i.cod_depe i.rural_rbd GPA_percentil, absorb(curso_numeric mrun) 

* Parallel trends using regression
reg GPA_percentil TREAT##ibn.agno i.curso_numeric i.gen_alu i.cod_depe i.rural_rbd if T == 0, hascons
	
}


******************** SECTION 1 - CROSS SECTION DID ************************************

**** GRADE LEVEL = 4 ****

{
	
clear all
set more off
set maxvar 120000

*Import the data 
use "C:\Users\JoaquinFarina\OneDrive - Teachers College, Columbia University\Desktop\TC\IP\data_full_parental_exp.dta"

* Pre - post policy announcement 
gen T = 0
replace T = 1 if agno >=2015

* Treatment status 
gen TREAT = .
replace TREAT = 1 if inlist(SES_quintiles,1,2) 
replace TREAT = 0 if inlist(SES_quintiles,4,5) 

* Interaction 
gen TREATxT = TREAT*T

* keep population
keep if inlist(TREAT, 0, 1)

* keep grade 4
keep if curso_numeric == 4
keep if agno > 2011


* DiD
reghdfe parental_exp  TREATxT T TREAT, noabsorb 
reghdfe parental_exp  TREATxT T TREAT, absorb(rbd) 
reghdfe parental_exp  TREATxT T TREAT i.curso_numeric i.gen_alu i.cod_depe i.rural_rbd GPA_percentil, absorb(rbd) 


* Run the regression with fixed effects 
*reghdfe parental_exp_college_or_more T##TREAT i.agno, noabsorb
* Use honestdid for sensitivity analysis
*honestdid, pre(1/2) post(3/4) mvec(0.5 (0.5)2)

* Parallel Trends Plot
bysort agno TREAT: egen mean_parental_exp = mean(parental_exp)

twoway (line mean_parental_exp agno if TREAT == 0, sort lcolor(red) lwidth(thick) lpattern(solid) legend(label(1 "Control"))) ///
       (line mean_parental_exp agno if TREAT == 1, sort lcolor(blue) lwidth(thick) lpattern(solid) legend(label(2 "Treated"))) ///
       (scatter mean_parental_exp agno if TREAT == 0, mcolor(red) msize(large)) ///
       (scatter mean_parental_exp agno if TREAT == 1, mcolor(blue) msize(large)), ///
       legend(order(2 1)) ///
       xline(2014.5) ///
       title("Mean Parental Experience Over Time") ///
       xtitle("Year") ///
       ytitle("Mean Parental Experience")
	   
   
* Parallel trends using regression
reg parental_exp TREAT##ibn.agno i.SES_quintiles i.curso_numeric i.gen_alu i.cod_depe i.rural_rbd GPA_percentil if T == 0, hascons

}