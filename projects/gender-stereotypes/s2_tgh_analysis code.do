

import delimited "TGH/Repo/DataBaseTGH.csv", bindquote(strict) clear 


* clean and create demographics/scales
tab genderstereotypes_communion_1_co


foreach var of varlist genderstereotypes_communion_1_co-genderstereotypes_agency_12_inde {
replace `var' = "." if `var' == "NA"
}


tab ambivalentsexism_1
foreach var of varlist ambivalentsexism_1-ambivalentsexism_6 awareness_gender_inequality gender_ideology_men_should_prior gender_ideology_women_should_pri age {
replace `var' = "." if `var' == "NA"
}
 
destring genderstereotypes_communion_1_co-genderstereotypes_agency_12_inde ambivalentsexism_1-ambivalentsexism_6 awareness_gender_inequality gender_ideology_men_should_prior gender_ideology_women_should_pri age, replace


tab gender
replace gender = "." if gender == "NA"
encode gender, gen(gender_num)
*need to dl the fre package
fre gender_num

gen gender3 = .
replace gender3 = 1 if gender_num == 2
replace gender3 = 2 if gender_num == 4
replace gender3 = 3 if gender_num == 3 | gender_num == 5 | gender_num == 6

alpha genderstereotypes_communion_1_co-genderstereotypes_communion_12_s, item
egen communal = rmean(genderstereotypes_communion_1_co-genderstereotypes_communion_12_s)

alpha genderstereotypes_agency_1_decis genderstereotypes_agency_2_ambit genderstereotypes_agency_3_compe genderstereotypes_agency_5_confi genderstereotypes_agency_6_has_l genderstereotypes_agency_8_deter genderstereotypes_agency_9_coura genderstereotypes_agency_10_acti genderstereotypes_agency_12_inde, item

egen agentic = rmean(genderstereotypes_agency_1_decis genderstereotypes_agency_2_ambit genderstereotypes_agency_3_compe genderstereotypes_agency_5_confi genderstereotypes_agency_6_has_l genderstereotypes_agency_8_deter genderstereotypes_agency_9_coura genderstereotypes_agency_10_acti genderstereotypes_agency_12_inde)


alpha ambivalentsexism_1-ambivalentsexism_6, item
egen asi = rmean(ambivalentsexism_1-ambivalentsexism_6)

alpha ambivalentsexism_1 ambivalentsexism_3 ambivalentsexism_6, item
egen bsi = rmean(ambivalentsexism_1 ambivalentsexism_3 ambivalentsexism_6)

alpha ambivalentsexism_2 ambivalentsexism_4 ambivalentsexism_5, item
egen hsi = rmean(ambivalentsexism_2 ambivalentsexism_4 ambivalentsexism_5)

revrs awareness_gender_inequality

tab social_status
replace social_status = "." if social_status== "NA"
destring social_status, replace
mcenter social_status

sum age
mcenter age

rename (C_social_status C_age)(c_ses c_age)
save "data2.dta", replace


ex




use "data2.dta", clear

**# Overall Desriptives
tab gender3
sum age c_age
sum social_status c_ses
sum communal agentic
sum asi bsi hsi
sum revawareness_gender_inequality

hist communal
hist agentic
hist asi
hist bsi
hist hsi

**#Mean Difference in communal agency
preserve
rename (communal agentic) (stereotype1 stereotype2) 
reshape long stereotype, i(id) j(measure) 
mixed stereotype i.measure ib2.gender3 c_ses age || country: || id:,

***Mixed Models	  
** Overall stereotypes and attitudes analysis

**# Splines
mkspline s1_communal 4 s2_communal = communal
mkspline s1_agentic 4 s2_agentic = agentic

**## Fig. 3 
** note that graphs are produced in R, using the saved predicted margins from the models below - code on OSF

mixed hsi s1_agentic s2_agentic  s1_communal s2_communal || country:, 
mixed hsi s1_communal s2_communal s1_agentic s2_agentic ib2.gender3 c_ses c_age || country:, 
margins, at(s1_communal=1 s2_communal=0) ///
         at(s1_communal=4  s2_communal=0) ///
         at(s1_communal=4  s2_communal=3) saving(s2_margins_hs_com, replace)		 
margins, at(s1_agentic=1 s2_agentic=0) ///
         at(s1_agentic=4  s2_agentic=0) ///
         at(s1_agentic=4  s2_agentic=3) saving(s2_margins_hs_agent, replace)
		 
mixed bsi s1_communal s2_communal  s1_agentic s2_agentic || country:, 
mixed bsi s1_communal s2_communal  s1_agentic s2_agentic ib2.gender3 c_ses c_age|| country:, 
margins, at(s1_communal=1 s2_communal=0) ///
         at(s1_communal=4  s2_communal=0) ///
         at(s1_communal=4  s2_communal=3) saving(s2_margins_bs_com, replace)
margins, at(s1_agentic=1 s2_agentic=0) ///
         at(s1_agentic=4  s2_agentic=0) ///
         at(s1_agentic=4  s2_agentic=3) saving(s2_margins_bs_agent, replace)
		 
mixed revawareness_gender_inequality s1_communal s2_communal s1_agentic s2_agentic || country:, 
mixed revawareness_gender_inequality s1_communal s2_communal s1_agentic s2_agentic ib2.gender3 c_ses c_age|| country:, 
margins, at(s1_communal=1 s2_communal=0) ///
         at(s1_communal=4  s2_communal=0) ///
         at(s1_communal=4  s2_communal=3) saving(s2_margins_ds_com, replace)
		 
margins, at(s1_agentic=1 s2_agentic=0) ///
         at(s1_agentic=4  s2_agentic=0) ///
         at(s1_agentic=4  s2_agentic=3) saving(s2_margins_ds_agent, replace)
		 
**# Quadratic + Interactions
gen c_communal =  communal-4
gen c_agentic = agentic-4

sum c_agentic c_communal

gen communal2 = c_communal*c_communal
gen agentic2 = c_agentic*c_agentic
sum communal2 agentic2 

**## Fig. 4
** note that graphs are produced in R, using the saved predicted margins from the models below 

mixed hsi c.c_communal##c.c_communal c.c_agentic##c.c_agentic ib2.gender3 c_ses c_age || country:,
*step1
mixed hsi c.c_communal##c.c_communal c.c_agentic ib2.gender3 c_ses c_age || country:,

**##Fig. 4 panel A
*step2 
mixed hsi  c.c_agentic c.c_communal c.communal2 c.c_communal#c.c_agentic c.communal2#c.c_agentic ib2.gender3 c_ses c_age || country:,
margins, at(c.c_agentic = (-3(0.1)3) c.c_communal = (-3 0 3))  saving(s2_margins_inter_hsi, replace)
marginsplot, ytitle (Hostile Sexism) xtitle(Agency) ylabel(0(1)5) legend(order(1 "Women" 2 "Equal" 3 "Men") title(Communality, size(medium)))

margins, dydx(c_agentic) at(c.c_communal = (-3 0 3))
margins, dydx(c.c_communal) at( c_agentic= (-3 0 3))

margins, at(c.c_agentic = (-3 0 3) c.c_communal = (-3 0 3)) pwcompare(effects)

margins, at(c.c_communal = (-3(0.1)3) c.c_agentic = (-3 0 3)) 
marginsplot, ytitle (Hostile Sexism) xtitle(Communality) ylabel(0(1)5) legend(order(1 "Women" 2 "Equal" 3 "Men") title(Agency, size(medium)))


*step1
mixed bsi c.c_agentic##c.c_agentic c.c_communal##c.c_communal ib2.gender3 c_ses c_age || country:,
**##Fig. 4 panel B
*step2
mixed bsi c.c_agentic##c.c_agentic##c.c_communal##c.c_communal ib2.gender3 c_ses c_age || country:,
margins, at(c.c_agentic = (-3(0.1)3) c.c_communal = (-3 0 3)) saving(s2_margins_inter_bsi, replace)
marginsplot, ytitle (Benovalent Sexism) xtitle(Agency) ylabel(0(1)5) legend(order(1 "Women" 2 "Equal" 3 "Men") title(Communality, size(medium)))

margins, dydx(c_agentic) at(c.c_communal = (-3 0 3))
margins, dydx(c.c_communal) at( c_agentic= (-3 0 3))

margins, at(c.c_communal = (-3 0 3) c.c_agentic = (-3 0 3)) pwcompare(effects) 

margins, at(c.c_communal = (-3(0.1)3) c.c_agentic = (-3 0 3)) 
marginsplot, ytitle (Benovalent Sexism) xtitle(Communality) ylabel(0(1)5) legend(order(1 "Women" 2 "Equal" 3 "Men") title(Agency, size(medium)))

*step1
mixed revawareness_gender_inequality c.c_communal##c.c_communal c.c_agentic##c.c_agentic ib2.gender3 c_ses c_age || country:,

**##Fig. 4 panel C
*step2
mixed revawareness_gender_inequality c.c_communal##c.c_communal##c.c_agentic##c.c_agentic ib2.gender3 c_ses c_age || country:,
margins, at(c.c_agentic = (-3(0.1)3) c.c_communal = (-3 0 3)) saving(s2_margins_inter_ds, replace)
marginsplot, ytitle (Denial of Sexism) xtitle(Agency) ylabel(0(1)5) legend(order(1 "Women" 2 "Equal" 3 "Men") title(Communality, size(medium)))

margins, dydx(c_agentic) at(c.c_communal = (-3 0 3))
margins, dydx(c.c_communal) at( c_agentic= (-3 0 3))

margins, at(c.c_communal = (-3 0 3) c.c_agentic = (-3 0 3)) pwcompare(effects) 


margins, at(c.c_communal = (-3(0.1)3) c.c_agentic = (-3 0 3)) 
marginsplot,  ytitle (Denial of Sexism) ylabel(1(1)7) xlabel(-3 "1 Woman" -2 "2" -1 "3" 0 "4" 1 "5" 2 "6" 3 "7 Man")
