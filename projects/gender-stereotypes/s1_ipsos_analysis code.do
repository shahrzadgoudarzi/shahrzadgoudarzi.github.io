

use "data1.dta", clear
/*
*composites and mean-centered variables are created using the following code: 
egen communal = rmean(handle_people-impressionable)
egen agentic = rmean(make_decision-stubborn)
egen oldf_atw = rmean(sexism1-sexism4)
egen mod_sexism = rmean(sexism5 sexism6) 
egen reverse_sexism = rmean(sexism8-sexism10)
egen eg_voc = rmean(revsexism13 sexism14 sexism15)
*must dl mcenter package
sum resp_age
mcenter resp_age

sum ses10
mcenter ses10
rename (C_ses10 C_resp_age) (c_ses10 c_age)

*/
**# Center Covariates

**# Overall Descriptives and Scale Reliability
alpha handle_people-impressionable, item detail
sum communal 
hist communal

alpha make_decision-stubborn , item detai
sum agentic
hist agentic

alpha sexism1-sexism4, std item detail
sum oldf_atw

alpha sexism5-sexism7, std item detail
alpha sexism5 sexism6, std item detail 
sum mod_sexism

alpha sexism8-sexism10, std item detail 
sum reverse_sexism

alpha sexism13-sexism15, std item detail 
sum eg_voc

**#Mean Difference in communal agency
preserve 
rename (communal agentic) (stereotype1 stereotype2) 
reshape long stereotype, i(respondent_serial) j(measure) 
mixed stereotype i.measure i.female c_ses10 c_age || country: || respondent_serial:, reml dfmethod(kroger)
ex
**Overall stereotypes and attitudes analysis
**# Splines
mkspline s1_communal 0 s2_communal = communal
mkspline s1_agentic 0 s2_agentic = agentic

**## Fig. 1-2
** note that graphs are produced in R, using the saved predicted margins from the models below 
mixed oldf_atw s1_communal s2_communal s1_agentic s2_agentic  female c_ses10 c_age || country:, reml dfmethod(kroger)
margins, at(s1_communal=-1 s2_communal=0) ///
         at(s1_communal=0  s2_communal=0) ///
         at(s1_communal=0  s2_communal=1) saving(s1_margins_ms_com, replace)
		 
margins, at(s1_agentic=-1 s2_agentic=0) ///
         at(s1_agentic=0  s2_agentic=0) ///
         at(s1_agentic=0  s2_agentic=1) saving(s1_margins_ms_agent, replace)
		 
mixed mod_sexism  s1_communal s2_communal s1_agentic s2_agentic female c_ses10 c_age|| country:, reml dfmethod(kroger)
margins, at(s1_communal=-1 s2_communal=0) ///
         at(s1_communal=0  s2_communal=0) ///
         at(s1_communal=0  s2_communal=1) saving(s1_margins_ds_com, replace)
margins, at(s1_agentic=-1 s2_agentic=0) ///
         at(s1_agentic=0  s2_agentic=0) ///
         at(s1_agentic=0  s2_agentic=1) saving(s1_margins_ds_agent, replace)
		 
mixed reverse_sexism  s1_communal s2_communal s1_agentic s2_agentic female c_ses10 c_age|| country:, reml dfmethod(kroger)
margins, at(s1_communal=-1 s2_communal=0) ///
         at(s1_communal=0  s2_communal=0) ///
         at(s1_communal=0  s2_communal=1) saving(s1_margins_rs_com, replace)
margins, at(s1_agentic=-1 s2_agentic=0) ///
         at(s1_agentic=0  s2_agentic=0) ///
         at(s1_agentic=0  s2_agentic=1) saving(s1_margins_rs_agent, replace)		 
		 
mixed eg_voc s1_communal s2_communal s1_agentic s2_agentic female c_ses10 c_age|| country:,reml dfmethod(kroger)
margins, at(s1_communal=-1 s2_communal=0) ///
         at(s1_communal=0  s2_communal=0) ///
         at(s1_communal=0  s2_communal=1) saving(s1_margins_tgr_com, replace)
margins, at(s1_agentic=-1 s2_agentic=0) ///
         at(s1_agentic=0  s2_agentic=0) ///
         at(s1_agentic=0  s2_agentic=1) saving(s1_margins_tgr_agent, replace)
		 
mixed sj_m s1_communal s2_communal s1_agentic s2_agentic female c_ses10 c_age|| country:,reml dfmethod(kroger)
margins, at(s1_communal=-1 s2_communal=0) ///
         at(s1_communal=0  s2_communal=0) ///
         at(s1_communal=0  s2_communal=1) saving(s1_margins_sj_com, replace)
margins, at(s1_agentic=-1 s2_agentic=0) ///
         at(s1_agentic=0  s2_agentic=0) ///
         at(s1_agentic=0  s2_agentic=1) saving(s1_margins_sj_agent, replace)


**# Quadratic + Interaction of communal and agentic stereotypes

gen communal2 = communal^2
gen agentic2 = agentic^2

** note that graphs are produced in R, using the saved predicted margins from the models below 

mixed oldf_atw c.communal##c.communal c.agentic##c.agentic || country:, reml dfmethod(kroger)
mixed oldf_atw c.communal##c.communal c.agentic##c.agentic i.female c_ses10 c_age  || country:, reml dfmethod(kroger)

mixed oldf_atw c.communal c.agentic c.agentic2 c.communal#c.agentic2 c.agentic#c.communal i.female c_ses10 c_age  || country:, reml dfmethod(kroger)
margins, at(c.agentic = (-1(0.1)1) c.communal = (-1 0 1))  saving(s1_margins_inter_ms, replace)
marginsplot

mixed mod_sexism c.communal##c.communal c.agentic##c.agentic || country:, reml dfmethod(kroger)
mixed mod_sexism c.communal##c.communal c.agentic##c.agentic female c_ses10 c_age || country:, reml dfmethod(kroger)
mixed mod_sexism c.communal c.agentic c.agentic2 c.communal#c.agentic2 c.agentic#c.communal i.female c_ses10 c_age  || country:, reml dfmethod(kroger)
margins, at(c.agentic = (-1(0.1)1) c.communal = (-1 0 1))  saving(s1_margins_inter_ds, replace)
marginsplot

mixed reverse_sexism c.communal##c.communal c.agentic##c.agentic || country:, reml dfmethod(kroger)
mixed reverse_sexism c.communal##c.communal c.agentic##c.agentic female c_ses10 c_age  || country:, reml dfmethod(kroger)
mixed reverse_sexism c.communal c.agentic##c.agentic female c_ses10 c_age  || country:, reml dfmethod(kroger)
mixed reverse_sexism c.communal c.agentic c.agentic2 c.communal#c.agentic2 c.agentic#c.communal i.female c_ses10 c_age  || country:, reml dfmethod(kroger)
margins, at(c.agentic = (-1(0.1)1) c.communal = (-1 0 1))  saving(s1_margins_inter_rs, replace)
marginsplot


mixed eg_voc c.communal##c.communal c.agentic##c.agentic || country:, reml dfmethod(kroger)
mixed eg_voc c.c.communal##c.communal c.agentic##c.agentic female c_ses10 c_age || country:, reml dfmethod(kroger)
mixed eg_voc c.communal female c_ses10 c_age c.agentic##c.agentic|| country:, reml dfmethod(kroger)
mixed eg_voc c.communal c.agentic c.agentic2 c.communal#c.agentic2 c.agentic#c.communal i.female c_ses10 c_age  || country:, reml dfmethod(kroger)
margins, at(c.agentic = (-1(0.1)1) c.communal = (-1 0 1))  saving(s1_margins_inter_tgr, replace)
marginsplot

mixed sj_m c.communal##c.communal c.agentic##c.agentic || country:, reml dfmethod(kroger)
mixed sj_m c.c.communal##c.communal c.agentic##c.agentic female c_ses10 c_age || country:, reml dfmethod(kroger)
mixed sj_m c.communal female c_ses10 c_age c.agentic##c.agentic|| country:, reml dfmethod(kroger)
mixed sj_m c.communal c.agentic c.agentic2 c.communal#c.agentic2 c.agentic#c.communal i.female c_ses10 c_age  || country:, reml dfmethod(kroger)
margins, at(c.agentic = (-1(0.1)1) c.communal = (-1 0 1))  saving(s1_margins_inter_sj, replace)
marginsplot
