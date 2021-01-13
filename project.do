import excel "/Users/andreacutrera/Desktop/Micro-Econometrics/project/stata/final/2020 Stata", sheet("Finale") firstrow
encode states, gen(state)
xtset state year, yearly

* square of economic variables
gen gdp_i = incumbent*gdp
gen real_gdp_i = incumbent*real_gdp
gen real_pc_gdp_i = incumbent*real_pc_gdp
gen gdp_i2 = gdp_i^2
gen real_gdp_i2 = real_gdp_i^2
gen real_pc_gdp_i2 = real_pc_gdp_i^2
gen highlev_educ2 = highlev_educ^2
gen deflatorinc2 = deflatorinc^2
gen g_ninc2 = g_ninc^2

sort year state, stable

***********DOMANDA_1***************
*fixed effect in stata as Fair did with the fixed effect also for year

reg share_d incumbent growthinc deflatorinc g_ninc dper dur i.state if year < 2012
testparm growthinc deflatorinc g_ninc  // Perform a test for the joint significance of economic variables in the model
testparm i.state //Check if the panel dataset can be pooled by testing the joint significance of state-level fixed effects.

predict fair_12 if year == 2012

reg share_d incumbent growthinc deflatorinc g_ninc dper dur i.state if year < 2016
testparm growthinc deflatorinc g_ninc //Perform a test for the joint significance of economic variables in the model
testparm i.state //Check if the panel dataset can be pooled by testing the joint significance of state-level fixed effects.

predict fair_16 if year == 2016

reg share_d incumbent growthinc deflatorinc g_ninc dper dur i.state if year < 2020
*good news with incumbent significative
testparm growthinc deflatorinc g_ninc // Perform a test for the joint significance of economic variables in the model
testparm i.state //Check if the panel dataset can be pooled by testing the joint significance of state-level fixed effects.

predict fair_20 if year == 2020

gen e_fair_12 = share_d - fair_12 if year == 2012
gen e_fair_16 = share_d - fair_16 if year == 2016
gen e_fair_20 = share_d - fair_20 if year == 2020

gen e_fair_12sq = e_fair_12^2 if year == 2012
gen e_fair_16sq = e_fair_16^2 if year == 2016
gen e_fair_20sq = e_fair_20^2 if year == 2020


summarize e_fair_12sq e_fair_16sq e_fair_20sq

*who won 12
gen w_fair_12 = 1 if fair_12 > 50.00 & year == 2012
gen ge_fair_12 = Great_El*w_fair_12
egen s_fair_12 = sum(ge_fair_12)
*who won 16
gen w_fair_16 = 1 if fair_16 > 50.00 & year == 2016
gen ge_fair_16 = Great_El*w_fair_16
egen s_fair_16 = sum(ge_fair_16)
*who won 20
gen w_fair_20 = 1 if fair_20 > 50.00 & year == 2020
gen ge_fair_20 = Great_El*w_fair_20
egen s_fair_20 = sum(ge_fair_20)

*domanda 3
*******************************************************************************
*LASSO Double selection

* create a set of variables, variables from which lasso selects
global X_lasso gdp_i real_gdp_i population incumbent dper dur governor_dem old_voters perc_over highlev_educ density growthinc deflatorinc deflatorinc2 gdp_i2 real_gdp_i2 real_pc_gdp_i2 highlev_educ2 g_ninc new_midterm unemployment

global X_lasso_pc population dper dur governor_dem old_voters perc_over highlev_educ density highlev_educ2 new_midterm unemployment

*fixed effects and time effects
qui tab state, gen(S)
*qui tab year, gen(Y)
*put them in another macro Alabama and 1980 as base cases until 2008
global X_effects S2-S50 
*Y2-Y7

*******************************************************************************
*DSLASSO for 2012

lasso linear share_d ($X_effects) $X_lasso in 1/408, selection(plugin, heteroskedastic)
lassocoef
*selected: incumbent dur highlev_educ2 new_midterm / deflatorinc2 highlev_educ2 growthinc
lasso linear real_pc_gdp_i ($X_effects) $X_lasso_pc in 1/408, selection(plugin, heteroskedastic)
lassocoef
*selected: dper real_pc_gdp_i2 dur / ----
reg share_d real_pc_gdp_i growthinc deflatorinc2 highlev_educ2 i.state if year < 2012
*only highlev_educ2 and new_midterm are significative

predict l_12 if year == 2012
gen e_l_12 = share_d - l_12
gen e_l_12sq = e_l_12^2


********************************************************************************
*DSLASSO for 2016

global X_effects S2-S50 
*Y2-Y8
lasso linear share_d ($X_effects) $X_lasso in 1/459, selection(plugin, heteroskedastic)
lassocoef
*selected: highlev_educ2 new_midterm
lasso linear real_pc_gdp_i ($X_effects) $X_lasso_pc in 1/459, selection(plugin, heteroskedastic)
lassocoef
*selected: dper dur perc_over real_pc_gdp_i2
reg share_d real_pc_gdp_i dur growthinc deflatorinc2 dper i.state if year < 2016
*only highlev_educ2 and new_midterm are significative

predict l_16 if year == 2016
gen e_l_16 = share_d - l_16
gen e_l_16sq = e_l_16^2

********************************************************************************

*DSLASSO for 2020
global X_effects S2-S50 
*Y2-Y9
lasso linear share_d ($X_effects) $X_lasso in 1/511, selection(plugin, heteroskedastic)
lassocoef
*selected: highlev_educ2 new_midterm
lasso linear real_pc_gdp_i ($X_effects) $X_lasso_pc in 1/511, selection(plugin, heteroskedastic)
lassocoef
*selected: dper dur perc_over real_pc_gdp_i2

reg share_d real_pc_gdp_i dur growthinc deflatorinc2 dper i.state if year < 2020
*only highlev_educ2 and new_midterm and gdp_i are significative
predict l_20 if year == 2020
gen e_l_20 = share_d - l_20
gen e_l_20sq = e_l_20^2


summarize e_fair_12sq e_fair_16sq e_fair_20sq e_l_12sq e_l_16sq e_l_20sq

*who won 12
gen w_lasso_12 = 1 if l_12 > 50.00 & year == 2012
gen ge_lasso_12 = Great_El*w_lasso_12
egen s_lasso_12 = sum(ge_lasso_12)

*who won 16
gen w_lasso_16 = 1 if l_16 > 50.00 & year == 2016
gen ge_lasso_16 = Great_El*w_lasso_16
egen s_lasso_16 = sum(ge_lasso_16)

*who won 20
gen w_lasso_20 = 1 if l_20 > 50.00 & year == 2020
gen ge_lasso_20 = Great_El*w_lasso_20
egen s_lasso_20 = sum(ge_lasso_20)



reg share_d highlev_educ highlev_educ2 new_midterm i.state i.year if year < 2012
predict y12 if year == 2012
gen e12 = share_d - y12 
gen e12sq = e12^2

summarize e12sq
********************************************************************************
****************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************************




*midterm seems to be more influent for latest period
twoway (scatter share_d midterm) (lfit share_d midterm) if year == 2016
twoway (scatter share_d midterm) (lfit share_d midterm) if year == 2012
twoway (scatter share_d midterm) (lfit share_d midterm) if year == 2008
twoway (scatter share_d midterm) (lfit share_d midterm) if year == 2004
twoway (scatter share_d midterm) (lfit share_d midterm) if year == 2000
twoway (scatter share_d midterm) (lfit share_d midterm) if year == 1996
twoway (scatter share_d midterm) (lfit share_d midterm) if year == 1992
twoway (scatter share_d midterm) (lfit share_d midterm) if year == 1988
twoway (scatter share_d midterm) (lfit share_d midterm) if year == 1984
twoway (scatter share_d midterm) (lfit share_d midterm) if year == 1980
*also high level education
twoway (scatter share_d highlev_educ) (lfit share_d highlev_educ) if year == 2016
twoway (scatter share_d highlev_educ) (lfit share_d highlev_educ) if year == 2012
twoway (scatter share_d highlev_educ) (lfit share_d highlev_educ) if year == 2008
twoway (scatter share_d highlev_educ) (lfit share_d highlev_educ) if year == 2000
twoway (scatter share_d highlev_educ) (lfit share_d highlev_educ) if year == 1996   
twoway (scatter share_d highlev_educ) (lfit share_d highlev_educ) if year == 1992
twoway (scatter share_d highlev_educ) (lfit share_d highlev_educ) if year == 1988  
twoway (scatter share_d highlev_educ) (lfit share_d highlev_educ) if year == 1984
twoway (scatter share_d highlev_educ) (lfit share_d highlev_educ) if year == 1980
*with new midterm
*midterm seems to be more influent for latest period
twoway (scatter share_d new_midterm) (lfit share_d new_midterm) if year == 2016
twoway (scatter share_d new_midterm) (lfit share_d new_midterm) if year == 2012
twoway (scatter share_d new_midterm) (lfit share_d new_midterm) if year == 2008
twoway (scatter share_d new_midterm) (lfit share_d new_midterm) if year == 2004
twoway (scatter share_d new_midterm) (lfit share_d new_midterm) if year == 2000
twoway (scatter share_d new_midterm) (lfit share_d new_midterm) if year == 1996
twoway (scatter share_d new_midterm) (lfit share_d new_midterm) if year == 1992
twoway (scatter share_d new_midterm) (lfit share_d new_midterm) if year == 1988
twoway (scatter share_d new_midterm) (lfit share_d new_midterm) if year == 1984
twoway (scatter share_d new_midterm) (lfit share_d new_midterm) if year == 1980

twoway (scatter share_d old_voters) (lfit share_d old_voters) if year == 2016
twoway (scatter share_d old_voters) (lfit share_d old_voters) if year == 2012
twoway (scatter share_d old_voters) (lfit share_d old_voters) if year == 2016
twoway (scatter share_d old_voters) (lfit share_d old_voters) if year == 2016
twoway (scatter share_d old_voters) (lfit share_d old_voters) if year == 2016
twoway (scatter share_d old_voters) (lfit share_d old_voters) if year == 2016
twoway (scatter share_d old_voters) (lfit share_d old_voters) if year == 2016
twoway (scatter share_d old_voters) (lfit share_d old_voters) if year == 2016


*trend in '80 was more grothinc more vote for rep;
twoway (scatter share_d growthinc) (lfit share_d growthinc) if year == 2016
twoway (scatter share_d growthinc) (lfit share_d growthinc) if year == 2000
twoway (scatter share_d growthinc) (lfit share_d growthinc) if year == 1992
twoway (scatter share_d growthinc) (lfit share_d growthinc) if year == 1988
twoway (scatter share_d growthinc) (lfit share_d growthinc) if year == 1980

*in 80' there was the opposite trend again
twoway (scatter share_d deflatorinc) (lfit share_d deflatorinc) if year == 2016
twoway (scatter share_d deflatorinc) (lfit share_d deflatorinc) if year == 2000
twoway (scatter share_d deflatorinc) (lfit share_d deflatorinc) if year == 1992
twoway (scatter share_d deflatorinc) (lfit share_d deflatorinc) if year == 1980


twoway (scatter share_d g_ninc) (lfit share_d g_ninc) if year == 2016







*********************************************************************************************
*********************************************************************************************
*********************************************************************************************

lasso linear new_midterm ($X_effects) $X_lasso_1, selection(plugin,heteroskedastic)
lassocoef

lasso linear dur ($X_effects) $X_lasso_1, selection(plugin,heteroskedastic)
lassocoef

lasso linear deflator ($X_effects) $X_lasso_1, selection(plugin,heteroskedastic)
lassocoef

lasso linear highlev_educ2 ($X_effects) $X_lasso_1, selection(plugin,heteroskedastic)
lassocoef



reg share_d growth new_midterm highlev_educ highlev_educ2 i.year i.state if year < 2012
predict y_1 if year == 2012
gen e_1 = share_d-y_1
*who won
gen win = 1 if y_1 > 50.00 & year == 2012
gen winner = Great_El*win
egen total_seats = sum(winner)

*********************************************************************************************
*FOR "2016"

global X_effects S2-S50 Y2-Y8
lasso linear share_d ($X_effects) $X_lasso, selection(plugin, heteroskedastic)
lassocoef

global X_lasso_2 gdp gdp_i real_gdp real_gdp_i real_pc_gdp_i deflator population logpop real_pc_gdp log_gdp growth good_news incumbent dper dur governor_dem old_voters perc_over highlev_educ density growthinc deflatorinc state gdp_i2 real_gdp_i2 real_pc_gdp_i2 old_voters2 deflator2
lasso linear highlev_educ2 ($X_effects) $X_lasso_2, selection(plugin, heteroskedastic)
lassocoef

lasso linear new_midterm ($X_effects) $X_lasso_2, selection(plugin, heteroskedastic)
lassocoef

reg highlev_educ2 highlev_educ gdp_i2 deflator i.year i.state if year < 2016

reg share_d highlev_educ2 highlev_educ new_midterm i.year i.state if year < 2016
predict y_2 if year == 2016
gen e_2 = share_d-y_2
summarize e_2

*who won
gen win_1 = 1 if y_2 > 50.00 & year == 2016
gen winner_1 = Great_El*win_1
egen total_seats_1 = sum(winner_1)


***********************************************************************************************
*FOR "2020"

global X_effects S2-S50 Y2-Y9
lasso linear share_d ($X_effects) $X_lasso, selection(plugin, heteroskedastic)
lassocoef

lasso linear new_midterm ($X_effects) $X_lasso_2, selection(plugin, heteroskedastic)
lassocoef

lasso linear highlev_educ2 ($X_effects) $X_lasso_2, selection(plugin, heteroskedastic)
lassocoef


reg highlev_educ2 gdp highlev_educ gdp_i2 deflator i.year i.state if year < 2020




reg share_d growth highlev_educ2 highlev_educ new_midterm i.year i.state if year<2020
predict y_3 if year == 2020
gen e_3 = share_d-y_3
summarize e_3

*who will win
gen win_2 = 1 if y_3 > 50.00 & year == 2020
gen winner_2 = Great_El*win_2
egen total_seats_2 = sum(winner_2)



summarize e_1 e_2 e_3 e_fair_12 e_fair_16 e_fair_20
gen e_1sq = e_1^2
gen e_2sq = e_2^2
gen e_3sq = e_3^2
gen e_fair_12sq = e_fair_12^2
gen e_fair_16sq = e_fair_16^2
gen e_fair_20sq = e_fair_20^2

summarize e_1sq e_2sq e_3sq e_fair_12sq e_fair_16sq e_fair_20sq
***************************************************************************************


reg unemployment share_d












