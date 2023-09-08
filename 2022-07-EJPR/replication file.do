* ============================================================================ *
* PROJECT:		COALITION COMPROMISE SPAIN
* AIM: replicate all tables and graphs of the paper: 
* Plescia, C., Ecker, A., & Meyer, T. M. (2022). Do party supporters accept 
*	policy compromises in coalition governments?. European Journal of Political Research, 61(1), 214-229.
* AUTHOR: 		Carolina Plescia
* DATE:			16-06-2023
* ============================================================================ *


* ---------------------------------------------------------------------------- *
* You need two datasets to run this do-file:

* 1. "main_data.dta" that you can download here: https://data.aussda.at/dataset.xhtml?persistentId=doi:10.11587/W3AN2V

* 2. "seat_data.dta" that you can download from this same repository

* ---------------------------------------------------------------------------- *



* ---------------------------------------------------------------------------- *
* Settings
* ---------------------------------------------------------------------------- *
clear all
set more off
set scheme s1mono

*package name:  marhis.pkg

* ---------------------------------------------------------------------------- *
* Open data file
* ---------------------------------------------------------------------------- *

* ------ set working directory (where data are stored)
use "main_data.dta", clear

keep if WAVE==2

* ---------------------------------------------------------------------------- *
* Recode & clean data
* ---------------------------------------------------------------------------- *


* ------  DV: Question 7: how much compromise
rename Q43_exp_ESP compromise       /*higher score less compromise*/
recode compromise (0=10) (1=9) (2=8) (3=7) (4=6) (5=5) (6=4) (7=3) (8=2) (9=1) (10=0), gen (compromise_rev)


* ------   minor coalition parner
gen minor=0 if Q41_exp_party_shown_ESP==1 | Q41_exp_party_shown_ESP==2
replace minor=1 if Q41_exp_party_shown_ESP>2 & Q41_exp_party_shown_ESP<=5

label define lab2 0 "mainstream" 1 "challenger"
label value minor lab2
tab minor


* ------   issue saliency
rename Q42_exp_ESP issue_saliency


* ------   Questions 13-16: PID
*does R have PID?

gen PID=.
replace PID=0 
replace PID=1 if (Q22_1==21) | ///
(Q22_1==22) | ///
(Q22_1==23) | ///
(Q22_1==24) | ///
(Q22_1==25) 

gen strength_PID=0 if PID==0
replace strength_PID=1 if Q23==3
replace strength_PID=2 if Q23==2
replace strength_PID=3 if Q23==1


* --------------------------------------------------
* ------   Question 5: PTV (PP, PSOE, Cs, VOX, UP)
*find pref. top party
forvalues i=1(1)5 {
recode Q19_`i' (-77 -98 -99=.)
}

egen topp= rmax(Q19_1 Q19_2 Q19_3 Q19_4 Q19_5)

forvalues i=1(1)5 {
gen tp`i'=0
recode tp`i' 0=1 if topp==Q19_`i' & topp!=.
}

egen ties=rowtotal(tp1 tp2 tp3 tp4 tp5) 
tab ties
replace ties=. if Q19_1==. & Q19_2==. & Q19_3==. & Q19_4==. & Q19_5==.
tab ties

gen tiesD=0
replace tiesD=1 if ties==2 | ties==3 | ties==4 | ties==5
tab tiesD

* --------------------------------------------------
*does the shown coalition include R's PID party?

gen coalitionPID=0
replace coalitionPID=1 if PID!=0 & ( ///
	((Q22_1==21 | Q22_1==22) & Q41_exp_COALITION_ESP==1) ///
| 	((Q22_1==22 | Q22_1==23) & Q41_exp_COALITION_ESP==4) ///
| 	((Q22_1==22 | Q22_1==25) & Q41_exp_COALITION_ESP==5) ///
| 	((Q22_1==21 | Q22_1==23 | Q22_1==24) & Q41_exp_COALITION_ESP==23))
tab coalitionPID

gen coalitionPIDstrength=coalitionPID*strength_PID


* ------   Question 19: coalition preferences
recode Q35_1_ESP -99=. /*PP-PSOE*/
recode Q35_2_ESP -99=. /*PP-VOX-Cs*/
recode Q35_3_ESP -99=. /*PSOE-Cs*/
recode Q35_4_ESP -99=. /*PSOE-UP*/

* ------   pref. shown coalition
gen pref_coalition=Q35_1_ESP if Q41_exp_COALITION_ESP==1
replace pref_coalition=Q35_3_ESP if Q41_exp_COALITION_ESP==4
replace pref_coalition=Q35_4_ESP if Q41_exp_COALITION_ESP==5
replace pref_coalition=Q35_2_ESP if Q41_exp_COALITION_ESP==23

label define lab1 1 "PP-PSOE" 4 "PSOE-Cs" 5 "PSOE-UP" 23 "PP-VOX-Cs" 
label value Q41_exp_COALITION_ESP lab1
fre Q41_exp_COALITION_ESP


* ------   Question 6: issue
tab Q41_exp_ESP

recode Q41_exp_ESP (2=0) (1=1) (3=2) (4=3) (5=4) (6=5) (7=6), gen (ref_aut)
label define lab0 0 "autonomías" 1 "Economía" 2 "Redistribución" 3 "Inmigración" 4 "Cambio climático" ///
5 "Integración europea" 6 "LGBT"
label value ref_aut lab0
fre ref_aut


* ------ socio-demo
recode Q2 (1=0) (2=1), gen (female)
gen age=Q1
gen educationx=Q4
recode educationx (1=1) (2=2) (3=3) (5=4) (6=5) (7=6) (8=7) (9=8) (else=.), gen (education)

recode Q5 (4=0) (3=1) (2=2) (1=3) (else=.), gen (int_politics)
recode Q6 (4=0) (3=1) (2=2) (1=3) (else=.), gen (int_elections)
recode Q16 88=., gen (SWD)

gen dummy_grand=0
replace dummy_grand=1 if Q41_exp_COALITION_ESP==1
tab dummy_grand

* ------   IV1: Question 12: extreme ideology
recode Q10 -99=.
gen extreme_ideo=abs(Q10-5)
fre extreme_ideo
tab Q10 extreme_ideo


* recode missing values for left-right placements *
mvdecode Q9_*, mv(-98 -99)

* generate variables for left-right placement and ptv-scores for own and up to two other parties *
foreach var in ptv lr {
	gen `var'_pref_party = .
	gen `var'_coal_party1 = .
	gen `var'_coal_party2 = .
}

* replace information on left-right placement and ptv-scores based on party preference and shown coalition alternative *
clonevar PARTIDO_VIS_pos1=Q43_exp_PARTY_1stPOSITION_ESP
clonevar PARTIDO_VIS_pos2=Q43_exp_PARTY_2ndPOSITION_ESP

levelsof Q41_exp_party_shown_ESP, local(pref_party)
foreach party of local pref_party {
	replace ptv_pref_party =  Q19_`party' if Q41_exp_party_shown_ESP == `party'
	replace lr_pref_party =   Q9_`party' if Q41_exp_party_shown_ESP == `party'
	forval x = 1/2 {
		replace ptv_coal_party`x' = Q19_`party' if PARTIDO_VIS_pos`x' == `party'
		replace lr_coal_party`x'  = Q9_`party' if PARTIDO_VIS_pos`x' == `party'
	}
}



keep Q41_exp_party_shown_ESP Q43_exp_PARTY_1stPOSITION_ESP Q43_exp_PARTY_2ndPOSITION_ESP issue_saliency compromise compromise_rev minor PID strength_PID topp tiesD coalitionPID coalitionPIDstrength pref_coalition ref_aut female age education int_politics int_elections SWD dummy_grand extreme_ideo ptv_pref_party ptv_coal_party1 ptv_coal_party2 lr_pref_party lr_coal_party1 lr_coal_party2 Q41_exp_COALITION_ESP END_TIME


* reshape data set to add party seat data *
gen party_idown = Q41_exp_party_shown_ESP
gen party_idother1 = Q43_exp_PARTY_1stPOSITION_ESP 
gen party_idother2 = Q43_exp_PARTY_2ndPOSITION_ESP

gen id = _n
reshape long party_id, i(id) j(party) string 
drop if party_id == .

merge m:1 party_id using "seat_data.dta", keep(1 3)
assert _merge == 3
drop _merge party_id party_abb

reshape wide seats, i(id) j(party) string 
drop id seatsown
rename (seatsother1 seatsother2) (seats_coal_party1 seats_coal_party2)


* generate (seat-weighted) left-right and ptv scores of shown coalition alternative *
foreach var in ptv lr {
	gen `var'_w_coal = ((`var'_coal_party1*seats_coal_party1)+(`var'_coal_party2*seats_coal_party2))/(seats_coal_party1+seats_coal_party2)
	replace `var'_w_coal = `var'_coal_party1 if `var'_w_coal == . & `var'_coal_party2 == .
	
}



* calculate hostility and difference between own party and shown coalition alternative *
gen hostility = (ptv_w_coal - 10) * -1
gen ptv_diff = ptv_pref_party - ptv_w_coal
assert ptv_diff >= 0
gen lr_diff = abs(lr_pref_party - lr_w_coal)

* clean data set *
drop *_pref_party *_coal_party1 *_coal_party2



* ---------------------------------------------------------------------------- *
* ANALYSIS
* ---------------------------------------------------------------------------- *

* Figure 1 *
hist compromise_rev, by(Q41_exp_COALITION_ESP, note("")) percent ytitle(Percent of respondents) xtitle(Willingness to compromise)
graph export "figure1.tif", replace


* Multivariate analysis *
* define control variables *
global controls "female age education hostility i.ref_aut i.Q41_exp_COALITION_ESP"
set more off
eststo clear

* Model 1*
eststo: reg compromise_rev i.minor c.issue_saliency c.coalitionPIDstrength $controls

* Figure 2 *
marhis issue_saliency, percent summaryno label(1) 
graph export "figure2.tif", replace

* Model 2* 
eststo: reg compromise_rev i.minor##c.issue_saliency c.coalitionPIDstrength $controls

* Figure 3 *
margins, at(issue_saliency=(0(1)10) minor=(0 1))
marginsplot, ///
	plot1opts(lcol(black) lpat(shortdash) lw(medthick)) recast(line) recastci(rarea) ///
	xtitle("Issue saliency") ytitle("Linear prediction of willingness to compromise" " ") title("") ///
	ylabel(0 "0" 5 "5" 10 "10") xlabel(0 "0" 5 "5" 10 "10")
graph export "figure3.tif", replace

* Table 1 *
esttab using "table1.rtf", ///
	b(%6.3f) se(%6.3f) starlevels(* 0.05 ** 0.01 *** 0.001) ///
	order(coalitionPIDstrength issue_saliency 1.minor 1.minor#c.issue_saliency) ///
	indicate("Issue fixed effects = *ref_aut" "Coalition fixed effects = *Q41_exp_COALITION_ESP*") drop(0.minor) ///
	scalars(r2_a) noomitted compress noeqlines replace

	
	
	
	
	
	
	
	

* ---------------------------------------------------------------------------- *
* APPENDIX
* ---------------------------------------------------------------------------- * 

gen coalition_FE2=.

replace coalition_FE2=0 if Q41_exp_party_shown_ESP==2 & Q41_exp_COALITION_ESP==1 // PSOE supp in PSOE-PP
replace coalition_FE2=1 if Q41_exp_party_shown_ESP==1 & Q41_exp_COALITION_ESP==1  // PP supp in PSOE-PP

replace coalition_FE2=2 if Q41_exp_COALITION_ESP==4 // PSOE-Cs

replace coalition_FE2=3 if Q41_exp_party_shown_ESP==2 & Q41_exp_COALITION_ESP==5 // PSOE supp in PSOE-UP
replace coalition_FE2=4 if Q41_exp_party_shown_ESP==5 & Q41_exp_COALITION_ESP==5 // UP supp in PSOE-UP

replace coalition_FE2=5 if Q41_exp_COALITION_ESP==23 // PP PP-Vox-Cs

label define coalition_FE2_lbl  0 "PSOE in PSOE-PP" 1 "PP in PSOE-PP" 2 "PSOE-Cs" 3 "PSOE in PSOE-UP" 4 "UP in PSOE-UP" 5 "PP-Vox-Cs"
label values coalition_FE2  coalition_FE2_lbl

tabstat compromise_rev, by(coalition_FE2) statistics(n mean sd)

global controls2 "female age education hostility i.ref_aut i.coalition_FE2"


*** randomization check (in R) using Hansen & Bowers 2008
* Hansen, B. B. and Bowers, J. (2008) 'Covariate balance in simple, stratified and clustered comparative studies', Statistical Science, 219-36. 
* covs: female age education lr
* strata: PARTIDO_MOSTRADO_NewP6
* assignment: COALICION_MOSTRADA_NewP6
*gen lr=P12
egen strata=group(Q41_exp_party_shown_ESP)
egen assign=group(Q41_exp_COALITION_ESP)

gen assign_in_strata=0 if strata==1 & assign==1
replace assign_in_strata=1 if strata==1 & assign==4

replace assign_in_strata=0 if strata==2 & assign==1
replace assign_in_strata=1 if strata==2 & assign==2
replace assign_in_strata=2 if strata==2 & assign==3

replace assign_in_strata=0 if strata==3 & assign==2
replace assign_in_strata=1 if strata==3 & assign==4

replace assign_in_strata=0 if strata==4 & assign==4

replace assign_in_strata=0 if strata==5 & assign==3

bysort  Q41_exp_party_shown_ESP: tab Q41_exp_COALITION_ESP 
sum female age education  if Q41_exp_party_shown_ESP==1 & Q41_exp_COALITION_ESP==1 // PP supporters & PP-PSOE
sum female age education  if Q41_exp_party_shown_ESP==1 & Q41_exp_COALITION_ESP==23 // PP supporters & PP-Cs-Vox
sum female age education  if Q41_exp_party_shown_ESP==2 & Q41_exp_COALITION_ESP==1 // PSOE supporters & PP-PSOE
sum female age education  if Q41_exp_party_shown_ESP==2 & Q41_exp_COALITION_ESP==5 // PSOE supporters & PSOE-UP
sum female age education  if Q41_exp_party_shown_ESP==3 & Q41_exp_COALITION_ESP==4 // Cs supporters & PSOE-Cs
sum female age education  if Q41_exp_party_shown_ESP==3 & Q41_exp_COALITION_ESP==23 // Cs supporters & PP-Cs-Vox


* export data for the randomization check
export delimited female age education  Q41_exp_party_shown_ESP  strata  assign assign_in_strata using "rand_check.csv", replace


*** Table A3: descriptives ***
eststo clear
estpost tabstat compromise_rev issue_saliency coalitionPIDstrength minor female ///
	age education hostility, statistics(mean sd min max) columns(statistics)

esttab . using "tableA3.rtf", cells("mean(fmt(a2)) sd(fmt(a2)) min max") replace


*** alternative models specifications ***
* ordered logit and tobit *
local i = 3
foreach model_com in ologit tobit {
	* linear and model interactive *
	foreach model_spec in "i.minor c.issue_saliency" i.minor##c.issue_saliency {
		`model_com' compromise_rev `model_spec' c.coalitionPIDstrength $controls
		est store model`i'
		local ++i
	}
}

* alternative operationalizations of PID *
foreach pid in coalitionPID topp {
	* linear and model interactive *
	foreach model_spec in "i.minor c.issue_saliency" i.minor##c.issue_saliency {
		reg compromise_rev `model_spec' `pid' $controls
		est store model`i'
		local ++i
	}
}

* excluding respondents interviewed after November 12 *
gen date_end = END_TIME
format date_end %td
gen subsample = date_end <= date("12nov2019","DMY")

foreach model_spec in "i.minor c.issue_saliency" i.minor##c.issue_saliency {
	reg compromise_rev `model_spec' c.coalitionPIDstrength $controls if subsample
	est store model`i'
	local ++i
}

* controlling for difference between left-right position of preferred party and (seat-weighted) left-right position of coalition party/ies *
foreach model_spec in "i.minor c.issue_saliency" i.minor##c.issue_saliency {
	reg compromise_rev `model_spec' c.coalitionPIDstrength $controls lr_diff
	est store model`i'
	local ++i
}

* Table A4: alternative model specifications and operationalizations *
esttab model* using "tableA4.rtf", ///
	b(%6.3f) se(%6.3f) starlevels(* 0.05 ** 0.01 *** 0.001) ///
	order(coalitionPIDstrength coalitionPID topp issue_saliency 1.minor 1.minor#c.issue_saliency) ///
	indicate("Issue fixed effects = *ref_aut" "Coalition fixed effects = *COALIC*") drop(0.minor) ///
	scalars(r2_a aic) noomitted compress noeqlines replace


*** Linearity of interaction effect ***

* linear interaction diagnostic plots *
gen maxY = -2
forval x = 0/1 {
	egen med_w_`x' = median(issue_saliency) if minor == `x' 
	egen upq_w_`x' = pctile(issue_saliency) if minor == `x', p(75) 
	egen loq_w_`x' = pctile(issue_saliency) if minor == `x', p(25) 
	egen iqr_w_`x' = iqr(issue_saliency) if minor == `x' 
	egen upper_w_`x' = max(min(issue_saliency, upq_w_`x' + 1.5 * iqr_w_`x')) if minor == `x' 
	egen lower_w_`x' = min(max(issue_saliency, loq_w_`x' - 1.5 * iqr_w_`x')) if minor == `x' 
	
	if `x' == 0 local subgroup "Mainstream party"
	else local subgroup "Challenger party"
	
	twoway (sc compromise_rev issue_saliency if minor == `x', msize(small) ms(oh) jitter(2) xtitle("Issue saliency") ytitle("Willingness to compromise") ylabel(0(5)10) subtitle("`subgroup'")) ///
		(rbar med_w_`x' upq_w_`x' maxY,  bfc(white) barw(1.3) hor) /// 
		(rbar med_w_`x' loq_w_`x' maxY,  bfc(white) barw(1.3) hor) ///
		(rspike upq_w_`x' upper_w_`x' maxY , hor) ///
		(rspike loq_w_`x' lower_w_`x' maxY , hor) ///
		(rcap upper_w_`x' upper_w_`x' maxY , msize(*1) hor) ///
		(rcap lower_w_`x' lower_w_`x' maxY , msize(*1) hor) ///
		(lfit compromise_rev issue_saliency if minor == `x', lpattern(shortdash)) ///
		(lowess compromise_rev issue_saliency if minor == `x', legend(off))
		
	graph export "figureA1_`subgroup'.png", replace
}

* binning estimator +
tab Q41_exp_COALITION_ESP, gen(ind_coal_)
tab ref_aut, gen(ind_issue_)

interflex compromise_rev minor issue_saliency ///
	coalitionPIDstrength female age education hostility ///
	ind_coal_? ind_issue_?, type(binning) ///
	dlabel("Challenger party") ///
	xlabel("Issue saliency") ///
	ylabel("willingness to compromise") ///
	xdistr(density)
	
graph export "figureA2.png", replace

	
*** Model with party-specific coalition FEs
* note: party-specific FEs only for two coalitions with notable differences in supporters' willingness to compromise (PSOE-PP & PSOE-UP)


global controls2 "female age education hostility i.ref_aut i.coalition_FE2"
set more off
eststo clear

* Model 1*
eststo: reg compromise_rev i.minor c.issue_saliency c.coalitionPIDstrength $controls2

* Figure 2 *
marhis issue_saliency, percent summaryno label(1)
graph export "figure2_partyFE.tif", replace

* Model 2* 
eststo: reg compromise_rev i.minor##c.issue_saliency c.coalitionPIDstrength $controls2

* Figure 3 *
margins, at(issue_saliency=(0(1)10) minor=(0 1))
marginsplot, ///
	plot1opts(lcol(black) lpat(shortdash) lw(medthick)) recast(line) recastci(rarea) ///
	xtitle("Issue saliency") ytitle("Linear prediction of willingness to compromise" " ") title("") ///
	ylabel(0 "0" 5 "5" 10 "10") xlabel(0 "0" 5 "5" 10 "10")
graph export "figure3_partyFE.tif", replace

* Table XXX *
esttab using "table1_partyFE.rtf", ///
	b(%6.3f) se(%6.3f) starlevels(* 0.05 ** 0.01 *** 0.001) ///
	order(coalitionPIDstrength issue_saliency 1.minor 1.minor#c.issue_saliency) ///
	indicate("Issue fixed effects = *ref_aut" "Party-Coalition fixed effects = *coalition_FE*") drop(0.minor) ///
	scalars(r2_a) noomitted compress noeqlines replace
