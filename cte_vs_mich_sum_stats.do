***********
* set dir *
***********
* written by Nathan Mather May-2018

 * the point of this scrit is to try and get at the question "who is taking CTE Classes" 
* we will  create tables and graphs to compare the demographics 
* of students within CTE and students in the General population. i.e. if women make up 
* 49% of michigan HS students, do they make up 49% of CTE students? (spoiler alert..no) 


* you will need this package to run esttab
ssc install estout, replace

* set more off  because its annoying 
set more off, perm

* clear data
clear

* start log *note I actually dont really like logs  
*log using ${out_log}nm_staff_investigation_rep.log, replace


* set directories 
global in_data_cte /afs/umich.edu/group/m/mmcmps/data/data_cte/clean/
global out_tables /afs/umich.edu/group/m/mmcmps/data/data_cte/working/nm_working/student_dem_tables/
global out_graphs /afs/umich.edu/group/m/mmcmps/data/data_cte/working/nm_working/student_dem_graphs/
global out_int /afs/umich.edu/group/m/mmcmps/data/data_cte/working/nm_working/int_data/

*******************
* set up cte data *
*******************


* load on srsd to student cte data 
gzuse "/afs/umich.edu/group/m/mmcmps/data/data_cte/cte_student_year.dta.gz", clear

* check what makes row unique
duplicates tag ric year, gen(dup)
tab dup
drop dup
tab year 

* save as temp file for merge 
tempfile cte_stud_year
 save "`cte_stud_year'"

*************************
* set up mich wide data *
*************************

* load in ssrd file 
gzuse "/afs/umich.edu/group/m/mmcmps/data/data_srsd/clean2/srsd_master.dta.gz", clear

* drop years that arent in cte_student_year
drop if year < 2007
drop if year >2016

* check what makes this file unique 
duplicates tag ric year, gen(dup)
tab dup
drop dup

* merge on cte student year to get cte subset (boht unique by ric year so 1:1)
merge 1:1 ric year using `cte_stud_year'

* check the merge rates 
tab _merge year, missing

* save merge rates 
tabout _merge year using ${out_int}srsd_merge_rate.xls, cells(col) h1("merge rate srsd %") replace
tabout _merge year using ${out_int}srsd_merge_rate.xls, cells(freq) h1("Cmerge rate srsd count") append


* we can drop the "using only" obs from merge. These are CTE students without dems. 
* we wont be able to use them in analysis anyway 
drop if _merge == 2

*************************
* make plots and tables *
*************************

* generate varible that indicates if studs are cte or not (for a given student year obs)
gen cte_f = 1 if _merge == 3
replace cte_f = 0 if _merge != 3

**********************************
* make graph of total enrollment *
**********************************


* subset to high school. (vast majority of CTE are in HS)
keep if grade_fnl >= 9 
keep if grade_fnl <= 12

*preserve data
preserve 

* get percentages of CTE for entire sample 
collapse (sum) n_var = cte_f (count) total = cte_f, by(year)
gen perc_cte = n_var/total*100
drop n_var total


* save table
 export excel using "${out_tables}/perc_studs_CTE_by_year.xls", replace firstrow(varlabels)
 
 * graph yearly Student envolvment in CTE 
twoway connected perc_cte year , ytitle(" % Of All High School Students") ///
title(" % High School Students in at Least one CTE Class by Year")

* save graph
graph export ${out_graphs}/perc_studs_CTE_by_year.png, replace 

* restore data 
restore 

*******************************
* students who ever touch CTE *
*******************************
* preserve data 
preserve 

* subset down to just needed vars 
keep ric grade_fnl year cte_f

* initialize a cohort variable 
gen cohort = "Not Enough Data" 

* save as temp data 
tempfile full_sample
 save "`full_sample'"

* create cohots. We need 4 years of data so go from 2007-2013
forvalues i = 2007/2013{

* subset to cohort i. Considered cohort i if in 9th grade in year i
keep if grade_fnl == 9 & year == `i'
keep ric

* merge onto full sample 
merge 1:m ric using `full_sample'

* repplace cohort if sutdnet merged in from this cohort and doesn't already have a cohort 
* students may already have a cohort if they repeated 9th grade. This will ensure they stay 
* with thier first cohort 
replace cohort = "cohort_`i'" if cohort == "Not Enough Data" & _merge == 3

drop _merge
tempfile full_sample
 save "`full_sample'"

}

* keep only first four ovservations per student. Otherwise we will have extra students in 
* earlier years who wentto school for 6 years and we wont have those student in 2013 
* so the across year comparison gets fuzzier. 
sort ric year 
quietly by ric :  gen obs = _n

* subset to first four years of students 
keep if inrange(obs, 1, 4)

* get vars to reshape 
keep ric cte_f obs cohort

* reshape wide to one row per student rather than one row per student_year 
reshape wide cte_f, i(ric cohort) j(obs)

* get rid of the cohorts without enough data to analyze 
drop if cohort == "Not Enough Data"

* mark if ever in a cte class ( at least in thier first 4 years in school) 
gen cte_ever = 1 if cte_f1 == 1 | cte_f2 == 1 | cte_f3 == 1 | cte_f4 == 1 
replace cte_ever = 0 if cte_ever != 1

* actually lets also drop students that dont have 4 years of data. This way our sample 
* is the first four years of all students that spent a full four years in high-school
* otherwise transfers dropouts over time fuzzy the comparison 
drop if cte_f1 == .
drop if cte_f2 == .
drop if cte_f3 == .
drop if cte_f4 == .

* generate numeric cohort var for graph 
* gotta do this by hand because can't figure out as.numeric() for stata 
gen cohort_year = 2007 if cohort == "cohort_2007"
replace cohort_year = 2008 if cohort == "cohort_2008"
replace cohort_year = 2009 if cohort == "cohort_2009"
replace cohort_year = 2010 if cohort == "cohort_2010"
replace cohort_year = 2011 if cohort == "cohort_2011"
replace cohort_year = 2012 if cohort == "cohort_2012"
replace cohort_year = 2013 if cohort == "cohort_2013"

* now collapse to get "ever in cte" by cohort 
collapse (sum) n_var = cte_ever (count) total = cte_ever, by(cohort_year)
gen perc_cte_ever = n_var/total*100
drop n_var total

* label vars 
label var cohort_year "Year Student Started 9th Grade"
label var perc_cte_ever "% Took CTE in 4 Years of High School"

* graph
twoway connected perc_cte_ever cohort_year, title("% Students That Took CTE in 4 Years")

* save graph
graph export ${out_graphs}studs_ever_cte_by_cohort.png, replace 


* restore data 
restore 

*********
* grade *
*********

* table of grades for all cte students 
tabout grade_fnl using ${out_log}cte_grade_srsd.xls if cte_f == 1, cells(freq col) h1("CTE Grades from srsd: all years") replace
tabout grade_fnl using ${out_log}grade_srsd.xls if , cells(freq col) h1("Grades from srsd: all years") replace


***************************
* create weighting matrix *
***************************

* check out grades 
tab grade_fnl year if cte_f==1, matcell(grade_cte) col
tab grade_fnl year, matcell(grade) col

* create grade weights to re-weight general population to match CTE.
* start by getting totals for each year (cte and total) 
mata : st_matrix("coltot_cte", colsum(st_matrix("grade_cte")))
mata : st_matrix("coltot", colsum(st_matrix("grade")))

* now get percentage by grade for each year, for cte and full sample 
mata : st_matrix("perc", 100 * st_matrix("grade") :/ st_matrix("coltot"))
mata : st_matrix("perc_cte", 100 * st_matrix("grade_cte") :/ st_matrix("coltot_cte"))

* now get reweighting factors to put general population in CTE grade proportions 
mata : st_matrix("weights", st_matrix("perc_cte") :/ st_matrix("perc"))

* now stack weights matrix into one column for merge with data 
mat lw = weights[1..4, 1]
forvalues i = 2/10{

mat col = weights[1..4, `i']

mat lw = lw\col
}

**************************
* set up vars for graphs *
**************************

* check for missing values 
tabulate female year, missing

* create properly named frl var 
gen frl = freereduceddummy
label var frl "Free and Reduced Lunch"

* create other race var
gen other = 1 if amerin == 1
replace other = 1 if asianamer == 1
replace other = 1 if hawaiian == 1
replace other = 1 if hisp == 1
replace other = 0 if other !=1

* label other variable 
label var other "amerin|asianamer|hawaiian|hisp ==1"

********************************
* loop to create tables/graphs *
********************************

************
* weighted *
************

* i guess do this as a loop because writing funcitons in stata is hard
* loop over each demographic and get percentages in general population and re weight, 
* then get percentages for CTE students. Merge these samples and make a graph 
foreach var of varlist female lep frl snap black white other {

* preserve data 
preserve

* subset out missing data 
drop if `var' == .

* get percentages of var for entire sample 
collapse (sum) n_var = `var' (count) total = `var', by(year grade_fnl)
svmat lw

* generate weighted number 
gen n_var_w = n_var * lw1
gen total_w = total * lw1

 * collapse on weighted numbers 
 collapse (sum) n_var = n_var_w (sum) total = total_w, by(year)

gen perc_`var'_w = n_var/total*100
drop n_var total

* save as temp file 
tempfile col_total
 save "`col_total'"

restore 

* preserve large data 
preserve

* subset to only cte students 
keep if cte_f == 1

* subset out missing data 
drop if `var' == .

* get percentages of gender for subsample 
collapse (sum) n_var_cte = `var' (count) total_cte = `var', by(year)
gen perc_`var'_cte = n_var_cte/total_cte*100
drop n_var_cte total_cte

* merge samples 
merge 1:1 year using `col_total'

* dont need this anymore 
drop _merge

* add labels 
label var perc_`var'_cte "% `var' within CTE"
label var perc_`var'_w "% `var' in MI weighted by CTE grade"

* save as variable specific file 
 export excel using "${out_tables}weighted/perc_`var'_weighted.xls", replace firstrow(varlabels)

*also save as dta int file for easy use later 
 save "${out_int}perc_`var'_weighted", replace

* graph
twoway connected perc_`var'_w  perc_`var'_cte year, lpattern(solid dash) title("`var' Representation in CTE and MI Weighted by CTE Grades")

* save graph
graph export ${out_graphs}weighted/`var'_w_graph.png, replace 

* restore data 
restore 

}

************
* grade 12 *
************

* basically we are going to do the same thing but rather than weight them,
* we are just going to subset to grade 12 

* subset to grade 12 
keep if grade_fnl == 12

* i guess do this as a loop because writing funcitons in stata is odd
foreach var of varlist female lep frl snap black white other {

* preserve data 
preserve

* subset out missing data 
drop if `var' == .

* get percentages of gender for entire sample 
collapse (sum) n_var = `var' (count) total = `var' , by(year)
gen perc_`var' = n_var/total*100
drop n_var total

* save as temp file 
tempfile col_total
 save "`col_total'"

restore 
* preserve large data 
preserve

* subset to only cte students 
keep if cte_f == 1


* subset out missing data 
drop if `var' == .

* get percentages of gender for subsample 
collapse (sum) n_var_cte = `var' (count) total_cte = `var', by(year)
gen perc_`var'_cte = n_var_cte/total_cte*100
drop n_var_cte total_cte

* merge samples 
merge 1:1 year using `col_total'

drop _merge

* add labels 
label var perc_`var'_cte "% `var' within CTE"
label var perc_`var' "% `var' in MI"

* save as variable specific file 
 export excel using "${out_tables}grade_12/perc_`var'_grade_12.xls", replace firstrow(varlabels)

* graph
twoway connected perc_`var'  perc_`var'_cte year, lpattern(solid dash) title("`var' Representation in CTE and MI Grade 12")

* save graph
graph export ${out_graphs}grade_12/`var'_grade12_graph.png, replace 

* restore data 
restore 

}

*******************
* compined graphs *
*******************
* put some of the graphs together because that will look pretty dont you think? 

********
* race *
********

* now load all the int files ew saved earlier and merge them together to make one graph for race 

* load in one of the race files 
use "${out_int}perc_white_weighted.dta", clear

local races "black other"

* merge on all the other temp files 
foreach var in `races' {

* merge samples 
merge 1:1 year using "${out_int}perc_`var'_weighted"

drop _merge

}


* graph
twoway connected perc_black_w  perc_black_cte perc_white_w perc_white_cte perc_other_w perc_other_cte  year, ///
 lcolor(red red blue blue green green) lpattern(dash dash_dot shortdash shortdash_dot longdash longdash_dot) ///
 mcolor(red red blue blue green green) ylabel(0(20)100) ymtick(0(5)100) ///
title("Race Percentages in CTE and in MI Weighted by CTE Grades")

* save graph
graph export ${out_graphs}weighted/race_w_graph.png, replace 

****************
* snap and frl *
****************
* one graph combining SNAP and FRL 

* load in one of the race files 
use "${out_int}perc_snap_weighted.dta", clear

* merge samples 
merge 1:1 year using "${out_int}perc_frl_weighted"

* graph
twoway connected perc_frl_w  perc_frl_cte perc_snap_w perc_snap_cte year, ///
 lcolor(red red blue blue ) lpattern(dash dash_dot shortdash shortdash_dot) ///
 mcolor(red red blue blue) ///
title("FRL/SNAP Percentages in CTE and in MI Weighted by CTE Grades")

* save graph
graph export ${out_graphs}weighted/income_w_graph.png, replace 










// this is old code I dont want to delete becaues there is no version control available  
// PS start using github 
/*
**********
* gender *
**********

* check for missing values 
tabulate female year, missing

* create better gender var 
gen gender = "Male" if female == 0
replace gender = "Female" if female == 1

tab gender female

* output tables 
tabout gender year using ${out_log}gender_year_g12.xls if grade_fnl == 12, cells(col) h1("Total grade 12 Population") replace

tabout gender year using ${out_log}gender_year_g12.xls if cte_f == 1 & grade_fnl == 12, cells(col) h1("CTE grade 12 Population") append


* preserve data 
preserve

* get percentages of gender for entire sample 
collapse (sum) n_female = female (count) total = female , by(year grade_fnl)
gen perc_female = n_female/total*100
drop n_female total

* save as temp file 
tempfile col_total
 save "`col_total'"

restore 

* preserve large data 
preserve

* subset to only cte students 
keep if cte_f == 1

* get percentages of gender for subsample 
collapse (sum) n_female_cte = female (count) total_cte = female, by(year)
gen perc_female_cte = n_female_cte/total_cte*100
drop n_female_cte total_cte

* merge samples 
merge 1:1 year using `col_total'


twoway connected perc_female perc_female_cte year,  ylabel(40(5)55) ymtick(40(1)55) lpattern(solid dash)
graph export ${out_log}gender_graph.png, replace 

* restore data 
restore 

********
* race *
********

* check for missing values 
tabulate racecat year, missing

tabout racecat year using ${out_log}race_year.xls, cells(col) h1("Total Population") replace

tabout racecat year using ${out_log}race_year.xls if cte_f == 1, cells(col) h1("CTE Population") append

foreach var of varlist amerin asianamer black hawaiian white hisp {

* preserve data 
preserve

* get percentages of gender for entire sample 
collapse (sum) n_var = `var' (count) total = `var' , by(year)
gen perc_`var' = n_var/total*100
drop n_var total

* save as temp file 
tempfile col_total
 save "`col_total'"

restore 

* preserve large data 
preserve

* subset to only cte students 
keep if cte_f == 1

* get percentages of gender for subsample 
collapse (sum) n_var_cte = `var' (count) total_cte = `var', by(year)
gen perc_`var'_cte = n_var_cte/total_cte*100
drop n_var_cte total_cte

* merge samples 
merge 1:1 year using `col_total'

drop _merge

* save as variable specific file 
 save "/afs/umich.edu/group/m/mmcmps/data/data_cte/working/nm_working/int_data/perc_`var'"

* graph
twoway connected perc_`var'  perc_`var'_cte year, lpattern(solid dash)

* save graph
graph export ${out_log}`var'_graph.png, replace 

* restore data 
restore 

}

* now load all the temp files and merge them together to make one graph 
*preserve data
preserve

* load in one of the race files 
use "/afs/umich.edu/group/m/mmcmps/data/data_cte/working/nm_working/int_data/perc_amerin", clear

local races "asianamer black hawaiian white hisp"

* merge on all the other temp files 
foreach var in `races' {

* merge samples 
merge 1:1 year using "/afs/umich.edu/group/m/mmcmps/data/data_cte/working/nm_working/int_data/perc_`var'"

drop _merge

}

* create other variable for smaller groups 
gen perc_other = perc_hawaiian + perc_asianamer + perc_amerin + perc_hisp
gen perc_other_cte = perc_hawaiian_cte + perc_asianamer_cte + perc_amerin_cte + perc_hisp_cte

* graph
twoway connected perc_black  perc_black_cte perc_white perc_white_cte perc_other perc_other_cte  year, ///
 lcolor(red red blue blue green green) lpattern(solid dash solid dash solid dash) mcolor(red red blue blue green green) ylabel(0(20)100) ymtick(0(5)100)

* save graph
graph export ${out_log}race_graph.png, replace 


*restore data
restore

*******
* lep *
*******

* check for missing values 
tabulate lep year, missing

tabout lep year using ${out_log}lep_year.xls, cells(col) h1("Total Population") replace

tabout lep year using ${out_log}lep_year.xls if cte_f == 1, cells(col) h1("CTE Population") append

* preserve data 
preserve

* get percentages of gender for entire sample 
collapse (sum) n_lep = lep (count) total = lep , by(year)
gen perc_lep = n_lep/total*100
drop n_lep total

* save as temp file 
tempfile col_total
 save "`col_total'"

restore 

* preserve large data 
preserve

* subset to only cte students 
keep if cte_f == 1

* get percentages of gender for subsample 
collapse (sum) n_lep_cte = lep (count) total_cte = lep, by(year)
gen perc_lep_cte = n_lep_cte/total_cte*100
drop n_lep_cte total_cte

* merge samples 
merge 1:1 year using `col_total'


twoway connected perc_lep perc_lep_cte year, ylabel(0(2)10) ymtick(0(1)10) lpattern(solid dash)

graph export ${out_log}lep_graph.png, replace 

* restore data 
restore 

*******
* FRL *
*******

* check for missing values 
tabulate freereduceddummy year, missing

tabout freereduceddummy year using ${out_log}frl_year.xls, cells(col) h1("Total Population") replace

tabout freereduceddummy year using ${out_log}frl_year.xls if cte_f == 1, cells(col) h1("CTE Population") append

* preserve data 
preserve

* get percentages of gender for entire sample 
collapse (sum) n_frl = freereduceddummy (count) total = freereduceddummy , by(year)
gen perc_frl = n_frl/total*100
drop n_frl total

* save as temp file 
tempfile col_total
 save "`col_total'"

restore 

* preserve large data 
preserve

* subset to only cte students 
keep if cte_f == 1

* get percentages of gender for subsample 
collapse (sum) n_frl_cte = freereduceddummy (count) total_cte = freereduceddummy, by(year)
gen perc_frl_cte = n_frl_cte/total_cte*100
drop n_frl_cte total_cte

* merge samples 
merge 1:1 year using `col_total'

drop if perc_frl == .

twoway connected perc_frl perc_frl_cte year , ylabel(30(5)55) ymtick(30(1)55) lpattern(solid dash)

graph export ${out_log}frl_graph.png, replace

* restore data 
restore 



********
* SNAP *
********

* check for missing values 
tabulate snap year, missing

tabout snap year using ${out_log}snap_year.xls, cells(col) h1("Total Population") replace

tabout snap year using ${out_log}snap_year.xls if cte_f == 1, cells(col) h1("CTE Population") append

* preserve data 
preserve

* get percentages of gender for entire sample 
collapse (sum) n_snap = snap (count) total = snap , by(year)
gen perc_snap = n_snap/total*100
drop n_snap total

* save as temp file 
tempfile col_total
 save "`col_total'"

restore 

* preserve large data 
preserve

* subset to only cte students 
keep if cte_f == 1

* get percentages of gender for subsample 
collapse (sum) n_snap_cte = snap (count) total_cte = snap, by(year)
gen perc_snap_cte = n_snap_cte/total_cte*100
drop n_snap_cte total_cte

* merge samples 
merge 1:1 year using `col_total'

* remove missing years 
drop if perc_snap == .

twoway connected perc_snap perc_snap_cte year, ylabel(10(5)40) ymtick(10(1)40) lpattern(solid dash)

graph export ${out_log}snap_graph.png, replace

* restore data 
restore 

*/

