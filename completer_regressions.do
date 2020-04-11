***********
* set dir *
***********
* written by Nathan Mather May-2018

* the point of this script is to try and get at the question of "who is completing CTE" 


* you will need thi package to run the code 
ssc install estout, replace
ssc install ivreg2

* set more off
set more off, perm

* clear data
clear

* start log if you wnat for some reason 
*log using ${out_log}nm_staff_investigation_rep.log, replace

* set directories 
global in_data_cte /afs/umich.edu/group/m/mmcmps/data/data_cte/clean/
global out_log /afs/umich.edu/group/m/mmcmps/data/data_cte/ 


******************************
* student data investigation *
******************************

* load in student data 
gzuse "${in_data_cte}enrStudent_clean.dta.gz", clear

* save as temp file for merge 
tempfile enrstudent
 save "`enrstudent'"

* load in other student data 
gzuse "/afs/umich.edu/group/m/mmcmps/data/data_cte/cte_student_year.dta.gz", clear

****************************
* set up data for analysis *
****************************

* remove some of the min of three variables that are giving me trouble 
drop totmin totseg_year fedc_p1 fedc_p2 fedc_p3
rename totseg_p1 tot_num_seg_p1
rename totseg_p2 tot_num_seg_p2
rename totseg_p3 tot_num_seg_p3

* reshape. make data one row per student-year-program rather than one row per student year
reshape long completer cepd tot_num_seg oano obno coop techprep psn funding totmin totseg_year totseg_approved ncourses_year begenr enrdsv enrlep yeargrade assessment score progname fedcodename cipcode eligible_assessment cluster, i(year ric) j(p_number) string

* fix labels 
label var completer "completer  status"
label var cepd "vareer ed planning district"
label var oano "operating agency" 
label var obno "operating building code"
label var coop "student was enrolled in co-op class"
label var techprep "Tech-Prep program"
label var psn "program serial number" 
label var funding "resources resieved by the program in previous year"
label var totmin " total number of minutes of instruction" 
label var totseg_year "total number of segments taken by student this year"
label var totseg_approved "total number of segments approved by the student"
label var ncourses_year "number of courses per year"
label var begenr "mean number of students in courses"
label var enrdsv "mean number of disadvantaged students in courses"
label var enrlep "mean number of LEP students in courses"
label var yeargrade "simple average grade received by the student in the year"
label var assessment "student took CTE assessment"
label var score "assessment score"
label var progname "program of study name" 
label var fedcodename "federal cluster name" 
label var cipcode "program of study code"
label var cluster "federal cluster code"
label var eligible_assessment "student was eligible for taking the assessment" 



* merge on static student characteristics to cte_student_year
merge m:1 ric using `enrstudent'

* check out what years merged 
tabulate _merge year, missing

* looks like mostly 2007 - 2014 is valud so subset to those 
*keep if year == 2008| year == 2009| year == 2010| year == 2011| year == 2012| year == 2013| year == 2014

* tabulate some stuff to get a feel for the variables 
tab grade year, missing
tab mathvalid year, missing
tab  srsd_update, missing
tab readingvalid year, missing


tabulate _merge

*****************
* merge on srsd *
*****************
/*
* save current  data as temp file 
tempfile workng_data
 save "`workng_data'"


* load in ssrd file 
gzuse "/afs/umich.edu/group/m/mmcmps/data/data_srsd/clean2/srsd_master.dta.gz", clear

* subset to relevent years
keep if year == 2014


* merge on cte student year to get cte subset 
merge 1:1 ric year using `workng_data'

*/



* 
***************
* do analysis *
***************

* for some reason this is causeing issues and idk what it is __000000 
drop __000000 
drop __000001

* drop all of the blank completer rows that got made in the reshape. 
* ( if a student only had one CTE the still had three columns so two  blank rows exist in the long data) 
drop if completer == .

* create binary completer variable 
gen completer_bi = 1 if completer == 1
replace completer_bi = 0 if completer != 1

* fix disadvantaged variable
drop if disadvantaged == "9"
gen nm_disadv = 1 if disadvantaged == "E"
replace nm_disadv = 0 if disadvantaged == "0"

* create demo vars macro for regressions 
global demo techprep nm_disadv lep migrant i.raceethnic handicapped i.grade gender

* strip of the i. so I can look at missing values in these vars 
global demo_vars: subinstr global demo "i." "", all

* check that none of the demo vars have too many missing values 
foreach var of varlist $demo_vars sendingbldg sendingdist begenr enrdsv enrlep{
misstable summarize `var'
}

* specify some base variables 
fvset base 7 raceethnic
fvset base 12 grade

* check that students are eunique wthin a progname 
duplicates tag ric year progname, gen(prog_dup)

* change some labels to make things display better 
label define techprep_lab 0 "No Tech-Prep" 1 "Yes Tech-Prep"
label values techprep techprep_lab

label define nm_disad_lab 0 "Not Disadvantaged" 1 "Disadvantaged"
label values nm_disadv nm_disad_lab

label define grade_lab 1 "Grade 1" 2 "Grade 2" 3 "Grade 3" 4 "Grade 4" 6 "Grade 6" 7 "Grade 7" 8 "Grade 8" 9 "Grade 9" 10 "Grade 10" 11 "Grade 11" 12 "Grade 12" 14 "Grade SPED"
label values grade grade_lab

* generate completer_bi*100 so coeffs look nicer
gen completer_bi_t = completer_bi*100


****************************************************
* create variable for "ever completed" and cohorts *
****************************************************
* drop _merge var 
drop _merge

* save data for merge 
tempfile full
 save "`full'"

* subset down to needed vars 
keep year ric progname completer_bi

* cast wide to get one row per student program 
reshape wide completer_bi, i(ric progname) j(year)

* store completer var in macro 
local completer_years completer_bi2007 completer_bi2008 completer_bi2009 completer_bi2010 completer_bi2011 completer_bi2012 completer_bi2013 completer_bi2014 completer_bi2015 completer_bi2016


* if anyof these is 1, mark the student program as ever completer 
* also figure out their first time in the program and mark them as that cohort 
* initialize variables 
gen ever_completer = 0
gen cohort = .
* loop though years 
forvalues i = 2007/2016{

* create and index to grab variable from macro 
local index = `i' - 2006
* grab variable from macro 
local comp_var_i `: word `index' of `completer_years''

* mark if the student ever comleted the program 
replace ever_completer = 1 if `comp_var_i' == 1

* put in the students first year showing up in the program 
replace cohort = `i' if  cohort == . & `comp_var_i' != .

}

* now subset to usefull vars
keep ric progname ever_completer cohort

* now merge onto full data set 
merge 1:m ric progname using `full'

* transform ever complter variable so regressions are easier to read 
gen ever_completer_t = ever_completer * 100

* now for regressions lets subset to a group of interest. 
* am going to subset to year 2013 cohort 2013. This gives me the group of students 
* that started the program in 2013 along with their 2013 demographics. 
* 2013 is a good year because that gives students 4 years to complete the program 
keep if cohort == 2013
keep if year == 2013

*************************
*regs with peer effects *
*************************

* cclear and estto that exists 
eststo clear

* run full sample with peer effects, clster standard errors by student and school  
*eststo: regress ever_completer_t $demo begenr enrdsv enrlep, vce(cluster ric)
eststo: regress ever_completer_t $demo begenr enrdsv enrlep,  cluster(sendingbldg)

* change the progname variable because stata doesn't like spaces, also ditch the commas bc it messes with csv
capture drop nm_progname
gen nm_progname = subinstr(progname, " ", "_", .)
replace nm_progname = subinstr(nm_progname, ",", ".", .)

* get a macro of all the prognames 
levelsof nm_progname, l(prog_list_all)

* create a macro for just the prognames with high enrollment 
global prog_list 

* count each progname, if lots of observation, keep it in list
foreach x in `prog_list_all' {
display "`x'"
count if nm_progname == "`x'"
if r(N) > 7000{
global prog_list $prog_list "`x'"
}
}

*run on each program with peer effects 
 foreach x in $prog_list {

display "`x'"
* run regression on subgroup with peer effects, cluster at school level 
eststo: regress ever_completer_t $demo begenr enrdsv enrlep if nm_progname == "`x'",  cluster(sendingbldg)

}

 * initialize macro 
 global reg_names
foreach x in $prog_list {

* fix regression labels 
display regexm("`x'", "^[^_]+") 
local temp =  regexs(0)
local temp = subinstr("`temp'", ".", "", .)
di "`temp'"
   global reg_names $reg_names "`temp'"
   }
   
   * now save regressions as csv for clean open in excel
  esttab using "/afs/umich.edu/group/m/mmcmps/data/data_cte/working/nm_working/regressions/peer_eff_regs.csv", ///
   se ar2 label  replace nobaselevels nogaps ///
    title(Probabiltiy of Completion) varwidth(25) modelwidth(25)     ///
    nonumbers mtitles("Full Sample" "$reg_names") stats(N N_clust r2, fmt(0 0 3) labels("Number of Observations" "Number of School Clusters" "R2"))
  
  * save xlsx so its easier to look at on mfile  
  esttab using "/afs/umich.edu/group/m/mmcmps/data/data_cte/working/nm_working/regressions/peer_eff_regs.xlsx", ///
   se ar2 label  replace nobaselevels nogaps ///
    title(Probabiltiy of Completion) varwidth(25)  modelwidth(25)       ///
    nonumbers mtitles("Full Sample" "$reg_names") stats(N N_clust r2, fmt(0 0 3) labels("Number of Observations" "Number of School Clusters" "R2"))
    
********************
* regs no controls *
********************

* clear old regs 
eststo clear
    
 * run on full sample, no peer effects 
//eststo: regress ever_completer_t $demo, vce(cluster ric)
eststo: regress ever_completer_t $demo, cluster(sendingbldg)

 mean(ever_completer_t)
 
 mat mean_m = e(b)
 
   *run on each program with peer effects 
 foreach x in $prog_list {

display "`x'"
* run regression on subgroup with peer effects
eststo: regress ever_completer_t $demo if nm_progname == "`x'", cluster(sendingbldg)
mean(ever_completer_t) if nm_progname == "`x'"

 mat row = e(b)
 mat mean_m = mean_m\row
}
    
  * now save regressions as csv for clean open in excel
  esttab using "/afs/umich.edu/group/m/mmcmps/data/data_cte/working/nm_working/regressions/no_control_regs.csv", ///
  se ar2 label replace nobaselevels nogaps ///
  title(Probabiltiy of Completion) varwidth(25) modelwidth(25)       ///
  nonumbers mtitles("Full Sample" "$reg_names") stats(N N_clust r2, fmt(0 0 3) labels("Number of Observations" "Number of School Clusters" "R2"))
    
    
  * save xlsx so its easier to look at on mfile  
  esttab using "/afs/umich.edu/group/m/mmcmps/data/data_cte/working/nm_working/regressions/no_control_regs.xlsx", ///
  se ar2 label  replace nobaselevels nogaps ///
  title(Probabiltiy of Completion) varwidth(25) modelwidth(25)       ///
  nonumbers mtitles("Full Sample" "$reg_names") stats(N N_clust r2, fmt(0 0 3) labels("Number of Observations" "Number of School Clusters" "R2"))
  
  * cretae table of mean outcomes 
  *preserve data 
  preserve 
  clear
  
  * load in matrix of means as data 
   svmat mean_m
   
   * create labels 
   gen seqnum=_n
   gen reg_label = "Full Sample" if seqnum == 1 
   local N = _N
   forvalues i = 1/`N' {
   
   local re_label_i `: word `i' of $prog_list'
   di "`re_label_i'"
   replace reg_label = "`re_label_i'" if seqnum == `i'+1
   
   }


* drop extra variable 
drop seqnum 

* label 
label var mean_m1 "Mean Completer status"
label var reg_label "Sample for Mean"

* round values 
replace mean_m1 = round(mean_m1,  .001)

* svae table 
 export excel using "/afs/umich.edu/group/m/mmcmps/data/data_cte/working/nm_working/regressions/mean_outcomes.xls", replace firstrow(varlabels)

restore



***********************  
* regs school dummies *
***********************

* clear old regs 
eststo clear
    
* set matrix size if it isnt already 
set matsize 1600

 * run on full sample, bulding dummies
//eststo: regress ever_completer_t $demo i.sendingbldg, vce(cluster ric)
eststo: regress ever_completer_t $demo i.sendingbldg, cluster(sendingbldg)

* now run regressions on populare programs with school dummies 
foreach x in $prog_list {

display "`x'"
* run regression on subgroup with school dummies 
eststo: regress ever_completer_t $demo i.sendingbldg if nm_progname == "`x'", cluster(sendingbldg)

}
  
  * now save regressions as csv for clean open in excel
  esttab using "/afs/umich.edu/group/m/mmcmps/data/data_cte/working/nm_working/regressions/school_dummy_regs.csv", ///
  se ar2 label  replace nobaselevels nogaps ///
  title(Probabiltiy of Completion) varwidth(25) modelwidth(25)       ///
  nonumbers mtitles("Full Sample" "$reg_names") stats(N N_clust r2, fmt(0 0 3) labels("Number of Observations" "Number of School Clusters" "R2"))
    
    
  * save xlsx so its easier to look at on mfile  
  esttab using "/afs/umich.edu/group/m/mmcmps/data/data_cte/working/nm_working/regressions/school_dummy_regs.xlsx", ///
  se ar2 label  replace nobaselevels nogaps ///
  title(Probabiltiy of Completion) varwidth(25) modelwidth(25)       ///
  nonumbers mtitles("Full Sample" "$reg_names") stats(N N_clust r2, fmt(0 0 3) labels("Number of Observations" "Number of School Clusters" "R2"))
      




* to display macro elements 
foreach x in $prog_list {
display "`x'"
}

foreach x in $reg_names {
display "`x'"
}




