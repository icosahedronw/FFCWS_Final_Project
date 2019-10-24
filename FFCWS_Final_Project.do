* Clear everything
clear
clear matrix
clear mata
* Set maximum number of variables
set maxvar 30000
* Set working directory
cd "C:\Chanel\NYU\Data Analysis\FF_Data"
* Load data set 
use "FF_week9.dta"

* Weighted summary statistics when restricting to complete cases
sum m4abvgh m2abvgh m4stredepr m4edunew m2medicaid m2insurance m2age m2married m3earnings [aw=m1natwt] if m4abvgh!=.&m3abvgh!=.&m4stredepr!=.&m4edunew!=.&m2medicaid!=.&m2insurance!=.&m2age!=.&m2married!=.&cm1ethrace!=.&lnm3earnings!=.&m3edudegree!=. 
tab m3edudegree [aw=m1natwt] if m4abvgh!=.&m3abvgh!=.&m4stredepr!=.&m4edunew!=.&m2medicaid!=.&m2insurance!=.&m2age!=.&m2married!=.&cm1ethrace!=.&lnm3earnings!=.&m3edudegree!=. 
tab cm1ethrace  [aw=m1natwt] if m4abvgh!=.&m3abvgh!=.&m4stredepr!=.&m4edunew!=.&m2medicaid!=.&m2insurance!=.&m2age!=.&m2married!=.&cm1ethrace!=.&lnm3earnings!=.&m3edudegree!=. 
outsum m4abvgh m3abvgh m4edunew m2medicaid m2insurance m2age m2married cm1ethrace lnm3earnings [aw=m1natwt] if m4abvgh!=.&m3abvgh!=.&m4stredepr!=.&m4edunew!=.&m2medicaid!=.&m2insurance!=.&m2age!=.&m2married!=.&cm1ethrace!=.&lnm3earnings!=.&m3edudegree!=. using firstdraft.txt, bracket title("Table 1: Descriptive Statistics") ctitle("Mean and [SD]") 

* Fit model controlling for Y1 covariates
logit m4abvgh i.m4edunew i.m3edudegree c.m2age i.m2married c.lnm3earnings i.m2medicaid i.m2insurance i.mblack##i.m3edudegree i.mblack##i.m4edunew, nolog r
est store m1
outreg2 using 1.doc, replace

logit m4abvgh i.m4edunew i.m3edudegree c.m2age i.m2married c.lnm3earnings i.m2medicaid i.m2insurance i.mblack##i.m3edudegree, nolog r
est store m2
outreg2 using 1.doc

logit m4abvgh i.m4edunew i.m3edudegree m3abvgh c.m2age i.m2married c.lnm3earnings i.m2medicaid i.m2insurance i.mblack##i.m3edudegree i.mblack##i.m4edunew, nolog r
est store m3
outreg2 using 1.doc

* Fit model controlling for Y3 covariates
* Recoding
* Code mother's age at Year 3
tab cm3age
gen m3age=cm3age
replace m3age=. if cm3age<0
lab var m3age "Year 3: Mother's age"
tab m3age

* Code mother's marital status at Year 3
tab cm3marf // Constructed - Is mother married to child's father at year 3
tab cm3marp // Constructed - Mother married to partner other than father at year 3
recode cm3marf (-9=.) (-3=.), gen(m3married)
lab var m3married "Year 3: Is mother married"
lab val m3married yesno
tab m3married

* For mothers who were married to their current partners rather than fathers at Y3, change m3married to 1
replace m3married=1 if cm3marp==1
tab m3married
sum m3married


logit m4abvgh i.m4edunew i.m3edudegree c.m3age i.m3married c.lnm3earnings i.m3medicaid i.m3insurance i.mblack##i.m3edudegree i.mblack##i.m4edunew, nolog r
est store m4
outreg2 using 1.doc, replace

logit m4abvgh i.m4edunew i.m3edudegree c.m3age i.m3married c.lnm3earnings i.m3medicaid i.m3insurance i.mblack##i.m3edudegree, nolog r
est store m5
outreg2 using 1.doc

logit m4abvgh i.m4edunew i.m3edudegree m3abvgh c.m3age i.m3married c.lnm3earnings i.m3medicaid i.m3insurance i.mblack##i.m3edudegree i.mblack##i.m4edunew, nolog r
est store m6
outreg2 using 1.doc


* Education as a predictor of income
* Generate an indicator of change in education from Y1 to Y3
tab m3k3
recode m3k3 (1=1) (2=0) (-9=.) (-2=.) (-1=.), gen(m3edunew)
lab var m3edunew "Year 3: Has mother completed any years of schooling or training programs since child's first birthday?"
lab val m3edunew yesno
tab m3edunew

drop lnm4earnings
tab m4k20 // 1503
replace m4earnings=2500 if m4k20==1
replace m4earnings=7500 if m4k20==2
replace m4earnings=12500 if m4k20==3
replace m4earnings=17500 if m4k20==4
replace m4earnings=22500 if m4k20==5
replace m4earnings=27500 if m4k20==6
replace m4earnings=35000 if m4k20==7
replace m4earnings=50000 if m4k20==8
replace m4earnings=80000 if m4k20==9
tab m4earnings // 4013

* Take log of earnings
gen lnm4earnings=log(m4earnings+1)
lab var lnm4earnings "Year 5: Mother's earnings from regular job in the last 12 months (after log transformation)"
tab lnm4earnings


reg lnm4earnings i.m4edunew i.m3edudegree c.m3age i.m3married i.mblack##i.m3edudegree i.mblack##i.m4edunew, r
est store m7
outreg2 using 1.doc, replace

reg lnm4earnings i.m4edunew c.lnm3earnings i.m3edudegree c.m3age i.m3married i.mblack##i.m3edudegree i.mblack##i.m4edunew, r
est store m8
outreg2 using 1.doc


* Fit model using categorical race
logit m4abvgh i.m4edunew i.m3edudegree c.m2age i.m2married c.lnm3earnings i.m2medicaid i.m2insurance ib1.cm1ethrace##i.m3edudegree ib1.cm1ethrace##i.m4edunew, nolog r
est store m9
outreg2 using 2.doc, replace

logit m4abvgh i.m4edunew i.m3edudegree c.m2age i.m2married c.lnm3earnings i.m2medicaid i.m2insurance ib1.cm1ethrace##i.m3edudegree, nolog r
est store m10
outreg2 using 1.doc

logit m4abvgh i.m4edunew i.m3edudegree i.m3abvgh c.m2age i.m2married c.lnm3earnings i.m2medicaid i.m2insurance ib1.cm1ethrace##i.m3edudegree ib1.cm1ethrace##i.m4edunew, nolog r
est store m11
outreg2 using 2.doc

* Construct measure of mental stress/depression
tab m4j5 // During past year, been time you felt sad/depressed for 2+ weeks in row?
recode m4j5 (2=0) (-9 -2 -1=.) (-14=1), gen(m4stredepr)
lab var m4stredepr "Y5: During past year, has there been time mother felt sad/depressed for 2+ weeks in row?"
lab val m4stredepr yesno
tab m4stredepr

tab m3j5 // In past year, have you felt sad/depressed for 2 or more weeks in a row?
recode m3j5 (2=0) (-9 -2 -1=.) (-14=1), gen(m3stredepr)
lab var m3stredepr "Y3: During past year, has there been time mother felt sad/depressed for 2+ weeks in row?"
lab val m3stredepr yesno
tab m3stredepr


* Construct measure of life satisfaction
tab m4j0
recode m4j0 (1=4) (2=3) (3=2) (4=1) (-9 -1=.), gen(m4satisfied)
lab var m4satisfied "Y5: Mother's overall life satisfaction"
lab def satisfaction 4 "4 Very satisfied" 3 "3 Somewhat satisfied" 2 "2 Somewhat dissatisfied" 1 "1 Very dissatisfied"
lab val m4satisfied satisfaction
tab m4satisfied


logit m4abvgh i.m4edunew i.m3edudegree i.m4stredepr i.m3abvgh c.m2age i.m2married c.lnm3earnings i.m2medicaid i.m2insurance ib1.cm1ethrace##i.m3edudegree ib1.cm1ethrace##i.m4edunew, nolog r
est store m12
outreg2 using 2.doc

logit m4abvgh i.m4edunew i.m3edudegree i.m4satisfied i.m3abvgh c.m2age i.m2married c.lnm3earnings i.m2medicaid i.m2insurance ib1.cm1ethrace##i.m3edudegree ib1.cm1ethrace##i.m4edunew, nolog r
est store m13
outreg2 using 1.doc


* Depression as an outcome
logit m4stredepr i.m4edunew i.m3edudegree i.m3abvgh c.m2age i.m2married i.cm1ethrace c.lnm3earnings, r

logit m4stredepr i.m4edunew i.m3edudegree i.m3stredepr i.m3abvgh c.m2age i.m2married i.cm1ethrace c.lnm3earnings, r
est store m14
outreg2 using 2.doc, replace


* Including the interaction of change in education and education
logit m4abvgh i.m4edunew i.m3edudegree i.m4stredepr i.m3abvgh c.m2age i.m2married c.lnm3earnings i.m2medicaid i.m2insurance ib1.cm1ethrace##i.m3edudegree ib1.cm1ethrace##i.m4edunew i.m3edudegree#i.m4edunew, nolog r
est store m15
outreg2 using 2.doc


* Create dummy for Hispanic
tab cm1ethrace
gen mhispanic=.
replace mhispanic=1 if cm1ethrace==3
replace mhispanic=0 if cm1ethrace!=3&cm1ethrace!=.
lab var mhispanic "Y2012: Is mother Hispanic"
lab val mhispanic yesno
tab mhispanic

logit m4abvgh i.m4edunew i.m3edudegree i.m4stredepr i.m3abvgh c.m2age i.m2married c.lnm3earnings i.m2medicaid i.m2insurance i.mhispanic##i.m3edudegree i.mhispanic##i.m4edunew i.m3edudegree#i.m4edunew, nolog r
est store m15
outreg2 using 1.doc

sum m4abvgh m2abvgh m3edunew m2medicaid m2insurance m2age m2married cm1ethrace lnm2earnings if m4abvgh!=.&m2abvgh!=.&m3workstress_o!=.&m3edunew!=.&m2medicaid!=.&m2insurance!=.&m2age!=.&m2married!=.&cm1ethrace!=.&lnm2earnings!=.&m2edudegree!=. 
sum m4abvgh m2abvgh m3edunew m2medicaid m2insurance m2age m2married cm1ethrace lnm2earnings [aw=m1natwt] if m4abvgh!=.&m2abvgh!=.&m3workstress_o!=.&m3edunew!=.&m2medicaid!=.&m2insurance!=.&m2age!=.&m2married!=.&cm1ethrace!=.&lnm2earnings!=.&m2edudegree!=. 

* Weighted summary statistics when restricting to specific education group
sum m4abvgh m2abvgh m3edunew m2medicaid m2insurance m2age m2married cm1ethrace lnm2earnings [aw=m1natwt] if m3edunew==1&m4abvgh!=.&m2abvgh!=.&m3workstress_o!=.&m3edunew!=.&m2medicaid!=.&m2insurance!=.&m2age!=.&m2married!=.&cm1ethrace!=.&lnm2earnings!=.&m2edudegree!=. 
sum m4abvgh m2abvgh m3edunew m2medicaid m2insurance m2age m2married cm1ethrace lnm2earnings [aw=m1natwt] if m3edunew==0&m4abvgh!=.&m2abvgh!=.&m3workstress_o!=.&m3edunew!=.&m2medicaid!=.&m2insurance!=.&m2age!=.&m2married!=.&cm1ethrace!=.&lnm2earnings!=.&m2edudegree!=. 

tab m2edudegree [aw=m1natwt] if m4abvgh!=.&m2abvgh!=.&m3workstress_o!=.&m3edunew!=.&m2medicaid!=.&m2insurance!=.&m2age!=.&m2married!=.&cm1ethrace!=.&lnm2earnings!=.&m2edudegree!=. 
tab cm1ethrace [aw=m1natwt] if m4abvgh!=.&m2abvgh!=.&m3workstress_o!=.&m3edunew!=.&m2medicaid!=.&m2insurance!=.&m2age!=.&m2married!=.&cm1ethrace!=.&lnm2earnings!=.&m2edudegree!=. 
tab m3workstress_o [aw=m1natwt] if m4abvgh!=.&m2abvgh!=.&m3workstress_o!=.&m3edunew!=.&m2medicaid!=.&m2insurance!=.&m2age!=.&m2married!=.&cm1ethrace!=.&lnm2earnings!=.&m2edudegree!=. 

tab m2edudegree [aw=m1natwt] if m3edunew==1&m4abvgh!=.&m2abvgh!=.&m3workstress_o!=.&m3edunew!=.&m2medicaid!=.&m2insurance!=.&m2age!=.&m2married!=.&cm1ethrace!=.&lnm2earnings!=.&m2edudegree!=. 
tab cm1ethrace [aw=m1natwt] if m3edunew==1&m4abvgh!=.&m2abvgh!=.&m3workstress_o!=.&m3edunew!=.&m2medicaid!=.&m2insurance!=.&m2age!=.&m2married!=.&cm1ethrace!=.&lnm2earnings!=.&m2edudegree!=. 
tab m3workstress_o [aw=m1natwt] if m3edunew==1&m4abvgh!=.&m2abvgh!=.&m3workstress_o!=.&m3edunew!=.&m2medicaid!=.&m2insurance!=.&m2age!=.&m2married!=.&cm1ethrace!=.&lnm2earnings!=.&m2edudegree!=. 

tab m2edudegree [aw=m1natwt] if m3edunew==0&m4abvgh!=.&m2abvgh!=.&m3workstress_o!=.&m3edunew!=.&m2medicaid!=.&m2insurance!=.&m2age!=.&m2married!=.&cm1ethrace!=.&lnm2earnings!=.&m2edudegree!=. 
tab cm1ethrace [aw=m1natwt] if m3edunew==0&m4abvgh!=.&m2abvgh!=.&m3workstress_o!=.&m3edunew!=.&m2medicaid!=.&m2insurance!=.&m2age!=.&m2married!=.&cm1ethrace!=.&lnm2earnings!=.&m2edudegree!=. 
tab m3workstress_o [aw=m1natwt] if m3edunew==0&m4abvgh!=.&m2abvgh!=.&m3workstress_o!=.&m3edunew!=.&m2medicaid!=.&m2insurance!=.&m2age!=.&m2married!=.&cm1ethrace!=.&lnm2earnings!=.&m2edudegree!=. 

tab m3edunew [aw=m1natwt] if analsamp==1

* Unweighted summary statistics when restricting to specific education group
sum m4abvgh m2abvgh m3edunew m2medicaid m2insurance m2age m2married cm1ethrace lnm2earnings if m3edunew==1&m4abvgh!=.&m2abvgh!=.&m3workstress_o!=.&m3edunew!=.&m2medicaid!=.&m2insurance!=.&m2age!=.&m2married!=.&cm1ethrace!=.&lnm2earnings!=.&m2edudegree!=. 
sum m4abvgh m2abvgh m3edunew m2medicaid m2insurance m2age m2married cm1ethrace lnm2earnings if m3edunew==0&m4abvgh!=.&m2abvgh!=.&m3workstress_o!=.&m3edunew!=.&m2medicaid!=.&m2insurance!=.&m2age!=.&m2married!=.&cm1ethrace!=.&lnm2earnings!=.&m2edudegree!=. 

tab m2edudegree if m4abvgh!=.&m2abvgh!=.&m3workstress_o!=.&m3edunew!=.&m2medicaid!=.&m2insurance!=.&m2age!=.&m2married!=.&cm1ethrace!=.&lnm2earnings!=.&m2edudegree!=. 
tab cm1ethrace if m4abvgh!=.&m2abvgh!=.&m3workstress_o!=.&m3edunew!=.&m2medicaid!=.&m2insurance!=.&m2age!=.&m2married!=.&cm1ethrace!=.&lnm2earnings!=.&m2edudegree!=. 
tab m3workstress_o if m4abvgh!=.&m2abvgh!=.&m3workstress_o!=.&m3edunew!=.&m2medicaid!=.&m2insurance!=.&m2age!=.&m2married!=.&cm1ethrace!=.&lnm2earnings!=.&m2edudegree!=. 

tab m2edudegree if m3edunew==1&m4abvgh!=.&m2abvgh!=.&m3workstress_o!=.&m3edunew!=.&m2medicaid!=.&m2insurance!=.&m2age!=.&m2married!=.&cm1ethrace!=.&lnm2earnings!=.&m2edudegree!=. 
tab cm1ethrace if m3edunew==1&m4abvgh!=.&m2abvgh!=.&m3workstress_o!=.&m3edunew!=.&m2medicaid!=.&m2insurance!=.&m2age!=.&m2married!=.&cm1ethrace!=.&lnm2earnings!=.&m2edudegree!=. 
tab m3workstress_o if m3edunew==1&m4abvgh!=.&m2abvgh!=.&m3workstress_o!=.&m3edunew!=.&m2medicaid!=.&m2insurance!=.&m2age!=.&m2married!=.&cm1ethrace!=.&lnm2earnings!=.&m2edudegree!=. 

tab m2edudegree if m3edunew==0&m4abvgh!=.&m2abvgh!=.&m3workstress_o!=.&m3edunew!=.&m2medicaid!=.&m2insurance!=.&m2age!=.&m2married!=.&cm1ethrace!=.&lnm2earnings!=.&m2edudegree!=. 
tab cm1ethrace if m3edunew==0&m4abvgh!=.&m2abvgh!=.&m3workstress_o!=.&m3edunew!=.&m2medicaid!=.&m2insurance!=.&m2age!=.&m2married!=.&cm1ethrace!=.&lnm2earnings!=.&m2edudegree!=. 
tab m3workstress_o if m3edunew==0&m4abvgh!=.&m2abvgh!=.&m3workstress_o!=.&m3edunew!=.&m2medicaid!=.&m2insurance!=.&m2age!=.&m2married!=.&cm1ethrace!=.&lnm2earnings!=.&m2edudegree!=. 

* Not restricting to complete cases
sum m4abvgh m2abvgh m3edunew m2medicaid m2insurance m2age m2married cm1ethrace lnm2earnings 
sum m4abvgh m2abvgh m3edunew m2medicaid m2insurance m2age m2married cm1ethrace lnm2earnings if m3edunew==1
sum m4abvgh m2abvgh m3edunew m2medicaid m2insurance m2age m2married cm1ethrace lnm2earnings if m3edunew==0

tab m2edudegree 
tab cm1ethrace
tab m3workstress_o

tab m2edudegree if m3edunew==1
tab cm1ethrace if m3edunew==1
tab m3workstress_o if m3edunew==1

tab m2edudegree if m3edunew==0
tab cm1ethrace if m3edunew==0
tab m3workstress_o if m3edunew==0


tab m4k16a
recode m4k16a (1 2 3=1) (4 -6=0) (-9 -2 -1=.), gen(m4workstress)
lab var m4workstress "Y5: Mother's shift & work schedule causes extra stress for her & child"
lab val m4workstress yesno
tab m4workstress

tab m4k16a
recode m4k16a (1=3) (3=1) (4 -6=0) (-9 -2 -1=.), gen(m4workstress_o)
lab var m4workstress_o "Y5: Mother's shift & work schedule causes extra stress for her & child (ordinal)"
lab def frequency 1 "1 Sometimes" 2 "2 Often" 3 "3 Always"
lab val m4workstress_o frequency
tab m4workstress_o

logit m4abvgh i.m4edunew i.m3edudegree i.m4workstress_o i.m3abvgh c.m2age i.m2married c.lnm3earnings i.m2medicaid i.m2insurance ib1.cm1ethrace##i.m3edudegree ib1.cm1ethrace##i.m4edunew i.m3edudegree#i.m4edunew, nolog r
est store m16
outreg2 using 1.doc


* Work stress
logit m4abvgh i.m4edunew i.m3edudegree c.m2age i.m2married c.lnm3earnings i.m2medicaid i.m2insurance ib1.cm1ethrace##i.m3edudegree ib1.cm1ethrace##i.m4edunew, nolog r
est store m17
outreg2 using 3.doc, replace

logit m4abvgh i.m4edunew i.m3edudegree i.m3abvgh c.m2age i.m2married c.lnm3earnings i.m2medicaid i.m2insurance ib1.cm1ethrace##i.m3edudegree ib1.cm1ethrace##i.m4edunew, nolog r
est store m18
outreg2 using 3.doc

logit m4abvgh i.m4edunew i.m3edudegree i.m4workstress_o i.m3abvgh c.m2age i.m2married c.lnm3earnings i.m2medicaid i.m2insurance ib1.cm1ethrace##i.m3edudegree ib1.cm1ethrace##i.m4edunew, nolog r
est store m19
outreg2 using 3.doc

logit m4abvgh i.m4edunew i.m3edudegree i.m4workstress_o i.m3abvgh c.m2age i.m2married c.lnm3earnings i.m2medicaid i.m2insurance ib1.cm1ethrace##i.m3edudegree ib1.cm1ethrace##i.m4edunew i.m3edudegree#i.m4edunew, nolog r
est store m20
outreg2 using 3.doc

* Generate an indicator of change in education from Y1 to Y3
tab m3k3 // Have you completed training programs/school since child's first birthday?
recode m3k3 (1=1) (2=0) (-9=.) (-2=.) (-1=.), gen(m3edunew)
lab var m3edunew "Year 3: Has mother completed any years of schooling or training programs since Y1?"
lab val m3edunew yesno
tab m3edunew

* Recode mother's Year 1 subject physical health
tab m2subhealth
recode m2subhealth (5 4=1) (3 2 1=0), gen(m2abvgh)
lab var m2abvgh "Year 1: Does mother report very good or excellent health"
lab val m2abvgh yesno
tab m2abvgh

tab m2k12
tab m2k13a
recode m2k13a (1=3) (3=1) (4 -6=0) (-9 -2 -1=.), gen(m2workstress_o)
lab var m2workstress_o "Y1: Mother's shift & work schedule causes extra stress for her & child (ordinal)"
lab val m2workstress_o frequency
tab m2workstress_o

tab m3k16a
recode m3k16a (1=3) (3=1) (4 -6=0) (-9 -2 -1=.), gen(m3workstress_o)
lab var m3workstress_o "Y3: Mother's shift & work schedule causes extra stress for her & child (ordinal)"
lab val m3workstress_o frequency
tab m3workstress_o


* Y1 earnings
tab m2k8c // HAS RESPONDENT EVER WORKED FOR TWO CONSECUTIVE WEEKS?
tab m2k15 // 4009 skipped, How much did you earn total from all on-the-books jobs in past year?
tab m2k15a // How much did you earn, last year, from all on-books jobs?-($ Range)
tab m2k12 // 927 mothers hadn't worked since child was born, so they skipped m2k15 
tab m2k14
tab m2k10a
gen m2earnings=.
replace m2earnings=m2k10a*8*52*5 if m2k10ap==1
replace m2earnings=m2k10a*52 if m2k10ap==3
replace m2earnings=m2k10a*26 if m2k10ap==4
replace m2earnings=m2k10a*12 if m2k10ap==5
replace m2earnings=m2k10a if m2k10ap==6
replace m2earnings=m2k10a*52*5 if m2k10ap==2
replace m2earnings=m2k10a*6 if m2k10ap==101
replace m2earnings=m2k10a*52/3 if m2k10ap==103
replace m2earnings=m2k10a*2 if m2k10ap==104
replace m2earnings=m2k10a*3 if m2k10ap==105
replace m2earnings=m2k10a*52*2 if m2k10ap==106
replace m2earnings=0 if m2k10a==-6
tab m2earnings

replace m2earnings=m2k15 if m2earnings==.&m2k15>=0
tab m2k15a if m2earnings==.&m2k15a>=0
replace m2earnings=2500 if m2earnings==.&m2k15a==1
replace m2earnings=7500 if m2earnings==.&m2k15a==2
replace m2earnings=12500 if m2earnings==.&m2k15a==3
replace m2earnings=22500 if m2earnings==.&m2k15a==5
tab m2earnings
lab var m2earnings "Year 1: Mother's earnings from regular job in the last 12 months"

* Take log of earnings
gen lnm2earnings=log(m2earnings+1)
lab var lnm2earnings "Year 1: Mother's earnings from regular job in the last 12 months (after log transformation)"
tab lnm2earnings


* Y3 work stress
* Model 0
logit m4abvgh i.m3edunew i.m2edudegree c.m2age i.m2married c.lnm2earnings i.m2medicaid i.m2insurance ib1.cm1ethrace##i.m2edudegree ib1.cm1ethrace##i.m3edunew if analsamp==1, nolog r
est store m21
outreg2 using 4.doc, replace

* Model 1
logit m4abvgh i.m3edunew i.m2edudegree i.m2abvgh c.m2age i.m2married c.lnm2earnings i.m2medicaid i.m2insurance ib1.cm1ethrace##i.m2edudegree ib1.cm1ethrace##i.m3edunew if analsamp==1, nolog r
est store m22
outreg2 using 5.doc

* Model 2
logit m4abvgh i.m3edunew i.m2edudegree i.m3workstress_o i.m2abvgh c.m2age i.m2married c.lnm2earnings i.m2medicaid i.m2insurance ib1.cm1ethrace##i.m2edudegree ib1.cm1ethrace##i.m3edunew if analsamp==1, nolog r
est store m23
outreg2 using 4.doc

* Model 3
logit m4abvgh i.m3edunew i.m2edudegree i.m3workstress_o i.m2abvgh c.m2age i.m2married c.lnm2earnings i.m2medicaid i.m2insurance ib1.cm1ethrace##i.m2edudegree ib1.cm1ethrace##i.m3edunew i.m2edudegree#i.m3edunew if analsamp==1, nolog r
est store m24
outreg2 using 4.doc

logit m4abvgh i.m3edunew i.m2edudegree i.m3workstress_o i.m2abvgh c.m2age i.m2married c.lnm2earnings i.m2medicaid i.m2insurance ib1.cm1ethrace##i.m2edudegree ib1.cm1ethrace##i.m3edunew i.m2edudegree#i.m3edunew if analsamp==1, nolog 

reg m4abvgh i.m3edunew i.m2edudegree i.m3workstress_o i.m2abvgh c.m2age i.m2married c.lnm2earnings i.m2medicaid i.m2insurance ib1.cm1ethrace##i.m2edudegree ib1.cm1ethrace##i.m3edunew i.m2edudegree#i.m3edunew if analsamp==1, r
estat vif


gen analsamp=(m4abvgh!=.)&(m2abvgh!=.)&(m3workstress_o!=.)&(m3edunew!=.)&(m2medicaid!=.)&(m2insurance!=.)&(m2age!=.)&(m2married!=.)&(cm1ethrace!=.)&(lnm2earnings!=.)&(m2edudegree!=.)
tab analsamp, m

* Revising model so that the model with mediator is the last model 
* Model 1
logit m4abvgh i.m3edunew i.m2edudegree i.m2abvgh c.m2age i.m2married c.lnm2earnings i.m2medicaid i.m2insurance ib1.cm1ethrace##i.m2edudegree ib1.cm1ethrace##i.m3edunew if analsamp==1, nolog r
est store m
margins, dydx(m3edunew) post
est sto m1
forval i = 3/6 {
est res m
margins, dydx(*) pr(out(`i')) post
est sto m`i'
}
estout m1 m3 m4 m5 m6, cells(b se)


quietly margins, dydx(edu12 training12yes cert12yes forlan12) pr(out(1)) post
est sto m1
forval i = 3/6 {
est res m
quietly margins, dydx(edu12 training12yes cert12yes forlan12) pr(out(`i')) post
est sto m`i'
} 


outreg2 using 4.doc, replace

* Model 2
logit m4abvgh i.m3edunew i.m2edudegree i.m2abvgh c.m2age i.m2married c.lnm2earnings i.m2medicaid i.m2insurance ib1.cm1ethrace##i.m2edudegree ib1.cm1ethrace##i.m3edunew i.m2edudegree#i.m3edunew if analsamp==1, nolog r
est store m33
outreg2 using 4.doc

* Model 3
logit m4abvgh i.m3edunew i.m2edudegree i.m3workstress_o i.m2abvgh c.m2age i.m2married c.lnm2earnings i.m2medicaid i.m2insurance ib1.cm1ethrace##i.m2edudegree ib1.cm1ethrace##i.m3edunew i.m2edudegree#i.m3edunew if analsamp==1, nolog r
est store m34
outreg2 using 4.doc


sum m4abvgh m2abvgh m3edunew m2medicaid m2insurance m2age m2married cm1ethrace lnm2earnings [aw=m1natwt] if analsamp==1
tab m3edunew [aw=m1natwt] if m3edunew==1&analsamp==1
tab m3edunew [aw=m1natwt] if analsamp==1
tab m3edunew [aw=m1natwt] if m3edunew==0&analsamp==1
tab m2edudegree [aw=m1natwt] if m3edunew==1&analsamp==1
tab cm1ethrace [aw=m1natwt] if m3edunew==1&analsamp==1
tab m3workstress_o [aw=m1natwt] if m3edunew==1&analsamp==1
