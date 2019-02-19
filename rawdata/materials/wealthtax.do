* Created Feb 2019 to create a single micro-dataset used for simulations that combines SCF, DINA, and Forbes


cap cd 		 "/Users/manu/Dropbox/"
global root "`c(pwd)'"
cd $root/bookwebsite/wealthtaxsim

global  dirwork  "$root/bookwebsite/wealthtaxsim"
global  datawork   "$root/bookwebsite/wealthtaxsim/data"

prog drop _all
discard
global ado_dir "$root/bookwebsite/wealthtaxsim/ado"
sysdir set PERSONAL "$ado_dir"

global yr=2019
global evade=.15

* Forbes 400 2018 data (Forbes data forbes_20112018_bdays.dta for years 2011-18 built by RAs)
use "$datawork/forbes_20112018_bdays.dta", clear
keep if forbes_yr==2018
gen networth=net_worthmillions*1e+6
gen weight=1
gen data="FB400"
keep weight data networth
order data networth weight
keep if networth!=.
sum networth [w=weight]
local forbesmin=r(min)
local f400tot=r(sum)*1e-12
display "TOTAL FORBES NETWORTH 2018 (Tr) = " `f400tot'  " FORBES MIN WEALTH 2018 = " `forbesmin'
save $datawork/fb400.dta, replace

* SCF 2016 data blown to 2019 (official rscfp2016.dta data downloaded from Fed website)
use "$datawork/rscfp2016.dta", clear
* increase wealth from 2016 to $yr=2019 (benchmarking on DINA aggregate household wealth of 2019 of $94.0Tr - Forbes 2.9tr)
sum networth [w=wgt] 
local totw=r(sum)*1e-12
display "TOTAL SCF NETWORTH 2016  (Tr) " `totw'
replace wgt=round(wgt*1.009^($yr-2016))
sum networth [w=wgt] 
local totw=r(sum)*1e-12
local totn=r(sum_w)
replace networth=networth*(94-`f400tot')/`totw'
gen weight=wgt
gen data="SCF"
keep weight data networth
order data networth weight
save $datawork/scf.dta, replace


* DINA 2019 (complete  data  usdina$yr.dta built in usdina project, directory usdina/output/dinafiles)

* use hweal id dweght married using $root/SaezZucman2014/usdina/output/dinafiles/usdina$yr.dta, clear 
* save "$root/bookwebsite/wealthtaxsim/data/usdina$yr.dta"

use "$datawork/usdina$yr.dta", clear
	// Collapse at tax unit level
	collapse (sum) hweal (mean) dweght married, by(id)
gen networth= hweal 
gen weight=dweght*1e-5	
sum networth [w=weight] 
local totw=r(sum)*1e-12
display "TOTAL DINA NETWORTH 2019  (Tr) " `totw'
local totn=r(sum_w)
gen data="DINA"
* average by groups of 5
gsort -networth
gen group=floor((_n+3)/5)
collapse (sum) weight (mean) networth, by(group)
sum networth [w=weight] 
gen data="DINA"
keep weight data networth
order data networth weight
save $datawork/dina.dta, replace

* combining the 3 datasets
use $datawork/fb400.dta, clear
append using $datawork/scf.dta
append using $datawork/dina.dta
replace weight=round(weight/2) if data=="SCF" | data=="DINA"
drop if networth>`forbesmin' & (data=="SCF" | data=="DINA")
sum networth [w=weight]
display r(sum)*1e-12
sum networth [w=weight] if networth>=1e+9
display r(sum)*1e-12
gsort -networth
save $datawork/wealth.dta, replace

* creating an excel table for the simulation
use $datawork/wealth.dta, clear
gperc networth [w=weight], matname(wealthperc)	
mat list wealthperc	
clear
svmat wealthperc, names(col)
qui compress
export excel using "$datawork/wealthperc.xlsx", first(var) replace




