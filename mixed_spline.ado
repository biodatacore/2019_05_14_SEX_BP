capture program drop mixed_spline
program mixed_spline
	preserve

	set more off
	syntax varlist [if] , [Knots(numlist max=1) hist log NOcis Trim(numlist max=2) bin(numlist max=1) id(varname) by(varname) breakties path(string) savename(string)]
	if "`if'" != "" keep `if'

	


	****Make sure all variables names are clear****
	capture drop outvar
	capture drop expos
	capture drop spvar
	capture drop spvarsp*
        capture drop lb*
	capture drop ub*
	capture drop se*
	capture drop pred*
	capture drop min*
	capture drop max*
	****Insert outcome variable here: echo parameter of interest****
	tokenize `varlist'
	local cov `2'
	local cov2 `1'
	macro shift
	macro shift
	local adjusters `*'
quietly	gen outvar=`cov2'

quietly inspect `by'
	if r(N_unique)==0 {
	quietly gen name_`by'=`by'
	quietly drop `by'
	quietly encode name_`by', gen(`by')
	quietly inspect `by'
	}

quietly sum `by'
quietly replace `by' = `by' - r(min)

	**chose number of knots; default is 5***
	
	if "`knots'"!="" {
			local nk `knots'
		} 
		else {
			local nk=5
		}


	***sort "adjusting" variables to identify categorical covariates***

	**list of variables to adjust for, listing continuous(normal) and binary variables 
	local nbvars
	**categorical covariates below
	local catvars  

if wordcount("`adjusters' ")==0{
gen Cons=1
local adjusters Cons
}


foreach var of varlist `adjusters'{
quietly inspect `var'
local Lev=r(N_unique)
	
	if r(N_unique)==0 {
	quietly gen name_`var'=`var'
	quietly drop `var'
	quietly encode name_`var', gen(`var')
	quietly inspect `var'
	local Lev=r(N_unique)
	}
		if `Lev'<=2{
			local nbvars `nbvars' `var'
					}
		if `Lev'>2&`Lev'<14{
			local catvars `catvars' `var'
					}					
		if `Lev'>=14{
			local nbvars `nbvars' `var'
					}
		}			
				

	***Spline variable****
quietly	gen spvar=`cov'

	if "`breakties'"!="" {
quietly  replace spvar=spvar+runiform()/1e12
	}
	

	if "`trim'"!="" {
	tokenize `trim'
	}
	else {
	local trim 1 99
	tokenize `trim'
	}
	
_pctile spvar, p(`1' `2')
	scalar max=r(r2)
	scalar min=r(r1)

if "`log'"!="" {
quietly gen scalevar=log(spvar)
}
else{
quietly gen scalevar=spvar
}


local nktemp = `nk'
if `nk'<3 {
local nktemp = 3
}
	mkspline sp_splinevar=scalevar, cubic displayknots nk(`nktemp')

if `nk'<=2 {
quietly drop sp_splinevar2
}	


	****UNADJUSTED ANALYSES START HERE*****

	****GLOBAL*****



	****ADJUSTED ANALYSES START HERE*****

if wordcount("`catvars' ")!=0{

	foreach var in `catvars'{
quietly	tab `var', generate(catcent`var')
quietly	drop catcent`var'1
	 }

	quietly ds catcent*
	 local varc `r(varlist)'

	foreach var in `varc'{
	quietly sum event if `var'==1
	if r(mean) <=0 {
quietly	drop `var'
	}
	}

quietly	 ds catcent*
quietly	 di "`r(varlist)'"
	 local varc `r(varlist)'

	local vars `nbvars' `varc'
}
 
 else{ 
 local vars `nbvars'
 }



	****GLOBAL*****
	foreach var in `vars'{
	quietly sum `var'
quietly	replace `var'=`var'-r(mean)
quietly	gen temp`var'=`var'
	}

quietly	ds temp*
quietly	di "`r(varlist)'"
	local tempvars `r(varlist)'




qui xtmixed outvar i.`by' c.(sp_splinevar*) `tempvars' if !missing(spvar) || `id':
est store a
qui xtmixed outvar c.(sp_splinevar*) `tempvars' if !missing(spvar)&!missing(`by') || `id':
est store b
xtmixed outvar i.`by'##c.(sp_splinevar*) `tempvars' if !missing(spvar) || `id':
est store c
display as text _newline(3)
display as result "Test for Equality" 
lrtest b c, force
display as text _newline(3)
display as result "Test for interaction" 
lrtest a c, force

estat ic
	foreach regvar in `tempvars'{
quietly	replace `regvar'=0
	}
quietly	predict pred if e(sample)
quietly	predict se if e(sample), stdp
quietly	gen lb=pred-1.96*se
quietly	gen ub=pred+1.96*se
quietly	drop temp*

qui levelsof `by'
foreach lev in `r(levels)' {
qui gen pred_`lev' = pred if `by'==`lev'
qui gen lb_`lev' = lb if `by'==`lev'
qui gen ub_`lev' = ub if `by'==`lev'
}

if "`nocis'"!="" {
qui levelsof `by'
foreach lev in `r(levels)' {
qui replace lb_`lev' = .
qui replace ub_`lev' = .
}
}

cd `path'
outsheet spvar* pred* lb* ub* using `savename'.csv, comma nolabel replace
  
end


