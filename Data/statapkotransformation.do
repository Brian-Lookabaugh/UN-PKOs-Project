**Note that Stata was used to format this data due to the helpfulness of the expand function*

clear

use "C:\Users\brian\Desktop\Peacebuilding Dissertation\PKO\Data\rawmullenbachpkodata.dta"

keep if TPTYPE == 1

rename CCODE1 ccode

gen case=_n

gen duration = ENDYR - STARTYR + 1
expand duration

sort case
by case: gen time = _n

gen year = STARTYR + time - 1

sort ccode year
gen PKO = 1 

collapse (max) PKO, by(ccode year)
