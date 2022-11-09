global input D:\PEEA_2021_2\2. Variables Instrumentales\Aplicacion\1. Input
global output D:\PEEA_2021_2\2. Variables Instrumentales\Aplicacion\3. Output

cd "D:\PEEA_2021_2\2. Variables Instrumentales\Aplicacion\2. Temp"

**************************************************************************************************************************
**************************************************Creación de Variables***************************************************
**************************************************************************************************************************
use "$input\enaho01a-2020-500.dta", clear
**Limitaciones: Ocupación principal con ingresos regulares como dependiente 
***nacidos después de 1960, edad en lugar de años de experiencia.
drop if p524e1==.
keep conglome vivienda hogar codperso ubigeo dominio estrato p513* p524e1 p523 p208a p207 p209 ocupinf emplpsec fac500a

tab p523
tab p523, nolab

**Variable Dependiente: Logaritmo del Salario
gen hprom=(p513a+p513b+p513c+p513d+p513e+p513f+p513g)/7
gen salth=(p524e1/hprom) if p523==1
replace salth=((p524e1/7)/hprom) if p523==2
replace salth=((p524e1/15)/hprom) if p523==3
replace salth=((p524e1/30)/hprom) if p523==4

gen lsalth=log(salth)
label variable lsalth "Logaritmo del salario por hora"

drop p513*

**Variables independientes: Edad
rename p208a edad
gen edad2=edad*edad

**Variables independientes: Estado Civil
recode p209 (1/2=1 "Casado o conviviente") (4/.=0 "Otro"), gen("matrim")
drop p209
label variable matrim "Estado civil Casado o conviviente"

**Variables independientes: Sexo
recode p207 (1=0 "Varon") (2=1 "Mujer"), gen(sexo)
drop p207

**Variables Independientes: Pertenencia a Lima
recode dominio (1/7=0 "Costa Sierra Selva") (8=1 "Lima Metropolitana"), gen(lima)
recode dominio (1/3=2 "Costa") (4/6=3 "Sierra") (7=4 "Selva") (8=1 "Lima Metropolitana"), gen(dom)
drop dominio

label variable lima "Pertenencia a Lima Metropolitana"
label variable dom "Dominio"

save empleo_2020.dta, replace

**Variables Independientes: Años de Educación
use "$input\enaho01a-2020-300.dta", clear
keep conglome vivienda hogar codperso p301a p301b p302 p301d p311t1

gen educ=0 if p301a==1
replace educ=p301b if p301a==2
replace educ=3+p301b if p301a==3
replace educ=9 if p301a==4
replace educ=9+p301b if p301a==5
replace educ=14 if p301a==6
replace educ=14+p301b if p301a==7 | p301a==8 | p301a==9 | p301a==10
replace educ=20+p301b if p301a==11
replace educ=p301b if p301a==12

drop p301a p301b
sort conglome vivienda codperso

save educacion_2020, replace

use empleo_2020, clear
sort conglome vivienda codperso

merge n:n conglome vivienda codperso using educacion_2020
drop if _merge==1 | _merge==2
drop _merge

save empleo_educacion_2020, replace

**Variables Instrumentales: PBI per capita
import excel using "$input\PBI_PC.xlsx", clear firstrow
rename periodo nac
sort nac
save PBI_PC, replace

use empleo_educacion_2020, clear

gen nac=2020-edad

drop if nac<=1960

sort nac
merge n:n nac using PBI_PC
drop if _merge==1 | _merge==2
drop _merge

save "$output\data_final_2020.dta", replace

**************************************************************************************************************************
**************************************************Estimación del Modelo***************************************************
**************************************************************************************************************************

use "$output\data_final_2020.dta", clear

***El Modelo de Mincer podría presentar exogeneidad, dado que el error (habilidades o motivación) podría estar correlacionado
***con los años de educación y con el nivel de ingresos.

***El autor propone tres métodos para instrumentalizar la variable educ
***1. Nivel educativo de los padres.
***2. Experimentos naturales (trimestre de nacimiento).
***3. Variables macroeconomicas (preferido por el autor).

***Se puede concluir con el test de Sargan, que los instrumentos son fuertes, y con el test de Hausman, que la variable educ presenta endogeneidad.

ivreg2 lsalth edad edad2 matrim sexo lima (educ = edad edad2 matrim sexo lima pbipc_nac pbipc_6 pbipc_11)
ivendog