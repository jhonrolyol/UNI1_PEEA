********************************************************************************
**********Tratamiento de endogeneidad mediante variables instrumentales*********
********************************************************************************
cd "D:\PEEA_2021_2\2. Variables Instrumentales\1. Input"
use mroz, clear	/*Base de datos sobre empleo*/
reg hours lwage educ age kidslt6 kidsge6 nwifeinc
*findit ivreg2
***Regresión utilizando variables instrumentales e incluye test de sobreidentificación
ivreg2 hours educ age kidslt6 kidsge6 nwifeinc (lwage= exper expersq)
ivendog	/*Test de Endogeneidad de Hausman. Se puede verificar que la variable lwage presenta endogeneidad*/

**Una limitación del Test de Hausman es que compara modelos estimados, por lo cual no
**permite detectar endogeneidad ex ante. En este caso, si se desea evaluar anticipadamente
**las variables que presentan endogeneidad, será necesario conocer los instrumentos a usar:
reg lwage educ age kidslt6 kidsge6 nwifeinc exper expersq, notab
predict lwage_res, residual
reg hours lwage educ age kidslt6 kidsge6 nwifeinc lwage_res
test lwage_res

***A diferencia del caso anterior, ahora las horas de trabajo corresponderán
***a la variable dependiente
reg hours educ age kidslt6 kidsge6 nwifeinc exper expersq if lwage~=.
test age kidslt6 kidsge6 nwifeinc
ivreg2 lwage educ exper expersq (hours=educ age kidslt6 kidsge6 nwifeinc)
ivendog
reg lwage educ age kidslt6 kidsge6 nwifeinc exper expersq
test age kidslt6 kidsge6 nwifeinc

ivreg2 lwage hours exper expersq (educ=age kidslt6 kidsge6 nwifeinc)
ivendog


***Los instrumentos serán las variables independientes cuadráticas
gen lwagesq = lwage^2
gen agesq=age^2
gen educsq = educ^2
gen nwifeincsq = nwifeinc^2
ivreg2 hours educ age kidslt6 kidsge6 nwifeinc (lwage lwagesq = exper expersq educsq)
ivendog
