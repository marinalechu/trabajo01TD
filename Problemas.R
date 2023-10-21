#Marina García Lechuga
#Trabajo01 TD

#####
#Problema 1: Aplicar todos los criterios de toma de decisiones bajo incertidumbre, en tablas separadas, donde los valores vienen recogidos por la siguiente matriz. Son 6 estados de la naturaleza y 3 situaciones.
#     e1  e2  e3  e4  e5  e6
# d1  8   3   5   2   0   0
# d2  5   4   10  9   8   1
# d3  12  11  3   7   5   1

source("teoriadecision_funciones_incertidumbre.R") #cargamos todas las funciones necesarias para resolver el problema

tabla1 = crea.tablaX(c(8,3,5,2,0,0,
                       5,4,10,9,8,1,
                       12,11,3,7,5,1), numalternativas = 3, numestados = 6)
tabla1

##Si la tabla fuera favorable:
#Criterio de Wald
cWaldF = criterio.Wald(tabla1, favorable = T)
cWaldF
cat("Para el criterio de Wald, las aleternativas óptimas serian", names(cWaldF$AlternativaOptima))

#Criterio Optimista
cOptF = criterio.Optimista(tabla1, favorable = T)
cOptF
cat("Para el criterio optimista, la alernativa óptima sería", names(cOptF$AlternativaOptima))

#Criterio de Hurwicz con alpha = 0.6
cHurF = criterio.Hurwicz(tabla1, alfa = 0.6, favorable = T)
dibuja.criterio.Hurwicz(tabla1) #Se ve graficamente cual es la decisión tomada
cHurF
cat("Para el criterio de Hurwica, ", names(cHurF$AlternativaOptima), " sería la alternativa óptima")

#Criterio de Savage
cSavF = criterio.Savage(tabla1, favorable = T)
cSavF
cat("Para el criterio de Savage, las aleternativas óptimas serian", names(cSavF$AlternativaOptima))

#Criterio de Laplace
cLapF = criterio.Laplace(tabla1, favorable = T)
cLapF
cat(names(cLapF$AlternativaOptima), "es la alternativa óptima según el criterio de Laplace")

#Criterio del punto ideal
cPIF = criterio.PuntoIdeal(tabla1, favorable = T)
cPIF
cat("La alternativa óptima por el criterio del punto ideal es ", names(cPIF$AlternativaOptima))

cat("Para esta matriz de datos y siendo favorable la alternativa escogida por todos los criterios ha sido la tercera.")


##Si la tabla fuera desfavorable:
#Criterio de Wald
cWaldnF = criterio.Wald(tabla1, favorable = F)
cWaldnF
cat("Para el criterio de Wald, las aleternativas óptimas serian", names(cWaldnF$AlternativaOptima))

#Criterio Optimista
cOptnF = criterio.Optimista(tabla1, favorable = F)
cOptnF
cat("Para el criterio optimista, la alernativa óptima sería", names(cOptnF$AlternativaOptima))

#Criterio de Hurwicz con alpha = 0.6
cHurnF = criterio.Hurwicz(tabla1, alfa = 0.6, favorable = F)
dibuja.criterio.Hurwicz(tabla1, favorable = F) #Se ve graficamente cual es la decisión tomada
cHurnF
cat("Para el criterio de Hurwica, ", names(cHurnF$AlternativaOptima), " sería la alternativa óptima")

#Criterio de Savage
cSavnF = criterio.Savage(tabla1, favorable = F)
cSavnF
cat("Para el criterio de Savage, las aleternativas óptimas serian", names(cSavnF$AlternativaOptima))

#Criterio de Laplace
cLapnF = criterio.Laplace(tabla1, favorable = F)
cLapnF
cat(names(cLapnF$AlternativaOptima), "es la alternativa óptima según el criterio de Laplace")

#Criterio del punto ideal
cPInF = criterio.PuntoIdeal(tabla1, favorable = F)
cPInF
cat("La alternativa óptima por el criterio del punto ideal es ", names(cPInF$AlternativaOptima))

cat("Para esta matriz de datos y siendo desfavorable, la alternativa escogida por todos los criterios ha sido la primera.")



#####
#Problema 2: Una almazara, cooperativa de aceite, tiene que decidir si abre para la cosecha de ese año o por el contrario la alquila. Esto dependerá de la posible cosecha y de los beneficios que se obtendrán de la venta de aceite. Se sabe que la producción de todos los años ronda las 200 toneladas de aceitunas, por lo tanto, los gastos de producción rondan los 15 céntimos por litro. A parte, se tienen unos gastos fijos de 15000€. Si se alquilase la almazara, los gastos fijos habría que pagarlos, sin embargo, los gastos de producción quedarían a cargo del arrendatario. 
#Si durante el año llueve y no hace viento, la producción de aceite es de 1 litro por cada kilo de aceitunas y el precio del litro al venderlo es de 1€. Si durante el año llueve bastante y hace viento, la producción de aceite es la misma que en el caso anterior pero el precio de venta aumenta a 1,50€ el litro. Por el contrario, si durante el año no llueve, la producción de aceite disminuye a por cada 3 kilos de aceitunas se hace un litro de aceite y el precio de venta de un litro de aceite aumenta hasta 2,50€.
#Por otra parte si se alquila se hace por 150000€ y la empresa produce y gana el 10% de la producción anual.
#Si se quieren maximizar los beneficios, ¿qué opción deberá elegir la empresa de la almazara?

#Introducir los datos:
#e1: llueve y no hace viento
#e2: llueve y hace viento
#e3: no llueve

#d1: producir el aceite
#d2: alquilar la almazara

m11 = (200000/1)*1 - 15000 - (200000*0.15)
m21 = 150000 - 15000 + ((200000/1)*1 - (200000*0.15))*0.1

m12 = (200000/1)*1.5 - 15000 - (200000*0.15)
m22 = 150000 - 15000 + ((200000/1)*1.5 - (200000*0.15))*0.1

m13 = (200000/3)*2.5 - 15000 - (200000*0.15)
m23 = 150000 - 15000 + ((200000/3)*2.5 - (200000*0.15))*0.1

#Resolución:
source("teoriadecision_funciones_incertidumbre.R") #cargamos todas las funciones necesarias para resolver el problema
tabla2 = crea.tablaX(c(m11,m12,m13,
                       m21,m22,m23), numalternativas = 2, numestados = 3)
tabla2

res2 = criterio.Todos(tabla2, 0.4, favorable = T)
res2
cat("Todos los criterios han elegido que la almazara produzca el aceite.")
