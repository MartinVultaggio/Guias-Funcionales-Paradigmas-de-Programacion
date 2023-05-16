import Text.Show.Functions ()


--Guía Funcional 4

--1. Definir una función que sume una lista de números

suma :: [Int] -> Int
suma lista = sum lista

--2.Durante un entrenamiento físico de una hora, cada 10 minutos de entrenamiento se tomóo la frecuencia cardíaca de uno de los participantes obteniéndose un total de 7 muestras que son las siguientes:
frecuenciaCardiaca :: [Int]
frecuenciaCardiaca = [80, 100, 120, 128, 130, 123, 125] 
{-Comienza con un frecuencia de 80 min 0. 
A los 10 min la frecuencia alcanza los 100 
A los 20 min la frecuencia es de 120, 
A los 30 min la frecuencia es de 128
A los 40 min la frecuencia es de 130, …etc.. 
A los 60 min la frecuencia es de 125 frecuenciaCardiaca es un función constante. 
    a) Definir la función promedioFrecuenciaCardiaca, que devuelve el promedio de la frecuencia cardíaca. -}

promedioFrecuenciaCardiaca :: [Int]->Int
promedioFrecuenciaCardiaca lista = div (suma lista) (length lista)


{-
    b) Definir la función frecuenciaCardiacaMinuto/1, que recibe m que es el minuto en el cual quiero conocer la frecuencia cardíaca, m puede ser a los 10, 20, 30 ,40,..hasta 60. -}

frecuenciaCardiacaMinuto :: Int-> Int
frecuenciaCardiacaMinuto minuto
    |minuto < 10 = 80
    |(minuto >= 10) && (minuto < 20) = 100
    |(minuto >= 20) && (minuto < 30) = 120
    |(minuto >= 30) && (minuto < 40) = 128
    |(minuto >= 40) && (minuto < 50) = 130
    |(minuto >= 50) && (minuto < 60) = 123
    |otherwise = 125

    {-
    c) Definir la función frecuenciasHastaMomento/1, devuelve el total de frecuencias que se obtuvieron hasta el minuto m. -}

frecuenciasHastaMomento :: Int-> [Int]
frecuenciasHastaMomento minuto
    |minuto < 10 = [80]
    |(minuto >= 10) && (minuto < 20) = [80, 100]
    |(minuto >= 20) && (minuto < 30) = [80, 100, 120]
    |(minuto >= 30) && (minuto < 40) = [80, 100, 120, 128]
    |(minuto >= 40) && (minuto < 50) = [80, 100, 120, 128, 130] 
    |(minuto >= 50) && (minuto < 60) = [80, 100, 120, 128, 130, 123] 
    |otherwise = [80, 100, 120, 128, 130, 123, 125]

--3. Definir la función esCapicua/1, si data una lista de listas, me devuelve si la concatenación de las sublistas es una lista capicua

esCapicua :: [Int] -> Bool
esCapicua lista = lista == reverse lista

esCapicuaLaConcat :: [Int]->[Int]->Bool
esCapicuaLaConcat listaA listaB = esCapicua . (++) listaA $ listaB

--4. Se tiene información detallada de la duración en minutos de las llamadas que se llevaron a cabo en un período determinado, discriminadas en horario normal y horario reducido
--duracionLlamadas = (("horarioReducido",[20,10,25,15]),("horarioNormal",[10,5,8,2,9,10]))

{-
    a) Definir la función cuandoHabloMasMinutos, devuelve en que horario se habló más cantidad de minutos, en el de tarifa normal o en el reducido. 
    -}

type Informacion = ((String,[Int]),(String,[Int]))

cuandoHabloMasMinutos :: Informacion -> String
cuandoHabloMasMinutos ((_,duraciones1),(_,duraciones2))
    |(suma duraciones1) > (suma duraciones2) = "horarioReducido"
    |otherwise = "horarioNormal"

{-
    b) Definir la función cuandoHizoMasLlamadas, devuelve en que franja horaria realizó más cantidad de llamadas, en el de tarifa normal o en el reducido. 
-}

cuandoHizoMasLlamadas :: Informacion -> String
cuandoHizoMasLlamadas ((_,llamadas1),(_,llamadas2))
    |(length llamadas1) > (length llamadas2) = "horarioReducido"
    |otherwise = "horarioNormal"

--5. Definir la función existsAny/2, que dadas una función booleana y una tupla de tres elementos devuelve True si existe algún elemento de la tupla que haga verdadera la función

existsAny :: (Int->Bool)->(Int,Int,Int)->Bool
existsAny funcion (num1,num2,num3) = (funcion num1) || (funcion num2) || (funcion num3)

--6. Definir la función mejor/3, que recibe dos funciones y un número, y devuelve el resultado de la función que dé un valor más alto

mejorDe2 :: (Int->Int) -> (Int->Int) -> Int -> Int
mejorDe2 funcion1 funcion2 num = max (funcion1 num)(funcion2 num)

--7. Definir la función aplicarPar/2, que recibe una función y un par, y devuelve el par que resulta de aplicar la función a los elementos del par

aplicarPar :: (Int->Int)-> (Int,Int) -> (Int,Int)
aplicarPar funcion (num1,num2)= (funcion num1, funcion num2)

--8. Definir la función parDeFns/3, que recibe dos funciones y un valor, y devuelve un par ordenado que es el resultado de aplicar las dos funciones al valor

parDeFns :: (Int->Int) -> (Int->Int) -> Int -> (Int,Int)
parDeFns funcion1 funcion2 valor = (funcion1 valor, funcion2 valor)


--Ejercicios:

--1.Definir la función esMultiploDeAlguno/2, que recibe un número y una lista y devuelve True si el número es múltiplo de alguno de los números de la lista

esMultiploDe :: Int->Int->Bool
esMultiploDe num1 num2 = mod num1 num2 == 0

esMultiploDeAlguno :: Int->[Int]->Bool
esMultiploDeAlguno numero lista = any (esMultiploDe numero) lista

--2.Armar una función promedios/1, que dada una lista de listas me devuelve la lista de los promedios de cada lista-elemento

type Lista = [Int]

promedioLista :: Lista->Int
promedioLista lista = div (sum lista) (length lista) 

promedios :: [Lista]->Lista
promedios listaDeListas = map (promedioLista) listaDeListas

--3. Armar una función promediosSinAplazos que dada una lista de listas me devuelve la lista de los promedios de cada lista-elemento, excluyendo los que sean menores a 4 que no se cuentan.

promediosSinAplazos :: [Lista]->Lista
promediosSinAplazos listaDeListas = (filter (>=4) . map (promedioLista)) listaDeListas

--4. Definir la función mejoresNotas, que dada la información de un curso devuelve la lista con la mejor nota de cada alumno

mejoresNotas :: [Lista]->Lista
mejoresNotas listaDeListas = map (maximum) listaDeListas

--5. Definir la función aprobó/1, que dada la lista de las notas de un alumno devuelve True si el alumno aprobó. Se dice que un alumno aprobó si todas sus notas son 6 o más

aprobo :: Lista->Bool
aprobo lista = all (>=6) lista

--6. Definir la función aprobaron/1, que dada la información de un curso devuelve la información de los alumnos que aprobaron

aprobaron :: [Lista]->[Lista]
aprobaron listaDeListas = filter aprobo listaDeListas

--7. Definir la función divisores/1, que recibe un número y devuelve la lista de divisores.

divisoresDe :: Int->[Int]
divisoresDe num = [x | x <- [1..num], esMultiploDe num x]--generacion de listas

--8.Definir la función exists/2, que dadas una función booleana y una lista devuelve True si la función da True para algún elemento de la lista

exists :: (Int->Bool) -> Lista -> Bool
exists condicion lista = any condicion lista

--9. Definir la función hayAlgunNegativo/2, que dada una lista de números y un (…algo…) devuelve True si hay algún nro. negativo.

--hayAlgunNegativo :: Num a => ([a],a)->Bool
--hayAlgunNegativo (lista,_) = any (<0) lista

--10. Definir la función aplicarFunciones/2, que dadas una lista de funciones y un valor cualquiera, devuelve la lista del resultado de aplicar las funciones al valor.

aplicarFunciones :: [Int->Int]->Int->[Int]
aplicarFunciones funciones valor = map ($ valor) funciones

--11. Definir la función sumaF/2, que dadas una lista de funciones y un número, devuelve la suma del resultado de aplicar las funciones al número

sumaF :: [Int->Int]->Int->Int
sumaF funciones valor = (sum . map ($ valor)) funciones

--12.Un programador Haskell está haciendo las cuentas para un juego de fútbol virtual. En un momento le llega la información sobre la habilidad de cada jugador de un equipo, que es un número entre 0 y 12, y la orden de subir la forma de todos los jugadores en un número entero. Escribir una función subirHabilidad/2 que reciba un número (que se supone positivo sin validar) y una lista de números, y le suba la habilidad a cada jugador cuidando que ninguno se pase de 12.

subirHabilidad :: Int -> [Int] -> [Int]
subirHabilidad _ [] = []--no hace falta para consigna pero lo pongo sino salta
subirHabilidad valor (valorActual:valorSiguienteJugador)
    | valorActual + valor <= 12 = (valorActual + valor) : subirHabilidad valor valorSiguienteJugador
    | otherwise   = 12 : subirHabilidad valor valorSiguienteJugador

--13.Ahora el requerimiento es más genérico: hay que cambiar la habilidad de cada jugador según una función que recibe la vieja habilidad y devuelve la nueva. Armar: una función flimitada que recibe una función f y un número n, y devuelve f n garantizando que quede entre 0 y 12 (si f n < 0 debe devolver 0, si f n > 12 debe devolver 12)

fLimitada :: (Int->Int) -> Int -> Int
fLimitada funcion valor
    | funcion valor < 0 = 0
    | funcion valor > 12 = 12
    | otherwise = funcion valor

--a)Definir una función cambiarHabilidad/2, que reciba una función f y una lista de habilidades, y devuelva el resultado de aplicar f con las garantías de rango que da flimitada.

cambiarHabilidad :: (Int->Int) -> [Int] -> [Int]
cambiarHabilidad funcion lista= map (fLimitada funcion) lista

--b)Usar cambiarHabilidad/2 para llevar a 4 a los que tenían menos de 4, dejando como estaban al resto

llevarA4 :: Int -> Int
llevarA4 valor = if (valor < 4) then 4 else valor

cambiarHabilidad' :: [Int] -> [Int]
cambiarHabilidad' lista = map llevarA4 lista

--14. Investigar lo que hace la función takeWhile/2, que está incluida en el prelude. Preguntar primero el tipo, y después hacer pruebas. Ayudarse con el nombre.

--prueba
resultado :: [Int] -> [Int]
resultado lista = takeWhile (< 4) lista

--15.a. Usar takeWhile/2 para definir las siguientes funciones: primerosPares/1, que recibe una lista de números y devuelve la sublista hasta el primer no par exclusive

primerosPares :: [Int] -> [Int]
primerosPares lista = takeWhile even lista

--15.b. primerosDivisores/2, que recibe una lista de números y un número n, y devuelve la sublista hasta el primer número que no es divisor de n exclusive

primerosDivisores :: Int -> [Int] -> [Int]
primerosDivisores valor lista = takeWhile (\x -> esMultiploDe valor x) lista

primerosDivisores' :: Int -> [Int] -> [Int]
primerosDivisores' valor lista = takeWhile (\x -> mod valor x == 0) lista

--15.c. primerosNoDivisores/2, que recibe una lista de números y un número n, y devuelve la sublista hasta el primer número que sí es divisor de n exclusive

primerosNoDivisores :: Int -> [Int] -> [Int]
primerosNoDivisores valor lista = takeWhile (\x -> mod valor x /= 0) lista

--16. no entendi nada la consigna

--17. En una población, se estudió que el crecimiento anual de la altura de las personas sigue esta fórmula de acuerdo a la edad:
{-1 año: 22 cm 
2 años: 20 cm 
3 años: 18 cm 
... así bajando de a 2 cm por año hasta 
9 años: 6 cm 
10 a 15 años: 4 cm 
16 y 17 años: 2 cm 
18 y 19 años: 1 cm 
20 años o más: 0 cm -}
--A partir de esta información:
--17.a. Definir la función crecimientoAnual/1,que recibe como parámetro la edad de la persona, y devuelve cuánto tiene que crecer en un año. Hacerlo con guardas. La fórmula para 1 a 10 años es 24 - (edad * 2).

crecimientoAnual :: Int -> Int
crecimientoAnual edad
    |edad >= 1 && edad < 10 = 24 - ( 2 * edad )
    |edad >= 10 && edad <=15 = 4
    |edad == 16 || edad ==17 = 2
    |edad == 18 || edad ==19 = 1
    |otherwise = 0

--17.b. Definir la función crecimientoEntreEdades/2, que recibe como parámetros dos edades y devuelve cuánto tiene que crecer una persona entre esas dos edades.

crecimientoEntreEdades :: Int -> Int -> Int
crecimientoEntreEdades edad1 edad2 = if (edad1 < edad2) then (crecimientoAnual edad1 + crecimientoEntreEdades (edad1+1) edad2) else 0

--17.c.Armar una función alturasEnUnAnio/2, que dada una edad y una lista de alturas de personas, devuelva la altura de esas personas un año después.

alturasEnUnAnio :: Int -> [Int] -> [Int]
alturasEnUnAnio edad lista = map (+ crecimientoAnual edad) lista

--17.d. Definir la función alturaEnEdades/3, que recibe la altura y la edad de una persona y una lista de edades, y devuelve la lista de la altura que va a tener esa persona en cada una de las edades.

alturaEnEdades :: Int -> Int -> [Int] -> [Int]
alturaEnEdades altura edad listaEdades = map ((+altura) . (crecimientoEntreEdades edad)) listaEdades

--18. no entiendo

--19. Definir una función que sume una lista de números

sumarLista :: [Int] -> Int
sumarLista lista = foldl (+) 0 lista

--20. Definir una función que resuelva la productoria de una lista de números.

multiplicarLista :: [Int] -> Int
multiplicarLista lista = foldl (*) 1 lista

--21. Definir la función dispersion, que recibe una lista de números y devuelve la dispersión de los valores, o sea máximo - mínimo.

dispersion :: [Int] -> (Int,Int)
dispersion lista = (foldr max (head lista) lista,foldr min (head lista) lista)