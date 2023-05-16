import Text.Show.Functions ()

--Funcional 3: Tuplas

--1. Definir las funciones fst3, snd3, trd3, que dada una tupla de 3 elementos devuelva el elemento correspondiente

fst3 :: (Int, Int, Int) -> Int
fst3 (elemento, _, _) = elemento

snd3 :: (Int, Int, Int) -> Int
snd3 (_, elemento, _) = elemento

trd3 :: (Int, Int, Int) -> Int
trd3 (_, _, elemento) = elemento

--2. Definir la función aplicar, que recibe como argumento una tupla de 2 elementos con funciones y un entero, me devuelve como resultado una tupla con el resultado de aplicar el elemento a cada una de la funciones

aplicar :: (Int->Int,Int->Int)-> Int ->(Int,Int)
aplicar (funcion1, funcion2) numero = (funcion1 numero, funcion2 numero)

--3.Definir la función cuentaBizarra, que recibe un par y: si el primer elemento es mayor al segundo devuelve la suma, si el segundo le lleva más de 10 al primero devuelve la resta 2do – 1ro, y si el segundo es más grande que el 1ro pero no llega a llevarle 10, devuelve el producto

cuentaBizarra :: (Int->Int) -> Int
cuentaBizarra (primero, segundo)
    |(primero >= segundo) = primero+segundo
    |((segundo-primero)>10) = segundo-primero
    |otherwise = primero*segundo

{-
4. Representamos las notas que se sacó un alumno en dos parciales mediante un par (nota1,nota2), p.ej. un patito en el 1ro y un 7 en el 2do se representan mediante el par (2,7). 
A partir de esto: 
    a) Definir la función esNotaBochazo, recibe un número y devuelve True si no llega a 6, False en caso contrario. No vale usar guardas. 
    b) Definir la función aprobo, recibe un par e indica si una persona que se sacó esas notas aprueba. Usar esNotaBochazo. 
    c) Definir la función promociono, que indica si promocionó, para eso tiene las dos notas tienen que sumar al menos 15 y además haberse sacado al menos 7 en cada parcial. 
    d) Escribir una consulta que dado un par indica si aprobó el primer parcial, usando esNotaBochazo y composición 
-}

esNotaBochazo :: Int -> Bool
esNotaBochazo nota = nota < 6

aprobo :: (Int, Int) -> Bool
aprobo (nota1, nota2) = ((not . esNotaBochazo) nota1) && ((not . esNotaBochazo) nota2)

promociono :: (Int, Int) -> Bool
promociono (nota1, nota2) = ((nota1 + nota2) >= 15) && (nota1>=7) && (nota2>=7)

consulta1erParcial :: (Int, Int) -> String
consulta1erParcial (nota1, nota2) = if ((not.esNotaBochazo) nota1 && (not.esNotaBochazo) nota2) then "Aprobo" else "Reprobo"

{-
5. Siguiendo con el dominio del ejercicio anterior, tenemos ahora dos parciales con dos recuperatorios, lo representamos mediante un par de pares ((parc1,parc2),(recup1,recup2)). 
Si una persona no rindió un recuperatorio, entonces ponemos un "-1" en el lugar correspondiente.  Observamos que con la codificación elegida, siempre la mejor nota es el máximo entre nota del parcial y nota del recuperatorio. Considerar que vale recuperar para promocionar. En este ejercicio vale usar las funciones que se definieron para el anterior. 
    a) Definir la función notasFinales que recibe un par de pares y devuelve el par que corresponde a las notas finales del alumno para el 1er y el 2do parcial.
    b) Escribir la consulta que indica si un alumno cuyas notas son ((2,7),(6,-1)) recursa o no. O sea, la respuesta debe ser True si recursa, y False si no recursa. Usar las funciones definidas en este punto y el anterior, y composición.
    c) Escribir la consulta que indica si un alumno cuyas notas son ((2,7),(6,-1)) recuperó el primer parcial. Usar composición.
    d) Definir la función recuperoDeGusto que dado el par de pares que representa a un alumno, devuelve True si el alumno, pudiendo promocionar con los parciales (o sea sin recup.), igual rindió al menos un recup. Vale definir funciones auxiliares. Hacer una definición que no use pattern matching, en las eventuales funciones auxiliares tampoco; o sea, manejarse siempre con fst y snd
-}

type NotaAnual = ((Parc1,Parc2),(Recup1,Recup2))
type Parc1 = Int
type Parc2 = Int
type Recup1 = Int
type Recup2 = Int

notasFinales :: NotaAnual -> (Int,Int)
notasFinales ((notaParc1,notaParc2),(notaRecup1,notaRecup2)) = (max notaParc1 notaRecup1, max notaParc2 notaRecup2)

recursa :: NotaAnual -> Bool
recursa ((notaParc1,notaParc2),(notaRecup1,notaRecup2)) = (esNotaBochazo notaParc1 && esNotaBochazo notaRecup1) || (esNotaBochazo notaParc2 && esNotaBochazo notaRecup2)

recup1erParc :: NotaAnual -> Bool
recup1erParc ((notaParc1,notaParc2),(notaRecup1,notaRecup2)) = (not.esNotaBochazo) notaRecup1

recuperoDeGusto :: NotaAnual -> Bool
recuperoDeGusto ((notaParc1,notaParc2),(notaRecup1,notaRecup2)) = (promociono (notaParc1, notaParc2) && ((notaRecup1 == -1) || (notaRecup2 == -1)))

--6.
--Definir la función esMayorDeEdad, que dada una tupla de 2 elementos (persona, edad) me devuelva True si es mayor de 21 años y False en caso contrario

type Persona = (Nombre,Edad)
type Nombre = String
type Edad = Int

esMayorDeEdad :: Persona -> Bool
esMayorDeEdad (_ , edad) = edad >= 21

--7. Definir la función calcular, que recibe una tupla de 2 elementos, si el primer elemento es par lo duplica, sino lo deja como está y con el segundo elemento en caso de ser impar le suma 1 y si no deja esté último como esta.