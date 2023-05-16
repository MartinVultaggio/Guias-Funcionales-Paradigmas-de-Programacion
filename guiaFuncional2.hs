import Text.Show.Functions ()

--Funcional 2: Aplicación Parcial y Composición

--1. Definir una función siguiente, que al invocarla con un número cualquiera me devuelve el resultado de sumar a ese número el 1. 
siguiente :: Int -> Int
siguiente numero = numero + 1

--2. Definir la función mitad que al invocarla con un número cualquiera me devuelve la mitad de dicho número
mitad :: Float -> Float
mitad numero = numero / 2

--3. Definir una función inversa, que invocando a la función con un número cualquiera me devuelva su inversa
inversa :: Float -> Float
inversa numero = 1/numero

--4. Definir una función triple, que invocando a la función con un número cualquiera me devuelva el triple del mismo
triple :: Int -> Int
triple numero = numero*3

--5. Definir una función esNumeroPositivo, que invocando a la función con un número cualquiera me devuelva true si el número es positivo y false en caso contrario.
esNumeroPositivo :: Float -> Bool
esNumeroPositivo numero = if numero>0 then True else False
{-
hacerNumeroNegativo :: Float -> Float
hacerNumeroNegativo numero = (-1)*numero

esNumeroPositivo2 :: Float -> Bool
esNumeroPositivo2 numero = if (hacerNumeroNegativo numero) >0 then True else False
-} --para probar, porque no puedo poner numeros negativos en la consola, va, nose como


-- g(f(x)) = (g o f) x
-- aca es (g . f) x
--6. Resolver la función del ejercicio 2 de la guía anterior esMultiploDe/2, utilizando aplicación parcial y composición
esIgualA0 :: Int -> Bool
esIgualA0 numero = numero == 0
esMultiploDeSegundo numero = esIgualA0 . mod numero

--7. Resolver la función del ejercicio 5 de la guía anterior esBisiesto/1, utilizando aplicación parcial y composición.
esMultiploDe :: Int -> Int -> Bool
esMultiploDe divisor numero = (esIgualA0 . mod numero) divisor

esBisiesto :: Int -> Bool
esBisiesto año = esMultiploDe 400 año || (esMultiploDe 4 año && (not . esMultiploDe 100) año)

--8. Resolver la función inversaRaizCuadrada/1, que dado un número n devolver la inversa su raíz cuadrada
inversaRaizCuadrada :: Float->Float
inversaRaizCuadrada = inversa . sqrt

--9. Definir una función incrementMCuadradoN, que invocándola con 2 números m y n, incrementa un valor m al cuadrado de n
cuadrado :: Int -> Int
cuadrado numero = numero ^ 2

sumar :: Int->Int->Int
sumar a b = a+b

incrementMCuadradoN :: Int->Int->Int
incrementMCuadradoN m = sumar m . cuadrado

--10.