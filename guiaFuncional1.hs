import Text.Show.Functions ()

--Guía Funcional 1: Primeros Ejercicios

--1. Definir la función esMultiploDeTres/1, que devuelve True si un número es múltiplo de 3
esMultiploDeTres :: Int -> Bool
esMultiploDeTres numero = mod numero 3 == 0

--2. Definir la función esMultiploDe/2, que devuelve True si el segundo es múltiplo del primero
esMultiploDeSegundo :: Int -> Int -> Bool
esMultiploDeSegundo numero1 numero2 = mod numero1 numero2 == 0

--3. Definir la función cubo/1, devuelve el cubo de un número
cubo :: Int -> Int
cubo numero = numero ^ 3

--4. Definir la función area/2, devuelve el área de un rectángulo a partir de su base y su altura
areaRectangulo :: Int -> Int -> Int
areaRectangulo base altura = base * altura

--5. Definir la función esBisiesto/1, indica si un año es bisiesto. (Un año es bisiesto si es divisible por 400 o es divisible por 4 pero no es divisible por 100) Nota: Resolverlo reutilizando la función esMultiploDe/2
esBisiesto :: Int -> Bool
esBisiesto año = esMultiploDeSegundo año 400 || (esMultiploDeSegundo año 4 && not (esMultiploDeSegundo año 100)) 

--6. Definir la función celsiusToFahr/1, pasa una temperatura en grados Celsius a grados Fahrenheit
celsiusToFahr :: Float -> Float
celsiusToFahr gradosCelsius = gradosCelsius * 9/5 + 32

--7. Definir la función fahrToCelsius/1, la inversa de la anterior
fahrToCelsius :: Float -> Float
fahrToCelsius gradosFahr = (gradosFahr - 32) * 5/9

--8. Definir la función haceFrioF/1, indica si una temperatura expresada en grados Fahrenheit es fría. Decimos que hace frío si la temperatura es menor a 8 grados Celsius. 
haceFrio :: Float -> Bool
haceFrio gradosFahr = fahrToCelsius gradosFahr < 8

--9. Definir la función mcm/2 que devuelva el mínimo común múltiplo entre dos números, de acuerdo a esta fórmula
mcm :: Int -> Int -> Int
mcm num1 num2 = div (num1 * num2) (gcd num1 num2)

--10. Dispersión
{-Trabajamos con tres números que imaginamos como el nivel del río Paraná a la altura de Corrientes medido en tres días consecutivos; cada medición es un entero que representa una cantidad de cm. 
P.ej. medí los días 1, 2 y 3, las mediciones son: 322 cm, 283 cm, y 294 cm. 
A partir de estos tres números, podemos obtener algunas conclusiones. 
Definir estas funciones:

a. dispersion, que toma los tres valores y devuelve la diferencia entre el más alto y el más bajo. Ayuda: extender max y min a tres argumentos, usando las versiones de dos elementos. De esa forma se puede definir dispersión sin escribir ninguna guarda (las guardas están en max y min, que estamos usando).-}
dispersion :: Int -> Int -> Int -> Int
dispersion dia1 dia2 dia3 = max (max dia1 dia2) dia3 - min (min dia1 dia2) dia3

{-
b. diasParejos, diasLocos y diasNormales reciben los valores de los tres días. Se dice que son días parejos si la dispersión es chica, que son días locos si la dispersión es grande, y que son días normales si no son ni parejos ni locos. Una dispersión se considera chica si es de menos de 30 cm, y grande si es de más de un metro. 
Nota: Definir diasNormales a partir de las otras dos, no volver a hacer las cuentas-}

--11

--12