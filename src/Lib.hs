module Lib where
import Text.Show.Functions

laVerdad = True

{-De cada gimnasta nos interesa saber su edad, su peso y su coeficiente de tonificación. Tenemos algunos 
ejemplos de socios para realizar las pruebas: ­­
data Gimnasta = Gimnasta String Float Float Float deriving(Show)
pancho = Gimnasta "Francisco" 40.0 120.0 1.0
andres = Gimnasta "Andy" 22.0 80.0 6.0
-}

data Gimnasta = UnGimnasta {nombre :: String,
                            edad:: Float,
                            peso :: Float,
                            tonificacion :: Float} deriving (Show, Eq)

pancho :: Gimnasta
pancho = UnGimnasta "Francisco" 40.0 120.0 1.0

andres:: Gimnasta
andres = UnGimnasta "Andy" 22.0 80.0 6.0

{-Los ejercicios que se pueden hacer son funciones que dado un gimnasta y una cantidad de minutos, 
retorna a el gimnasta luego de realizar el ejercicio. Un ejemplo simple de ejercicio en el cual el 
gimnasta no hace nada (y por ende queda igual que al principio sin importar cuánto tiempo lo realice) podría ser: 
relax minutos gimnasta = gimnasta
-}

type Ejercicio = Float-> Gimnasta -> Gimnasta

relax :: Ejercicio
relax minutos gimnasta = gimnasta

{-Saber si alguien está saludable, lo cual se cumple si no está obeso y 
tiene una tonificación mayor a 5. Alguien es obeso si pesa más de 100 kilos. -}

{-estaSaludable :: Gimnasta -> Bool
estaSaludable gimnasta = (not.estaObeso) gimnasta && tonificado gimnasta

cumple :: (Gimnasta -> Float) -> Float -> Gimnasta -> Bool
cumple funcion numero gimnasta = (funcion gimnasta) > numero
--estaSaludable gimnasta = not (cumple peso 100 gimnasta) && cumple tonificacion 5 gimnasta
--No eran muy necesarias estas funciones teniendo ya la funcion cumple pero son buenas abtracciones y ayudan a la declaratividad
estaObeso :: Gimnasta -> Float
estaObeso gimnasta = cumple (peso 100 gimnasta)

tonificado :: Gimnasta -> Float
tonificado gimnasta = cumple (tonificacion 5 gimnasta)-}

-----Otra opcion pero con más repeticion de lógica
estaObeso :: Gimnasta -> Bool
estaObeso gimnasta = peso gimnasta > 100

tonificado :: Gimnasta -> Bool
tonificado gimnasta = tonificacion gimnasta > 5
estaSaludable gimnasta = (not.estaObeso) gimnasta && tonificado gimnasta

----punto 2
{-Hacer que el gimnasta queme una cantidad de calorías, lo que produce que baje de peso.
Si el gimnasta es obeso, baja 1 kilo cada 150 calorías quemadas.
Si no es obeso pero tiene más de 30 años y las calorías quemadas son más de 200, baja siempre un kilo.
En cualquier otro caso se baja la cantidad de calorías quemadas dividido por el producto entre el peso y la edad del gimnasta. 
> quemarCalorias pancho 300
Gimnasta "Francisco" 40.0 118.0 1.0
> quemarCalorias andres 300 
Gimnasta "Andy" 22.0 79.8 6.0-}

--quemarCalorias :: Int -> Gimnasta -> Gimnasta 
--cambiarPeso (bajarPeso caloriasQuemadas) gimnasta

quemarCalorias :: Float -> Gimnasta -> Gimnasta
quemarCalorias calorias gimnasta 
    |estaObeso gimnasta =  cambiarPeso (disminuirPeso (calorias / 150)) gimnasta
    |edad gimnasta > 30 && calorias > 200 = cambiarPeso (disminuirPeso 1) gimnasta
    |otherwise = cambiarPeso (disminuirPeso (calorias / (peso gimnasta) * (edad gimnasta))) gimnasta

cambiarPeso :: (Float-> Float ) -> Gimnasta -> Gimnasta
cambiarPeso funcion gimnasta = gimnasta {peso = (funcion.peso) gimnasta}

disminuirPeso :: Float -> Float -> Float
disminuirPeso kilos peso  = peso - kilos


 --cambiarPeso :: (Float -> Gimnasta -> Float) -> Float-> Gimnasta -> Gimnasta
 --cambiarPeso funcion caloriasQuemadas gimnasta = gimnasta {peso = peso gimnasta - (funcion caloriasQuemadas gimnasta)    
     {-bajarPeso::  Int -> Gimnasta -> Float
bajarPeso calorias gimnasta = peso gimnasta - calorias / (_)
    p-}

{-Desarrollar las funciones para los ejercicios caminataEnCinta, entrenamientoEnCinta, pesas, 
colina y montania sabiendo que:
La cinta quema calorías en función de la velocidad promedio alcanzada durante el ejercicio, quemando 1 caloría 
por la velocidad promedio por minuto.
La caminata es un ejercicio en cinta con velocidad constante de 5 km/h. 
> caminataEnCinta 40 pancho 
Gimnasta "Francisco" 40.0 118.6 1.0 ­­­ --quema 200 calorías (1*5*40) -}

{-El entrenamiento en cinta arranca en 6 km/h y cada 5 minutos incrementa la velocidad en 1 km/h, con lo cual 
la velocidad máxima dependerá de los minutos de entrenamiento.
> entrenamientoEnCinta 40 pancho 
Gimnasta "Francisco" 40.0 117.3 1.0 ­­­ --quema 400 calorías (1* ((6+14)/2) * 40) 
siendo 14 la velocidad máxima alcanzada por los 8 incrementos durante los 40 minutos
-}

cinta :: Float -> Float -> Float
cinta velocidad tiempo = velocidad * tiempo

--caminataEnCinta :: Float -> Gimnasta -> Gimnasta
caminataEnCinta :: Ejercicio
caminataEnCinta tiempo gimnasta = quemarCalorias (cinta 5 tiempo) gimnasta


entrenamientoEnCinta :: Ejercicio
entrenamientoEnCinta tiempo gimnasta = quemarCalorias (cinta (velocidad tiempo) tiempo ) gimnasta

velocidad :: Float -> Float
velocidad tiempo = 6 + tiempo / 5

{-Las pesas tonifican la décima parte de los kilos a levantar si se realiza por más de 10 minutos, sino nada. 
> pesas 50 15 pancho 
Gimnasta "Francisco" 40.0 120.0 6.0 ­­­ --tonifica 5 (50 / 10) 
-}

tonificar :: (Float-> Float) -> Gimnasta -> Gimnasta
tonificar funcion gimnasta = gimnasta {tonificacion = (funcion.tonificacion) gimnasta }

pesas :: Float -> Float-> Gimnasta -> Gimnasta
pesas pesoPesa tiempo gimnasta
    |tiempo > 10 =  tonificar (+pesoPesa/10) gimnasta
    |otherwise = gimnasta

{-La colina quema 2 calorías por minuto multiplicado por la inclinación de la colina. 
> colina 5 40 pancho 
Gimnasta "Francisco" 40.0 117.3 1.0  --quema 400 calorías (2*40*5) -}

colina :: Float -> Float -> Gimnasta -> Gimnasta
colina minutos inclinacion gimnasta = quemarCalorias (2*minutos*inclinacion) gimnasta

{-La montaña son 2 colinas sucesivas (cada una con la mitad de duración respecto de los minutos totales indicados),
 donde la segunda colina tiene una inclinación de 3 más que la inclinación inicial elegida. 
Además de hacer perder peso por las calorías quemadas por las colinas, este ejercicio incrementa en una unidad 
la tonificación de la gimnasta. 
Resolver usando composición y aplicación parcial. 
> montania 5 40 pancho 
Gimnasta "Francisco" 40.0 116.5 2.0
-- quema 520 calorias (2*20*5 por la 1ra, 2*20*8 por la 2da) y se tonifica 1
-}

montaña :: Float -> Float -> Gimnasta -> Gimnasta
montaña minutos inclinacion  = colina (minutos/2) inclinacion . colina (minutos/2) (inclinacion * 3). tonificar (+1) 

{-Rutina de ejercicios:
Dada una Rutina (es un Data con un nombre, duración total y lista de ejercicios específicos) y un gimnasta, 
obtener al gimnasta luego de realizar la rutina. 
La cantidad de minutos dedicada a cada ejercicio es la misma. 
Mostrar un ejemplo de uso usando todos los ejercicios del punto anterior. 
Resolverlo usando recursividad.
Hacer otra solución usando fold.
Dada una rutina y un gimnasta, obtener el resumen de rutina que es una tupla con el nombre de la misma, 
los kilos perdidos y la tonificación ganada por dicho gimnasta al realizarla.
-}








                        