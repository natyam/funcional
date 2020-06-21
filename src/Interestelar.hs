module Interestelar where

import Text.Show.Functions

---------------------------
-- Datos
---------------------------
{--- Cada planeta tiene un nombre, una posición en el espacio y
 una relación que indica a cuánto tiempo terrestre equivale pasar un año allí-}
data Planeta = UnPlaneta String Posicion (Int -> Int)

posicion (UnPlaneta _ p _) = p
tiempo (UnPlaneta _ _ t) = t

type Posicion = (Float, Float, Float)
coordX (x,_,_) = x
coordY (_,y,_) = y
coordZ (_,_,z) = z

-- De los astronautas sabemos el nombre, la edad terrestre y el planeta en el que están

data Astronauta = UnAstronauta String Int Planeta
nombre (UnAstronauta n _ _) = n
edad (UnAstronauta _ e _) = e
planeta (UnAstronauta _ _ p) = p

---------------------------
-- Punto 1
---------------------------
------ 1A
distancia :: Posicion -> Posicion -> Int

distancia planeta1 planeta2 = round(sqrt (restaCuadrado (coordX planeta1) (coordX planeta2) + restaCuadrado (coordY planeta1) (coordY planeta2) + restaCuadrado (coordZ planeta1) (coordZ planeta2)))
--base ^ exponente ((coordX planeta1) ^ 2) - ((coordX planeta2) ^ 2)
--
restaCuadrado :: Float -> Float -> Float
restaCuadrado a b = (a ^ 2) - (b ^ 2)

---1B

{-
Saber cuánto tiempo se tarda en viajar de un planeta a otro yendo a una determinada velocidad, 
que es la distancia entre ambos dividido por la velocidad de viaje.
-}
tiempoEnViajarEntrePlanetas :: Int -> Planeta -> Planeta -> Int 
tiempoEnViajarEntrePlanetas velocidad planeta1 planeta2 = div (distancia (posicion planeta1) (posicion planeta2)) velocidad

--tiempoEnViajarEntrePlanetas :: Float -> Planeta -> Planeta -> Float  
--tiempoEnViajarEntrePlanetas velocidad planeta1 planeta2 = (distancia (posicion planeta1) (posicion planeta2)) / velocidad

-- asumo que la velocidad es un int pero tmb puede ser un vector, revisar


---------------------------
-- Punto 2
---------------------------
pasarTiempo :: Astronauta -> Int-> Astronauta
pasarTiempo astronauta  anos =cambiarEdad  astronauta (tiempo(planeta astronauta) $ anos) 
    
cambiarEdad :: Astronauta -> Int -> Astronauta
cambiarEdad  astronauta anos = (UnAstronauta (nombre astronauta) ((+ edad astronauta) $ anos) (planeta astronauta))

{-
--          esto trae un planeta
--  tiempo.(planeta astronauta) $ anos
pasarTiempo que haga que un astronauta pase una determinada cantidad de años en su planeta actual.
Debería aumentar su edad terrestre en la cantidad de tiempo que el planeta indique a partir de los años indicados.
--no necesito pasarle el planeta xq ya lo tiene el Astronauta
-}

---------------------------
-- Punto 3
---------------------------
{-
Queremos que un astronauta pueda viajar a otro planeta usando una nave determinada.
En principio nos interesa modelar las siguientes naves:
Realizar un viaje implica que el astronauta aumente su edad en el tiempo de viaje correspondiente 
para llegar al destino elegido y cambie de planeta al mismo
-}
type Nave = Planeta -> Planeta -> Int
naveVieja :: Int -> Nave
naveVieja tanques
    | tanques >= 6 = tiempoEnViajarEntrePlanetas 10
    | otherwise = tiempoEnViajarEntrePlanetas 7

naveFuturista :: Nave 
naveFuturista _ _ = 0
--naveFuturista no cambia la edad
viajar :: Nave ->Planeta -> Astronauta -> Astronauta
viajar nave planeta2 astronauta 
    | tiempoViaje == 0 = cambiarPlaneta planeta2 astronauta
    | otherwise =  cambiarPlaneta planeta2 (pasarTiempo  astronauta tiempoViaje )
    where
        tiempoViaje = (nave (planeta astronauta) planeta2)

cambiarPlaneta ::  Planeta -> Astronauta -> Astronauta
cambiarPlaneta planeta2 astronauta = (UnAstronauta (nombre astronauta) (edad astronauta) planeta2 )    

--nave planeta1 planeta2 velocidad = tiempoEnViajarEntrePlanetas velocidad planeta1 planeta2  

---------------------------
-- Punto 4
---------------------------
sumarTripulacion :: Astronauta->[Astronauta]->[Astronauta]
sumarTripulacion astronauta lista = (astronauta : lista)

rescate :: Nave -> Astronauta-> [Astronauta] -> [Astronauta] 
rescate nave rescatado astronautas = undefined
    --volver(nave (planeta(head astronautas) ( sumarTripulacion rescatado (volver nave (planeta rescatado) astronautas)) ) )
--viajan a rescatarlo map (viajar nave (planeta rescatado)) astronautas
--lo rescatan sumarTripulacion rescatado astronautas
--el planeta2 es el planeta q esta dentro del astronaua rescatad0
--volver la uso para viajar al astronauta perdido
--volver(nave (planeta(head astronautas) ( sumarTripulacion rescatado (volver nave (planeta rescatado) astronautas)) ) )
volver :: Nave -> Planeta -> [Astronauta] -> [Astronauta] 
volver nave planeta astronautas  = map (viajar nave planeta) astronautas
{-

Hacer que un grupo de astronautas rescate a un astronauta que quedó varado en otro planeta usando una determinada nave.
Lo que se espera como resultado de efectuar un rescate es la lista de astronautas luego de que todos los rescatistas viajen 
en la nave a buscar al astronauta al planeta donde está varado, incorporen a la tripulación al rescatado
tras pasar el tiempo correspondiente en ese planeta 
y luego viajen todos en la misma nave al planeta de donde vinieron los rescatistas.

o
viajar :: Nave -> Planeta -> Astronauta -> Astronauta
viajar nave astronauta planeta2 

data Planeta = UnPlaneta String Posicion (Int -> Int)
posicion (UnPlaneta _ p _) = p
tiempo (UnPlaneta _ _ t) = t
type Posicion = (Float, Float, Float)
coordX (x,_,_) = x
coordY (_,y,_) = y
coordZ (_,_,z) = z
-- De los astronautas sabemos el nombre, la edad terrestre y el planeta en el que están
data Astronauta = UnAstronauta String Int Planeta
nombre (UnAstronauta n _ _) = n
edad (UnAstronauta _ e _) = e
planeta (UnAstronauta _ _ p) = p
-}
