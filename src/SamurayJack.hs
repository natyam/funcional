module SamurayJack where

import Text.Show.Functions

data Elemento = UnElemento { tipo :: String,
    ataque :: (Personaje-> Personaje),
     defensa :: (Personaje-> Personaje) } deriving(Show)

data Personaje = UnPersonaje { nombre :: String,
 salud :: Float,
 elementos :: [Elemento],
 anioPresente :: Int } deriving(Show)




---------------------------
-- Punto 1
---------------------------
mandarAlAnio:: Int ->Personaje -> Personaje
mandarAlAnio anio personaje = personaje {anioPresente = anio}

meditar:: Personaje -> Personaje
meditar personaje = cambiarSalud ((+).(/2) $ salud personaje) personaje

cambiarSalud :: (Float -> Float) -> Personaje -> Personaje
cambiarSalud f personaje = personaje { salud = f.salud $ personaje }

causarDanio:: Float ->Personaje -> Personaje
causarDanio a personaje 
   | a > salud personaje = cambiarSalud (*0) personaje
   | otherwise = cambiarSalud (+ (- a)) personaje

{-
En caso de que no se indique cuál es el efecto defensivo o el ofensivo,
significa que no se altera de ninguna forma al personaje recibido.
-}
---------------------------
-- Punto 2 
---------------------------
--esMalvado, que retorna verdadero si alguno de los elementos que tiene el personaje en cuestión es de tipo “Maldad”.

maldad :: String
maldad = "Maldad"
--esMalvado (aku 300 30000)
esMalvado :: Personaje -> Bool
esMalvado personaje =any  ((=="Maldad").tipo $) (elementos personaje)
    --any ( (== maldad).tipo $) (elementos personaje)
--( (== maldad).tipo $)

danioQueProduce :: Personaje -> Elemento -> Float
danioQueProduce personaje elemento = salud personaje - salud (ataque elemento personaje)

danioQueProduce' :: Personaje -> Elemento -> Float
danioQueProduce' personaje elemento= (salud personaje -).salud.ataque elemento $ personaje

enemigosMortales :: Personaje-> [Personaje]-> [Personaje]
enemigosMortales personaje enemigos = filter (puedeMatarloConUnElemento personaje) enemigos
puedeMatarloConUnElemento :: Personaje -> Personaje -> Bool
puedeMatarloConUnElemento personaje enemigo = any  ((==0).danioQueProduce personaje) (elementos enemigo)

---------------------------
-- Punto 3
---------------------------
--3a
concentracion :: Int  -> Elemento
concentracion nivel  = UnElemento { tipo = "Magia",ataque = id, defensa = meditarVariasVeces nivel }

meditarVariasVeces :: Int -> (Personaje-> Personaje)
meditarVariasVeces nivel =foldl1 (.).take nivel.repeat $ meditar
--revisar si funciona igual con iterate
--meditarVariasVeces nivel =head.reverse.take (nivel+1).iterate meditar 
----3b

esbirroMalvado :: Elemento
esbirroMalvado = UnElemento {
    tipo = "Maldad",
    defensa= id,
    ataque = causarDanio 1
}

esbirrosMalvados :: Int -> [Elemento]
esbirrosMalvados cant = take cant.repeat $ esbirroMalvado

-------3c
jack :: Personaje
jack = UnPersonaje{
    nombre = "Jack",
    salud = 300,
    elementos= [concentracion 3, katanaMagica],
    anioPresente= 200
}
katanaMagica :: Elemento
katanaMagica = UnElemento {
    tipo = "Magia",
    ataque = causarDanio 1000,
    defensa= id
}
---3d
aku :: Int -> Float -> Personaje
aku anio salud = UnPersonaje{nombre= "Aku", salud = salud, anioPresente= anio,
   elementos=[concentracion 4]++esbirrosMalvados (100 * anio) ++[portalAlFuturo anio]}
portalAlFuturo :: Int ->Elemento
portalAlFuturo anio= UnElemento {
    tipo = "Magia",
    ataque =(mandarAlAnio.(2800+) $ anio ) ,
    defensa= id
 }

---------------------------
-- Punto 4
---------------------------

luchar :: Personaje -> Personaje -> (Personaje, Personaje)
luchar atacante defensor
   | salud atacante ==0 =(defensor,atacante)
   |otherwise = luchar (rondaAtaques atacante defensor)  (rondaDefensa  atacante) 

rondaAtaques :: Personaje ->Personaje -> Personaje
rondaAtaques atacante  = foldl1 (.).map ataque $  elementos atacante

rondaDefensa :: Personaje -> Personaje
rondaDefensa atacante  = (defensas atacante) $ atacante

defensas:: Personaje -> (Personaje -> Personaje)
defensas atacante = foldl1 (.).map defensa $  elementos atacante


traerAtaquesOdDefensas :: (Elemento->Personaje -> Personaje) -> Personaje ->  (Personaje -> Personaje)
traerAtaquesOdDefensas     criterio                     personaje =  foldl1 (.).map criterio $  elementos personaje




----aca hago lo mismo sin repetir codigo
{-
rondaDefensa' :: Personaje -> Personaje
rondaDefensa' atacante = (defensas atacante) $ atacante

rondaAtaques' :: Personaje ->Personaje -> Personaje
rondaAtaques' atacante  = traerAtaquesOdDefensas ataque atacante
-}
defensas2:: Personaje -> (Personaje -> Personaje)
defensas2 atacante = traerAtaquesOdDefensas defensa atacante


luchar2 :: Personaje -> Personaje -> (Personaje, Personaje)
luchar2 atacante defensor
   | salud atacante ==0 =(defensor,atacante)
   | otherwise = luchar ((traerAtaquesOdDefensas ataque atacante) $defensor ) ( (defensas2 atacante) $ atacante )
   -- otherwise = luchar (rondaAtaques' atacante defensor)  (rondaDefensa'  atacante) 

-- luchar2 jack (aku 1 3000)
-- luchar (aku 01 3000) jack


---------------------------
-- Punto 5
---------------------------
{-
Inferir el tipo de la siguiente función:
f x y z
    | y 0 == z = map (fst.x z)
    | otherwise = map (snd.x (y 0))
-}
{-
fst y snd son funciones para tuplas  y como esta fst.x z ya sabes que x es una funcion que aplicada con z trae una tupla
esto trae una funcion que usa map y le falta una lista

y :: Int -> a

a y z son del mismo tipo entonces tienen que ser Eqpara que funcionen con un == entonces queda
y :: Eq a =>Int -> a
de lo que podemos sacar x
x:: Eq a =>a -> ((->),(->))

           ------x---------------                 ------------y---------                  z         me        
f:: Eq a =>     (a -> ((b->d),(b->d)))        ->     (Int -> a)          ->        a     -> ([b] -> [d])

-}