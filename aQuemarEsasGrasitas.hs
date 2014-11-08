data Gimnasta = CGimnasta String Float Float Float deriving(Show)

pancho = CGimnasta "Francisco" 40.0 120.0 1.0
andres = CGimnasta "Andy" 22.0 80.0 6.0

nombreGimnasta (CGimnasta nombre anios peso tonificacion) = nombre
edadGimnasta (CGimnasta nombre anios peso tonificacion) = anios
pesoGimnasta (CGimnasta nombre anios peso tonificacion) = peso
tonificacionGimnasta (CGimnasta nombre anios peso tonificacion) = tonificacion

--1

esObeso gimnasta = pesoGimnasta gimnasta > 100
esTonificado gimnasta = tonificacionGimnasta gimnasta > 5

saludable gimnasta =  not(esObeso gimnasta) && esTonificado gimnasta

--2

quemarCalorias (CGimnasta nombre edad peso tonificacion) calorias | esObeso (CGimnasta nombre edad peso tonificacion) = CGimnasta nombre edad (peso - (calorias/150)) tonificacion
																  | edad > 30 && calorias > 200 = CGimnasta nombre edad (peso-1) tonificacion
												 				  | otherwise = CGimnasta nombre edad (peso - calorias/(edad*peso)) tonificacion
												 
--3


caminata tiempo gimnasta =  quemarCalorias gimnasta (5*tiempo)
entrenamientoEnCinta tiempo gimnasta = quemarCalorias gimnasta ((6+(tiempo/5)/2)*tiempo)
pesas levanta tiempo (CGimnasta nombre edad peso tonificacion)|tiempo>10 = CGimnasta nombre edad peso (tonificacion+levanta/10)
											 				  |otherwise = CGimnasta nombre edad peso tonificacion


colina inclinacion tiempo gimnasta = quemarCalorias gimnasta (2*tiempo*inclinacion)

montania inclinacion tiempo gimnasta = ((incrementoTonif).((colina (inclinacion+3) (tiempo/2)).(colina inclinacion (tiempo/2)))) gimnasta
incrementoTonif (CGimnasta nombre edad peso tonificacion) = (CGimnasta nombre edad peso (tonificacion+1))
--4
--a
data UnaRutina = Rutina String Float [Float->Gimnasta->Gimnasta]

--i.
reparteTiempo tiempo listaDeEjercicios = tiempo / fromIntegral (length listaDeEjercicios)
laRutina = [(caminata),(entrenamientoEnCinta),(pesas 15),(colina 5),(montania 5)]

rutina1 gimnasta (Rutina nombreRut tiempo laRutina) = (head.tail.tail.tail.tail) laRutina (reparteTiempo tiempo laRutina) ((head.tail.tail.tail) laRutina (reparteTiempo tiempo laRutina) ((head .tail.tail) laRutina (reparteTiempo tiempo laRutina) ((head.tail) laRutina (reparteTiempo tiempo laRutina) ((head laRutina (reparteTiempo tiempo laRutina)) gimnasta))))
--ii.

rutina2 nombreRut tiempo []  gimnasta = gimnasta
rutina2 nombreRut tiempo laRutina gimnasta =  rutina2 nombreRut (reparteTiempo tiempo laRutina) (tail (laRutina)) (head(laRutina) (reparteTiempo tiempo laRutina) gimnasta)

--iii.
haceEjercicio tiempo gimnasta unEjercicio = unEjercicio tiempo gimnasta 

rutina3 gimnasta (Rutina nombreRut tiempo laRutina) = foldl (haceEjercicio (reparteTiempo tiempo laRutina) ) gimnasta laRutina 

--b

resumenRutina gimnasta (Rutina nombreRut tiempo rutina) = (nombreRut,pesoGimnasta gimnasta - pesoGimnasta(rutina3 gimnasta (Rutina nombreRut tiempo rutina)),tonificacionGimnasta (rutina3 gimnasta (Rutina nombreRut tiempo rutina)) - tonificacionGimnasta gimnasta )

--5
--a una lista de rutinas va a tener por cada rutina un nombre, el tiempo y la lista de ejercicios.

rutinaSaludable (CGimnasta nombre edad peso tonificacion) (nombreRutina,pesoPerdido,tonificacionGanada) = saludable (CGimnasta nombre edad (peso-pesoPerdido) (tonificacion+tonificacionGanada)) 

resumenDeRutinas listaDeRutinas gimnasta = filter (rutinaSaludable gimnasta) (map (resumenRutina gimnasta) listaDeRutinas)
