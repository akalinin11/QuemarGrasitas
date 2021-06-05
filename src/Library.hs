module Library where
import PdePreludat

--Kalinin Alexader


--Modelado de GImnasta
data Gimnasta = Gimnasta {
 nombre::String 
,edad:: Number 
, peso:: Number 
, coeficenteTonifacion:: Number
}deriving(Show)


pancho = Gimnasta "Francisco" 40 120 1
andres = Gimnasta "Andy" 22 80 6


--Ejercicio ejemplo
relax minutos gimnasta = gimnasta


--1)

estaSaludable :: Gimnasta->Bool
estaSaludable  aGimnasta = noEstaObeso aGimnasta && coeficenteTonifacion aGimnasta >5 

noEstaObeso :: Gimnasta->Bool
noEstaObeso  = not.(>100).peso


--2)

type Calorias = Number

bajarPeso :: Calorias->Gimnasta->Gimnasta
bajarPeso calorias aGimnasta | not (noEstaObeso aGimnasta) = quemarCalorias  (1/150 * calorias) aGimnasta
                             | noEstaObeso aGimnasta && edad aGimnasta >30  && calorias>200 = quemarCalorias  1 aGimnasta
                             | otherwise = quemarCalorias  (calorias / (peso aGimnasta * edad aGimnasta) ) aGimnasta


quemarCalorias :: Calorias->Gimnasta->Gimnasta
quemarCalorias  pesoo aGimnasta = aGimnasta{ peso= peso aGimnasta-pesoo}


--3)

type Minutos = Number
-----
caminataEnCinta :: Minutos->Ejercicio
caminataEnCinta minutos  = bajarPeso  (caloriasCaminata minutos)

caloriasCaminata :: Minutos->Calorias
caloriasCaminata = (*1).(*5)

-----
entrenamientoEnCinta :: Minutos->Ejercicio
entrenamientoEnCinta minutos = bajarPeso (caloriasEntrenCinta minutos) 

caloriasEntrenCinta :: Minutos->Calorias
caloriasEntrenCinta minutos = 1* (promedioVelocidad minutos) * minutos

type Velocidad = Number
promedioVelocidad :: Minutos->Velocidad
promedioVelocidad  = (/2).(+6).velocidadMax

velocidadMax :: Minutos->Velocidad
velocidadMax  = (+6).(/5)


-----
type Kilos = Number
pesas :: Kilos->Minutos->Ejercicio
pesas kilos minutos aGimnasta | minutos > 10 = tonificar (kilos/10) aGimnasta
                              | otherwise = aGimnasta

tonificar :: Number->Gimnasta->Gimnasta
tonificar n aGimnasta = aGimnasta{coeficenteTonifacion= coeficenteTonifacion aGimnasta+n}
----
type Inclinacion = Number

colina :: Inclinacion->Minutos->Ejercicio
colina inclinacion minutos  = bajarPeso (caloriasColina inclinacion minutos)

caloriasColina :: Inclinacion->Minutos->Calorias
caloriasColina inclinacion = (*2).(*inclinacion)
-----

montania :: Inclinacion->Minutos->Ejercicio
montania inclinacion minutos = tonificar 1 . segundaColina inclinacion minutos  . primerColina inclinacion minutos


primerColina :: Inclinacion->Minutos->Ejercicio
primerColina inclinacion minutos  = colina inclinacion (minutos/2) 

segundaColina :: Inclinacion->Minutos->Ejercicio
segundaColina inclinacion minutos  = colina (inclinacion+3) (minutos/2) 
-----

type Ejercicio = Gimnasta->Gimnasta

--4) Rutina de ejercicios


--a) 

data Rutina = UnaRutina {
 name::String 
,duracionTotal:: Number 
, listaEjercicios:: [Ejercicio] 
}deriving(Show)


rutinaExtrema=UnaRutina{
 name= "Extrema"
, duracionTotal = 30
, listaEjercicios  = [caminataEnCinta 30  , entrenamientoEnCinta 30  , pesas 30 30 ,colina 5 30 , montania 4 30 ]
}


rutinaPobre=UnaRutina{
 name= "pobre"
, duracionTotal = 30
, listaEjercicios  = [caminataEnCinta 30  , pesas 30 30 ]
}

---Version fold

hacerEjercicio :: Gimnasta->Ejercicio->Gimnasta
hacerEjercicio  aGimnasta ejerc = ejerc aGimnasta

hacerRutina :: Rutina->Gimnasta->Gimnasta
hacerRutina rutina aGimnasta = foldl hacerEjercicio aGimnasta (listaEjercicios rutina)  --Pensar en aplicacion parcial "hacerEjercicio" para despues usar flip

listaEjer :: Rutina->[Ejercicio]
listaEjer rutina = listaEjercicios rutina



--Version con recursividad



hacerRutina' :: Rutina->Gimnasta->Gimnasta
hacerRutina' rutina aGimnasta =   completarEjerc'  (listaEjercicios rutina) aGimnasta

completarEjerc' :: [Ejercicio]->Gimnasta->Gimnasta
completarEjerc' [] aGimnasta = aGimnasta --Caso en el que no tiene ejercicios XD o los termino todos
completarEjerc' (ejrprimero:ejercicios) aGimnasta =  completarEjerc' ejercicios (hacerEjercicio  aGimnasta (ejrprimero)) 



--b)
type TuplaResultados = (String,Number,Number)

resultados :: Rutina->Gimnasta->TuplaResultados
resultados rutina aGimnasta = (name rutina, pesoPerdido aGimnasta (hacerRutina rutina aGimnasta) ,flip tonificacionGanada aGimnasta (hacerRutina rutina aGimnasta))                  


pesoPerdido :: Gimnasta->Gimnasta->Number
pesoPerdido aGimnasta = (pesooo aGimnasta -).pesooo

pesooo :: Gimnasta->Number
pesooo aGimnasta = peso aGimnasta



tonificacionGanada :: Gimnasta->Gimnasta->Number
tonificacionGanada aGimnasta  = (tonifacioN aGimnasta - ). tonifacioN

tonifacioN :: Gimnasta->Number
tonifacioN aGimnasta = coeficenteTonifacion aGimnasta

--5)

rutinaSaludables :: [Rutina]->Gimnasta->[Rutina]
rutinaSaludables rutina aGimnasta = filter (esSaludable aGimnasta) rutina


esSaludable :: Gimnasta->Rutina->Bool
esSaludable aGimnasta  = estaSaludable . flip hacerRutina aGimnasta
