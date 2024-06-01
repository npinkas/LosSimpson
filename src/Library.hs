module Library where
import PdePreludat

type Nombre = String
type Dinero = Number
type Felicidad = Number

data Personaje = UnPersonaje{
    nombre :: Nombre,
    dinero :: Dinero,
    felicidad :: Felicidad
}deriving (Show, Eq)

lisa :: Personaje
lisa = UnPersonaje "Lisa" 200 10

homero :: Personaje
homero = UnPersonaje "Homero" 120 15

skinner :: Personaje
skinner = UnPersonaje "Skinner" 200 20

srBurns :: Personaje
srBurns = UnPersonaje "Sr Burns" 400 32

--Parte 1

type Actividad = Personaje -> Personaje

irEscuelaElementalSpringfield :: Actividad
irEscuelaElementalSpringfield personaje
    |nombre personaje == "Lisa" = modificarFelicidad 20 lisa
    |otherwise = modificarFelicidad (-20) personaje

comerCiertaCantDonas :: Number -> Actividad
comerCiertaCantDonas cantDonas = modificarDinero 10 . modificarNCantVeces (modificarFelicidad 10) cantDonas 

irATrabajarPlantaNuclear :: Actividad
irATrabajarPlantaNuclear = modificarDinero 14 

irATrabajarDirector :: Actividad
irATrabajarDirector = irEscuelaElementalSpringfield

empezarUniversidad :: Actividad
empezarUniversidad = modificarFelicidad 20 . modificarNombre "Estudiante " 

modificarFelicidad ::Felicidad -> Personaje -> Personaje
modificarFelicidad x personaje = personaje {felicidad = felicidad personaje + x}

modificarDinero :: Dinero -> Personaje -> Personaje
modificarDinero dineroNuevo personaje = personaje {dinero = dinero personaje + dineroNuevo}

modificarNCantVeces :: (Personaje -> Personaje) -> Number -> Personaje -> Personaje
modificarNCantVeces _ 0 personaje = personaje
modificarNCantVeces prop n personaje = modificarNCantVeces prop (n - 1) (prop personaje)

modificarNombre :: Nombre -> Personaje -> Personaje
modificarNombre nuevoNombre personaje = personaje {nombre = nuevoNombre ++ nombre personaje}

{-

-homero come una docena de donas

ghci> comerCiertaCantDonas 12 homero  
UnPersonaje {nombre = "Homero", dinero = 130, felicidad = 165}

- skinner va a trabajar como director

ghci> irATrabajarDirector skinner 
UnPersonaje {nombre = "Skinner", dinero = 200, felicidad = 0}

lisa va a la escuela y luego realiza la actividad inventada

ghci> empezarUniversidad (irEscuelaElementalSpringfield lisa)
UnPersonaje {nombre = "Estudiante Lisa", dinero = 200, felicidad = 50}

-}

--Parte 2

type Logro = Personaje -> Bool

serMillonario :: Logro
serMillonario = (> dinero srBurns) . dinero

alegrarse :: Number ->  Logro
alegrarse felicidadDeseada = (> felicidadDeseada) . felicidad 

verProgramaKrosti :: Logro
verProgramaKrosti = (>= 10) . dinero

enEquilibrio :: Logro
enEquilibrio personaje = dinero personaje == felicidad personaje

-- Parte 3

esDecisiva :: Logro -> Actividad  -> Personaje -> Bool
esDecisiva logro actividad personaje 
    |not (logro personaje) = logro (actividad personaje)
    |otherwise = False 

-- Parte 4

aplicarActDecisiva :: Logro -> [Actividad]  -> Personaje -> Personaje
aplicarActDecisiva logro actividades personaje = (primerActividadDecisiva logro actividades personaje) personaje

primerActividadDecisiva ::  Logro -> [Actividad]  -> Personaje -> Actividad
primerActividadDecisiva _ [] personaje = id
primerActividadDecisiva logro (x : xs) personaje
    |esDecisiva logro x  personaje = x
    |otherwise = primerActividadDecisiva logro xs personaje

-- Parte 5

actividadesInfinitas :: Actividad -> [Actividad]
actividadesInfinitas actividad = actividad : actividadesInfinitas actividad

{-
En caso que haya una actividad que sea decisiva, Haskell no evaluará el resto de la lista ya que opera con Lazy Evaluation. Por ende, cuando encuentre una actividad que cumpla la condicion, la devolverá sin necesidad de evaluar el resto.

En cambio, si no hay ninguna actividad decisiva, Haskell continuará recorriendo la lista hasta generar un stack overflow.

-}