module Library where
import PdePreludat


data Relleno = DulceDeLeche | Mousse | Fruta deriving (Show, Eq)
type Peso = Number
type Dulzor = Number
type Nombre = String
type Dinero = Number

data Alfajor = UnAlfajor {
    relleno :: [Relleno],
    peso :: Peso,
    dulzorInnato :: Dulzor,
    nombre :: Nombre
}deriving (Show, Eq)


jorgito = UnAlfajor [DulceDeLeche] 80 8 "Jorgito"

havanna = UnAlfajor [Mousse, Mousse] 60 12 "Havanna"

capitanDelEspacio = UnAlfajor [DulceDeLeche] 40 12 "Capitán del Espacio"

--Parte 1

type Propiedad = Alfajor -> Number

--Propiedad 1

coeficienteDeDulzor :: Propiedad
coeficienteDeDulzor alfajor = div (dulzorInnato alfajor) (peso alfajor)

--Propiedad 2

precioAlfajor :: Propiedad
precioAlfajor alfajor = doblePesoAlfajor alfajor + sumatoriaRelleno alfajor

sumatoriaRelleno ::  Alfajor -> Number
sumatoriaRelleno alfajor = sum (map precioRellenos (relleno alfajor))

doblePesoAlfajor :: Alfajor -> Number
doblePesoAlfajor = (2*) . peso

precioRellenos :: Relleno -> Number
precioRellenos relleno
    | relleno == DulceDeLeche = 12
    | relleno == Mousse = 15
    | relleno == Fruta = 10


--Propiedad 3


tieneCapasRelleno :: Alfajor -> Bool
tieneCapasRelleno = not . null . relleno

todasCapasIguales ::  Alfajor -> Bool
todasCapasIguales alfajor = all (== head (relleno alfajor)) (relleno alfajor)

coefDulzorMayorIgual :: Number -> Alfajor -> Bool
coefDulzorMayorIgual x = (>= x) . dulzorInnato

esPotable :: Alfajor-> Bool
esPotable alfajor = tieneCapasRelleno alfajor && todasCapasIguales alfajor && coefDulzorMayorIgual 0.1 alfajor

--Parte 2

-- Ej a

modificarPeso :: Peso -> Alfajor -> Alfajor
modificarPeso nuevoPeso alfajor = alfajor {peso = peso alfajor + nuevoPeso}

modificarDulzor :: Dulzor -> Alfajor -> Alfajor
modificarDulzor nuevoDulzor alfajor = alfajor {dulzorInnato = dulzorInnato alfajor + nuevoDulzor}

abaratarAlfajor :: Alfajor -> Alfajor
abaratarAlfajor = modificarDulzor (-7) . modificarPeso (-10)


-- Parte b

renombrarAlfajor :: Nombre -> Alfajor -> Alfajor
renombrarAlfajor nuevoNombre alfajor = alfajor{nombre = nuevoNombre}

-- Parte c

agregarCapa :: Relleno-> Alfajor -> Alfajor
agregarCapa nuevoRelleno alfajor = alfajor {relleno = relleno alfajor ++ [nuevoRelleno]}

-- Parte d

obtenerCapaDelMismoTipo :: Alfajor -> Relleno
obtenerCapaDelMismoTipo alfajor = head (relleno alfajor)

agregarNombrePremium :: Alfajor -> Alfajor
agregarNombrePremium alfajor = alfajor {nombre = "Premium " ++ nombre alfajor}

modificarAlfajorPremium :: Alfajor -> Alfajor
modificarAlfajorPremium alfajor = (agregarNombrePremium . agregarCapa (obtenerCapaDelMismoTipo alfajor)) alfajor

hacerPremium :: Alfajor -> Alfajor
hacerPremium alfajor
    |esPotable alfajor = modificarAlfajorPremium alfajor
    |otherwise = alfajor

-- Parte e

hacerPremiumDeCiertoGrado :: Number ->Alfajor -> Alfajor
hacerPremiumDeCiertoGrado 1 alfajor = hacerPremium alfajor
hacerPremiumDeCiertoGrado n  alfajor = hacerPremiumDeCiertoGrado (n-1) (hacerPremium alfajor) 

--Parte f

jorgitito :: Alfajor
jorgitito = abaratarAlfajor jorgito

jorgelin :: Alfajor
jorgelin = (renombrarAlfajor "Jorgelin" . agregarCapa DulceDeLeche) jorgito

capitanCostaACosta :: Alfajor
capitanCostaACosta =  (renombrarAlfajor "Capitan del Espacio Costa a Costa" . hacerPremiumDeCiertoGrado 4 . abaratarAlfajor) capitanDelEspacio

-- Parte 3

--Ej a

data Cliente = UnCliente{
    dinero :: Dinero,
    alfajoresComprados :: [Alfajor],
    criterio :: [Criterio]
} deriving (Show, Eq)

type Criterio = Alfajor -> Bool

emi :: Cliente
emi = UnCliente 120 [] [buscaMarca "Capitan del Espacio"]

tomi :: Cliente
tomi = UnCliente 1000 [] [esPretencioso, esDulcero]

dante :: Cliente
dante = UnCliente 200 [] [noTieneCiertoRelleno DulceDeLeche, esExtraño]

juan :: Cliente
juan = UnCliente 500 [] [esDulcero, buscaMarca "Jorgito", esPretencioso, noTieneCiertoRelleno Mousse]

contieneEnElNombre :: String -> String -> Bool
contieneEnElNombre [] _ = True
contieneEnElNombre _ [] = False
contieneEnElNombre (p:ps) (s:ss)
  | p == s = contieneEnElNombre ps ss
  | otherwise = contieneEnElNombre (p:ps) ss

buscaMarca :: Nombre -> Criterio
buscaMarca marca alfajor = contieneEnElNombre marca (nombre alfajor)

esPretencioso :: Criterio
esPretencioso alfajor = contieneEnElNombre "Premium" (nombre alfajor)

esDulcero :: Criterio
esDulcero = (>0.15) . dulzorInnato

noTieneCiertoRelleno :: Relleno ->Criterio
noTieneCiertoRelleno rellenoBuscado alfajor = not $ elem rellenoBuscado (relleno alfajor)

esExtraño :: Criterio
esExtraño alfajor = not (esPotable alfajor)

--Ej b

type Alfajores = [Alfajor]

leGustaAlfajor :: Cliente -> Alfajor -> Bool
leGustaAlfajor cliente alfajor  = all ($ alfajor) (criterio cliente)

alfajoresQueLeGustanCliente :: Alfajores -> Cliente -> Alfajores
alfajoresQueLeGustanCliente alfajores cliente = filter (leGustaAlfajor cliente) alfajores

--Ej c

comprarAlfajor :: Alfajor -> Cliente -> Cliente
comprarAlfajor alfajor cliente
    |puedeComprarAlfajor alfajor cliente = (modificarDinero alfajor . agregarAlfajorComprado alfajor) cliente
    |otherwise = cliente


agregarAlfajorComprado ::  Alfajor -> Cliente -> Cliente
agregarAlfajorComprado alfajor cliente = cliente {alfajoresComprados = alfajoresComprados cliente ++ [alfajor]}

modificarDinero :: Alfajor -> Cliente -> Cliente
modificarDinero alfajor cliente = cliente {dinero = dinero cliente - precioAlfajor alfajor}

puedeComprarAlfajor ::  Alfajor -> Cliente -> Bool
puedeComprarAlfajor alfajor cliente = dinero cliente >= precioAlfajor alfajor 

--Ej d

comprarAlfajoresGustanCliente ::  Alfajores -> Cliente -> Cliente
comprarAlfajoresGustanCliente alfajores cliente = foldl (flip comprarAlfajor) cliente (alfajoresQueLeGustanCliente alfajores cliente)