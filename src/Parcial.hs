module Parcial where
import Text.Show.Functions()

-- DATAS

data Perrito = Perrito {
    raza :: String,
    juguetesFav :: [String],
    tiempoDePermanencia :: Int,
    energia :: Int
} deriving (Show)

data Actividad = Actividad {
    ejercicio :: Perrito -> Perrito,
    tiempoDeActividad :: Int
} deriving (Show)

data Guarderia = Guarderia {
    nombre :: String,
    rutina :: [Actividad]
} deriving (Show)

-- FUNCIONES USADAS POR LAS FUNCIONES PRINCIPALES

modificarEnergia :: Int -> Perrito -> Perrito
modificarEnergia energiaAAniadir unPerro = unPerro{energia = max 0 . (+ energiaAAniadir) . energia $ unPerro}

calcularEnergiaParaQueSea100 :: Perrito -> Int
calcularEnergiaParaQueSea100 unPerro = (100-) . energia $ unPerro

razaExtravaganteO50OMasMinutos :: Perrito -> Bool
razaExtravaganteO50OMasMinutos unPerro = esDeRaza "dalmata" unPerro || esDeRaza "pomerania" unPerro || tiempoMayoroIguala 50 unPerro

esDeRaza :: String -> Perrito -> Bool
esDeRaza laRaza unPerro = (==laRaza) . raza $ unPerro

tiempoMayoroIguala :: Int -> Perrito -> Bool
tiempoMayoroIguala unosMinutos unPerro = (>= unosMinutos) . tiempoDePermanencia $ unPerro

tieneMasDeXJuguetes :: Int -> Perrito -> Bool
tieneMasDeXJuguetes unaCantidadDeJuguetes unPerro = (>unaCantidadDeJuguetes) . length . juguetesFav $ unPerro

sacarPrimerJuguete :: Perrito -> Perrito
sacarPrimerJuguete unPerro = unPerro{juguetesFav = drop 1 . juguetesFav $ unPerro}

tiempoDeRutina :: [Actividad] -> Int
tiempoDeRutina unaRutina = sum . map tiempoDeActividad $ unaRutina

-- FUNCIONES PRINCIPALES

jugar :: Perrito -> Perrito
jugar unPerro = modificarEnergia (-10) unPerro

ladrar :: Int -> Perrito -> Perrito
ladrar cantidadLadridos unPerro = modificarEnergia (div cantidadLadridos 2) unPerro

regalar :: String -> Perrito -> Perrito
regalar unJuguete unPerro = unPerro{juguetesFav = (++[unJuguete]) . juguetesFav $ unPerro}

diaDeSpa :: Perrito -> Perrito
diaDeSpa unPerro
    | razaExtravaganteO50OMasMinutos unPerro = regalar "peine de goma" . modificarEnergia (calcularEnergiaParaQueSea100 unPerro) $ unPerro
    | otherwise = unPerro

diaDeCampo :: Perrito -> Perrito
diaDeCampo unPerro = sacarPrimerJuguete . jugar $ unPerro

puedeEstarEnGuarderia :: Perrito -> Guarderia -> Bool
puedeEstarEnGuarderia unPerro unaGuarderia = (>(tiempoDeRutina . rutina $ unaGuarderia)) . tiempoDePermanencia $ unPerro

esPerroResponsable :: Perrito -> Bool
esPerroResponsable unPerro = (tieneMasDeXJuguetes 3) . diaDeCampo $ unPerro

-- MODELOS

zara :: Perrito
zara = Perrito{
    raza = "dalmata",
    juguetesFav = ["pelota","mantita"],
    tiempoDePermanencia = 90,
    energia = 80
}

guarderiaPdePerritos :: Guarderia
guarderiaPdePerritos = Guarderia{
    nombre = "GuarderiaPdePerritos",
    rutina = [Actividad{ejercicio = jugar, tiempoDeActividad = 30},
    Actividad{ejercicio = ladrar 18, tiempoDeActividad = 20},
    Actividad{ejercicio = regalar "pelota", tiempoDeActividad = 0},
    Actividad{ejercicio = diaDeSpa, tiempoDeActividad = 120},
    Actividad{ejercicio = diaDeCampo, tiempoDeActividad = 720}]
}
