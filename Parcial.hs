module Parcial where
import Text.Show.Functions()

data Perrito = Perrito {
    raza :: String,
    juguetesFav :: [String],
    tiempoDePermanencia :: Int,
    energia :: Int
} deriving (Show)

modificarEnergia :: Int -> Perrito -> Perrito
modificarEnergia energiaAAniadir unPerro = unPerro{energia = max 0 . (+ energiaAAniadir) . energia $ unPerro}

jugar :: Perrito -> Perrito
jugar unPerro = modificarEnergia -10 unPerro

ladrar :: Int -> Perrito -> Perrito
ladrar cantidadLadridos unPerro = modificarEnergia (div cantidadLadridos 2) unPerro

regalar :: String -> Perrito -> Perrito
regalar unJuguete unPerro = unPerro{juguetesFav = (:) unJuguete . juguetesFav $ unPerro}

diaDeSpa :: Perrito -> Perrito
diaDeSpa unPerro
    | razaExtravaganteO50OMasMinutos unPerro = regalar "peine de goma" . modificarEnergia 100-(energia unPerro) $ unPerro
    | otherwise = unPerro

razaExtravaganteO50OMasMinutos :: Perrito -> Bool
razaExtravaganteO50OMasMinutos unPerro = esDeRaza "dálmata" unPerro || esDeRaza "pomerania" unPerro || tiempoMayoroIguala 50 unPerro 

esDeRaza :: String -> Perrito -> Bool
esDeRaza laRaza unPerro = (==laRaza) . raza $ unPerro

tiempoMayoroIguala :: Int -> Perrito -> Bool
tiempoMayoroIguala unosMinutos unPerro = (>= unosMinutos) . tiempoDePermanencia $ unPerro  

diaDeCampo :: Perrito -> Perrito
diaDeCampo unPerro = sacarPrimerJuguete . jugar $ unPerro

sacarPrimerJuguete :: Perrito -> Perrito
sacarPrimerJuguete unPerro = unPerro{juguetesFav = drop 1 . juguetesFav $ unPerro}

zara :: Perrito
zara = Perrito{
    raza = "dálmata",
    juguetesFav = ["pelota","mantita"],
    tiempoDePermanencia = 90,
    energia = 80
}

data Rutina = Rutina {
    ejercicio :: Perrito -> Perrito,
    tiempoDeRutina :: Int
} deriving (Show)

guarderiaPdePerritos :: [Rutina]
guarderiaPdePerritos = [rutinajugar
    Rutina{
        ejercicio = jugar,
        tiempoDeRutina = 30
    },
    Rutina{ejercicio = ladrar 18, tiempoDeRutina = 20},
    Rutina{ejercicio = regalar "pelota", tiempoDeRutina = 0}
    Rutina{ejercicio = diaDeSpa, tiempoDeRutina = 120}
    Rutina{ejercicio = diaDeCampo, tiempoDeRutina = 720}]

puedeEstarEnGuarderia :: Int -> Perrito -> Bool
puedeEstarEnGuarderia tiempoDeRutinaTotal unPerro = (>tiempoDeRutinaTotal) . tiempoDePermanencia $ unPerro 

tiempoDeRutinaTotal :: [Rutina] -> Int
tiempoDeRutinaTotal unaRutinaEntera = sum . map tiempoDeRutina $ unaRutinaEntera

esPerroResponsable :: Perrito -> Bool
esPerroResponsable unPerro = tieneMasDe3Juguetes . diaDeCampo $ unPerro

tieneMasDe3Juguetes :: Perrito -> Bool
tieneMasDe3Juguetes unPerro = (>3) . length . juguetesFav $ unPerro