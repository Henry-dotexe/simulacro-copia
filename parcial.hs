{-
Nombre: Soto, Henry
Legajo: 177.160-7
-}
-- Punto 1, modelado de animal
data Animal = Animal {
    coef_intel :: Int,
    especie :: String,
    capacidades :: [String]
}deriving(Show) 

maximus = Animal {
    coef_intel=10,
    especie = "Perro",
    capacidades = ["sentarse"]
}

dumbo = Animal {
    coef_intel=25,
    especie= "Elefante",
    capacidades = ["volar"]
}

remy = Animal {
    coef_intel=101,
    especie = "Raton",
    capacidades = ["cocinar"]
}
type Transformacion = Animal ->Animal
--Punto 2, transformar animales
inteligenciaSuperior :: Int->Transformacion
inteligenciaSuperior n animal = animal{
    coef_intel= coef_intel animal + n
} 
inutilizar :: Transformacion
inutilizar animal = animal {
    capacidades=[]
}
agregarCapacidad :: [String] -> Transformacion
agregarCapacidad nuevaCapacidad animal = animal{ capacidades = (capacidades animal)++nuevaCapacidad}

superpoderes :: Transformacion
superpoderes animal 
 |(especie animal)=="Elefante" = agregarCapacidad ["no tenerle miedo a los ratones"] animal
 |(especie animal)=="Raton" = agregarCapacidad ["hablar"] animal
 |otherwise = animal

type Sustancia = String

sustancia :: Sustancia -> Transformacion
sustancia sust animal
 | (sust == "sustanciaX") && ((coef_intel animal)>=100) = agregarCapacidad ["pensamiento profundo","insomnio"] animal
 | (sust== "sustanciaY") && ((coef_intel animal)>=20) = agregarCapacidad ["soñar"] animal
 |otherwise = animal

--Punto 3, criterios de éxito

agil :: Animal -> Bool
agil animal = elem "correr" (capacidades animal) && 80<(coef_intel animal)

llegoAlIntelecto :: Int -> Animal -> Bool
llegoAlIntelecto n animal = n<=(coef_intel animal)
