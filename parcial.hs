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
superpoderes :: Transformacion
superpoderes animal 
 |(especie animal)=="Elefante" = animal { capacidades = (capacidades animal)++["sin miedo a los ratones"]}
 |(especie animal)=="Raton" = animal { capacidades = (capacidades animal)++["hablar"]}
 |otherwise = animal
