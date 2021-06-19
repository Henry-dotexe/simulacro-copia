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
    coef_intel=20,
    especie = "Perro",
    capacidades = ["sentarse","comer caca"]
}

misifu = Animal {
    coef_intel=99,
    especie="Gato",
    capacidades = ["ser maligno","correr","romper cosas"]
}

toto = Animal {
    coef_intel = 15,
    especie = "Perico",
    capacidades = ["volar","hablar","decir qwrt","decir zxcv"]
}
dumbo = Animal {
    coef_intel=35,
    especie= "Elefante",
    capacidades = ["volar","derribar arboles"]
}

remy = Animal {
    coef_intel=101,
    especie = "Raton",
    capacidades = ["cocinar","buen gusto"]
}
ratonX = Animal {
    coef_intel=17,
    especie="Raton",
    capacidades=["destruir el mundo","hacer planes desalmados"]
}
lola = Animal {
    coef_intel=32,
    especie="Vaca",
    capacidades= ["decir muuu"]
}
tantor = Animal {
    coef_intel=26,
    especie="Elefante",
    capacidades = []
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

type Sustancia = ((Int->Bool),[String])
sustanciaX :: Sustancia
sustanciaX = ((>100),["pensamiento profundo","insomnio"])
sustanciaY ::Sustancia
sustanciaY = ((>20),["soñar"])

aplicarSustancia :: Sustancia -> Transformacion
aplicarSustancia sust animal
 | (snd sust == snd sustanciaX) && ((fst sust) (coef_intel animal)) = agregarCapacidad (snd sust) animal
 | (snd sust== snd sustanciaY) && ((fst sust) (coef_intel animal)) = agregarCapacidad (snd sust) animal
 |otherwise = animal


--Punto 3, criterios de éxito

agil :: Animal -> Bool
agil animal = elem "correr" (capacidades animal) && 80<(coef_intel animal)

llegoAlIntelecto :: Int -> Animal -> Bool
llegoAlIntelecto n animal = n<=(coef_intel animal)

--sonidoRaro :: String -> Bool
--sonidoRaro sonido = 

--noTancuerdo animal = 2<(length.(filter (sonidoRaro) (capacidades animal)))

--Punto 4, experimentos
--type Experimento = [Transformaciones]->Animal->Animal

--experimentoExitoso exp animal = 
hacerExperimento :: [Transformacion]->Animal->Animal
hacerExperimento [transformaciones] animal= (foldl1 (.) [transformaciones]) animal

listaTransf :: [Transformacion]
listaTransf = [inutilizar,(inteligenciaSuperior 20)]
