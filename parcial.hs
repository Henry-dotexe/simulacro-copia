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
sustanciaW :: Sustancia
sustanciaW = ((>20),["decir grrr"])

aplicarSustancia :: Sustancia -> Transformacion
aplicarSustancia sust animal
 | (snd sust == snd sustanciaX) && ((fst sust) (coef_intel animal)) = agregarCapacidad (snd sust) animal
 | (snd sust== snd sustanciaY) && ((fst sust) (coef_intel animal)) = agregarCapacidad (snd sust) animal
 |(snd sust== snd sustanciaW) && ((fst sust) (coef_intel animal)) = agregarCapacidad (snd sust) animal
 |otherwise = animal


--Punto 3, criterios de éxito

agil :: Animal -> Bool
agil animal = elem "correr" (capacidades animal) && 80<(coef_intel animal)

llegoAlIntelecto :: Int -> Animal -> Bool
llegoAlIntelecto n animal = n<=(coef_intel animal)

noTanCuerdo :: Animal -> Bool
noTanCuerdo animal = ((>=2).length.(filter (==True)).(map sonidoRaro).capacidades) animal
--Funciones auxiliares pto 3
empiezaConDecir :: String->Bool
empiezaConDecir = (=="decir ").(take 6)

sinConsonantes :: String->Bool
sinConsonantes palabra = (not.(any (==True)).(map ($palabra))) [elem 'a',elem 'e',elem 'i',elem 'o',elem 'u']

segundaPalabra :: String -> String
segundaPalabra = drop 6

sonidoRaro :: String -> Bool
sonidoRaro sonido = (empiezaConDecir sonido) && ((sinConsonantes.segundaPalabra) sonido)

--Punto 4, experimentos
--type Experimento = [Transformacion]
data Experimento = Experimento {
    transformaciones:: [Transformacion],
    criterio :: (Animal->Bool)
}

hacerExperimento :: Experimento->Animal->Animal
hacerExperimento experim = foldl1 (.) (transformaciones experim)


exp1 = Experimento{
    transformaciones = [superpoderes,(inteligenciaSuperior 10),inutilizar],
    criterio = ((>=32).coef_intel)
}

exp2 = Experimento{
    transformaciones = [aplicarSustancia sustanciaW,superpoderes],
    criterio = noTanCuerdo 
}

exp3 = Experimento{
    transformaciones = [superpoderes],
    criterio = ((elem "no tenerle miedo a los ratones").capacidades)
}

