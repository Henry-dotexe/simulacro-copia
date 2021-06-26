import Data.List
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
tantor = Animal {
    coef_intel=26,
    especie="Elefante",
    capacidades = []
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
perry = Animal{
    coef_intel=10,
    especie="Ornitorrinco",
    capacidades = ["decir grrr"]
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
 |(especie animal)=="Ornitorrinco" = agregarCapacidad ["ser el mejor espia"] animal
 |otherwise = animal

type Sustancia = ((Int->Bool),[String])
--Sustancias de ejemplo
sustanciaX :: Sustancia
sustanciaX = ((>100),["pensamiento profundo","insomnio"])
sustanciaY ::Sustancia
sustanciaY = ((>20),["soñar"])
--Sustancias del ejercicio
sustanciaW :: Sustancia
sustanciaW = ((>20),["decir grrr"])

aplicarSustancia :: Sustancia -> Transformacion
aplicarSustancia sust animal = if (fst sust) (coef_intel animal) then agregarCapacidad (snd sust) animal else animal

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

sinVocales :: String->Bool
sinVocales palabra = (not.(any (==True)).(map ($palabra))) [elem 'a',elem 'e',elem 'i',elem 'o',elem 'u']

segundaPalabra :: String -> String
segundaPalabra = drop 6

sonidoRaro :: String -> Bool
sonidoRaro sonido = (empiezaConDecir sonido) && ((sinVocales.segundaPalabra) sonido)

--Punto 4, experimentos

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
    criterio = (elem "no tenerle miedo a los ratones").capacidades
}
exp4 = Experimento{
    transformaciones= [superpoderes,inteligenciaSuperior 100],
    criterio = not.noTanCuerdo
}
experimentoExitoso :: Experimento -> Animal -> Bool
experimentoExitoso experim = ((criterio experim).(hacerExperimento experim))

--Punto 5, informes

hacerExperimentoMulti :: [Transformacion]->[Animal]->[Animal]
hacerExperimentoMulti experimento = map (foldl1 (.) experimento)

tieneCapacidad :: [String]->([String]->Bool)->[Animal]->[Animal]
tieneCapacidad listaCapacidades crit = filter (crit.(intersect listaCapacidades).capacidades)

informe1 :: [String]->[Transformacion]->[Animal]->[Int]
informe1 listaCapacidades experimento = (map coef_intel).(tieneCapacidad listaCapacidades ((>0).length)).(hacerExperimentoMulti experimento)

informe2 :: [String]->[Transformacion]->[Animal]->[String]
informe2 listaCapacidades experimento = (map especie).(tieneCapacidad listaCapacidades (==listaCapacidades)).(hacerExperimentoMulti experimento)

informe3 :: [String]->[Transformacion]->[Animal]->[Int]
informe3 listaCapacidades experimento = (map length).(map capacidades).(tieneCapacidad listaCapacidades (==[])).(hacerExperimentoMulti experimento)

caso1 :: [Int]
caso1 = informe1 ["hablar","ser el mejor espia","no tenerle miedo a los ratones"] [superpoderes,inteligenciaSuperior 30] [remy,perry,lola]
--Punto 6
decirGInfinito :: String
decirGInfinito = "decir gggg" ++ cycle "g"
animalInfinito = Animal{
    coef_intel=30,
    especie="Desconocido",
    capacidades=[decirGInfinito]
}

exp5 = Experimento{
    transformaciones=[inutilizar,inteligenciaSuperior 10],
    criterio=((>30).coef_intel)
}

exp6 = Experimento{
    transformaciones = [agregarCapacidad ["decir muuu"]],
    criterio= noTanCuerdo
}
--Este caso no funciona ya que noTanCuerdo funciona al operar con la palabra o letras restantes de las capacidades que empiezan con decir
--Ya que la capacidad del animal infinita, evalua letras hasta el infinito. Se puede solucionar utilizando onomatopeyas de longitud fija, lo que
--cortaría la evaluación del string.
casoExperimentoQueNoFunciona :: Bool
casoExperimentoQueNoFunciona = experimentoExitoso exp6 animalInfinito

--Este caso funciona ya que elimina la capacidad infinita del animal, es básicamente un caso trivial de éxito
casoExperimentoQueFunciona :: Bool
casoExperimentoQueFunciona = experimentoExitoso exp5 animalInfinito



