module Library where
import PdePreludat

-- Una empresa de turismo localizada en una isla que nadie sabe dónde está nos pide construir un software que los ayude a 
-- sacar estadísticas de los tours que ofrece a sus clientes: los turistas…
-- De cada turista nos interesa:
--  a) Sus niveles de cansancio y stress
--  b) Si está viajando solo
--  c) Los idiomas que habla
type Idioma = String
-- type Espaniol = Idioma
-- type Catalan = Idioma
-- type Aleman = Idioma

data Turista = Turista {
  cansancio :: Number,
  stress :: Number,
  viajaSolo :: Bool,
  idiomas :: [Idioma]
} deriving (Show)

type Excursion = Turista -> Turista

-- La isla contiene varias excursiones para los turistas, por ahora nos pidieron modelar estas:

-- *  Ir a la playa: si está viajando solo baja el cansancio en 5 unidades, si no baja el stress 1 unidad.
irALaPlaya :: Excursion
irALaPlaya turista 
  | viajaSolo turista = modificarCansancioTurista 5 turista
  | otherwise = modificarStressTurista 1 turista

modificarCansancioTurista :: Number -> Excursion
modificarCansancioTurista nivel turista = turista{
  cansancio = cansancio turista - nivel
}

modificarStressTurista :: Number -> Excursion
modificarStressTurista nivel turista = turista {
  stress = stress turista - nivel
}
-- *  Apreciar algún elemento del paisaje: reduce el stress en la cantidad de letras de lo que se aprecia. 
apreciarAlgunElemento :: String -> Excursion
apreciarAlgunElemento elemento turista = modificarStressTurista (length elemento) turista
-- *  Salir a hablar un idioma específico: el turista termina aprendiendo dicho idioma y continúa el viaje acompañado.
salirHablarIdioma :: Idioma -> Excursion
salirHablarIdioma idioma turista = modificarAcompanianteTurista True (modificarIdiomaTurista idioma turista)

modificarAcompanianteTurista :: Bool -> Excursion
modificarAcompanianteTurista estado turista = turista {
  viajaSolo = estado
}

modificarIdiomaTurista :: Idioma -> Excursion
modificarIdiomaTurista idioma turista = turista {
  idiomas = idiomas turista ++ [idioma]
}
-- *  Caminar ciertos minutos: aumenta el cansancio pero reduce el stress según la intensidad de la caminad, 
--    ambos en la misma cantidad. El nivel de intensidad se calcula en 1 unidad cada 4 minutos que se caminen.
caminarCiertosMinutos :: Number -> Excursion
caminarCiertosMinutos minutos turista = modificarCansancioTurista ((div minutos 4) * (-1)) (modificarStressTurista (div minutos 4) turista)
-- *  Paseo en barco: depende de cómo esté la marea
--    - si está fuerte, aumenta el stress en 6 unidades y el cansancio en 10.
--    - si está moderada, no pasa nada.
--    - si está tranquila, el turista camina 10’ por la cubierta, aprecia la vista del “mar”, y sale a hablar con los tripulantes alemanes.
type Marea = String
mareaFuerte :: Marea
mareaFuerte = "Fuerte"
mareaModerada :: Marea
mareaModerada = "Moderada"
mareaTranquila :: Marea
mareaTranquila = "Tranquila"
paseoEnBarco :: Marea -> Excursion
paseoEnBarco marea turista
  | marea == mareaFuerte    = modificarStressTurista (-6) (modificarCansancioTurista (-10) turista)
  | marea == mareaModerada  = turista
  | otherwise               = caminarCiertosMinutos 10 turista

-- Nos avisaron que es común que, cada cierto tiempo, se vayan actualizando las excursiones que ofrecen, en base a las nuevas demandas que surgen en el mercado turístico. 

-- Se pide
-- 1) Crear un modelo para los turistas y crear los siguientes tres ejemplos:
--  - Ana: está acompañada, sin cansancio, tiene 21 de stress y habla español.
--  - Beto y Cathi, que hablan alemán, viajan solos, y Cathi además habla catalán. Ambos tienen 15 unidades de cansancio y stress.

ana = Turista {
  cansancio = 0,
  stress = 21,
  viajaSolo = False,
  idiomas = ["Espaniol"]
}

beto = Turista {
  cansancio = 15,
  stress = 15,
  viajaSolo = True,
  idiomas = ["Alemán"]
}

cathi = Turista {
  cansancio = 15,
  stress = 15,
  viajaSolo = True,
  idiomas = ["Alemán","Catalán"]
}

-- 2) Modelar las excursiones anteriores de forma tal que para agregar una excursión al sistema no haga falta modificar las funciones existentes. Además:
--    a)  Hacer que un turista haga una excursión. Al hacer una excursión, el turista además de sufrir los efectos propios de la excursión, reduce en un 10% su stress.
hacerExcursion :: Excursion -> Turista -> Turista
hacerExcursion excursion turista = modificarStressTurista (0.10 * (stress turista)) (excursion turista)

--    b)  Dada la función
deltaSegun :: (a -> Number) -> a -> a -> Number
deltaSegun f algo1 algo2 = f algo1 - f algo2
--    Definir la función deltaExcursionSegun que a partir de un índice, un turista y una excursión determine cuánto varió dicho índice después de que 
--    el turista haya hecho la excursión. Llamamos índice a cualquier función que devuelva un número a partir de un turista.
--      Por ejemplo, si “stress” es la función que me da el stress de un turista:
--      > deltaExcursionSegun stress ana irALaPlaya
--      -3     -- porque al ir a la playa Ana queda con 18 de estrés (21 menos 1 menos 10% de 20)
deltaExcursionSegun :: (Turista -> Number) -> Turista -> Excursion -> Number
deltaExcursionSegun indice turista excursion = deltaSegun indice (hacerExcursion excursion turista) turista

--    c)  Usar la función anterior para resolver cada uno de estos puntos:
--        i) Saber si una excursión es educativa para un turista, que implica que termina aprendiendo algún idioma.
excursionEducativa :: Turista -> Excursion -> Bool
excursionEducativa turista = (/= 0).deltaExcursionSegun (length.idiomas) turista

--        ii) Conocer las excursiones desestresantes para un turista. Estas son aquellas que le reducen al menos 3 unidades de stress al turista.
excursionDesestresante :: Turista -> [Excursion] -> [Excursion]
excursionDesestresante turista = filter (esDesestresante turista)

esDesestresante :: Turista -> Excursion -> Bool
esDesestresante turista = (<= -3) . deltaExcursionSegun stress turista

-- 3) Para mantener a los turistas ocupados todo el día, la empresa vende paquetes de excursiones llamados tours. Un tour se compone por una serie de excursiones.
type Tour = [Excursion]
--  * Completo: Comienza con una caminata de 20 minutos para apreciar una "cascada", luego se camina 40 minutos hasta una playa, y 
--    finaliza con una salida con gente local que habla "melmacquiano".
tourCompleto :: Tour
tourCompleto = [caminarCiertosMinutos 20, apreciarAlgunElemento "cascada",caminarCiertosMinutos 40, irALaPlaya,salirHablarIdioma "melmacquiano"]
--  * Lado B: Este tour consiste en ir al otro lado de la isla a hacer alguna excursión (de las existentes) que elija el turista. 
--    Primero se hace un paseo en barco por aguas tranquilas (cercanas a la costa) hasta la otra punta de la isla, luego realiza la excursión elegida
--    y finalmente vuelve caminando hasta la otra punta, tardando 2 horas.
tourLadoB :: Excursion -> Tour
tourLadoB excursion = [paseoEnBarco mareaTranquila, excursion, caminarCiertosMinutos 120]

--  * Isla Vecina: Se navega hacia una isla vecina para hacer una excursión. Esta excursión depende de cómo esté la marea al llegar a la otra isla: 
--    si está fuerte se aprecia un "lago", sino se va a una playa. En resumen, este tour implica hacer un paseo en barco hasta la isla vecina, 
--    luego llevar a cabo dicha excursión, y finalmente volver a hacer un paseo en barco de regreso. La marea es la misma en todo el camino.
tourIslaVecina :: Marea -> Tour
tourIslaVecina marea = [paseoEnBarco marea, ejecutarExcursion marea, paseoEnBarco marea]

ejecutarExcursion :: Marea -> Excursion
ejecutarExcursion marea 
  | marea == mareaFuerte  = hacerExcursion (apreciarAlgunElemento "Lago")
  | otherwise             = hacerExcursion irALaPlaya

-- Modelar los tours para:

--  A) Hacer que un turista haga un tour. Esto implica, primero un aumento del stress en tantas unidades como cantidad de excursiones tenga el tour, 
--     y luego realizar las excursiones en orden.
hacerTour :: Turista -> Tour -> Turista
hacerTour turista tours = foldl (flip hacerExcursion) (modificarStressTurista (length tours * (-1)) turista) tours
--  B) Dado un conjunto de tours, saber si existe alguno que sea convincente para un turista. Esto significa que el tour tiene alguna excursión
--     desestresante la cual, además, deja al turista acompañado luego de realizarla.
tourConveniente :: Turista -> [Tour] -> Bool
tourConveniente turista = any (esConveniente turista)

esConveniente :: Turista -> Tour -> Bool
esConveniente turista = any (dejaAcompañado turista) . excursionDesestresante turista

dejaAcompañado :: Turista -> Excursion -> Bool
dejaAcompañado turista = not . viajaSolo . flip hacerExcursion turista 

--  C) Saber la efectividad de un tour para un conjunto de turistas. Esto se calcula como la sumatoria de la espiritualidad recibida de cada turista 
--     a quienes les resultó convincente el tour. 
--     La espiritualidad que recibe un turista es la suma de las pérdidas de stress y cansancio tras el tour.
saberEfectividad :: Tour -> [Turista] -> Number
saberEfectividad tour = sum.map (sumatoriaEspiritualidad tour) . filter (flip esConveniente tour) 
--efectividad tour = sum . map (espiritualidadAportada tour) . filter (flip esConvincente tour)

sumatoriaEspiritualidad :: Tour -> Turista -> Number
sumatoriaEspiritualidad tour turista = deltaSegun stress (hacerTour turista tour) turista  + deltaSegun cansancio (hacerTour turista tour) turista

-- deltaRutina :: Tour -> Turista -> Int
-- deltaRutina tour turista =
--   deltaSegun nivelDeRutina (hacerTour turista tour) turista

-- nivelDeRutina :: Turista -> Int
-- nivelDeRutina turista = cansancio turista + stress turista

-- 4) Implementar y contestar en modo de comentarios o pruebas por consola
--    A) Construir un tour donde se visiten infinitas playas.
tourInfinito :: Tour 
tourInfinito = irALaPlaya : repeat irALaPlaya
--    B) ¿Se puede saber si ese tour es convincente para Ana? ¿Y con Beto? Justificar.
{-
Para Ana sí porque la primer actividad ya es desestresante y siempre está acompañada.
Con Beto no se cumple ninguna de las 2 condiciones y el algoritmo diverge.
-}
--    C) ¿Existe algún caso donde se pueda conocer la efectividad de este tour? Justificar.
{-
No, solamente funciona para el caso que se consulte con una lista vacía de turista, que dará siempre 0.
-}