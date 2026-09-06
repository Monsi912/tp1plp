module TP1 where

data Caja = Bombilla Bool | Nada
              deriving Eq
instance Show Caja where
    show = showDeCaja

showDeCaja :: Caja -> String 
showDeCaja (Bombilla True) = "💡"
showDeCaja (Bombilla False) = "⚪️"
showDeCaja (Nada) = "🛑"

data Circuito = Caja     Caja
              | Serie    Circuito Circuito
              | Paralelo Caja Circuito Circuito Caja
                  deriving Eq
instance Show Circuito where
    show = showDeCircuito

showDeCircuito :: Circuito -> String
showDeCircuito (Caja caja) = showDeCaja caja
showDeCircuito (Serie circuitoInicial circuitoFinal) =
  (showDeCircuito circuitoInicial) ++ "-" ++ (showDeCircuito circuitoFinal)
showDeCircuito (Paralelo caEnt circuitoIzquierdo circuitoDerecho caSal) =
  (showDeCaja caEnt) ++
  "{" ++ (showDeCircuito circuitoIzquierdo) ++ "}" ++
  "{" ++ (showDeCircuito circuitoDerecho) ++ "}" ++
  (showDeCaja caSal)

showDeCircuitoConEstructura :: Circuito -> String
showDeCircuitoConEstructura (Caja caja) = showDeCaja caja
showDeCircuitoConEstructura (Serie circuitoInicial circuitoFinal) = "(" ++
  (showDeCircuitoConEstructura circuitoInicial) ++
    "-" ++
  (showDeCircuitoConEstructura circuitoFinal) ++ ")"
showDeCircuitoConEstructura (Paralelo caEnt circuitoIzquierdo circuitoDerecho caSal) =
  (showDeCaja caEnt) ++
  "{" ++ (showDeCircuitoConEstructura circuitoIzquierdo) ++ "}" ++
  "{" ++ (showDeCircuitoConEstructura circuitoDerecho) ++ "}" ++
  (showDeCaja caSal)

on  = Bombilla True
off = Bombilla False

cajaOn   = Caja on
cajaOff  = Caja off
cajaNada = Caja Nada

-- 1: recCircuito

recCircuito :: 
  (Caja -> b) -> 
  (Circuito -> b -> Circuito -> b -> b) -> 
  (Caja -> Circuito -> b -> Circuito -> b -> Caja -> b) -> 
  Circuito -> 
  b
  
recCircuito fC fS fP (Caja caja) = fC caja
recCircuito fC fS fP (Serie ctIzq ctDer) = fS ctIzq (recCircuito fC fS fP ctIzq) ctDer (recCircuito fC fS fP ctDer)
recCircuito fC fS fP (Paralelo caEnt ctIzq ctDer caSal) = fP caEnt ctIzq (recCircuito fC fS fP ctIzq) ctDer (recCircuito fC fS fP ctDer) caSal

-- 2: foldCircuito

foldCircuito :: 
  (Caja -> b) -> 
  (b -> b -> b) -> 
  (Caja -> b -> b -> Caja -> b) -> 
  Circuito -> 
  b
  
foldCircuito fC fS fP = recCircuito fC
                                    (\ctIzq resIzq ctDer resDer -> fS resIzq resDer)
                                    (\caEnt ctIzq resIzq ctDer resDer caSal -> fP caEnt resIzq resDer caSal)

-- 3 invertido

invertido :: Circuito -> Circuito
invertido circuito = foldCircuito invertirCaja invertirSerie invertirParalelo circuito

invertirCaja :: Caja -> Circuito
invertirCaja caja = Caja caja

invertirSerie :: Circuito -> Circuito -> Circuito
invertirSerie ctIzq ctDer = Serie ctDer ctIzq

invertirParalelo :: Caja -> Circuito -> Circuito -> Caja -> Circuito
invertirParalelo caEnt ctIzq ctDer caSal = Paralelo caSal ctDer ctIzq caEnt

-- 4: hayCaminoIluminado

hayCaminoIluminado :: Circuito -> Bool
hayCaminoIluminado circuito = foldCircuito iluminadoCaja iluminadoSerie iluminadoParalelo circuito

iluminadoCaja :: Caja -> Bool
iluminadoCaja (Bombilla b) = b
iluminadoCaja Nada = False

iluminadoSerie :: Bool -> Bool -> Bool
iluminadoSerie ctIzq ctDer = ctIzq && ctDer

iluminadoParalelo :: Caja -> Bool -> Bool -> Caja -> Bool
iluminadoParalelo caEnt ctIzq ctDer caSal = (iluminadoCaja caEnt && iluminadoCaja caSal) && (ctIzq || ctDer)

-- 5: cantidadPrendidas

cantidadPrendidas :: Circuito -> Int
cantidadPrendidas circuito = foldCircuito prendidasCaja prendidasSerie prendidasParalelo circuito

prendidasCaja :: Caja -> Int
prendidasCaja (Bombilla b) = if b then 1 else 0
prendidasCaja Nada = 0

prendidasSerie :: Int -> Int -> Int
prendidasSerie ctIzq ctDer = ctIzq + ctDer

prendidasParalelo :: Caja -> Int -> Int -> Caja -> Int
prendidasParalelo caEnt ctIzq ctDer caSal = (prendidasCaja caEnt) + ctDer + ctIzq + (prendidasCaja caSal)

-- 6: cajasDeCircuito

cajasDeCircuito :: Circuito -> [Caja]
cajasDeCircuito circuito = foldCircuito listaCaja listaSerie listaParalelo circuito

listaCaja :: Caja -> [Caja]
listaCaja caja = [caja]

listaSerie :: [Caja] -> [Caja] -> [Caja]
listaSerie cajasIzq cajasDer = cajasIzq ++ cajasDer

listaParalelo :: Caja -> [Caja] -> [Caja] -> Caja -> [Caja]
listaParalelo caEnt cajasIzq cajasDer caSal = [caEnt] ++ cajasIzq ++ cajasDer ++ [caSal]

-- 7: esCircuitoProlijo

esCircuitoProlijo :: Circuito -> Bool
esCircuitoProlijo circuito = recCircuito (\caja -> True) esProlijoSerie esProlijoParalelo circuito

esSerie :: Circuito -> Bool
esSerie (Serie _ _) = True
esSerie _ = False

{--
esProlijoSerie ::  (Bool, Bool) -> (Bool, Bool) -> (Bool, Bool)
esProlijoSerie (prolijidadIzq, _) (prolijidadDer, esSerieDer) = (prolijidadIzq && prolijidadDer && not esSerieDer, True)
--}

esProlijoSerie ::  Circuito -> Bool -> Circuito -> Bool -> Bool
esProlijoSerie _ resIzq ramaDer resDer = resIzq && resDer && not (esSerie ramaDer)

esProlijoParalelo :: Caja -> Circuito -> Bool -> Circuito -> Bool -> Caja -> Bool
esProlijoParalelo _ _ resIzq _ resDer _ = resIzq && resDer

-- 8: circuitoEmprolijado NO SE HACE

circuitoEmprolijado = undefined

-- 9: tienenLaMismaEstructura 

tienenLaMismaEstructura :: Circuito -> Circuito -> Bool
tienenLaMismaEstructura c1 c2 = (cajasVacias c1) == (cajasVacias c2)

cajasVacias :: Circuito -> Circuito
cajasVacias circuito = 
  foldCircuito (\_ -> cajaNada) Serie (\_ ramaIzq ramaDer _ -> Paralelo Nada ramaIzq ramaDer Nada) circuito

-- 10: subCircuitoMásResistente

subCircuitoMásResistente :: Circuito -> Circuito
subCircuitoMásResistente circuito = 
  recCircuito id resSerie resParalelo circuito

{--recCircuito :: 
  (Caja -> b) -> 
  (Circuito -> b -> Circuito -> b -> b) -> 
  (Caja -> Circuito -> b -> Circuito -> b -> Caja -> b) -> 
  Circuito -> 
  b

resCaja :: Caja -> Caja
resCaja caja = caja--}

resSerie :: Circuito -> Circuito -> Circuito -> Circuito -> Circuito
resSerie ramaIzq subRamaIzq ramaDer subRamaDer = 
  elDeMayorResistencia (elDeMayorResistencia subRamaIzq subRamaDer) (Serie ramaIzq ramaDer) 

resParalelo :: Caja -> Circuito -> Circuito -> Circuito -> Circuito -> Caja -> Circuito
resParalelo caEnt ramaIzq subRamaIzq ramaDer subRamaDer caSal = 
  elDeMayorResistencia (elDeMayorResistencia subRamaIzq subRamaDer) (Paralelo caEnt ramaIzq ramaDer caSal) 

elDeMayorResistencia :: Circuito -> Circuito -> Circuito
elDeMayorResistencia c1 c2 = if (resistenciaCircuito c1) >= (resistenciaCircuito c2) then c1 else c2



{-- 11: Demostrar: alternado . alternado = id

alternado :: Circuito -> Circuito
{AC} alternado (Caja caja) = Caja (cajaAlternada caja)
{AS} alternado (Serie ci cf) = Serie (alternado ci) (alternado cf)
{AP} alternado (Paralelo ce ci cd cs) =
       Paralelo (cajaAlternada ce) (alternado ci) (alternado cd) (cajaAlternada cs)

cajaAlternada :: Caja -> Caja
{CAN} cajaAlternada Nada = Nada
{CAB} cajaAlternada Bombilla booleano = Bombilla not booleano

(.) :: (b -> c) -> (a -> b) -> a -> c
{C} (f . f) x = f (f x)

id :: a -> a
{I} id x = x

not :: Bool -> Bool
{NT} not True = False
{NF} not False = True

-- TODO: COMPLETAR

--}