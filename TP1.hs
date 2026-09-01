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
showDeCircuito (Paralelo cajaEntrada circuitoIzquierdo circuitoDerecho cajaSalida) =
  (showDeCaja cajaEntrada) ++
  "{" ++ (showDeCircuito circuitoIzquierdo) ++ "}" ++
  "{" ++ (showDeCircuito circuitoDerecho) ++ "}" ++
  (showDeCaja cajaSalida)

showDeCircuitoConEstructura :: Circuito -> String
showDeCircuitoConEstructura (Caja caja) = showDeCaja caja
showDeCircuitoConEstructura (Serie circuitoInicial circuitoFinal) = "(" ++
  (showDeCircuitoConEstructura circuitoInicial) ++
    "-" ++
  (showDeCircuitoConEstructura circuitoFinal) ++ ")"
showDeCircuitoConEstructura (Paralelo cajaEntrada circuitoIzquierdo circuitoDerecho cajaSalida) =
  (showDeCaja cajaEntrada) ++
  "{" ++ (showDeCircuitoConEstructura circuitoIzquierdo) ++ "}" ++
  "{" ++ (showDeCircuitoConEstructura circuitoDerecho) ++ "}" ++
  (showDeCaja cajaSalida)

on  = Bombilla True
off = Bombilla False

cajaOn   = Caja on
cajaOff  = Caja off
cajaNada = Caja Nada

-- 1: recCircuito

recCircuito :: (Caja -> b) -> (Circuito -> b -> Circuito -> b -> b) -> (Caja -> Circuito -> b -> Circuito -> b -> Caja -> b) -> Circuito -> b
recCircuito f1 f2 f3 (Caja caja) = f1 caja
recCircuito f1 f2 f3 (Serie circuitoIzq circuitoDer) = f2 circuitoIzq (recCircuito f1 f2 f3 circuitoIzq) circuitoDer (recCircuito f1 f2 f3 circuitoDer)
recCircuito f1 f2 f3 (Paralelo cajaEntrada circuitoIzq circuitoDer cajaSalida) = f3 cajaEntrada circuitoIzq (recCircuito f1 f2 f3 circuitoIzq) circuitoDer (recCircuito f1 f2 f3 circuitoDer) cajaSalida

-- 2: foldCircuito

foldCircuito :: (Caja -> b) -> (b -> b -> b) -> (Caja -> b -> b -> Caja -> b) -> Circuito -> b
foldCircuito f1 f2 f3 = recCircuito f1
                                    (\circuitoIzq resIzq circuitoDer resDer -> f2 resIzq resDer)
                                    (\cajaEntrada circuitoIzq resIzq circuitoDer resDer cajaSalida -> f3 cajaEntrada resIzq resDer cajaSalida)

-- 3 invertido

invertido :: Circuito -> Circuito
invertido circuito = foldCircuito invertirCaja invertirSerie invertirParalelo circuito

invertirCaja :: Caja -> Circuito
invertirCaja caja = Caja caja

invertirSerie :: Circuito -> Circuito -> Circuito
invertirSerie circuitoIzq circuitoDer = Serie circuitoDer circuitoIzq

invertirParalelo :: Caja -> Circuito -> Circuito -> Caja -> Circuito
invertirParalelo cajaEntrada circuitoIzq circuitoDer cajaSalida = Paralelo cajaSalida circuitoDer circuitoIzq cajaEntrada

-- 4: hayCaminoIluminado

hayCaminoIluminado :: Circuito -> Bool
hayCaminoIluminado circuito = foldCircuito iluminadoCaja iluminadoSerie iluminadoParalelo circuito

iluminadoCaja :: Caja -> Bool
iluminadoCaja (Bombilla b) = b
iluminadoCaja Nada = False

iluminadoSerie :: Bool -> Bool -> Bool
iluminadoSerie circuitoIzq circuitoDer = circuitoIzq && circuitoDer

iluminadoParalelo :: Caja -> Bool -> Bool -> Caja -> Bool
iluminadoParalelo cajaEntrada circuitoIzq circuitoDer cajaSalida = (iluminadoCaja cajaEntrada && iluminadoCaja cajaSalida) && (circuitoIzq || circuitoDer)

-- 5: cantidadPrendidas

cantidadPrendidas :: Circuito -> Int
cantidadPrendidas circuito = foldCircuito prendidasCaja prendidasSerie prendidasParalelo circuito

prendidasCaja :: Caja -> Int
prendidasCaja (Bombilla b) = if b then 1 else 0
prendidasCaja Nada = 0

prendidasSerie :: Int -> Int -> Int
prendidasSerie circuitoIzq circuitoDer = circuitoIzq + circuitoDer

prendidasParalelo :: Caja -> Int -> Int -> Caja -> Int
prendidasParalelo cajaEntrada circuitoIzq circuitoDer cajaSalida = (prendidasCaja cajaEntrada) + circuitoDer + circuitoIzq + (prendidasCaja cajaSalida)

-- 6: cajasDeCircuito

cajasDeCircuito = undefined -- TODO: COMPLETAR

-- 7: esCircuitoProlijo

esCircuitoProlijo = undefined -- TODO: COMPLETAR

-- 8: circuitoEmprolijado

circuitoEmprolijado = undefined -- TODO: COMPLETAR

-- 9: tienenLaMismaEstructura 

tienenLaMismaEstructura = undefined -- TODO: COMPLETAR

-- 10: subCircuitoMásResistente

subCircuitoMásResistente = undefined -- TODO: COMPLETAR

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