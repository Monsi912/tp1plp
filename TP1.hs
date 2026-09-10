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
invertido = foldCircuito Caja (flip Serie) (\e i d s -> Paralelo s d i e)

-- 4: hayCaminoIluminado

hayCaminoIluminado :: Circuito -> Bool
hayCaminoIluminado = foldCircuito iluminadoCaja (&&) (\e i d s -> (iluminadoCaja e && iluminadoCaja s) && (i || d))

iluminadoCaja :: Caja -> Bool
iluminadoCaja (Bombilla b) = b
iluminadoCaja Nada = False

-- 5: cantidadPrendidas

cantidadPrendidas :: Circuito -> Int
cantidadPrendidas = foldCircuito prendidasCaja (+) (\e i d s -> prendidasCaja e + d + i + prendidasCaja s)

prendidasCaja :: Caja -> Int
prendidasCaja (Bombilla b) = if b then 1 else 0
prendidasCaja Nada = 0

-- 6: cajasDeCircuito

cajasDeCircuito :: Circuito -> [Caja]
cajasDeCircuito = foldCircuito (: []) (++) (\e i d s -> [e] ++ i ++ d ++ [s])

-- 7: esCircuitoProlijo

esCircuitoProlijo :: Circuito -> Bool
esCircuitoProlijo = recCircuito (const True) esProlijoSerie esProlijoParalelo

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
tienenLaMismaEstructura c1 c2 = cajasVacias c1 == cajasVacias c2

cajasVacias :: Circuito -> Circuito
cajasVacias = 
  foldCircuito (const cajaNada) Serie (\_ ramaIzq ramaDer _ -> Paralelo Nada ramaIzq ramaDer Nada)

-- 10: subCircuitoMásResistente

resistenciaCircuito :: Circuito -> Float
resistenciaCircuito (Caja Nada) = 0
resistenciaCircuito (Caja (Bombilla b)) = if b then 1 else -1
resistenciaCircuito (Serie cIzq cDer) = resistenciaCircuito cIzq + resistenciaCircuito cDer
resistenciaCircuito (Paralelo cEnt cIzq cDer cSal) = resistenciaCircuito (Caja cEnt) + resistenciaCircuito cIzq + resistenciaCircuito cDer + resistenciaCircuito (Caja cSal)

subCircuitoMásResistente :: Circuito -> Circuito
subCircuitoMásResistente = recCircuito Caja resSerie resParalelo

resSerie :: Circuito -> Circuito -> Circuito -> Circuito -> Circuito
resSerie ramaIzq subRamaIzq ramaDer subRamaDer = 
  elDeMayorResistencia (elDeMayorResistencia subRamaIzq subRamaDer) (Serie ramaIzq ramaDer) 

resParalelo :: Caja -> Circuito -> Circuito -> Circuito -> Circuito -> Caja -> Circuito
resParalelo caEnt ramaIzq subRamaIzq ramaDer subRamaDer caSal = 
  elDeMayorResistencia (elDeMayorResistencia subRamaIzq subRamaDer) (Paralelo caEnt ramaIzq ramaDer caSal) 

elDeMayorResistencia :: Circuito -> Circuito -> Circuito
elDeMayorResistencia c1 c2 = if resistenciaCircuito c1 >= resistenciaCircuito c2 then c1 else c2

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