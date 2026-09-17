module TP1 where
import Data.Functor.Classes (readPrec1)
import Data.Foldable (Foldable(fold))

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
tienenLaMismaEstructura = foldCircuito (const esCaja) (\rec1 rec2 c2 -> case c2 of
                                                                         Caja c -> False
                                                                         Serie c1' c2' -> rec1 c1' && rec2 c2'
                                                                         Paralelo cEnt c1' c2' cSal -> False) 
                                                      (\cEnt rec1 rec2 cSal c2 -> case c2 of
                                                                                   Caja c -> False
                                                                                   Serie c1' c2' -> False
                                                                                   Paralelo cEnt' c1' c2' cSal' -> rec1 c1' && rec2 c2')

esCaja :: Circuito -> Bool
esCaja (Caja _) = True
esCaja _ = False

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

Lema {L1} - Para todo b. not (not b) = b

Caso 1 : b = True
not (not True)
= {NT}  not False
= {NF}  True

Caso 2 : b = False
not (not False)
= {NF}  not True
= {NT}  False

Lema {L2} - cajaAlternada (cajaAlternada caja) = caja

Caso 1 : caja = Nada
cajaAlternada (cajaAlternada Nada) = Nada

cajaAlternada (cajaAlternada Nada)
= {CAN} cajaAlternada Nada
= {CAN} Nada

Caso 2 : caja = Bombilla b con b :: Bool
cajaAlternada (cajaAlternada (Bombilla b)) = Bombilla b

cajaAlternada (cajaAlternada b)
= {CAB} cajaAlternada (Bombilla (not b))
= {CAB} Bombilla (not (not b))
= {L1}  Bombilla b

Caso base (Caja c)
Queremos ver que P(Caja c) es verdadero ∀c::Caja

P(Caja c): (alternado . alternado) (Caja c) = id (Caja c)
(alternado . alternado) (Caja c)
= {C} alternado (alternado (Caja c))
= {AC} alternado (Caja (cajaAlternada c))
= {AC} Caja (cajaAlternada (cajaAlternada c))
= {L2} Caja c = id (Caja c)
= {I} Caja c = Caja c

Luego, (alternado . alternado) (Caja c) = id (Caja c) como queríamos probar.

Caso inductivo 1 (Serie c1 c2)

HIc1 = c1::Circuito. P(c1): (alternado . alternado) (c1) ={C} alternado (alternado c1) = id (c1)
HIc2 = c2::Circuito. P(c2): (alternado . alternado) (c2) ={C} alternado (alternado c2) = id (c2)

Suponiendo que vale P(c1) y P(c2) probamos P(Serie c1 c2)
Queremos ver que P(Serie c1 c2): (alternado . alternado) (Serie c1 c2) = id (Serie c1 c2) es verdadero

{AS} alternado (Serie ci cf) = Serie (alternado ci) (alternado cf)
(alternado . alternado) (Serie c1 c2)
= {C} alternado (alternado (Serie c1 c2))
= {AS} alternado (Serie (alternado c1) (alternado c2))
= {AS} Serie (alternado (alternado c1)) (alternado (alternado c2))
= {HIc1} Serie (id c1) (alternado (alternado c2))
= {HIc2} Serie (id c1) (id c2)
= {I} Serie c1 c2 = id (Serie c1 c2)
= {I} Serie c1 c2 = Serie c1 c2

Luego (alternado . alternado) (Serie c1 c2) = id (Serie c1 c2) que es lo que queríamos probar.

Caso inductivo 2 (Paralelo cEnt c1 c2 cSal)

Suponiendo que vale P(c1), P(c2) y habiendo probado que vale P(Caja c) probamos P(Paralelo cEnt c1 c2 cSal)
Queremos ver que P(Paralelo cEnt c1 c2 cSal): (alternado . alternado) (Paralelo cEnt c1 c2 cSal) = id (Paralelo cEnt c1 c2 cSal) es verdadero

(alternado . alternado) (Paralelo cEnt c1 c2 cSal)
= {C} alternado (alternado (Paralelo cEnt c1 c2 cSal))
= {AP} alternado (Paralelo (cajaAlternada cEnt) (alternado c1) (alternado c2) (cajaAlternada cSal))
= {AP} Paralelo (cajaAlternada (cajaAlternada cEnt)) (alternado (alternado c1)) (alternado (alternado c2)) (cajaAlternada (cajaAlternada cSal))
= {L2} Paralelo cEnt (alternado (alternado c1)) (alternado (alternado c2)) cSal
= {HIc1} Paralelo cEnt (id c1) (alternado (alternado c2)) cSal
= {HIc2} Paralelo cEnt (id c1) (id c2) cSal
= {I} Paralelo cEnt c1 c2 cSal = id (Paralelo cEnt c1 c2 cSal)
= {I} Paralelo cEnt c1 c2 cSal = Paralelo cEnt c1 c2 cSal

Luego (alternado . alternado) (Paralelo cEnt c1 c2 cSal) = id (Paralelo cEnt c1 c2 cSal) que es lo que queríamos probar.

Q.E.D para todos los casos que alternado . alternado = id

--}