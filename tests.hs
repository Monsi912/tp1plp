import Test.HUnit
import TP1

-- TESTS

testsInvertido :: Test
testsInvertido = TestList -- TODO: AGREGAR
  [ "Caja invertida (1)"
    ~: invertido cajaOn
    ~?= cajaOn
  , "Caja invertida (2)"
    ~: invertido cajaOff
    ~?= cajaOff
  , "Caja invertida (3)"
    ~: invertido cajaNada
    ~?= cajaNada
  , "Caja invertida (4)"
    ~: invertido (Serie(Paralelo on (Paralelo off cajaNada cajaOn on) (Paralelo Nada cajaOn cajaOff Nada) on) cajaOn)
    ~?= Serie cajaOn (Paralelo on (Paralelo Nada cajaOff cajaOn Nada) (Paralelo on cajaOn cajaNada off) on)
  ]

testsHayCaminoIluminado :: Test
testsHayCaminoIluminado = TestList -- TODO: AGREGAR
  [ "En una caja con bombilla encendida hay camino iluminado"
    ~: hayCaminoIluminado cajaOn
    ~?= True
  , "Camino no iluminado"
    ~: hayCaminoIluminado (Serie(Paralelo on (Paralelo off cajaNada cajaOn on) (Paralelo Nada cajaOn cajaOff Nada) on) cajaOn)
    ~?= False
  , "Camino iluminado"
    ~: hayCaminoIluminado (Serie(Paralelo on (Paralelo on cajaNada cajaOn on) (Paralelo Nada cajaOn cajaOff Nada) on) cajaOn)
    ~?= True
  , "Camino no iluminado serie"
    ~: hayCaminoIluminado (Serie cajaOff cajaOn)
    ~?= False
  , "Camino iluminado serie"
    ~: hayCaminoIluminado (Serie cajaOn cajaOn)
    ~?= True
  , "Camino no iluminado paralelo"
    ~: hayCaminoIluminado (Paralelo Nada cajaOn cajaOn Nada)
    ~?= False
  , "Camino iluminado paralelo"
    ~: hayCaminoIluminado (Paralelo on cajaNada cajaOn on)
    ~?= True
  ]
testsCantidadPrendidas :: Test
testsCantidadPrendidas = TestList -- TODO: AGREGAR
  [ "Cantidad prendidas en caja prendida es 1"
    ~: cantidadPrendidas cajaOn
    ~?= 1
  , "Cantidad prendidas en caja apagada es 0"
    ~: cantidadPrendidas cajaOff
    ~?= 0
  , "Cantidad prendidas sin caja es 0"
    ~: cantidadPrendidas cajaNada
    ~?= 0
  , "Cantidad prendidas en serie de 2"
    ~: cantidadPrendidas (Serie cajaOn cajaOn)
    ~?= 2
  , "Cantidad prendidas en serie de 2 con ubna apagada"
    ~: cantidadPrendidas (Serie cajaOff cajaOn)
    ~?= 1
  , "Cantidad prendidas en paralelo"
    ~: cantidadPrendidas (Paralelo off cajaNada cajaOn on)
    ~?= 2
  , "Cantidad prendidas circuito complejo"
    ~: cantidadPrendidas (Serie(Paralelo on (Paralelo off cajaNada cajaOn on) (Paralelo Nada cajaOn cajaOff Nada) on) cajaOn)
    ~?= 6
  ]

testsCajasDeCircuito :: Test
testsCajasDeCircuito = TestList -- TODO: AGREGAR
  [ "La lista de cajas de un circuito con una única caja es la lista con esa caja"
    ~: cajasDeCircuito cajaOn
    ~?= [on]
  ]

testsEsCircuitoProlijo :: Test
testsEsCircuitoProlijo = TestList -- TODO: AGREGAR
  [ "Una caja es prolija"
    ~: esCircuitoProlijo cajaOn
    ~?= True
  ]

-- NOTA: para correr este test, cambiar la línea 18 del archivo tp1.hs de "show = showDeCircuito" a
  -- "show = showDeCircuitoConEstructura".
  -- De esa forma, podrán distinguir la estructura de los circuitos en serie.
testsCircuitoEmprolijado :: Test
testsCircuitoEmprolijado = TestList -- TODO: AGREGAR
  [ "La versión emprolijada de una caja es la misma caja"
    ~: circuitoEmprolijado cajaOn
    ~?= cajaOn
  ]

testsTienenLaMismaEstructura :: Test
testsTienenLaMismaEstructura = TestList -- TODO: AGREGAR
  [
    
  ]

testsSubCircuitoMásResistente :: Test
testsSubCircuitoMásResistente = TestList -- TODO: AGREGAR
  [
    
  ]

tests :: Test
tests = TestList
  [ TestLabel "invertido"                testsInvertido
  , TestLabel "hayCaminoIluminado"       testsHayCaminoIluminado
  , TestLabel "cantidadPrendidas"        testsCantidadPrendidas
  --, TestLabel "cajasDeCircuito"          testsCajasDeCircuito
  --, TestLabel "esCircuitoProlijo"        testsEsCircuitoProlijo
  --, TestLabel "circuitoEmprolijado"      testsCircuitoEmprolijado
  --, TestLabel "tienenLaMismaEstructura"  testsTienenLaMismaEstructura
  --, TestLabel "subCircuitoMásResistente" testsSubCircuitoMásResistente
  ]

main :: IO ()
main = runTestTT tests >>= print