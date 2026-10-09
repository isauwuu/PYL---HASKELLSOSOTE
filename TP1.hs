esNumeroNegativo :: Int -> Bool
esNumeroNegativo n = n < 0

esMultiploDe :: Int -> Int ->Bool
esMultiploDe n d= n `mod` d == 0

fueraDeRango :: Int -> Int-> Int-> Bool
fueraDeRango n min max = n<=max && n>=min

ptoMedio :: (Float, Float) -> (Float, Float) -> (Float, Float)
ptoMedio (x1,y1) (x2,y2) = ((x1+x2)/2,(y1+y2)/2)

norma :: (Float, Float) -> Float
norma (x,y) = sqrt(x^2+y^2)

segundos2Tiempo :: Integer -> (Integer, Integer, Integer)
segundos2Tiempo s=(h,m,seg)
    where
        h=s'div'3600
        m=(s-h*3600)'div'60
        seg=s-(h*3600)-(m*60)

limpiar :: String -> String -> String
limpiar _ [] = [] --caso base
limpiar x (y:ys)   -- el (y:ys) separa el primer caracter de la cadena del resto de la cadena
y 'elem' x = limpiar x ys --si y esta contenido en lo q quiero borrar, entonces no lo agrego a la cadena final y llamo a la funcion con el resto de la cadena q quedo
otherwise = y: (limpiar x ys) -- se le agrega a lo q devuelva el prox caso recursivo !!

celcius2Fahrenheit :: Float -> Float
celcius2Fahrenheit c = (c*9/5)+32

listar:: Int -> Int -> Int -> [Int]
listar a b c = [a,b,c]

rangoDePaso :: Int -> Int -> Int -> [Int]
rangoDePaso a b c = [a,a+c .. b]

diff :: [Float] -> [Float]
diff xs = [x-promedio | x <- xs]
    where
        promedio = sum xs / fromIntegral (length xs)
    
todosIguales :: [Int] -> Bool
todosIguales rta = comparaConTodos x:xs

comparaConTodos :: Int -> [Int] -> Bool
comparaConTodos _ [] = True
comparaConTodos x (y:ys) == (x==y) && comparaConTodos x ys

repLista :: [Int] -> Int -> [Int]
repLista [] _ = []
repLista (x:xs) n = replicate n x ++ repLista xs n

repLista2 :: [Int] -> Int -> [Int]
repLista2 [] _ = []
repLista2 (x:xs) n= agrega n x ++ repLista xs n
where
    agrega 0 _ = []
    agrega c elem = elem : xs ++ agrega (c-1) elem

lista2lista :: [[Int]] -> [Int]
lista2lista [] = []
lista2lista (xs:xss) = xs ++ lista2lista xss

checkParentesis :: String -> Bool
checkParentesis cad = cuentaParentesisAbiertos cad == cuentaParentesisCerrados cad

cuentaParentesisAbiertos :: String -> Int
cuentaParentesisAbiertos [] = 0 
cuentaParentesisAbiertos (y:ys)
|y == '(' = 1 + cuentaParentesisAbiertos ys
|otherwise = 0 + cuentaParentesisAbiertos ys

cuentaParentesisCerrados :: String -> Int
cuentaParentesisCerrados [] = 0
cuentaParentesisCerrados (y:ys)
|y == ')' = 1 + cuentaParentesisCerrados ys 
|otherwise = 0 + cuentaParentesisCerrados ys

finales :: Int -> [Int] -> [Int]
finales n xs= reverse (take n (reverse xs))

extremos :: Int -> [Int] -> [Int]
extremos n xs = take n xs ++ finales n xs