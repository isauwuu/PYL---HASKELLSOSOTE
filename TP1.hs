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
|y 'elem' x = limpiar x ys --si y esta contenido en lo q quiero borrar, entonces no lo agrego a la cadena final y llamo a la funcion con el resto de la cadena q quedo
| otherwise = y: (limpiar x ys) -- se le agrega a lo q devuelva el prox caso recursivo !!