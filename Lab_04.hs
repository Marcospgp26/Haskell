fatorial_duplo::Int->Int

fatorial_duplo x | (x <= 0) = 1
                 | (x > 0) = x * fatorial_duplo(x - 2)



quo :: Int->Int->Int
quo x y = if((x - y) < 0) then  0 else  (1 + quo r y)
            where r = x - y


res :: Int->Int->Int
res x y = if(x < y) then x else (0 + res r y)
             where r = x - y


pot:: Int->Int->Int
pot x y 
        | (y == 0) = 1
        | (y > 0) = x * pot x (y - 1)


nandif :: Bool->Bool->Bool
nandif x y = if((x == True) && (y == x)) then False else True


nandguard :: Bool->Bool->Bool
nandguard x y | ((x == True) && (x == y)) = False
              | otherwise = True


nandcasa :: Bool->Bool->Bool
nandcasa True True = False
nandcasa _     _   = True