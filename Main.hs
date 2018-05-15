g f x = (f (x + (f x)))/(f x) - 1

phi f x = x - (f x/g f x)

steff ls _ 10000 _ _ = ls
steff ls (-11) _ _ _ = ls
steff ls e i f x = let xk = phi f x
                       eabs = abs (x - xk)
                   in if eabs < 10**e then steff (ls ++ [(i,xk,eabs)]) (e-1) (i+1) f xk
                                      else steff ls e (i+1) f xk

itera f x = showLineByLine (steff [] (-1) 0 f x)

showLineByLine as = do mapM print as
                       return ()

gabriela x = 2 + (x/2) + sin x

luiz x = x - log (x**2 + 4)

cleber x = sqrt (cos x + 1) - x
