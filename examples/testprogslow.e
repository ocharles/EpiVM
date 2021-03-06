include "Prelude.e"

main () -> Unit = 
  printInt(apply(natToInt, fact(eight())))

zero () -> Data = Con 0 ()
one () -> Data = Con 1 (zero())
two () -> Data = Con 1 (one())
three () -> Data = Con 1 (two())
four () -> Data = Con 1 (three())
five () -> Data = Con 1 (four())
six () -> Data = Con 1 (five())
seven () -> Data = Con 1 (six())
eight () -> Data = Con 1 (seven())
nine () -> Data = Con 1 (eight())
ten () -> Data = Con 1 (nine())

natToInt (x:Data) -> Int =
  case x of {
     Con 0 () -> 0
   | Con 1 (k:Data) -> 1+natToInt(k)
  }

plus (x:Data, y:Data) -> Data = 
  case x of {
     Con 0 () -> y
   | Con 1 (k:Data) -> Con 1 (plus(k, y))
  }

mult (x:Data, y:Data) -> Data = 
  case x of {
     Con 0 () -> Con 0 ()
   | Con 1 (k:Data) -> plus(y, (mult(k, y)))
  }

fact (x:Data) -> Data =
  case x of {
     Con 0 () -> one()
   | Con 1 (k:Data) -> apply(mult(x), fact(k)) -- deliberate, to slow
					       -- down and test thunks
  }

apply (f:Fun, a:Any) -> Any =
  f(a)

