include "Prelude.e"

main () -> Unit = printList(maintake(10,4))

maintake (x:Int, y:Int) -> Data
  = zip ( \a : Any . \b : Any . y+a*2+b , take(x, ones()), take(x, testList()))

zip (f:Any, xs:Data, ys:Data) -> Data
  = case xs of {
       Con 0 () -> Con 0 ()
     | Con 1 (x:Data, xs0:Data) -> case ys of {
           Con 0 () -> Con 0 ()
         | Con 1 (y:Data, ys0:Data) -> Con 1 (f(x,y), zip(f, xs0, ys0))
       }
    }

take (i:Int, x:Data) -> Data
  = if (i==0) then Con 0 () else
	case x of {
	   Con 0 () -> Con 0 ()
         | Con 1 (y:Any,ys:Data) -> Con 1 (y, take(i-1, ys))
        }

testList () -> Data
  = Con 1 (1, Con 1 (2, Con 1 (3, Con 1 (4, Con 1 (5, Con 0 ())))))

ones () -> Data
  = Con 1 (1, lazy(ones)) -- needs to be lazy or it runs forever!

{- IO stuff -}

printList (x:Data) -> Data
  = case x of {
        Con 1 (y:Int, ys:Data) -> 
	   putStr(append(intToStr(y),", "));
	   printList(ys)
      | Con 0 () -> putStrLn("nil")
    }

