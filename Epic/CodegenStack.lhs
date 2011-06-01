> module Epic.CodegenStack where

> import Control.Monad.State

> import Epic.Language
> import Epic.Stackcode
> import Debug.Trace

> codegenC :: Context -> [Decl] -> String
> codegenC ctxt decls = error $ concatMap (worker ctxt) decls

> codegenH :: String -> [Decl] -> String
> codegenH = undefined

> writeIFace :: [Decl] -> String
> writeIFace = undefined

> worker :: Context -> Decl -> String
> worker ctxt (Decl name ty fn exp flags) = 
>     show (name, compile ctxt name fn)
> worker _ _ = ""