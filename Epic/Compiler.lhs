> -- | 
> -- Module      : EMachine.Compiler
> -- Copyright   : Edwin Brady
> -- Licence     : BSD-style (see LICENSE in the distribution)
> --
> -- Maintainer  : eb@dcs.st-and.ac.uk
> -- Stability   : experimental
> -- Portability : portable
> -- 
> -- Public interface for Epigram Supercombinator Compiler

> module Epic.Compiler(CompileOptions(..),
>                      compile, 
>                      compileOpts,
>                      compileDecls,
>                      link) where

Brings everything together; parsing, checking, code generation

> import System.Process
> import System.Exit
> import System.IO
> import System.Directory
> import System.Environment
> import Data.Char

> import Epic.Language
> import Epic.Parser
> import Epic.Scopecheck
> import Epic.CodegenC
> import Epic.Simplify

> import Paths_epic

> addGCC :: [CompileOptions] -> String
> addGCC [] = ""
> addGCC ((GCCOpt s):xs) = s ++ " " ++ addGCC xs
> addGCC (_:xs) = addGCC xs

> outputHeader :: [CompileOptions] -> Maybe FilePath
> outputHeader [] = Nothing
> outputHeader ((MakeHeader f):_) = Just f
> outputHeader (_:xs) = outputHeader xs

> doTrace opts | elem Trace opts = " -DTRACEON"
>              | otherwise = ""

> -- |Compile a source file in supercombinator language to a .o
> compile :: FilePath -- ^ Input file name
>            -> FilePath -- ^ Output file name
>            -> Maybe FilePath -- ^ Interface (.ei) file name, if desired
>            -> IO ()
> compile fn outf iface
>     = compileOpts fn outf iface []

Chop off everything after the last / - get the directory a file is in

> trimLast f = case span (\x -> x /= '/') (reverse f) of
>                 (eman, htap) -> reverse htap

> compileOpts :: FilePath -- ^ Input file name
>            -> FilePath -- ^ Output file name
>            -> Maybe FilePath -- ^ Interface (.ei) file name, if desired
>            -> [CompileOptions] -- Keep the C file
>            -> IO ()
> compileOpts fn outf iface opts
>     = do input <- readFile fn
>          -- prelude <- readFile (libdir ++ "/Prelude.e")
>          let s = parse input fn
>          case s of
>              Failure err _ _ -> fail err
>              Success ds -> do
>                 compileDecls outf iface ds opts

> compileDecls :: FilePath -- ^ Output file name
>              -> Maybe FilePath -- ^ Interface (.ei) file name, if desired
>              -> [Decl] -- ^ Declarations
>              -> [CompileOptions]
>              -> IO ()
> compileDecls outf iface ds opts
>     = do (tmpn,tmph) <- tempfile
>          let hdr = outputHeader opts
>          scchecked <- checkAll opts ds
>          let simplified = simplifyAll scchecked
>          checked <- docompileDecls simplified tmph hdr
>          fp <- getDataFileName "evm/closure.h"
>          let libdir = trimLast fp
>          let dbg = if (elem Debug opts) then "-g" else "-O3"
>          let cmd = "gcc -DUSE_BOEHM -c " ++ dbg ++ " -foptimize-sibling-calls -x c " ++ tmpn ++ " -I" ++ libdir ++ " -o " ++ outf ++ " " ++ addGCC opts ++ doTrace opts
>          -- putStrLn $ cmd
>          -- putStrLn $ fp
>          exit <- system cmd
>          if (elem KeepC opts)
>             then do system $ "cp " ++ tmpn ++ " " ++ 
>                                (getRoot outf) ++ ".c"
>                     return ()
>             else return ()
>          -- removeFile tmpn
>          if (exit /= ExitSuccess) 
>             then fail $ "gcc failed"
>             else return ()
>          case iface of
>             Nothing -> return ()
>             (Just fn) -> do writeFile fn (writeIFace checked)

> getRoot fn = case span (/='.') fn of
>     (stem,_) -> stem


> docompileDecls (ctxt, decls) outh hdr
>     = do hPutStr outh $ codegenC ctxt decls
>          case hdr of
>              Just fpath ->
>                   do let hout = codegenH (filter isAlpha (map toUpper (getRoot fpath))) decls
>                      writeFile fpath hout
>              Nothing -> return ()
>          hFlush outh
>          hClose outh
>          return decls

> getExtra :: [CompileOptions] -> IO [String]
> getExtra ((MainInc x):xs) = do fns <- getExtra xs
>                                return (x:fns)
> getExtra (_:xs) = getExtra xs
> getExtra [] = return []

> -- |Link a collection of .o files into an executable
> link :: [FilePath] -- ^ Object files
>         -> FilePath -- ^ Executable filename
>         -> [CompileOptions] -- Keep the C file
>         -> IO ()
> link infs outf opts = do
>     extraIncs <- getExtra opts
>     mainprog <- if (not (elem ExternalMain opts)) then mkMain extraIncs else return ""
>     fp <- getDataFileName "evm/closure.h"
>     let libdir = trimLast fp
>     let dbg = if (elem Debug opts) then "-g" else "-O3"
>     let cmd = "gcc -DUSE_BOEHM -x c " ++ dbg ++ " -foptimize-sibling-calls " ++ mainprog ++ " -x none -L" ++
>               libdir++" -I"++libdir ++ " " ++
>               (concat (map (++" ") infs)) ++ 
>               " -levm -lgc -lpthread -lgmp -o "++outf ++ " " ++ addGCC opts
>     -- putStrLn $ cmd
>     exit <- system cmd
>     if (exit /= ExitSuccess)
>        then fail $ "Linking failed"
>        else return ()

Output the main progam, adding any extra includes needed. 
(Some libraries need the extra includes, notably SDL, to compile correctly.
Grr.)

> mkMain :: [FilePath] -> IO FilePath
> mkMain extra = 
>    do mppath <- getDataFileName "evm/mainprog.c"
>       mp <- readFile mppath
>       (tmp, tmpH) <- tempfile
>       hPutStr tmpH (concat (map (\x -> "#include <" ++ x ++ ">\n") extra))
>       hPutStr tmpH mp
>       hClose tmpH
>       return tmp

 -- |Get the path where the required C libraries and include files are stored
 libdir :: FilePath
 libdir = libprefix ++ "/lib/evm"

