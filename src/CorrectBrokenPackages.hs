module Main where

import System.IO(openFile
                ,IOMode(..)
                ,hGetContents
                ,hPutStrLn
                ,hPutStr
                ,hClose
                ,stdin
                ,stderr)
import System.Process(createProcess
                     ,CreateProcess(..)
                     ,CmdSpec(..)
                     ,StdStream(..)
                     ,waitForProcess 
                     )
import System.Exit(ExitCode(..))
import Distribution.InstalledPackageInfo(InstalledPackageInfo(..)
                                        , ParseResult(..)
                                        , InstalledPackageInfo_(..)
                                        , parseInstalledPackageInfo
                                        ,showInstalledPackageInfo)
import Distribution.Package(InstalledPackageId(..))
import System.Directory(getCurrentDirectory
                       ,getDirectoryContents)
import System.FilePath((</>),takeDirectory)
import Text.Regex.Posix((=~))

-- TODO: The package dir must be obtained by using another method.
-- pkgDir :: FilePath
-- pkgDir = "/usr/lib64/ghc-7.0.4/package.conf.d/"

readInsPkgInfo :: FilePath -> FilePath -> IO InstalledPackageInfo 
readInsPkgInfo pkgDir f = do
  h <- openFile (pkgDir </> f) ReadMode
  s <- hGetContents h
  let p = parseInstalledPackageInfo s
  case p of 
    (ParseOk _ ipi) -> return ipi
    _               -> error $ "The file " ++ f ++ " " -- TODO: This must be changed

helperPkgInfo :: FilePath -> [(String,String,[FilePath])] -> InstalledPackageId -> IO InstalledPackageId
helperPkgInfo pkgDir dep (InstalledPackageId n) = do r <- lookupNameDep pkgDir (extractPackageName n) dep
                                                     case r of 
                                                       (Just n') -> return $ InstalledPackageId n'
                                                       Nothing   -> return $ InstalledPackageId n

lookupPkgFiles :: FilePath -> String -> [FilePath] -> IO (Maybe String)
lookupPkgFiles pkgDir oldPkgId []            = return $ Nothing
lookupPkgFiles pkgDir oldPkgId (confFile:xs) = do 
  ipi <- readInsPkgInfo pkgDir confFile
  let (InstalledPackageId pkgId) = installedPackageId ipi
  if (oldPkgId == pkgId)
   then lookupPkgFiles pkgDir oldPkgId xs
   else return $ Just pkgId

getRidofCom :: String -> String
getRidofCom [] = []
getRidofCom (x:xs) 
    | x == '\"'    = getRidofCom xs
    | otherwise    = x : getRidofCom xs

lookupNameDep :: FilePath -> String -> [(String,String,[FilePath])] -> IO (Maybe String)
lookupNameDep pkgDir n [] = return Nothing
lookupNameDep pkgDir n ((n', oldPkgId, files):xs)
              | n == n'                  = lookupPkgFiles pkgDir oldPkgId files
              | otherwise                = lookupNameDep pkgDir n xs 

updatePkgInfo :: FilePath -> ((String, FilePath),[(String,[FilePath])]) -> IO ()
updatePkgInfo pkgDir ((pn,pf),dep) = do 
  ipi <- readInsPkgInfo pkgDir pf
  let dp = depends ipi
  let dep' = map (\(pId,ls) -> (extractPackageName pId, getRidofCom pId, ls)) dep
  nDep <- mapM (helperPkgInfo pkgDir dep') dp 
  let ipi' = ipi { depends = nDep }
  hnf <- openFile (pkgDir </> pf) WriteMode
  hPutStr hnf $ showInstalledPackageInfo ipi'
  hClose hnf

updatePkgInfo' :: FilePath -> ((String, FilePath),[(String,[FilePath])]) -> IO ()
updatePkgInfo' pkgDir ((pn,pf),dep) = do 
  ipi <- readInsPkgInfo pkgDir pf
  let dp = depends ipi
  let dep' = map (\(pId,ls) -> (extractPackageName pId, getRidofCom pId, ls)) dep
  nDep <- mapM (helperPkgInfo pkgDir dep') dp 
  let ipi' = ipi { depends = nDep }
  hnf <- openFile  pf WriteMode
  hPutStr hnf $ showInstalledPackageInfo ipi'
  hClose hnf

execCheckPackages :: IO String
execCheckPackages = do 
  pwd <- getCurrentDirectory
  (_, Just hOut, Just hErr, hProc) <- createProcess 
                                      $ CreateProcess { cmdspec      = RawCommand "ghc-pkg" ["check"
                                                                                            ,"--verbose"
                                                                                            ,"--global"]
                                                      , cwd          = Just pwd
                                                      , env          = Nothing
                                                      , std_in       = UseHandle stdin
                                                      , std_out      = CreatePipe
                                                      , std_err      = CreatePipe
                                                      , close_fds    = True
                 --                                     , create_group = False
                                                      }
  e <- waitForProcess hProc
  case e of 
     ExitSuccess     -> do s  <- hGetContents hOut
                           return s
     (ExitFailure i) -> do s   <- hGetContents hOut
                           s'  <- hGetContents hErr
                           return $ s ++ s'

execRecachePkgs :: IO ()
execRecachePkgs = do 
  pwd <- getCurrentDirectory
  (_, Just hOut, Just hErr, hProc) <- createProcess 
                                      $ CreateProcess { cmdspec   = RawCommand "ghc-pkg" ["recache"]
                                                      , cwd       = Just pwd
                                                      , env       = Nothing
                                                      , std_in    = UseHandle stdin
                                                      , std_out   = CreatePipe
                                                      , std_err   = CreatePipe
                                                      , close_fds = True
                   --                                   , create_group = False
                                                      }
  e <- waitForProcess hProc
  case e of 
     ExitSuccess     -> return ()
     (ExitFailure i) -> do hPutStrLn stderr $ "Error while recahing packages: " ++ (show i)


getBadPackages :: IO (FilePath, [(String, [String])])
getBadPackages = do
  s <- execCheckPackages
  let ls'    = lines s
      h      = ls' !! 1
      t      = ls'
      dir    = takeDirectory $ (words h) !! 2
      l      = map words t
      r = process l
  return (dir,r)

printBadPackages :: IO ()
printBadPackages = do { (_,l) <- getBadPackages; mapM_ (\ (x,_) -> putStrLn $ show x) l }

process :: [[String]] -> [(String,[String])]
process []   = []
process (x:xs) 
        | "problems" `elem` x   = let (l,xs') = process' xs
                                      l'      = process xs'
                                  in (filter (/= ':') $ x !! 5, l):l'
        | otherwise             = process xs

process' :: [[String]] -> ([String],[[String]])
process' [] = ([],[])
process' (x:xs)
         | null x                 = ([],xs)
         | x !! 0 == "dependency" = let (y,ys) = process' xs
                                    in (x !! 1 : y, ys)
         | otherwise              = ([],x:xs)

correctBrokenPackages :: IO ()
correctBrokenPackages = do 
  (pkgDir,p) <- getBadPackages
  s       <- getDirectoryContents pkgDir
  print $ "Package directory: " ++ pkgDir
  print "Packages:"
  mapM_ (putStrLn.show.fst) p
  mapM_ (putStrLn.show.snd) p
  let l = map (\(pn, ld) -> ((pn, getConfFile s pn)
                            ,(getConfFilePackages' s ld))) p
  mapM_ (updatePkgInfo pkgDir) l
  execRecachePkgs

correctBrokenPackages' :: IO ()
correctBrokenPackages' = do 
  (pkgDir,p) <- getBadPackages
  s       <- getDirectoryContents pkgDir
  let l = map (\(pn, ld) -> ((pn, getConfFile s pn)
                            ,(getConfFilePackages' s ld))) p
  mapM_ (updatePkgInfo pkgDir) l
  -- execRecachePkgs

expreg1, expreg2 :: String
expreg1 = "\\.*"
expreg2 = "[a-z]*(-[a-z]*)*-[0-9]*(\\.[0-9]*)*"

getConfFile :: [FilePath] -> String -> FilePath
getConfFile d = head.(getConfFile' d)

getConfFile' :: [FilePath] -> String -> [FilePath]
getConfFile' d n = filter (=~ (n ++ expreg1)) d

extractPackageName :: String -> String
extractPackageName pid = let (_,pn,_) = pid =~ expreg2 :: (String,String,String)
                         in pn

getConfFilePackages :: [FilePath] -> [String] -> [FilePath]
getConfFilePackages d ns = map ((getConfFile d).extractPackageName) ns

getConfFilePackages' :: [FilePath] -> [String] -> [(String,[FilePath])]
getConfFilePackages' d ns = map f ns
    where f n = (n,(getConfFile' d).extractPackageName $ n)

main :: IO ()
main =  correctBrokenPackages

