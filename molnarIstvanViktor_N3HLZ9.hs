inc :: Int -> Int-> Int
inc z y = z + y

--1. feladat
lesser_heal :: Int -> Int
lesser_heal x = inc x 3

--2.feladat
lookout :: Int -> Int -> Bool
lookout elet bekuldottEnergia = elet > bekuldottEnergia `div` 10

--2.feladat módosítva
gyakorlo :: Bool -> String
gyakorlo eredmeny =
  if eredmeny
    then "Emberünk képes még több energiát befogadni"
    else "Emberünk már nem képes több energiát befogadni"

--3.feladat
volume :: Int -> Int -> Int
volume bekuldottEnergia faradtsag = inc bekuldottEnergia (faradtsag * (bekuldottEnergia `mod` 12))

autoHealing :: Int -> Int -> Int -> Int -> Bool -> IO ()
autoHealing hp befogadottEnergia bekuldottEnergia faradtsag befogadas= do
  if befogadas
    then do
      let increasedHP = hp + 3
      let increasedBefogadottEnergia = befogadottEnergia + 50
      let increasedFaradtsag = faradtsag +1
      let increasedBekuldottEnergia = volume increasedBefogadottEnergia increasedFaradtsag
      let editedBefogadas = lookout increasedHP increasedBefogadottEnergia
      autoHealing increasedHP increasedBefogadottEnergia increasedBekuldottEnergia increasedFaradtsag editedBefogadas
    else do
      putStrLn ("Sikerült a hp-t: " ++ show hp ++ "-ra/re növelni. Eközben " ++ show bekuldottEnergia ++ " energiát használtál fel.")

main :: IO ()
main = do
  putStrLn ("1. feladat megoldasa:")
  putStrLn ("A megnövekedett életerő: " ++ show (lesser_heal (-1)))
  putStrLn ("A megnövekedett életerő: " ++ show (lesser_heal 2))
  putStrLn ("A megnövekedett életerő: " ++ show (lesser_heal 5))

  putStrLn ("\n2. feladat megoldasa:")
  putStrLn (show (lookout 5 10))
  putStrLn (show (lookout 8 40))
  putStrLn (show (lookout 11 90))
  putStrLn (show (not (lookout 14 150)))
  putStrLn (show (not (lookout 14 180)))

  putStrLn ("\n2. feladat megoldasa (gyakorlás):")
  putStrLn (gyakorlo (lookout 5 10))
  putStrLn (gyakorlo (lookout 8 40))
  putStrLn (gyakorlo (lookout 11 90))
  putStrLn (gyakorlo (not (lookout 14 150)))
  putStrLn (gyakorlo (lookout 14 180))

  putStrLn ("\n3. feladat megoldasa:")
  putStrLn ("Felhasznált energia: " ++ show (volume 10 1))
  putStrLn ("Felhasznált energia: " ++ show (volume 20 2))
  putStrLn ("Felhasznált energia: " ++ show (volume 30 2))
  putStrLn ("Felhasznált energia: " ++ show (volume 30 3))

  putStrLn ("\nHaskell gyakorlás: ") --Feltettem hogy 3 életerő növelés 40 bejutatott energiával egyenlő mert nem találtam az összefüggést a 10, 40, 90, 150, között
  putStrLn ("Írd be mennyi életenergiája volt a vándornak mikor elkezdted gyógyítani: ")
  readLine <- getLine
  let hp = read readLine :: Int
  autoHealing  hp 0 0 0 True
