module Party where
import Employee (Employee (empFun, Emp), GuestList (GL))
import Data.Tree (Tree (Node))

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL gl fun) = GL (emp:gl) $ empFun emp + fun

instance Semigroup GuestList where
    (<>) (GL la fa) (GL lb fb) = GL (la++lb) (fa+fb)

instance Monoid GuestList where
    mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

treeFold :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold fun init tree@(Node val forest) = fun val $ map (treeFold fun init) forest

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss bestLists = (maximumS withBossL, maximumS withoutBossL)
  where withoutBossL   = map fst bestLists
        withoutSubBoss = map snd bestLists
        withBossL      = map (glCons boss) withoutSubBoss

maximumS ::(Monoid a, Ord a) => [a] -> a
maximumS [] = mempty
maximumS lst = maximum lst

maxFun :: Tree Employee -> GuestList
maxFun tree = uncurry max $ treeFold nextLevel (mempty, mempty) tree

format :: GuestList -> String
format (GL list fun) = "Total fun: " ++ show fun ++ "\n" ++ unlines employees
  where employees = map (\(Emp name _) -> name) list

main :: IO()
main = readFile "company.txt" >>= putStrLn . format . maxFun . read
