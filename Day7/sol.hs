{-# LANGUAGE LambdaCase #-}
-- IMPORTS {{{
import Data.Either
import Data.Maybe
import Data.List
import Data.Tree

import Text.Parsec
import Text.Parsec.String

--- }}}
-- PARSER {{{
parseInput :: Parser [Command]
parseInput = many1 (pCommand)

pCommand = do
    char '$'
    spaces
    cmd <- pCd <|> pLs
    spaces
    return cmd

pCd = do 
    spaces
    string "cd"
    spaces
    s <- many1 $ alphaNum <|> char '/' <|> char '.'
    return (CD s)

pLs = do 
    spaces
    string "ls"
    spaces
    files <- many1 (pDir <|> pFile)
    return (LS files)

pDir = do
    string "dir"
    spaces
    s <- many1 alphaNum
    newline
    return (Dir s)

pFile = do
    size <- many1 digit
    spaces
    name <- many1 (alphaNum <|> char '.')
    newline
    return (File (read size) name)
--- }}}
-- COMMAND DATA TYPE {{{

data Command = CD String | LS [FileData] deriving (Show)
data FileData = File Int String | Dir String deriving (Show)

-- }}}
-- FILE TREE DATA TYPE {{{

data FNode = DirNode String Int
           | FileNode String Int
           deriving (Show,Eq)

-- Zipper
-- see heut-zipper.pdf (zotero)
--   - learn yourself a haskell


--                 OLDER        YOUNGER      UP   VALUE
data Path  = Path [Tree FNode] [Tree FNode] Path FNode | NullPath
type Focus = Tree FNode
type Zipper = (Focus,Path)

findChild :: Zipper -> String -> Maybe (Tree FNode)
findChild ((Node _ children),path) val  =
    find (\(Node x _) -> val  == (case x of {(DirNode v _ ) -> v; a-> ""})) children

findChildIndex :: Zipper -> String -> Maybe Int
findChildIndex ((Node _ children),path) val =
    findIndex (\(Node x _) -> val == (case x of {(DirNode v _) -> v; a-> ""})) children

moveLeft :: Zipper -> Zipper
moveLeft ((Node val (x:xs),path)) = (x, (Path [] xs path val))

moveUp :: Zipper -> Maybe Zipper
moveUp (_,NullPath) = Nothing
moveUp (node,(Path l r up val)) =
    let focus' = Node val (l ++ node:r)
        path' = up
     in Just (focus',path')

moveDown :: Zipper -> String -> Zipper
moveDown zipper val = 
    let focus' = fromJust $ findChild zipper val
        index = fromJust $ findChildIndex zipper val
        path = snd zipper 
        (value,children) = case (fst zipper) of 
                             Node value children -> (value,children)
                             a -> error "Unexpected type in zipper"
        younger = take index children
        older = drop (index+1) children 
     in (focus',(Path younger older path value))

-- Find the root node of a tree
root :: Zipper -> Zipper
root z = case (moveUp z) of
           Nothing -> z
           Just a -> root a

-- Find the depth of the curent node in the tree
depth :: Zipper -> Int
depth z = case (moveUp z) of
            Nothing -> 0
            Just a -> 1 + (depth a)


-- }}}
-- SOLUTION {{{

genTree :: [Command] -> Zipper
genTree cmds = root $ genTreeInner (drop 1 cmds) ((Node (DirNode "ROOT" (-1)) []), NullPath)
genTreeInner :: [Command] -> Zipper -> Zipper
genTreeInner [] zipper = zipper
genTreeInner ((LS fileData):cmds) z = genTreeInner cmds (genLS z fileData)
genTreeInner ((CD location):cmds) z = genTreeInner cmds $ (genCD z location)

-- on a cd, do not modify tree, just move
genCD z "/" = (root z)
genCD z ".." = 
    let z' = moveUp z
     in case z' of
          Nothing -> error $ "Node has no parent"
          Just a -> a

genCD z loc = 
    let z' = moveDown z loc
     in z'

-- on a dir, modify the tree
-- uses some messy knot-tying
-- as we havent changed pwd, return the same node given

genLS z [] = z
genLS z (f:fs) = 
    let z' = case f of
               File size name -> addChildFile z size name
               Dir name       -> addChildDir z name
     in genLS z' fs


addChildFile z size name = 
    let newNode = (Node (FileNode name size) [])
        (focus,path) = z
        (val,children) = (case focus of {Node val children -> (val,children)})
        focus' = Node val (newNode:children)
     in (focus',path)


addChildDir z name = 
    let newNode = (Node (DirNode name (-1)) [])
        (focus,path) = z
        (val,children) = case focus of {Node val children -> (val,children)}
        focus' = Node val (newNode:children)
     in (focus',path)


updateDirSizes z = 
    let (focus,path) = z
        (fnode,children) = case focus of {Node a b -> (a,b)}
        (name,size) = case fnode of {DirNode n v -> (n,v)}
        children' = map (\case {Node (DirNode v s) c -> fst $ updateDirSizes (moveDown z v); a -> a}) children
        size' = sum $ map (\case {Node (DirNode v s) c -> s; Node (FileNode v s) c -> s}) children'
        focus' = (Node (DirNode name size') children')
     in (focus',path)


dirsSmallerThan z s = 
    let (focus,path) = z
        (fnode,children) = case focus of {Node a b -> (a,b)}
        (name,size) = case fnode of {DirNode n v -> (n,v)}
        childrenSmallerThan = intercalate "" $ map (\case {Node (DirNode n v) c -> dirsSmallerThan (moveDown z n) s; Node (FileNode n v) c -> ""}) children
     in if size <= s then name ++ (" : " ++ (show size)) ++ '\n':childrenSmallerThan
                     else childrenSmallerThan

sumSizesSmallerThan z s = 
    let (focus,path) = z
        (fnode,children) = case focus of {Node a b -> (a,b)}
        (name,size) = case fnode of {DirNode n v -> (n,v)}
        childrenSmallerThan = sum $ map (\case {Node (DirNode n v) c -> sumSizesSmallerThan (moveDown z n) s; Node (FileNode n v) c -> 0}) children
     in if size <= s then size + childrenSmallerThan
                     else childrenSmallerThan

findSmallestLargerThan tree s = 
    foldr  (\x y -> case (x,y) of {((DirNode n1 v1),(DirNode n2 v2)) -> if (v1 < v2) then x else y}) (DirNode "None" (s*100000))
    $ filter (\case {DirNode n v -> v > s})
    $ filter (\case {DirNode n v -> True; a -> False})
    $ flatten tree -- laziness will make this fine
-- }}}
-- MAIN {{{

main = do
    stdin <- getContents
    let commands = fromRight [] $ parse parseInput "" stdin
        z  = genTree commands
        (tree,_) = z
        z2 = updateDirSizes z
        (tree2,_)  = z2
        (n,v) = case (findSmallestLargerThan tree2 30000000) of {DirNode a b -> (a,b)}

    putStrLn $ drawTree $ fmap show tree2
    putStrLn "DIRECTORIES SMALLER THAN 100,000\n"
    putStrLn $ dirsSmallerThan z2 100000
    print $ sumSizesSmallerThan z2  100000
    print $ filter (\case {DirNode n v -> v >= 30000000}) $ filter (\case {DirNode n v -> True; a-> False}) $ flatten tree2
    putStrLn "SMALLEST DIRECTORY LARGER THAN 30000000"
    putStrLn n
    print v
-- }}}
