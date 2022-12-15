import Data.Either
import Data.Maybe
import Data.List
import Text.Parsec
import Text.Parsec.String

data Command = CD String | LS [FileData] deriving (Show)
data FileData = File Int String | Dir String deriving (Show)

---PARSER
parseInp :: Parser [Command]
parseInp = many1 (command)

command = do
    char '$'
    spaces
    cmd <- cd <|> ls
    spaces
    return cmd

cd = do 
    spaces
    string "cd"
    spaces
    s <- many1 $ alphaNum <|> char '/' <|> char '.'
    return (CD s)

ls = do 
    spaces
    string "ls"
    spaces
    files <- many1 (dir <|> file)
    return (LS files)

dir = do
    string "dir"
    spaces
    s <- many1 alphaNum
    newline
    return (Dir s)

file = do
    size <- many1 digit
    spaces
    name <- many1 (alphaNum <|> char '.')
    newline
    return (File (read size) name)


data TreeNode = DirNode {value     :: String
                        ,parent    :: TreeNode
                        ,children  :: [TreeNode]
                        }

      | FileNode Int String | Nil deriving (Show,Eq)

-- find the root node of a tree
root :: TreeNode -> TreeNode
root node = case (parent node) of
              Nil -> node
              a -> (root a)

-- calculate the depth of a node in a tree
depth :: TreeNode -> Int
depth node = case (parent node) of 
               Nil -> 0
               parentNode   -> 1 + (depth parentNode)

-- turn parsed commands into a file tree
generateTreeFromCmds (cmd:cmds) = inner (DirNode "/" Nil []) cmds
    where inner node [] = node
          inner node (cmd:cmds) = 
              let node' = case cmd of 
                            LS _ -> addLsToTree node cmd
                            CD location -> fromMaybe (DirNode "err" node' []) (cdInTree node location)
               in inner node' cmds
                  

addLsToTree parentNode (LS fd) = inner parentNode fd
    where inner parentNode [] = parentNode
          inner parentNode (f:fs) = 
              let node = case f of
                           Dir v -> DirNode v parentNode'  []
                           File size v -> FileNode size v
                  parentNode' = parentNode{children = (node:(children parentNode))}
               in inner parentNode' fs

cdInTree node "/" = Just (root node)
cdInTree node ".." = case (parent node) of
                       Nil -> Nothing
                       a -> Just a

cdInTree node x = find (\n -> case n of { DirNode v a b -> (v == x); a -> False }) (children node)

