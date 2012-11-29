module Simplex (lex, parse, Token(..), Block(..), Document (Document)) where

import Prelude hiding (lex)
import Data.List (intersperse)
import Data.Char (isAlpha)

data Block =
          BAny String String
        | BParagraph String
        | BSection String
        | BSubsection String
        | BSubsubsection String
        | BDefine String String
        | BRemark String String
        | BVerbatim String String
        | BAdvise [String]
        | BItemize [String]
        | BEnumerate [String]
        | BDescribe [(String, String)]
        | BTherefore String
        | BBecause String
        | BNTherefore String
        | BNBecause String
        | BLine
        | BCommand String [String]
    deriving (Eq, Show)

data Document = Document [Block] [(String, String)]
    deriving (Eq, Show)

data Token = TControl String | TBlock String | TCommand String [String]
    deriving (Eq, Show)

data State = SStart | SNewline | SControl | SControlAfter | SSymbol | SSpace | SCommand
    deriving (Eq, Show)

isBlock (TBlock _) = True
isBlock _ = False

parse :: [Token] -> Document
parse = parse' $ Document [] []

parse' :: Document -> [Token] -> Document

parse' (Document blocks prop) s@(TControl ('@':cs) : TBlock b : xs)
    = parse' (Document blocks ((cs, b):prop)) xs

parse' doc s@(TCommand cmd args : xs)
    = upd doc xs $ BCommand cmd args

parse' doc s@(TControl ('.':cs@(_:_)) : xs)
    =   let (b, bs) = break (not.isBlock) xs
        in  upd doc bs $ BVerbatim cs $ unlines $ map (\(TBlock x) -> x) b

parse' doc s@(TControl c : TBlock b : xs)
    =   case c of
            "." -> upd doc xs $ BParagraph b
            "=" -> upd doc xs $ BSection b
            "==" -> upd doc xs $ BSubsection b
            "===" -> upd doc xs $ BSubsubsection b

            "--" -> upd doc xs $ BLine

            "=>" -> upd doc xs $ BTherefore b
            "<=" -> upd doc xs $ BBecause b
            "=!>" -> upd doc xs $ BNTherefore b
            "<!=" -> upd doc xs $ BNBecause b

            ":=" -> upd doc xs $ mkDefine b
            ":-" -> upd doc xs $ mkRemark b

            "->" -> let (l, r) = parseAdvise s in upd doc r $ BAdvise l
            "*" -> let (l, r) = parseItemize s in upd doc r $ BItemize l
            "-" -> let (l, r) = parseEnumerate s in upd doc r $ BEnumerate l

            _ -> upd doc xs $ BAny c b

parse' doc (TBlock b : xs)
    = upd doc xs $ BParagraph b

parse' (Document blocks prop) []
    = Document (reverse blocks) prop


mkDefine :: String -> Block
mkDefine xs =
    let (w, rs) = break (== ':') xs
    in  BDefine w $ tail rs

mkRemark :: String -> Block
mkRemark xs =
    let (w, rs) = break (== ':') xs
    in  BRemark w $ tail rs


upd :: Document -> [Token] -> Block -> Document
upd (Document blocks prop) xs block = parse' (Document (block:blocks) prop) xs

parseAdvise :: [Token] -> ([String], [Token])
parseAdvise (TControl "->" : TBlock b : xs) =
    let (l, r) = parseAdvise xs
    in  (b:l, r)
parseAdvise xs = ([], xs)

parseItemize :: [Token] -> ([String], [Token])
parseItemize (TControl "*" : TBlock b : xs) =
    let (l, r) = parseItemize xs
    in  (b:l, r)
parseItemize xs = ([], xs)

parseEnumerate :: [Token] -> ([String], [Token])
parseEnumerate (TControl "-" : TBlock b : xs) =
    let (l, r) = parseEnumerate xs
    in  (b:l, r)
parseEnumerate xs = ([], xs)


lex :: String -> [Token]
lex xs = lex' 1 1 [] SStart (xs ++ "\n\n")

lex' :: Int -> Int -> String -> State -> String -> [Token]
lex' _ _ _ _ [] = []

lex' l c m SStart s@(x:xs)
    | x == ' '          = lex' l (c+1) []  SSpace xs
    | x == '\n'         = lex' (l+1) 0 []  SStart xs
    | x == '\t'         = lex' l (c+1) []  SSymbol xs
    | isAlpha x         = lex' l (c+1) [x] SCommand xs
    | otherwise         = lex' l (c+1) [x] SControl xs

lex' l c m SNewline s@(x:xs)
    | x == '\n'         = mkBlock m : lex' (l+1) 0 m SStart xs
    | x == ' '          = lex' l (c+1) m SSpace xs
    | x == '\t'         = lex' l (c+1) ('\n':m) SSymbol xs
    | isAlpha x         = mkBlock m : lex' l (c+1) [x] SCommand xs
    | otherwise         = mkBlock m : lex' l (c+1) [x] SControl xs

lex' l c m SCommand s@(x:xs)
    | x == '\n'         = TCommand (reverse m) [] : lex' (l+1) 0 [] SStart xs
    | x == ' '          = lexCommand l (c+1) (reverse m) "" [] xs
    | x == '\t'         = lexCommand l (c+1) (reverse m) "" [] xs
    | otherwise         = lex' l (c+1) (x:m) SCommand xs

lex' l c m SControl s@(x:xs)
    | x == ' ' && c < 4 = TControl (reverse m) : lex' l (c+1) [] SControlAfter xs
    | x == ' '          = TControl (reverse m) : lex' l (c+1) [] SSymbol xs
    | x == '\t'         = TControl (reverse m) : lex' l (c+1) [] SSymbol xs
    | x == '\n'         = TControl (reverse m) : lex' (l+1) 0 [] SNewline xs
    | otherwise         = lex' l (c+1) (x:m) SControl xs

lex' l c m SControlAfter s@(x:xs)
    | x == '\n'         = lex' (l+1) 0 [] SNewline xs
    | x == ' ' && c < 4 = lex' l (c+1) [] SControlAfter xs
    | otherwise         = lex' l (c+1) [x] SSymbol xs

lex' l c m SSymbol s@(x:xs)
    | x == '\n'         = lex' (l+1) 0 m SNewline xs
    | otherwise         = lex' l (c+1) (x:m) SSymbol xs

lex' l c m SSpace s@(x:xs)
    | x == '\n'         = mkBlock m : lex' (l+1) 0 m SStart xs
    | x == ' ' && c < 4 = lex' l (c+1) m SSpace xs
    | x == '\t'         = lex' l (c+1) ('\n':m) SSymbol xs
    | otherwise         = lex' l (c+1) (x:'\n':m) SSymbol xs

lexCommand l c cmd b a s@(x:xs)
    | x == ' '          = lexCommand l (c+1) cmd "" (reverse b : a) xs
    | x == '\t'         = lexCommand l (c+1) cmd "" (reverse b : a) xs
    | x == '\n'         = TCommand cmd (reverse $ filter (/= "") (reverse b : a)) : lex' (l+1) 0 [] SStart xs
    | otherwise         = lexCommand l (c+1) cmd (x:b) a xs

mkBlock = TBlock . dropWhile (== '\n') . reverse

