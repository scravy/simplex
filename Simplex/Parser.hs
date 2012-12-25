module Simplex.Parser (
        lex, parse,
        Token(..), Block(..), Document (Document),
        Table, Cell(..), CellType(..), RowType(..),
        Items(..), ItemType(..)
    ) where

import Simplex.Util

import Data.List (intersperse, elemIndex)
import Data.Char (isAlpha, isDigit, isLower, isUpper)
import Data.Maybe

import Prelude hiding (lex)

data Block =
          BAny String String
        | BParagraph String
        | BSection String
        | BSubsection String
        | BSubsubsection String
        | BChapter String
        | BPart String
        | BDefine String String
        | BRemark String String
        | BVerbatim String String
        | BImport String String
        | BAdvise [String]
        | BItems Items
        | BDescription [(String, String)]
        | BDescribeItems [(String, String)]
        | BTherefore String
        | BBecause String
        | BNTherefore String
        | BNBecause String
        | BIff String
        | BNIff String
        | BLine
        | BTable Table
        | BCommand String [String]
    deriving (Eq, Show)

type Table = (String, [String], [(RowType, [(Cell, String)])])

data ItemType = Enumerate | Itemize
    deriving (Eq, Show)

data Items = Item String | Items ItemType [Items]
    deriving (Eq, Show)

data Cell = Cell (Maybe String) (Maybe Char) Int Int Bool Bool CellType
    deriving (Eq, Show)

data CellType = Default | Math | Verb | Head
    deriving (Eq, Show)

data RowType = NoBorder | SingleBorder | DoubleBorder
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
            concat' = concat . intersperse "\n\n"
        in  upd doc bs $ BVerbatim cs $ concat' $ map (\(TBlock x) -> x) b

parse' doc s@(TControl c@('>':_) : xs)
    =   let (t, r) = parseTable s in upd doc r $ BTable t

parse' doc s@(TControl c : TBlock b : xs)
    =   case c of
            "."   -> upd doc xs $ BParagraph b
            "="   -> upd doc xs $ BSection b
            "=="  -> upd doc xs $ BSubsection b
            "===" -> upd doc xs $ BSubsubsection b

            "!!"  -> upd doc xs $ BChapter b
            "!!!" -> upd doc xs $ BPart b

            "=>"  -> upd doc xs $ BTherefore b
            "<="  -> upd doc xs $ BBecause b
            "=!>" -> upd doc xs $ BNTherefore b
            "<!=" -> upd doc xs $ BNBecause b
            "<=>" -> upd doc xs $ BIff b
            "<!>" -> upd doc xs $ BNIff b

            ":="  -> upd doc xs $ mkDefine b
            ":-"  -> upd doc xs $ mkRemark b

            "*"   -> let (l, r) = parseItems s in upd doc r $ BItems l
            "+"   -> let (l, r) = parseItems s in upd doc r $ BItems l
            "-"   -> let (l, r) = parseItems s in upd doc r $ BItems l

            ":"   -> let (l, r) = parseDescription s in upd doc r $ BDescription l
            "::"  -> let (l, r) = parseDescribeItems s in upd doc r $ BDescribeItems l
            "->"  -> let (l, r) = parseAdvise s in upd doc r $ BAdvise l

            _ -> upd doc xs $ BAny c b

parse' doc (TBlock b : xs)
    = upd doc xs $ BParagraph b

parse' (Document blocks prop) []
    = Document (reverse blocks) prop

mkDefine :: String -> Block
mkDefine xs
  = let (w, rs) = break (== ':') xs
    in  BDefine w $ tail rs

mkRemark :: String -> Block
mkRemark xs
  = let (w, rs) = break (== ':') xs
    in  BRemark w $ tail rs

upd :: Document -> [Token] -> Block -> Document
upd (Document blocks prop) xs block = parse' (Document (block:blocks) prop) xs

parseAdvise :: [Token] -> ([String], [Token])
parseAdvise (TControl "->" : TBlock b : xs)
  = let (l, r) = parseAdvise xs
    in  (b:l, r)
parseAdvise xs = ([], xs)

parseDescription :: [Token] -> ([(String, String)], [Token])
parseDescription (TControl ":" : TBlock b : xs)
  = let (l, r) = parseDescription xs
    in  ((parseItem b):l, r)
parseDescription xs = ([], xs)

parseDescribeItems :: [Token] -> ([(String, String)], [Token])
parseDescribeItems (TControl "::" : TBlock b : xs)
  = let (l, r) = parseDescribeItems xs
    in  ((parseItem b):l, r)
parseDescribeItems xs = ([], xs)

parseItems :: [Token] -> (Items, [Token])
parseItems (TControl "*" : TBlock b : xs) = parseIt [Items Itemize   [Item b]] xs
parseItems (TControl "+" : TBlock b : xs) = parseIt [Items Enumerate [Item b]] xs

parseIt [Items t it, Items Itemize is] s@(TControl "*" : TBlock b : xs)
  = parseIt [Items Itemize $ Item b:Items t (reverse it):is] xs

parseIt [Items Itemize is] s@(TControl "*" : TBlock b : xs)
  = parseIt [Items Itemize $ Item b:is] xs

parseIt [ix] s@(TControl "**" : TBlock b : xs)
  = parseIt [Items Itemize [Item b], ix] xs

parseIt (Items Itemize is : ix) s@(TControl "**" : TBlock b : xs)
  = parseIt (Items Itemize (Item b:is) : ix) xs


parseIt [Items t it, Items Enumerate is] s@(TControl [x] : TBlock b : xs)
    | elem x "+-" = parseIt [Items Enumerate $ Item b:Items t (reverse it):is] xs

parseIt [Items Enumerate is] s@(TControl [x] : TBlock b : xs)
    | elem x "+-" = parseIt [Items Enumerate $ Item b:is] xs

parseIt [ix] s@(TControl "++" : TBlock b : xs)
  = parseIt [Items Enumerate [Item b], ix] xs

parseIt (Items Enumerate is : ix) s@(TControl "++" : TBlock b : xs)
  = parseIt (Items Enumerate (Item b:is) : ix) xs


parseIt ix@(Items t is:xs) s = (reduce ix, s)
    where
        reduce (Items t1 i1:Items t2 i2:ix) = reduce (Items t2 (Items t1 (reverse i1) : i2):ix)
        reduce [Items t is] = Items t $ reverse is

parseIt _ s = (Items Itemize [], s)


parseItem i
    | r == ""   = ("", w)
    | otherwise = (w, tail' r)
        where (w, r) = break (== ':') i

parseTable :: [Token] -> (Table, [Token])
parseTable = parseTable' ("", [], [(NoBorder, [])])

parseTable' :: Table -> [Token] -> (Table, [Token])

parseTable' (caption, opt, rows) (TControl ">@" : TBlock b : xs)
  = parseTable' (caption, (b:opt), rows) xs

parseTable' (caption, opt, rows) (TControl ">>" : TBlock b : xs)
  = parseTable' (b, opt, rows) xs

parseTable' (caption, opt, rows@((t,r):rs)) (TControl ">-" : TBlock b : xs)
  = parseTable' (caption, opt, ((NoBorder, []) : (SingleBorder, r) : rs)) xs

parseTable' (caption, opt, rows@((t,r):rs)) (TControl ">=" : TBlock b : xs)
  = parseTable' (caption, opt, ((NoBorder, []) : (DoubleBorder, r) : rs)) xs

parseTable' (caption, opt, rows@((t,r):rs)) (TControl ">+" : TBlock b : xs)
  = parseTable' (caption, opt, ((NoBorder, []) : (NoBorder, r) : rs)) xs

parseTable' (caption, opt, rows@((t,r):rs)) (TControl ('>':c) : TBlock b : xs)
  = parseTable' (caption, opt, ((t, (parseCell c, b):r):rs)) xs

parseTable' (caption, opt, rows) xs
  = ((caption, reverse opt, map (\(t,r) -> (t, reverse r)) $ reverse rows), xs)

parseCell c
  = let
        digits = filter isDigit c
        upper  = filter isUpper c
        lower  = filter isLower c
        comma  = elemIndex ',' c

        split  = splitAt (fromJust comma) c
        left   = filter isDigit $ fst split
        right  = filter isDigit $ snd split

        pos x
            | x < 1 = 1
            | otherwise = x

        colSpan
            | isJust comma && left /= [] = pos $ read left
            | isNothing comma && digits /= [] = pos $ read digits
            | otherwise    = 1

        rowSpan
            | isJust comma && right /= [] = read right
            | otherwise    = 1

        color = case lower of
            "red" -> Just "red"
            "yellow" -> Just "yellow"
            "blue" -> Just "blue"
            "green" -> Just "green"
            "gray" -> Just "gray"
            _ -> Nothing

        align = case upper of
            "L" -> Just 'l'
            "R" -> Just 'r'
            "C" -> Just 'c'
            _ -> Nothing

        typ
            | '$' `elem` c = Math
            | '#' `elem` c = Verb
            | '!' `elem` c = Head
            | otherwise    = Default

    in  Cell color align colSpan rowSpan (head c == '|') (last c == '|') typ

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

