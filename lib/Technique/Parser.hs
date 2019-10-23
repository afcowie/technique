{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-|

/Commentary/

We're optimizing for simplicity here. The language balances conventions
from other languages with choices to not overcomplicate things. Not
overloading operators, for example. Mostly we want to have "good error
messages" which is tough and subjective anyway. Not having multiline
anything, for example, might be a good choice, except that we also want to
be whitepsace insensitive.
-}
module Technique.Parser where

import Control.Monad
import Control.Monad.Combinators
import Core.Text.Rope
import Data.Void (Void)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Technique.Language
import Technique.Quantity

type Parser = Parsec Void Text

__VERSION__ :: Int
__VERSION__ = 0

ignore :: Parser a -> Parser ()
ignore p = void (try p <|> fail "No parse")

{-|
Skip /zero/ or more actual space characters. The __megaparsec__ function
@space@ etc consume all whitespace, not just ' '. That includes newlines,
which is very unhelpful.
-}
skipSpace :: Parser ()
skipSpace = void (hidden (many (char ' ')))

{-|
Skip at least /one/ actual space character.
-}
skipSpace1 :: Parser ()
skipSpace1 = void (hidden (some (char ' ')))

pMagicLine :: Parser Int
pMagicLine = do
    void (char '%') <?> "first line to begin with % character"
    void spaceChar <?> "a space character"
    void (string "technique")
    void spaceChar <?> "a space character"
    void (char 'v') <?> "the character v and then a number"
    v <- L.decimal <?> "the language version"
    void newline
    return v

pSpdxLine :: Parser (Text,Maybe Text)
pSpdxLine = do
    void (char '!') <?> "second line to begin with ! character"
    void spaceChar <?> "a space character"

    license <- takeWhile1P (Just "software license description (ie an SPDX-Licence-Header value)") (\c -> not (c == ',' || c == '\n'))

    copyright <- optional $ do
        void (char ',') <?> "a comma"
        hidden $ skipMany (spaceChar <?> "a space character")
        void (char '©') <|> void (string "(c)")
        void (spaceChar <?> "a space character")
        takeWhile1P (Just "a copyright declaration") (/= '\n')

    void newline
    return (license,copyright)

pProcfileHeader :: Parser Technique
pProcfileHeader = do
    version <- pMagicLine
    unless (version == __VERSION__) (fail ("currently the only recognized language version is v" ++ show __VERSION__))
    (license,copyright) <- pSpdxLine

    return $ Technique
        { techniqueVersion = version
        , techniqueLicense = intoRope license
        , techniqueCopyright = fmap intoRope copyright
        , techniqueBody = []
        }

---------------------------------------------------------------------

pProcedureDeclaration :: Parser (Identifier,[Identifier],[Type],Type)
pProcedureDeclaration = do
    name <- pIdentifier
    skipSpace
    -- zero or more separated by comma
    params <- sepBy pIdentifier (char ',')

    skipSpace
    void (char ':')
    skipSpace

    ins <- sepBy pType (char ',')

    skipSpace
    void (string "->")
    skipSpace

    out <- pType
    return (name,params,ins,out)

identifierChar :: Parser Char
identifierChar = hidden (lowerChar <|> digitChar <|> char '_')

pIdentifier :: Parser Identifier
pIdentifier = label "a valid identifier" $ do
    first <- lowerChar
    remainder <- many identifierChar
    return (Identifier (singletonRope first <> intoRope remainder))

typeChar :: Parser Char
typeChar = hidden (upperChar <|> lowerChar <|> digitChar)

pType :: Parser Type
pType = label "a valid type" $ do
    first <- upperChar
    remainder <- many typeChar
    return (Type (singletonRope first <> intoRope remainder))


---------------------------------------------------------------------

-- TODO What is special about L.charLiteral vs just using normal parsers?

stringLiteral0 :: Parser Text
stringLiteral0 = do
    void (char '\"')
    str <- takeWhileP Nothing (/= '"')
    notFollowedBy eol
    void (char '\"')
    return str

stringLiteral :: Parser Text
stringLiteral = label "a string literal" $ do
    void (char '\"')
    str <- many (do
        try (do
            void (char '\\')
            void (char '"')
            return '"')
        <|> try (do
            notFollowedBy (char '\"')
            printChar)
        )
    void (char '\"')
    return (T.pack str)

-- FIXME change this to numbers with decimal points!
-- FIXME read? Really?
numberLiteral :: Parser Int
numberLiteral = label "a number literal" $ do
    sign <- optional (char '-')
    digits <- some digitChar
    let number = read digits
    return (case sign of
        Just _ -> negate number
        Nothing -> number)

pQuantity :: Parser Quantity
pQuantity = do
    try (do
        str <- stringLiteral
        return (Text (intoRope str)))
    <|> try (do
        num <- numberLiteral
        return (Number num))

pExpression :: Parser Expression
pExpression =
    try pNone <|>
    try pUndefined <|>
    try pGrouping <|>
    try pApplication <|>
    try pLiteral <|>
    try pVariable
  where
    pNone = do
        void (string "()")
        return (Literal None)
    pUndefined = do
        void (char '?')
        return (Literal Undefined)
    pGrouping = do
        between (char '(') (char ')') $ do
            subexpr <- pExpression
            return (Grouping subexpr)
    pApplication = do
        name <- pIdentifier
        -- ie at least one space
        skipSpace1
        -- FIXME better do this manually, not all valid
        subexpr <- pExpression
        return (Application name subexpr)
    pLiteral = do
        qty <- pQuantity
        return (Literal qty)
    pVariable = do
        name <- pIdentifier
        return (Variable name)

pStatement :: Parser Statement
pStatement =
    try pAssignment <|>
    try pDeclaration <|>
    try pExecute <|>
    try pBlank <|>
    try pSeries
  where
    pAssignment = label "an assignment" $ do
        name <- pIdentifier
        skipSpace
        void (char '=')
        skipSpace
        expr <- pExpression
        return (Assignment name expr)

    pDeclaration = label "a declaration" $ do
        -- only dive into working out if this is a Procedure if there's a ':' here
        proc <- pProcedureFunction
        return (Declaration proc)

    pExecute = label "a value to execute" $ do
        expr <- pExpression
        return (Execute expr)

    pBlank = hidden $ do -- label "a blank line"
        void newline
        return Blank

    pSeries = do
        skipSpace
        void (char ';')
        skipSpace
        return Series

---------------------------------------------------------------------

pBlock :: Parser Block
pBlock = do
    void (char '{' <* skipSpace <* ignore newline)

    statements <- many
         (skipSpace *> pStatement <* skipSpace <* ignore newline)

    void (skipSpace *> char '}')

    return (Block statements)

pProcedureFunction :: Parser Procedure
pProcedureFunction = do
    (name,params,ins,out) <- pProcedureDeclaration

    fail (show (name,params,ins,out))
