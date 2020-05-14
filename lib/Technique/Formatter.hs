{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-matches -fno-warn-unused-imports #-}

module Technique.Formatter where

import Core.System.Pretty
import Core.Text.Rope
import Core.Text.Utilities
import Data.Foldable (foldl')
import Data.Int (Int8)
import Technique.Language
import Technique.Quantity

data TechniqueToken
  = MagicToken
  | ProcedureToken
  | TypeToken
  | SymbolToken
  | OperatorToken
  | VariableToken
  | ApplicationToken
  | LabelToken
  | StringToken
  | QuantityToken
  | RoleToken
  | ErrorToken
  | FilenameToken
  | StepToken

instance Pretty Procedure where
  pretty = unAnnotate . intoDocA

-- no use of white, suggesting colours
colourizeTechnique :: TechniqueToken -> AnsiStyle
colourizeTechnique token = case token of
  MagicToken -> color Black
  ProcedureToken -> colorDull Blue <> bold
  TypeToken -> colorDull Yellow
  SymbolToken -> colorDull Cyan <> bold
  OperatorToken -> colorDull Yellow <> bold
  VariableToken -> color Cyan
  ApplicationToken -> color Blue <> bold
  LabelToken -> color Green <> bold
  StringToken -> color Green <> bold
  QuantityToken -> color Magenta <> bold
  RoleToken -> colorDull Yellow
  ErrorToken -> color Red <> bold
  FilenameToken -> color White <> bold
  StepToken -> color Black <> bold -- for diagnostics in evalutator

instance Render Procedure where
  type Token Procedure = TechniqueToken
  colourize = colourizeTechnique
  intoDocA proc =
    let name = intoDocA . procedureName $ proc
        params = case procedureParams proc of
          [] -> emptyDoc
          xs -> commaCat xs <> " "
        from = commaCat . procedureInput $ proc
        into = intoDocA . procedureOutput $ proc
        block = intoDocA . procedureBlock $ proc
        description = case procedureDescription proc of
          Nothing -> emptyDoc
          Just text -> intoDocA text
     in description
          <> ( indent
                 4
                 ( annotate ProcedureToken name
                     <+> params
                     <> annotate SymbolToken ":"
                     <+> annotate TypeToken from
                     <+> annotate SymbolToken "->"
                     <+> annotate TypeToken into
                     <> line
                     <> block
                 )
             )

-- |
-- Punctuate a list with commas annotated with Symbol highlighting.
commaCat :: (Render a, Token a ~ TechniqueToken) => [a] -> Doc (Token a)
commaCat = hcat . punctuate (annotate SymbolToken comma) . fmap (annotate VariableToken . intoDocA)

instance Render Type where
  type Token Type = TechniqueToken
  colourize = colourizeTechnique
  intoDocA (Type name) = annotate TypeToken (pretty name)

instance Render Markdown where
  type Token Markdown = TechniqueToken
  colourize = colourizeTechnique
  intoDocA (Markdown text) = pretty text

instance Render Block where
  type Token Block = TechniqueToken
  colourize = colourizeTechnique
  intoDocA (Block statements) =
    nest
      4
      ( annotate SymbolToken lbrace
          <> go statements
      )
      <> line
      <> annotate SymbolToken rbrace
    where
      go :: [Statement] -> Doc TechniqueToken
      go [] = emptyDoc
      go (x@(Series _) : x1 : xs) = intoDocA x <> intoDocA x1 <> go xs
      go (x : xs) = line <> intoDocA x <> go xs

instance Render Statement where
  type Token Statement = TechniqueToken
  colourize = colourizeTechnique
  intoDocA statement = case statement of
    Assignment _ vars expr ->
      commaCat vars <+> annotate SymbolToken "=" <+> intoDocA expr
    Execute _ expr ->
      intoDocA expr
    Comment _ text ->
      "-- " <> pretty text -- TODO what about multiple lines?
    Declaration _ proc ->
      intoDocA proc
    Blank _ ->
      emptyDoc
    Series _ ->
      annotate SymbolToken " ; "

instance Render Attribute where
  type Token Attribute = TechniqueToken
  colourize = colourizeTechnique
  intoDocA role = case role of
    Role name -> annotate RoleToken ("@" <> pretty name)
    Place name -> annotate RoleToken ("#" <> pretty name)
    Inherit -> annotate ErrorToken "Inherit"

instance Render Expression where
  type Token Expression = TechniqueToken
  colourize = colourizeTechnique
  intoDocA expr = case expr of
    Application _ name subexpr ->
      annotate ApplicationToken (intoDocA name) <+> intoDocA subexpr
    None _ ->
      annotate SymbolToken ("()")
    Undefined _ ->
      annotate ErrorToken "?"
    Amount _ qty ->
      intoDocA qty
    Text _ text ->
      annotate SymbolToken dquote
        <> annotate StringToken (pretty text)
        <> annotate SymbolToken dquote
    Object _ tablet ->
      intoDocA tablet
    Variable _ vars ->
      commaCat vars
    Operation _ operator subexpr1 subexpr2 ->
      intoDocA subexpr1 <+> intoDocA operator <+> intoDocA subexpr2
    Grouping _ subexpr ->
      annotate SymbolToken lparen
        <> intoDocA subexpr
        <> annotate SymbolToken rparen
    Restriction _ attribute block ->
      intoDocA attribute
        <> line
        <> intoDocA block -- TODO some nesting?

instance Render Identifier where
  type Token Identifier = TechniqueToken
  colourize = colourizeTechnique
  intoDocA (Identifier name) = pretty name

instance Pretty Identifier where
  pretty = unAnnotate . intoDocA

instance Render Decimal where
  type Token Decimal = TechniqueToken
  colourize = colourizeTechnique
  intoDocA = pretty . decimalToRope

instance Render Quantity where
  type Token Quantity = TechniqueToken
  colourize = colourizeTechnique
  intoDocA qty = case qty of
    Number i ->
      annotate QuantityToken (pretty i)
    Quantity i u m unit ->
      let measurement =
            intoDocA i <> " "
          uncertainty =
            if isZeroDecimal u
              then emptyDoc
              else "± " <> intoDocA u <> " "
          magnitude =
            if m == 0
              then emptyDoc
              else "× 10" <> numberToSuperscript m <> " "
       in annotate QuantityToken (measurement <> uncertainty <> magnitude <> pretty unit)

numberToSuperscript :: Int8 -> Doc ann
numberToSuperscript number =
  let digits = show number
      digits' = fmap toSuperscript digits
   in pretty digits'

toSuperscript :: Char -> Char
toSuperscript c = case c of
  '0' -> '⁰' -- U+2070
  '1' -> '¹' -- U+00B9
  '2' -> '²' -- U+00B2
  '3' -> '³' -- U+00B3
  '4' -> '⁴' -- U+2074
  '5' -> '⁵' -- U+2075
  '6' -> '⁶' -- U+2076
  '7' -> '⁷' -- U+2077
  '8' -> '⁸' -- U+2078
  '9' -> '⁹' -- U+2079
  '-' -> '⁻' -- U+207B
  _ -> error "Invalid, digit expected"

instance Pretty Quantity where
  pretty = unAnnotate . intoDocA

instance Render Tablet where
  type Token Tablet = TechniqueToken
  colourize = colourizeTechnique
  intoDocA (Tablet bindings) =
    nest
      4
      ( annotate SymbolToken lbracket
          <> foldl' g emptyDoc bindings
      )
      <> line
      <> annotate SymbolToken rbracket
    where
      g :: Doc TechniqueToken -> Binding -> Doc TechniqueToken
      g built binding = built <> line <> intoDocA binding

instance Render Label where
  type Token Label = TechniqueToken
  colourize = colourizeTechnique
  intoDocA (Label text) =
    annotate SymbolToken dquote
      <> annotate LabelToken (pretty text)
      <> annotate SymbolToken dquote

instance Pretty Label where
  pretty = unAnnotate . intoDocA

-- the annotation for the label duplicates the code Quantity's Text
-- constructor, but for the LabelToken token. This distinction may not be
-- necessary (at present we have the same colouring for both).
instance Render Binding where
  type Token Binding = TechniqueToken
  colourize = colourizeTechnique
  intoDocA (Binding label subexpr) =
    intoDocA label
      <+> annotate SymbolToken "~"
      <+> intoDocA subexpr

instance Render Operator where
  type Token Operator = TechniqueToken
  colourize = colourizeTechnique
  intoDocA operator =
    annotate OperatorToken $ case operator of
      WaitBoth -> pretty '&'
      WaitEither -> pretty '|'
      Combine -> pretty '+'

instance Render Technique where
  type Token Technique = TechniqueToken
  colourize = colourizeTechnique
  intoDocA technique =
    let version = pretty . techniqueVersion $ technique
        license = pretty . techniqueLicense $ technique
        copyright = case techniqueCopyright technique of
          Just owner -> "; ©" <+> pretty owner
          Nothing -> emptyDoc
        body = fmap intoDocA . techniqueBody $ technique
     in annotate MagicToken ("%" <+> "technique" <+> "v" <> version) <> line
          <> annotate MagicToken ("!" <+> license <> copyright)
          <> line
          <> line
          <> vsep (punctuate line body)
