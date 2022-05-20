{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Tiger.Syntax.Pretty (pprExpr) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Prettyprinter
import Tiger.Syntax.Parser.Ast
import Tiger.Syntax.Position

pprExpr :: Expr -> ByteString
pprExpr = BC.pack . show . pretty

instance Pretty Expr where
  pretty = \case
    ENil span ->
      prettyNode
        "ENil"
        [ prettyProp "loc" (prettyLoc span)
        ]
    EBreak span ->
      prettyNode
        "EBreak"
        [ prettyProp "loc" (prettyLoc span)
        ]
    EInteger span value ->
      prettyNode
        "EInteger"
        [ prettyProp "loc" (prettyLoc span),
          prettyProp "value" (pretty value)
        ]
    EString span value ->
      prettyNode
        "EString"
        [ prettyProp "loc" (prettyLoc span),
          prettyProp "value" (prettyByteString value)
        ]
    EFunctCall span ident args ->
      prettyNode
        "EFunctCall"
        [ prettyProp "loc" (prettyLoc span),
          prettyProp "funct" (pretty ident),
          prettyProp "args" (prettyNodes args)
        ]
    EOp span lft op rgt ->
      prettyNode
        "EOp"
        [ prettyProp "loc" (prettyLoc span),
          prettyProp "left" (pretty lft),
          prettyProp "op" (pretty op),
          prettyProp "right" (pretty rgt)
        ]
    EIf span pred conseq alt ->
      prettyNode
        "EIf"
        [ prettyProp "loc" (prettyLoc span),
          prettyProp "predicate" (pretty pred),
          prettyProp "consequent" (pretty conseq),
          prettyProp "alternative" (pretty alt)
        ]
    EWhile span test body ->
      prettyNode
        "EWhile"
        [ prettyProp "loc" (prettyLoc span),
          prettyProp "test" (pretty test),
          prettyProp "body" (pretty body)
        ]
    EFor span ident lo hi body ->
      prettyNode
        "EFor"
        [ prettyProp "loc" (prettyLoc span),
          prettyProp "var" (pretty ident),
          prettyProp "lo" (pretty lo),
          prettyProp "hi" (pretty hi),
          prettyProp "body" (pretty body)
        ]
    EAssign span var body ->
      prettyNode
        "EAssign"
        [ prettyProp "loc" (prettyLoc span),
          prettyProp "var" (pretty var),
          prettyProp "body" (pretty body)
        ]
    ELet span decs body ->
      prettyNode
        "LetExp"
        [ prettyProp "loc" (prettyLoc span),
          prettyProp "decs" (prettyNodes decs),
          prettyProp "body" (pretty body)
        ]
    ESeq span exprs ->
      prettyNode
        "ESeq"
        [ prettyProp "loc" (prettyLoc span),
          prettyProp "exprs" (prettyNodes exprs)
        ]
    EArray span tyId size body ->
      prettyNode
        "EArray"
        [ prettyProp "loc" (prettyLoc span),
          prettyProp "typ" (pretty tyId),
          prettyProp "size" (pretty size),
          prettyProp "init" (pretty body)
        ]
    ERecord span tyId fields ->
      prettyNode
        "ERecord"
        [ prettyProp "loc" (prettyLoc span),
          prettyProp "typ" (pretty tyId),
          prettyProp "fields" (prettyNodes fields)
        ]
    EVar span var ->
      prettyNode
        "Var"
        [ prettyProp "loc" (prettyLoc span),
          prettyProp "var" (pretty var)
        ]

instance Pretty Dec where
  pretty = \case
    VarDec span name ty body ->
      prettyNode
        "VarDec"
        [ prettyProp "loc" (prettyLoc span),
          prettyProp "name" (pretty name),
          prettyProp "typ" (prettyMaybe pretty ty),
          prettyProp "init" (pretty body)
        ]
    TypeDec span name ty ->
      prettyNode
        "TypeDec"
        [ prettyProp "loc" (prettyLoc span),
          prettyProp "name" (pretty name),
          prettyProp "typ" (pretty ty)
        ]
    FunctDec span name params ty body ->
      prettyNode
        "FunctDec"
        [ prettyProp "loc" (prettyLoc span),
          prettyProp "name" (pretty name),
          prettyProp "params" (prettyNodes params),
          prettyProp "typ" (prettyMaybe pretty ty),
          prettyProp "body" (pretty body)
        ]

instance Pretty Ty where
  pretty = \case
    TyName span tyId ->
      prettyNode
        "TyAlias"
        [ prettyProp "loc" (prettyLoc span),
          prettyProp "typ" (pretty tyId)
        ]
    TyRecord span tyFields ->
      prettyNode
        "TyRecord"
        [ prettyProp "loc" (prettyLoc span),
          prettyProp "fields" (prettyNodes tyFields)
        ]
    TyArray span tyId ->
      prettyNode
        "TyArray"
        [ prettyProp "loc" (prettyLoc span),
          prettyProp "typ" (pretty tyId)
        ]

instance Pretty TyField where
  pretty (TyField span name tyId) =
    prettyNode
      "TyField"
      [ prettyProp "loc" (prettyLoc span),
        prettyProp "name" (pretty name),
        prettyProp "typ" (pretty tyId)
      ]

instance Pretty Var where
  pretty = \case
    SimpleVar span ident ->
      prettyNode
        "SimpleVar"
        [ prettyProp "loc" (prettyLoc span),
          prettyProp "ident" (pretty ident)
        ]
    FieldVar span var ident ->
      prettyNode
        "FieldVar"
        [ prettyProp "loc" (prettyLoc span),
          prettyProp "var" (pretty var),
          prettyProp "ident" (pretty ident)
        ]
    SubscriptVar span var expr ->
      prettyNode
        "SubscriptVar"
        [ prettyProp "loc" (prettyLoc span),
          prettyProp "var" (pretty var),
          prettyProp "expr" (pretty expr)
        ]

instance Pretty Op where
  pretty = dquotes . go
    where
      go = \case
        Plus -> "+"
        Minus -> "-"
        Times -> "*"
        Div -> "/"
        Eq -> "="
        NEq -> "<>"
        Gt -> ">"
        Lt -> "<"
        Ge -> ">="
        Le -> "<="
        And -> "&"
        Or -> "|"

instance Pretty Symbol where
  pretty (Symbol ident) = (pretty . BC.unpack) ident

prettyByteString :: ByteString -> Doc ann
prettyByteString = dquotes . pretty . BC.unpack

prettyMaybe :: Pretty a => (a -> Doc ann) -> Maybe a -> Doc ann
prettyMaybe f = \case
  Just value -> f value
  Nothing -> "None"

prettyNode :: String -> [Doc ann] -> Doc ann
prettyNode name props =
  vsep
    [ pretty name <+> lbrace,
      sepNode props,
      rbrace
    ]

prettyNodes :: Pretty a => [a] -> Doc ann
prettyNodes nodes =
  vsep
    [ lbracket,
      indent 1 $ sepNode (fmap pretty nodes),
      rbracket
    ]

sepNode :: [Doc ann] -> Doc ann
sepNode = vsep . map (<> comma)

prettyProp :: Doc ann -> Doc ann -> Doc ann
prettyProp name value = indent 1 $ name <> colon <+> value

prettyLoc :: Span -> Doc ann
prettyLoc (Span start end) =
  prettyNode
    "Span"
    [ prettyProp "start" (prettyPosition start),
      prettyProp "end" (prettyPosition end)
    ]

prettyPosition :: Position -> Doc ann
prettyPosition (Position offset line column) =
  prettyNode
    "Position"
    [ prettyProp "offset" (pretty offset),
      prettyProp "line" (pretty line),
      prettyProp "column" (pretty column)
    ]
