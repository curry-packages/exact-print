module Curry.ExactPrint where

import Data.List
import Data.Maybe

import Curry.Ident
import Curry.Types
import Curry.Comment
import Curry.Position
import Curry.Span
import Curry.SpanInfo
import Curry.ExactPrintClass

import Prelude hiding ( empty )

import Debug.Trace ( trace )

instance ExactPrint (Module a) where
  printS (Module spi _ ps mid mex im ds) = fill $
    if null ss
      then do
        printNode ps
        printNode im
        printNode ds
      else do
        printNode ps
        printHeader
        printNode im
        printNode ds
    where
      SpanInfo _ ss = spi
      printHeader = case mex of
        Nothing -> printNode mid
        Just ex -> printNode mid >> printNode ex
  keywords (Module spi _ _ _ _ _ _) =
    if null ss then [] else ["module", "where"]
    where
      SpanInfo _ ss = spi

instance ExactPrint ExportSpec where
  printS (Exporting _ ex) = fill $ printNode ex
  keywords (Exporting _ ex) =
    ["("] ++ replicate (length ex - 1) "," ++ [")"]

instance ExactPrint Export where
  printS (Export         _ i   ) = fill $ printNode i
  printS (ExportTypeWith _ i is) = fill $ printNode i >> printNode is
  printS (ExportTypeAll  _ i   ) = fill $ printNode i
  printS (ExportModule   _ i   ) = fill $ printNode i
  keywords (Export         _ _   ) = []
  keywords (ExportTypeWith _ _ is) =
    ["("] ++ replicate (length is - 1) "," ++ [")"]
  keywords (ExportTypeAll  _ _   ) = ["(", "..", ")"]
  keywords (ExportModule   _ _   ) = ["module"]

instance ExactPrint ImportDecl where
  printS (ImportDecl _ mid _ as spec) = fill $ do
    printNode mid
    maybe empty printNode as
    maybe empty printNode spec
  keywords (ImportDecl _ _ q as _) =
    ["import"] ++
    (if q         then ["qualified"] else []) ++
    (if isJust as then ["as"]        else [])

instance ExactPrint ImportSpec where
  printS (Importing _ im) = fill $ printNode im
  printS (Hiding    _ im) = fill $ printNode im
  keywords (Importing _ im) =
              ["("] ++ replicate (length im - 1) "," ++ [")"]
  keywords (Hiding    _ im) =
    ["hiding", "("] ++ replicate (length im - 1) "," ++ [")"]

instance ExactPrint Import where
  printS (Import         _ i   ) = fill $ printNode i
  printS (ImportTypeWith _ i is) = fill $ printNode i >> printNode is
  printS (ImportTypeAll  _ i   ) = fill $ printNode i
  keywords (Import         _ _   ) = []
  keywords (ImportTypeWith _ _ is) =
    ["("] ++ replicate (length is - 1) "," ++ [")"]
  keywords (ImportTypeAll  _ _   ) = ["(", "..", ")"]

instance ExactPrint ModulePragma where
  printS (LanguagePragma _ es) = fill $ printListAt es
  printS (OptionsPragma spi t s) = fill $
    let str = ppTool t s
        SpanInfo sp _ = spi
    in printStringAt (Span (incr (start sp) 12)
                           (incr (start sp) (12 + length str)))
                     str

  keywords (LanguagePragma _ es)  =
    ["{-# LANGUAGE"] ++ replicate (length es - 1) "," ++  [" #-}"]
  keywords (OptionsPragma _ _ _) = ["{-# OPTIONS", " #-}"]

ppTool :: Maybe Tool -> String -> String
ppTool Nothing  opts = opts
ppTool (Just (KnownTool t)) opts = case t of
  KICS2         -> "_KICS2 "       ++ opts
  PAKCS         -> "_PAKCS "       ++ opts
  CYMAKE        -> "_CYMAKE "      ++ opts
  FRONTEND      -> "_FRONTEND "    ++ opts
ppTool (Just (UnknownTool s)) opts = "_" ++ s ++ " " ++ opts

instance PrintAt Extension where
  printString (KnownExtension   _ e) = show e
  printString (UnknownExtension _ e) = e
  printSpan (KnownExtension p' e) = case p' of 
    SpanInfo (Span p _) _ -> Span p (incr p (length (show e)))
    _ -> error "printSpan Extension: NoSpan"
  printSpan (UnknownExtension p' e) = case p' of
    SpanInfo (Span p _) _ -> Span p (incr p (length e))
    _ -> error "printSpan Extension: NoSpan"

instance ExactPrint (Decl a) where
  printS (InfixDecl _ _ _ ids) = fill $ printNode ids
  printS (DataDecl _ ty vs cns der) = fill $
    printNode ty >> printNode vs >> printNode cns >> printNode der
  printS (ExternalDataDecl _ ty vs) = fill $
    printNode ty >> printNode vs
  printS (NewtypeDecl _ ty vs cn der) = fill $
    printNode ty >> printNode vs >> printNode cn >> printNode der
  printS (TypeDecl _ ty vs ty') = fill $
    printNode ty >> printNode vs >> printNode ty'
  printS (TypeSig _ fs ty) = fill $ printNode fs >> printNode ty
  printS (FunctionDecl _ _ _ eqs) = fill $ printNode eqs
  printS (ExternalDecl _ vs) = fill $ printNode $ map unVar vs
    where unVar (Var _ i) = i
  printS (PatternDecl _ p r) = fill $ printNode p >> printNode r
  printS (FreeDecl _ vs) = fill $ printNode $ map unVar vs
    where unVar (Var _ i) = i
  printS (DefaultDecl _ tys) = fill $ printNode tys
  printS (ClassDecl _ _ ctx c v fdeps ds) = fill $
    printNode ctx >> printNode c >> printNode v >> printNode fdeps >> printNode ds
  printS (InstanceDecl _ _ ctx c ts ds) = fill $
    printNode ctx >> printNode c >> printNode ts >> printNode ds

  keywords (InfixDecl _ f Nothing ids) =
    [show f] ++            replicate (length ids - 1) ","
  keywords (InfixDecl _ f (Just pr) ids) =
    [show f, show pr] ++ replicate (length ids - 1) ","
  keywords (DataDecl spi _ _ cns der) =
    ["data"] ++ (if null cns then [] else ["="]) ++
    replicate (length cns - 1) "|" ++
    case br of
      0 -> []
      1 -> ["deriving"]
      _ -> ["deriving", "("] ++ replicate (length der - 1) "," ++ [")"]
    where
      SpanInfo _ ss  = spi
      br = length ss - length cns - length der
  keywords (ExternalDataDecl _ _ _) = ["external", "data"]
  keywords (NewtypeDecl spi _ _ _ der) =
    ["newtype", "="] ++
    case br of
      0 -> []
      1 -> ["deriving"]
      _ -> ["deriving", "("] ++ replicate (length der - 1) "," ++ [")"]
    where
      SpanInfo _ ss = spi
      br = length ss - 2 - length der
  keywords (TypeDecl _ _ _ _) =
    ["type", "="]
  keywords (TypeSig _ fs _) =
    replicate (length fs - 1) "," ++ ["::"]
  keywords (FunctionDecl _ _ _ _) = []
  keywords (ExternalDecl _ vs) =
    replicate (length vs - 1) "," ++ ["external"]
  keywords (PatternDecl _ _ _) = []
  keywords (FreeDecl _ vs) =
    replicate (length vs - 1) "," ++ ["free"]
  keywords (DefaultDecl _ vs) =
    ["default", "("] ++ replicate (length vs - 1) "," ++ [")"]
  keywords (ClassDecl spi _ ctx _ _ fd _) = 
    ["class"] ++ cs ++ fds ++ ["where"]
   where 
    SpanInfo _ ss = spi
    
    br = length ss - 2 - length fd > 1
    cs | null ctx && not br = []
       | not br             = ["=>"]
       | otherwise          = ["("] ++ replicate (length ctx - 1) "," ++ [")", "=>"]
    fds | null fd   = []
        | otherwise = ["|"] ++ replicate (length fd - 1) ","
  keywords (InstanceDecl spi _ ctx _ _ _) = 
    ["instance"] ++ cs ++ ["where"]
   where
    SpanInfo _ ss = spi

    br = length ss - 2 > 1
    cs | null ctx && not br = []
       | not br             = ["=>"]
       | otherwise          = ["("] ++ replicate (length ctx - 1) "," ++ [")", "=>"] 

instance ExactPrint FunDep where
  printS (FunDep _ l r) = fill $ printNode l >> printNode r
  keywords _ = ["->"]

instance ExactPrint ConstrDecl where
  printS (ConstrDecl _ i tys) = fill $ printNode i >> printNode tys
  printS (ConOpDecl _ ty1 i ty2) = fill $
    printNode ty1 >> printNode i >> printNode ty2
  printS (RecordDecl _ c fs) = fill $ printNode c >> printNode fs

  keywords (ConstrDecl _ _ _) = []
  keywords (ConOpDecl _ _ _ _) = []
  keywords (RecordDecl _ _ fs) =
    ["{"] ++ replicate (length fs - 1) "," ++ ["}"]

instance ExactPrint NewConstrDecl where
  printS (NewConstrDecl _ idt ty) = fill $ printNode idt >> printNode ty
  printS (NewRecordDecl _ idt (f, ty)) = fill $
    printNode idt >> printNode f >> printNode ty

  keywords (NewConstrDecl _ _ _) = []
  keywords (NewRecordDecl _ _ _) = ["{", "::" , "}"]

instance ExactPrint FieldDecl where
  printS (FieldDecl _ is ty) = fill $ printNode is >> printNode ty
  keywords (FieldDecl _ is _) =
    replicate (length is - 1) "," ++ ["::"]

instance ExactPrint QualTypeExpr where
  printS (QualTypeExpr _ ctx ty) = fill $ printNode ctx >> printNode ty
  keywords (QualTypeExpr spi ctx _) =
    if len == 0
      then ["=>"]
      else ["("] ++ replicate (length ctx - 1) "," ++  [")", "=>"]
    where
      SpanInfo _ ss = spi
      len = length ss - length ctx

instance ExactPrint TypeExpr where
  printS (ConstructorType _ q) = fill $ printNode q
  printS (ApplyType _ ty1 ty2) = fill $ printNode ty1 >> printNode ty2
  printS (VariableType _ i) = fill $ printNode i
  printS (TupleType _ tys) = fill $ printNode tys
  printS (ListType _ ty) = fill $ printNode ty
  printS (ArrowType _ ty1 ty2) = fill $ printNode ty1 >> printNode ty2
  printS (ParenType _ ty) = fill $ printNode ty
  printS (ForallType _ vs ty) = fill $ printNode vs >> printNode ty

  keywords (ConstructorType _ _) = []
  keywords (ApplyType _ _ _) = []
  keywords (VariableType _ _) = []
  keywords (TupleType _ tys) =
    ["("] ++ replicate (length tys - 1) "," ++ [")"]
  keywords (ListType _ _) = ["[", "]"]
  keywords (ArrowType _ _ _) = ["->"]
  keywords (ParenType _ _) = ["(", ")"]
  keywords (ForallType _ _ _) = ["forall", "."]

instance ExactPrint Constraint where
  printS (Constraint _ c ty) = fill $ printNode c >> printNode ty
  keywords (Constraint spi _ _) =
    if null ss then [] else ["(", ")"]
    where
      SpanInfo _ ss = spi

instance ExactPrint (Equation a) where
  printS (Equation _ _ l r) = fill $ printNode l >> printNode r
  keywords _ = []

instance ExactPrint (Lhs a) where
  printS (FunLhs _ i ps) = fill $ printNode i >> printNode ps
  printS (OpLhs _ p1 i p2) = fill $ printNode p1 >> printNode i >> printNode p2
  printS (ApLhs _ l ps) = fill $ printNode l >> printNode ps

  keywords (FunLhs _ _ _) = []
  keywords (OpLhs spi _ _ _) = zipWith const ["`","`"] ss
    where
      SpanInfo _ ss = spi
  keywords (ApLhs _ _ _)  = ["(",")"]

instance ExactPrint (Rhs a) where
  printS (SimpleRhs  _ _ e  ds) = fill $ printNode e  >> printNode ds
  printS (GuardedRhs _ _ cs ds) = fill $ printNode cs >> printNode ds

  keywords (SimpleRhs  spi _ _ _) =
    (if snd (spanLength (head ss)) == 0
       then ["="]
       else ["->"])
    ++ if length ss == 1 then [] else ["where"]
    where SpanInfo _ ss = spi

  -- TODO: The spanInfo of `GuardedRhs` contains the span info of first pipe (`|`) 
  --       and additionally, if it exists, the span info of the `where` keyword. 
  -- 
  --       Every conditional expression of this `GuardedRhs` also contains the span info 
  --       of the corresponding pipe (`|`), and thus should be resposible for printing
  --       the `|`. By not supplying the `|` keyword here, we avoid duplicate `|`s, but
  --       in order to print the `where` keyword, we also need to return an empty string
  --       here, so that the `where` keyword is associated with the correct span info 
  --       (the second one in the list, not the first one).
  --
  --       This is a workaround, and should be fixed in the future (in the front-end).
  keywords (GuardedRhs spi _ _ _) = 
    if length ss == 1 then [] else ["", "where"]
    where SpanInfo _ ss = spi

instance ExactPrint (CondExpr a) where
  printS (CondExpr _ e1 e2) = fill $ printNode e1 >> printNode e2
  keywords (CondExpr spi _ _) =
    "|" :
    (if snd (spanLength (head (tail ss))) == 0
       then ["="]
       else ["->"])
    where SpanInfo _ ss = spi

instance ExactPrint (Pattern a) where
  printS (LiteralPattern  spi _ l) = fill $ printStringAt sp (ppLit l)
    where SpanInfo sp _ = spi
  printS (NegativePattern spi _ l) = fill $ printStringAt sp ('-' : ppLit l)
    where SpanInfo sp _ = spi
  printS (VariablePattern _ _ v) = fill $ printNode v
  printS (ConstructorPattern _ _ q ps) = fill $ printNode q >> printNode ps
  printS (InfixPattern _ _ p1 q p2) =
    fill $ printNode p1 >> printNode q >> printNode p2
  printS (ParenPattern _ p) = fill $ printNode p
  printS (RecordPattern _ _ q fs) = fill $ printNode q >> printNode fs
  printS (TuplePattern _ ps) = fill$ printNode ps
  printS (ListPattern _ _ ps) = fill $ printNode ps
  printS (AsPattern _ i p) = fill $ printNode i >> printNode p
  printS (LazyPattern _ p) = fill $ printNode p
  printS (FunctionPattern _ _ f ps) = fill $ printNode f >> printNode ps
  printS (InfixFuncPattern _ _ p1 op p2) =
    fill $ printNode p1 >> printNode op >> printNode p2

  keywords (LiteralPattern  _ _ _) = []
  keywords (NegativePattern _ _ _) = []
  keywords (VariablePattern _ _ _) = []
  keywords (ConstructorPattern spi _ _ _) =
    ["("] ++ replicate (length ss - 2) "," ++ [")"]
    where
      SpanInfo _ ss = spi
  keywords (InfixPattern spi _ _ _ _) =
    if null ss then [] else ["`", "`"]
    where
      SpanInfo _ ss = spi
  keywords (ParenPattern _ _) = ["(", ")"]
  keywords (RecordPattern _ _ _ fs) =
    ["{"] ++ replicate (length fs - 1) "," ++ ["}"]
  keywords (TuplePattern _ ps) =
    ["("] ++ replicate (length ps - 1) "," ++ [")"]
  keywords (ListPattern _ _ ps) =
    ["["] ++ replicate (length ps - 1) "," ++ ["]"]
  keywords (AsPattern _ _ _) = ["@"]
  keywords (LazyPattern _ _) = ["~"]
  keywords (FunctionPattern _ _ _ _) = []
  keywords (InfixFuncPattern _ _ _ _ _) = []

ppLit :: Literal -> String
ppLit (Char   c) = [c]
ppLit (Int    i) = show i
ppLit (Float  f) = show f
ppLit (String s) = "\"" ++ s ++ "\""

instance ExactPrint (Expression a) where
  printS (Literal spi _ l) = fill $ printStringAt sp (ppLit l)
    where SpanInfo sp _ = spi
  printS (Variable _ _ qid) = fill $ printNode qid
  printS (Constructor _ _ qid) = fill $ printNode qid
  printS (Paren _ e) = fill $ printNode e
  printS (Typed _ e ty) = fill $ printNode e >> printNode ty
  printS (Record _ _ q fs) = fill $ printNode q >> printNode fs
  printS (RecordUpdate _ e fs) = fill $ printNode e >> printNode fs
  printS (Tuple _ es) = fill $ printNode es
  printS (List _ _ es) = fill $ printNode es
  printS (ListCompr _ e stms) = fill $ printNode e >> printNode stms
  printS (EnumFrom _ e) = fill $ printNode e
  printS (EnumFromThen _ e1 e2) = fill $ printNode e1 >> printNode e2
  printS (EnumFromTo _ e1 e2) = fill $ printNode e1 >> printNode e2
  printS (EnumFromThenTo _ e1 e2 e3) =
    fill $ printNode e1 >> printNode e2 >> printNode e3
  printS (UnaryMinus _ e) = fill $ printNode e
  printS (Apply _ e1 e2) = fill $ printNode e1 >> printNode e2
  printS (InfixApply _ e1 op e3) =
    fill $ printNode e1 >> printNode (qidOp op) >> printNode e3
  printS (LeftSection _ e op) = fill $ printNode e >> printNode (qidOp op)
  printS (RightSection _ op e) = fill $ printNode (qidOp op) >> printNode e
  printS (Lambda _ ps e) = fill $ printNode ps >> printNode e
  printS (Let _ _ ds e) = fill $ printNode ds >> printNode e
  printS (Do _ _ stms e) = fill $ printNode stms >> printNode e
  printS (IfThenElse _ e1 e2 e3) =
    fill $ printNode e1 >> printNode e2 >> printNode e3
  printS (Case _ _ _ e as) = fill $ printNode e >> printNode as

  keywords (Literal _ _ _) = []
  keywords (Variable _ _ _) = []
  keywords (Constructor spi _ _) =
    ["("] ++ replicate (length ss - 2) "," ++ [")"]
    where
      SpanInfo _ ss = spi
  keywords (Paren _ _) = ["(", ")"]
  keywords (Typed _ _ _) = ["::"]
  keywords (Record _ _ _ fs) =
    ["{"] ++ replicate (length fs - 1) "," ++ ["}"]
  keywords (RecordUpdate _ _ fs) =
    ["{"] ++ replicate (length fs - 1) "," ++ ["}"]
  keywords (Tuple _ es) =
    ["("] ++ replicate (length es - 1) "," ++ [")"]
  keywords (List _ _ es) =
    ["["] ++ replicate (length es - 1) "," ++ ["]"]
  keywords (ListCompr _ _ stms) =
    ["[", "|"] ++ replicate (length stms - 1) "," ++ ["]"]
  keywords (EnumFrom _ _) = ["[", "..", "]"]
  keywords (EnumFromTo _ _ _) = ["[", "..", "]"]
  keywords (EnumFromThen _ _ _) = ["[", ",", "..", "]"]
  keywords (EnumFromThenTo _ _ _ _) = ["[", ",", "..", "]"]
  keywords (UnaryMinus _ _) = ["-"]
  keywords (Apply _ _ _) = []
  keywords (InfixApply _ _ _ _) = []
  keywords (LeftSection _ _ _) = ["(", ")"]
  keywords (RightSection _ _ _) = ["(", ")"]
  keywords (Lambda _ _ _) = ["\\", "->"]
  keywords (Let _ _ _ _) = ["let", "in"]
  keywords (Do _ _ _ _) = ["do"]
  keywords (IfThenElse _ _ _ _) = ["if", "then", "else"]
  keywords (Case _ _ _ _ _) = ["case" , "of"]

qidOp :: InfixOp a -> QualIdent
qidOp (InfixOp     _ q) = q
qidOp (InfixConstr _ q) = q

instance ExactPrint (Statement a) where
  printS (StmtExpr _ e   )   = fill $ printNode e
  printS (StmtDecl _ _ ds) = fill $ printNode ds
  printS (StmtBind _ p e )   = fill $ printNode p >> printNode e

  keywords (StmtExpr _ _  ) = []
  keywords (StmtDecl _ _ _) = ["let"]
  keywords (StmtBind _ _ _) = ["<-"]

instance ExactPrint (Alt a) where
  printS (Alt _ p r) = fill $ printNode p >> printNode r
  keywords _ = []

instance ExactPrint a => ExactPrint (Field a) where
  printS (Field _ qid a) = fill $ printNode qid >> printNode a
  keywords _ = ["="]

instance ExactPrint ModuleIdent where
  printS _ = noChilds
  keywords (ModuleIdent _ mods) = [intercalate "." mods]

instance ExactPrint Ident where
  printS _ = noChilds
  keywords (Ident spi name _)
    | length (getSrcInfoPoints spi)
       == 1         = [name]
    | isOpName name = ["(", name, ")"]
    | otherwise     = ["`", name, "`"]
    where
      isOpName = all (`elem` opChars)
      opChars  = "~!@#$%^&*+-=<>:?./|\\"

instance ExactPrint QualIdent where
  printS (QualIdent _ mid i) = fill $ do
    maybe empty printNode mid
    printNode i
  keywords _ = []
