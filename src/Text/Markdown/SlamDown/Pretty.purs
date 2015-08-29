module Text.Markdown.SlamDown.Pretty (prettyPrintMd) where

import Prelude

import Data.Either (either)
import Data.Foldable (fold)
import Data.List (concatMap, zipWith, (..), toList, fromList, List(..), singleton)
import Data.Maybe (fromMaybe, maybe)
import Data.Monoid (mempty)
import Data.String (split, indexOf, joinWith, length)

import Text.Markdown.SlamDown

unlines :: List String -> String
unlines lst = joinWith "\n" $ fromList lst

prettyPrintMd :: SlamDown -> String
prettyPrintMd (SlamDown bs) = unlines $ concatMap prettyPrintBlock bs

replicateS :: Int -> String -> String
replicateS n s = fold (const s <$> (1 .. n))

indent :: Int -> String -> String
indent n s = replicateS n " " <> s

overLines :: (String -> String) -> List String -> List String
overLines f = map f <<< concatMap lines

lines :: String -> List String
lines "" = mempty
lines s = toList $ split "\n" s

prettyPrintBlock :: Block -> List String
prettyPrintBlock (Paragraph is) = Cons (prettyPrintInlines is) (Cons "" Nil)
prettyPrintBlock (Header n is) = singleton (replicateS n "#" <> " " <> prettyPrintInlines is)
prettyPrintBlock (Blockquote bs) = overLines ((<>) "> ") (concatMap prettyPrintBlock bs)
prettyPrintBlock (Lst lt bss) = concatMap listItem bss
  where
  listItem :: List Block -> List String
  listItem bs =
    let ss = concatMap prettyPrintBlock bs
    in addMarker (concatMap lines ss)

  addMarker :: List String -> List String
  addMarker Nil = Nil
  addMarker (Cons s ss) =
    let m   = prettyPrintMarker lt
        len = length m
    in Cons (m <> " " <> s) $ overLines (indent (len + 1)) ss

  prettyPrintMarker :: ListType -> String
  prettyPrintMarker (Bullet s) = s
  prettyPrintMarker (Ordered s) = "1" <> s
prettyPrintBlock (CodeBlock Indented ss) = map (indent 4) ss
prettyPrintBlock (CodeBlock (Fenced info) ss) = singleton ("```" <> info) <> ss <> singleton "```"
prettyPrintBlock (LinkReference l url) = singleton $ squares l <> ": " <> url
prettyPrintBlock Rule = singleton "***"

prettyPrintInlines :: List Inline -> String
prettyPrintInlines is = joinWith "" $ fromList $ (map prettyPrintInline is)

prettyPrintInline :: Inline -> String
prettyPrintInline (Str s) = s
prettyPrintInline (Entity s) = s
prettyPrintInline Space = " "
prettyPrintInline SoftBreak = "\n"
prettyPrintInline LineBreak = "  \n"
prettyPrintInline (Emph is) = "*" <> prettyPrintInlines is <> "*"
prettyPrintInline (Strong is) = "**" <> prettyPrintInlines is <> "**"
prettyPrintInline (Code s) = "`" <> s <> "`"
prettyPrintInline (Link is tgt) = "[" <> prettyPrintInlines is <> "]" <> printTarget tgt
  where
  printTarget (InlineLink url) = parens url
  printTarget (ReferenceLink tgt) = squares (fromMaybe "" tgt)
prettyPrintInline (Image is url) = "![" <> prettyPrintInlines is <> "](" <> url <> ")"
prettyPrintExpr :: forall a. (String -> String) -> (a -> String) -> Expr a -> String
prettyPrintExpr _    f (Literal a) = f a
prettyPrintExpr wrap _ (Unevaluated c) = wrap $ "!`" <> c <> "`"

parens :: String -> String
parens s = "(" <> s <> ")"

braces :: String -> String
braces s = "{" <> s <> "}"

squares :: String -> String
squares s = "[" <> s <> "]"
