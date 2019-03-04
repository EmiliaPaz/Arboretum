module Parser exposing (..)

import List exposing (head,tail)
import Types exposing (..)

-------------------------------------- Parser --------------------------------------
toMaybe : List Token -> Maybe (List Token)
toMaybe tokens = case tokens of
                  [] -> Nothing
                  _ -> Just tokens

fromMaybeList : Maybe (List Token) -> List Token
fromMaybeList ls = case ls of
                  Nothing -> []
                  Just list -> list

parse : List Token -> Term
parse tokens = let (tree, toks) = expression tokens
                in tree

expression : List Token -> (Term, List Token)
expression tokens = let (termTree, tokens2) = expr tokens
                      in case head tokens2 of
                        Just (TokPlus) ->  let (expTree, tokens3) = expression (fromMaybeList(tail tokens2)) in (Plus termTree expTree, tokens3)
                        Just (TokMinus) ->  let (expTree, tokens3) = expression (fromMaybeList(tail tokens2)) in (Minus termTree expTree, tokens3)
                        Just (TokTimes) ->  let (expTree, tokens3) = expression (fromMaybeList(tail tokens2)) in (Times termTree expTree, tokens3)
                        Just (TokOr) ->  let (expTree, tokens3) = expression (fromMaybeList(tail tokens2)) in (Or termTree expTree, tokens3)
                        Just (TokAnd) ->  let (expTree, tokens3) = expression (fromMaybeList(tail tokens2)) in (And termTree expTree, tokens3)
                        Just (TokEq) ->  let (expTree, tokens3) = expression (fromMaybeList(tail tokens2)) in (Eq termTree expTree, tokens3)
                        Just (TokConstInt n) -> (termTree, tokens2)
                        _ -> (termTree, tokens2)

expr : List Token -> (Term, List Token)
expr tokens =
    case head tokens of
        Just (TokLParen)      -> let (leftTree, rightTokens) = expression (fromMaybeList(tail tokens))
                                  in case head rightTokens of
                                    Just (TokRParen)  -> (leftTree, fromMaybeList(tail rightTokens))
                                    _                 -> (EmptyTree, [])
        Just (TokConstInt i)  -> (CTerm (CInt i), fromMaybeList(tail tokens))
        Just (TokConstBool b) -> (CTerm (CBool b), fromMaybeList(tail tokens))
        Just (TokVar v)       -> (VTerm v, fromMaybeList(tail tokens))
        Just (TokPlus)        -> (EmptyTree, [])
        Just (TokMinus)       -> (EmptyTree, [])
        Just (TokTimes)       -> (EmptyTree, [])
        _                     -> (EmptyTree, [])
