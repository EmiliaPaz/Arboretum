module Parser exposing (..)

import List exposing (head,tail,take,drop,foldr)
import Types exposing (..)
import Debug exposing (toString)

-------------------------------------- Parser --------------------------------------
toMaybe : List Token -> Maybe (List Token)
toMaybe tokens = case tokens of
                  [] -> Nothing
                  _ -> Just tokens

fromMaybeList : Maybe (List Token) -> List Token
fromMaybeList ls = case ls of
                  Nothing -> []
                  Just list -> list

-- parse : List Token -> Term
-- parse tokens = let (tree, toks) = expression tokens
--                 in tree

parse : List Token -> Var
parse tokens = case take 2 tokens of
                    [TokVar v,TokAssign] -> let (tree, toks) = expression (drop 2 tokens)
                                                    in {name=v, term=tree}
                    _                    -> {name="",term=EmptyTree}

expression : List Token -> (Term, List Token)
expression tokens = let (termTree, tokens2) = expr tokens
                      in case head tokens2 of
                        Just (TokPlus) ->  case fromMaybeList(tail tokens2) of
                                            [] -> (Plus termTree MissingInt, tokens2)
                                            t2 -> let (expTree, tokens3) = expression t2 in (Plus termTree expTree, tokens3)
                        Just (TokMinus) ->  case fromMaybeList(tail tokens2) of
                                            [] -> (Minus termTree MissingInt, tokens2)
                                            t2 -> let (expTree, tokens3) = expression t2 in (Minus termTree expTree, tokens3)
                        Just (TokTimes) ->  case fromMaybeList(tail tokens2) of
                                            [] -> (Times termTree MissingInt, tokens2)
                                            t2 -> let (expTree, tokens3) = expression t2 in (Times termTree expTree, tokens3)
                        Just (TokOr)    ->  case fromMaybeList(tail tokens2) of
                                            [] -> (Or termTree MissingBool, tokens2)
                                            t2 -> let (expTree, tokens3) = expression t2 in (Or termTree expTree, tokens3)
                        Just (TokAnd)    ->  case fromMaybeList(tail tokens2) of
                                            [] -> (And termTree MissingBool, tokens2)
                                            t2 -> let (expTree, tokens3) = expression t2 in (And termTree expTree, tokens3)
                        Just (TokEq)    ->  case fromMaybeList(tail tokens2) of
                                            [] -> (Eq termTree MissingBool, tokens2)
                                            t2 -> let (expTree, tokens3) = expression t2 in (Eq termTree expTree, tokens3)
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
        Just (TokPlus)        -> (MissingInt, tokens)
        Just (TokMinus)       -> (MissingInt, tokens)
        Just (TokTimes)       -> (MissingInt, tokens)
        Just (TokOr)          -> (MissingBool, tokens)
        Just (TokAnd)         -> (MissingBool, tokens)
        Just (TokEq)          -> (MissingBool, tokens)
        _                     -> (EmptyTree, tokens)

