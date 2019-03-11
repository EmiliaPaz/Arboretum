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

-- expression : List Token -> (Term, List Token)
-- expression tokens = let (termTree, tokens2) = expr tokens
--                       in case head tokens2 of
--                         Just (TokPlus) ->  case fromMaybeList(tail tokens2) of
--                                             [] -> (Plus termTree MissingInt, tokens2)
--                                             t2 -> case expression t2 of
--                                                     (Missing,tokens3) -> (Plus termTree MissingInt, tokens3)
--                                                     (expTree,tokens3) -> (Plus termTree expTree, tokens3)
--                         Just (TokMinus) ->  case fromMaybeList(tail tokens2) of
--                                             [] -> (Minus termTree MissingInt, tokens2)
--                                             t2 -> let (expTree, tokens3) = expression t2 in (Minus termTree expTree, tokens3)
--                         Just (TokTimes) ->  case fromMaybeList(tail tokens2) of
--                                             [] -> (Times termTree MissingInt, tokens2)
--                                             t2 -> let (expTree, tokens3) = expression t2 in (Times termTree expTree, tokens3)
--                         Just (TokOr)    ->  case fromMaybeList(tail tokens2) of
--                                             [] -> (Or termTree MissingBool, tokens2)
--                                             t2 -> let (expTree, tokens3) = expression t2 in (Or termTree expTree, tokens3)
--                         Just (TokAnd)   ->  case fromMaybeList(tail tokens2) of
--                                             [] -> (And termTree MissingBool, tokens2)
--                                             t2 -> let (expTree, tokens3) = expression t2 in (And termTree expTree, tokens3)
--                         Just (TokEq)    ->  case fromMaybeList(tail tokens2) of
--                                             [] -> (Eq termTree MissingBool, tokens2)
--                                             t2 -> let (expTree, tokens3) = expression t2 in (Eq termTree expTree, tokens3)
--                         _ -> (termTree, tokens2)

twoSidedConnective : Term -> List Token -> (Term, Term, List Token)
twoSidedConnective left tokens = case tokens of 
                              [] -> (left, Missing, [])
                              t2 -> let (expTree, tokens3) = expression t2 in (left, expTree, tokens3)

expression : List Token -> (Term, List Token)
expression tokens = let (termTree, tokens2) = expr tokens
                      in case head tokens2 of
                        Just (TokPlus) ->  case twoSidedConnective termTree (fromMaybeList(tail tokens2)) of
                                            (Missing, Missing, tokens3) -> (Plus MissingInt MissingInt, tokens2)
                                            (left, Missing, tokens3) -> (Plus left MissingInt, tokens2)
                                            (Missing, right, tokens3) -> (Plus MissingInt right, tokens2)
                                            (left, right, tokens3) -> (Plus left right, tokens3)
                        Just (TokMinus) ->  case twoSidedConnective termTree (fromMaybeList(tail tokens2)) of
                                            (Missing, Missing, tokens3) -> (Minus MissingInt MissingInt, tokens2)
                                            (left, Missing, tokens3) -> (Minus left MissingInt, tokens2)
                                            (Missing, right, tokens3) -> (Minus MissingInt right, tokens2)
                                            (left, right, tokens3) -> (Minus left right, tokens3)
                        Just (TokTimes) ->  case twoSidedConnective termTree (fromMaybeList(tail tokens2)) of
                                            (Missing, Missing, tokens3) -> (Times MissingInt MissingInt, tokens2)
                                            (left, Missing, tokens3) -> (Times left MissingInt, tokens2)
                                            (Missing, right, tokens3) -> (Times MissingInt right, tokens2)
                                            (left, right, tokens3) -> (Times left right, tokens3)
                        Just (TokOr)    ->  case twoSidedConnective termTree (fromMaybeList(tail tokens2)) of
                                            (Missing, Missing, tokens3) -> (Or MissingBool MissingBool, tokens2)
                                            (left, Missing, tokens3) -> (Or left MissingBool, tokens2)
                                            (Missing, right, tokens3) -> (Or MissingBool right, tokens2)
                                            (left, right, tokens3) -> (Or left right, tokens3)
                        Just (TokAnd)   ->  case twoSidedConnective termTree (fromMaybeList(tail tokens2)) of
                                            (Missing, Missing, tokens3) -> (And MissingBool MissingBool, tokens2)
                                            (left, Missing, tokens3) -> (And left MissingBool, tokens2)
                                            (Missing, right, tokens3) -> (And MissingBool right, tokens2)
                                            (left, right, tokens3) -> (And left right, tokens2)
                        Just (TokEq)    ->  case twoSidedConnective termTree (fromMaybeList(tail tokens2)) of
                                            (Missing, Missing, tokens3) -> (Eq MissingBool MissingBool, tokens2)
                                            (left, Missing, tokens3) -> (Eq left MissingBool, tokens2)
                                            (Missing, right, tokens3) -> (Eq MissingBool right, tokens2)
                                            (left, right, tokens3) -> (Eq left right, tokens2)
                        _ -> (termTree, tokens2)


expr : List Token -> (Term, List Token)
expr tokens =
    case head tokens of
        Just (TokLParen)      -> let (leftTree, rightTokens) = expression (fromMaybeList(tail tokens))
                                  in case head rightTokens of
                                    Just (TokRParen)  -> (leftTree, fromMaybeList(tail rightTokens))
                                    _                 -> (leftTree, [])    --- PROBLEM!! don't doing the rest of the statement
        Just (TokConstInt i)  -> (CTerm (CInt i), fromMaybeList(tail tokens))
        Just (TokConstBool b) -> (CTerm (CBool b), fromMaybeList(tail tokens))
        Just (TokVar v)       -> (VTerm v, fromMaybeList(tail tokens))
        Just (TokPlus)        -> (MissingInt, tokens)
        Just (TokMinus)       -> (MissingInt, tokens)
        Just (TokTimes)       -> (MissingInt, tokens)
        Just (TokOr)          -> (MissingBool, tokens)
        Just (TokAnd)         -> (MissingBool, tokens)
        Just (TokEq)          -> (MissingBool, tokens)
        Just (TokHole)        -> (Missing,fromMaybeList(tail tokens))
        _                     -> (EmptyTree, tokens)

