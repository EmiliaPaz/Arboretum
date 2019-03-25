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


parse : List Token -> Var
parse tokens = case take 2 tokens of
                    [TokVar v,TokAssign] -> let (tree, toks) = expression (drop 2 tokens)
                                                    in {name=v, term=tree}
                    _                    -> {name="",term=EmptyTree}

twoSidedConnective : Term -> List Token -> TokTSC -> (Term, Term, List Token)
twoSidedConnective left tokens typeSign = case (left, tokens, typeSign) of
                              -- tsc int
                              (Missing, [], TTSCInt tsci)       -> (MissingInt, MissingInt, tokens)
                              (_, [], TTSCInt tsci)             -> (left, MissingInt, tokens)
                              (Missing, tokens2, TTSCInt tsci)  -> let (right, tokens3) = expression tokens2 in case right of
                                                                                                                Missing -> (MissingInt, MissingInt, tokens3)
                                                                                                                _       -> (MissingInt, right, tokens3)
                              (_, tokens2, TTSCInt tsci)        -> let (right, tokens3) = expression tokens2 in case right of
                                                                                                                Missing -> (left, MissingInt, tokens3)
                                                                                                                _       -> (left, right, tokens3)
                              -- tsc bool
                              (Missing, [], TTSCBool tscb)       -> (MissingBool, MissingBool, tokens)
                              (_, [], TTSCBool tscb)             -> (left, MissingBool, tokens)
                              (Missing, tokens2, TTSCBool tscb)  -> let (right, tokens3) = expression tokens2 in case right of
                                                                                                                Missing -> (MissingBool, MissingBool, tokens3)
                                                                                                                _       -> (left, right, tokens3)
                              (_, tokens2, TTSCBool tscb)        -> let (right, tokens3) = expression tokens2 in case right of
                                                                                                                Missing -> (left, MissingBool, tokens3)
                                                                                                                _       -> (left, right, tokens3)


expression : List Token -> (Term, List Token)
expression tokens = let (l, tokens2) = expr tokens
                      in case head tokens2 of
                        Just (TTSC tsci) -> let (left, right,tokens3) = twoSidedConnective l (fromMaybeList(tail tokens2)) tsci
                                              in case tsci of
                                                TTSCInt TokPlus  -> (Plus left right, tokens3)
                                                TTSCInt TokMinus  -> (Minus left right, tokens3)
                                                TTSCInt TokTimes  -> (Times left right, tokens3)
                                                TTSCBool TokOr  -> (Or left right, tokens3)
                                                TTSCBool TokAnd  -> (And left right, tokens3)
                                                TTSCBool TokEq  -> (Eq left right, tokens3)
                        Just TokArrow -> let (body, tokens4) = expression (fromMaybeList(tail tokens2))
                                      in (Lam l body, tokens4)
                        _ ->
                          case l of
                            Lam _ _ ->
                              let (arg, tokens5) = expression(tokens2)
                                in (App l arg, tokens5)
                            VTerm v ->
                              let (arg, tokens5) = expression(tokens2)
                                in (App l arg, tokens5)
                            _ -> (l, tokens2)


expr : List Token -> (Term, List Token)
expr tokens =
    case head tokens of
        Just (TokLParen)      ->
                                case head (drop 1 tokens) of
                                  Just (TokBackSlash) ->
                                    let (func, arg) = expression (fromMaybeList(tail tokens))
                                      in case head arg of
                                        Just (TokRParen)  -> (func, fromMaybeList(tail arg))
                                        _                 -> (func, [])
                                  _ ->
                                    let (leftTree, rightTokens) = expression (fromMaybeList(tail tokens))
                                      in case head rightTokens of
                                        Just (TokRParen)  -> (leftTree, fromMaybeList(tail rightTokens))
                                        _                 -> (leftTree, [])
        Just (TokBackSlash)       -> let (arg, arrow) = (head (drop 1 tokens), head(drop 2 tokens))
                                  in case (arg, arrow) of
                                    (Just (TokVar v), Just TokArrow) ->
                                      (VTerm v, drop 2 tokens)
                                    _ ->
                                      (EmptyTree, tokens)
        Just (TokConstInt i)  -> (CTerm (CInt i), fromMaybeList(tail tokens))
        Just (TokConstBool b) -> (CTerm (CBool b), fromMaybeList(tail tokens))
        Just (TokVar v)       -> (VTerm v, fromMaybeList(tail tokens))
        Just (TTSC tsc)       -> case tsc of
                                  TTSCInt tsci  -> (MissingInt, tokens)
                                  TTSCBool tscb -> (MissingBool, tokens)
        Just (TokHole)        -> (Missing,fromMaybeList(tail tokens))
        _                     -> (EmptyTree, tokens)
