{- |
Copyright: (c) 2021 Alex Mason
SPDX-License-Identifier: MIT
Maintainer: Alex Mason <github@me.axman6.com>

See README for more info
-}

{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}

module Utf8Monoid where

import Data.Text.Internal.Encoding.Utf8 (validate2, validate3, validate4)
import Data.Word (Word8)

data Utf8Monoid
    = Valid  {-# UNPACK #-} !Int -- Length of data represented by this chunk result
    | Errors {-# UNPACK #-} !Int !Continuations [Int] !Prefix -- length represented, possible continuations, known errors, possible prefix
  deriving (Show, Eq)

data Continuations
    = ZeroC
    | OneC   {-# UNPACK #-} !Word8
    | TwoC   {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
    | ThreeC {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
  deriving (Show, Eq)

contSize :: Continuations -> Int
contSize ZeroC    = 0
contSize OneC{}   = 1
contSize TwoC{}   = 2
contSize ThreeC{} = 3

data Prefix
    = ZeroP
    | OneP   {-# UNPACK #-} !Word8
    | TwoP   {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
    | ThreeP {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8 {-# UNPACK #-} !Word8
  deriving (Show, Eq)

prefixSize :: Prefix -> Int
prefixSize ZeroP    = 0
prefixSize OneP{}   = 1
prefixSize TwoP{}   = 2
prefixSize ThreeP{} = 3

combines :: Prefix -> Continuations -> Bool
combines (OneP w1) (OneC w2)         = validate2 w1 w2
combines (OneP w1) (TwoC w2 w3)      = validate3 w1 w2 w3
combines (OneP w1) (ThreeC w2 w3 w4) = validate4 w1 w2 w3 w4

combines (TwoP w1 w2) (OneC w3)      = validate3 w1 w2 w3
combines (TwoP w1 w2) (TwoC w3 w4)   = validate4 w1 w2 w3 w4

combines (ThreeP w1 w2 w3) (OneC w4) = validate4 w1 w2 w3 w4

combines _ _ = False

lengthOf :: Utf8Monoid -> Int
lengthOf (Valid len)           = len
lengthOf (Errors len _ _ _)    = len

getPrefix :: Utf8Monoid -> Prefix
getPrefix = \case
    Valid _             -> ZeroP
    Errors _ _ _ mpfx   -> mpfx

getCont :: Utf8Monoid -> Continuations
getCont = \case
    Valid _              -> ZeroC
    Errors _ mcont _ _   -> mcont

getErrorOffsets :: Utf8Monoid -> [Int]
getErrorOffsets = \case
    Errors _ _ es _ -> es
    _ -> []

instance Semigroup Utf8Monoid where
    a <> b =
        let leftLength  = lengthOf a
            totalLength = leftLength + lengthOf b
            errors      = Errors totalLength

            rightHasUnmatchedCont leftCont ls rs rightPfx      = errors leftCont (ls ++  leftLength                   : map (leftLength +) rs) rightPfx
            leftHasUnmatchedPrefix leftCont pfx ls rs rightPfx = errors leftCont (ls ++ (leftLength - prefixSize pfx) : map (leftLength +) rs) rightPfx

            -- Eliminate the Errors if we have no partial results and no known errors
            -- Otherwise offset errors from right and keep partials
            ok = case (getCont a, getPrefix b, getErrorOffsets a ++ map (leftLength +) (getErrorOffsets b)) of
                      (ZeroC,     ZeroP,       []) -> Valid totalLength
                      (leftCont,  rightPfx,    es) -> errors leftCont es rightPfx

        in case (a,b) of
            (Valid _,                      Valid _                       ) -> Valid totalLength
            (Errors _ _        _  ZeroP,   Valid _                       ) -> ok
            (Errors _ leftCont ls pfx,     Valid _                       ) -> leftHasUnmatchedPrefix leftCont pfx ls [] ZeroP
            (Valid _,                      Errors _ ZeroC _ _            ) -> ok
            (Valid _,                      Errors _ _ rs rightPfx        ) -> rightHasUnmatchedCont  ZeroC        [] rs rightPfx
            (Errors _ leftCont ls leftPfx, Errors _ rightCont rs rightPfx) -> case (leftPfx, rightCont) of
                (ZeroP, ZeroC)          -> ok
                (ZeroP, _)              -> rightHasUnmatchedCont  leftCont     ls rs rightPfx
                (pfx, cont)
                    | combines pfx cont -> ok
                    | otherwise         -> leftHasUnmatchedPrefix leftCont pfx ls rs rightPfx

instance Monoid Utf8Monoid where
    mempty = Valid 0