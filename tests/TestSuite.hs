{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec (Spec, hspec)

import Core.System

import CheckAbstractSyntax
import CheckSkeletonParser
import CheckQuantityBehaviour

main :: IO ()
main = do
    finally (hspec suite) (putStrLn ".")

suite :: Spec
suite = do
    checkQuantityBehaviour
    checkAbstractSyntax
    checkSkeletonParser