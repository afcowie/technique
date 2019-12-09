{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module CheckTranslationStage
    ( checkTranslationStage
    , main
    )
where

import Core.Data.Structures
import Core.Text.Rope ()
import Core.System
import Test.Hspec

import Technique.Builtins
import Technique.Failure
import Technique.Internal
import Technique.Language
import Technique.Translate

import ExampleProcedure hiding (main)

main :: IO ()
main = do
    finally (hspec checkTranslationStage) (putStrLn ".")

testEnv :: Environment
testEnv = Environment
    { environmentVariables = singletonMap (Identifier "x") (Name "!x")
    , environmentFunctions = insertKeyValue (Identifier "oven") (Subroutine exampleProcedureOven NoOp) builtinProcedures
    , environmentRole = Unspecified
    , environmentAccumulated = NoOp
    }

checkTranslationStage :: Spec
checkTranslationStage = do
    describe "Invalid code emits expected compiler failures" $ do
        it "encountering undefined symbol" $
          let
            expr = Undefined
          in do
            runTranslate testEnv (translateExpression expr)
                `shouldBe` Left EncounteredUndefined

        it "encountering unknown variable" $
          let
            expr = Variable [Identifier "y"]
          in do
            runTranslate testEnv (translateExpression expr)
                `shouldBe` Left (UseOfUnknownIdentifier (Identifier "y"))

        it "encountering unknown procedure" $
          let
            expr1 = Variable [Identifier "x"]
            expr2 = Application (Identifier "f") expr1
            stmt = Execute expr2
            block = Block [stmt]
            technique = emptyTechnique { techniqueBody = [ emptyProcedure { procedureBlock = block } ] }
          in do
            runTranslate testEnv (translateTechnique technique)
                `shouldBe` Left (CallToUnknownProcedure (Identifier "f"))

        it "encounters pre-existing procedure" $
          let
            proc = exampleProcedureOven -- already in testEnv
            stmt = Declaration proc
            block = Block [stmt]
          in do
            -- verify precondition that there is one already there
            fmap functionName (lookupKeyValue (Identifier "oven") (environmentFunctions testEnv))
                `shouldBe` Just (Identifier "oven")
            -- attempt to declare a procedure by a name already in use
            runTranslate testEnv (translateBlock block)
                `shouldBe` Left (ProcedureAlreadyDeclared (Identifier "oven"))
