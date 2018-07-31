{-# LANGUAGE TemplateHaskell #-}
module Parrows.CloudHaskell.EvalGen(
    mkEvalTasks,
    mkRemotables,
    mkEvaluatables,
    evalTaskFn,
) where

import Control.Distributed.Process.Closure

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

--(SendPort (SendPort (Thunk (Maybe Grid))), SendPort (Maybe Grid)) -> Process ()

nested :: Type -> Type -> Type
nested a b = a `AppT` (ParensT b)

tuple2 :: Type -> Type -> Type
tuple2 a b = (TupleT 2 `AppT` a) `AppT` b

fn :: Type -> Type -> Type
fn a b = (ArrowT `AppT` a) `AppT` b

nameToFnName :: Name -> Name
nameToFnName (Name (OccName str) _) = mkName $ ("__" ++ str ++ "_evalTaskImpl")

-- FIXME: make a function that works on Types instead of Names so we can use composite types

transInstance :: Name -> Q [Dec]
transInstance typeName = do
	let trans = ConT $ mkName "Trans"
	return [
			InstanceD (Nothing) [] (trans `nested` ConT typeName) [
			]
		]

evalTaskFn :: Name -> Name -> Q [Dec]
evalTaskFn typeName fnName = do
	let sendPort = ConT $ mkName "SendPort"
	    maybe = ConT $ mkName "Maybe"
	    thunk = ConT $ mkName "Thunk"
	    process = ConT $ mkName "Process"
	    firstTup = (sendPort `nested` (sendPort `nested` (thunk `nested` (ConT typeName))))
	    secondTup = sendPort `nested` (sendPort `nested` (sendPort `nested` (maybe `nested` (ConT typeName))))
	    procNil = process `AppT` (TupleT 0)
	return [
			SigD fnName ((firstTup `tuple2` secondTup) `fn` procNil),
			FunD fnName [Clause [] (NormalB (VarE $ mkName "evalTaskBase")) []]
		]

evaluatableInstance :: Name -> Name -> Q [Dec]
evaluatableInstance typeName fnName = do
	let evaluatable = ConT $ mkName "Evaluatable"
	closure <- mkClosure fnName
	return [
			InstanceD (Nothing) [] (evaluatable `nested` ConT typeName) [
				FunD (mkName "evalTask") [Clause [] (NormalB closure) []]
			]
		]

mkEvalTasks :: [Name] -> Q [Dec]
mkEvalTasks names = do
	let fnNames = map nameToFnName names
  	evalTasks <- (mapM (uncurry evalTaskFn) (zipWith (,) names fnNames))
  	transInstances <- (mapM transInstance names)
  	return . concat $ transInstances ++ (evalTasks)

mkRemotables :: [Name] -> Q [Dec]
mkRemotables names = do
	let fnNames = map nameToFnName names
	remotable fnNames

mkEvaluatables :: [Name] -> Q [Dec]
mkEvaluatables names = do
	let fnNames = map nameToFnName names
  	(mapM (uncurry evaluatableInstance) (zipWith (,) names fnNames)) >>= (return . concat)

