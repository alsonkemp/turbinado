-----------------------------------------------------------------------------
-- |
-- Module      :  HSX.Tranform
-- Copyright   :  (c) Niklas Broberg 2004,
-- License     :  BSD-style (see the file LICENSE.txt)
-- 
-- Maintainer  :  Niklas Broberg, d00nibro@dtek.chalmers.se
-- Stability   :  experimental
-- Portability :  portable
--
-- Functions for transforming abstract Haskell code extended with regular 
-- patterns into semantically equivalent normal abstract Haskell code. In
-- other words, we transform away regular patterns.
-----------------------------------------------------------------------------

module HSX.Transform (
    transform       -- :: HsModule -> HsModule
    ) where

import Language.Haskell.Exts.Syntax
import Language.Haskell.Exts.Build
import Data.List (union)

import Debug.Trace (trace)

-----------------------------------------------------------------------------
-- A monad for threading a boolean value through the boilerplate code,
-- to signal whether a transformation has taken place or not.

newtype HsxM a = MkHsxM (HsxState -> (a, HsxState))

instance Monad HsxM where
 return x = MkHsxM (\s -> (x,s))
 (MkHsxM f) >>= k = MkHsxM (\s -> let (a, s') = f s
                                      (MkHsxM f') = k a
                                   in f' s')

getHsxState :: HsxM HsxState
getHsxState = MkHsxM (\s -> (s, s))

setHsxState :: HsxState -> HsxM ()
setHsxState s = MkHsxM (\_ -> ((),s))

instance Functor HsxM where
 fmap f hma = do a <- hma
                 return $ f a

-----

type HsxState = (Bool, Bool)

initHsxState :: HsxState
initHsxState = (False, False)

setHarpTransformed :: HsxM ()
setHarpTransformed = 
    do (_,x) <- getHsxState
       setHsxState (True,x)

setXmlTransformed :: HsxM ()
setXmlTransformed =
    do (h,_) <- getHsxState
       setHsxState (h,True)

runHsxM :: HsxM a -> (a, (Bool, Bool))
runHsxM (MkHsxM f) = f initHsxState

-----------------------------------------------------------------------------
-- Traversing and transforming the syntax tree


-- | Transform away occurences of regular patterns from an abstract
-- Haskell module, preserving semantics.
transform :: HsModule -> HsModule
transform (HsModule s m mes is decls) =
    let (decls', (harp, hsx)) = runHsxM $ mapM transformDecl decls
        -- We may need to add an import for Match.hs that defines the matcher monad
        imps1 = if harp 
             then (:) $ HsImportDecl s match_mod True
                            (Just match_qual_mod)
                            Nothing
             else id
        imps2 = {- if hsx
                 then (:) $ HsImportDecl s hsx_data_mod False
                         Nothing
                         Nothing
                 else -} id     -- we no longer want to import HSP.Data
     in HsModule s m mes (imps1 $ imps2 is) decls'

-----------------------------------------------------------------------------
-- Declarations

-- | Transform a declaration by transforming subterms that could
-- contain regular patterns.
transformDecl :: HsDecl -> HsxM HsDecl
transformDecl d = case d of
    -- Pattern binds can contain regular patterns in the pattern being bound
    -- as well as on the right-hand side and in declarations in a where clause
    HsPatBind srcloc pat rhs decls -> do
        -- Preserve semantics of irrefutable regular patterns by postponing
        -- their evaluation to a let-expression on the right-hand side
        let ([pat'], rnpss) = unzip $ renameIrrPats [pat]
        -- Transform the pattern itself
        ([pat''], attrGuards, guards, decls'') <- transformPatterns srcloc [pat']
        -- Transform the right-hand side, and add any generated guards
        -- and let expressions to it
        rhs' <- mkRhs srcloc (attrGuards ++ guards) (concat rnpss) rhs 
        -- Transform declarations in the where clause, adding any generated
        -- declarations to it
        decls' <- case decls of
               HsBDecls ds -> do ds' <- transformLetDecls ds
                                 return $ HsBDecls $ decls'' ++ ds'
               _           -> error "Cannot bind implicit parameters in the \
                        \ \'where\' clause of a function using regular patterns."
        return $ HsPatBind srcloc pat'' rhs' decls'

    -- Function binds can contain regular patterns in their matches
    HsFunBind ms -> fmap HsFunBind $ mapM transformMatch ms
    -- Instance declarations can contain regular patterns in the
    -- declarations of functions inside it
    HsInstDecl s c n ts idecls ->
        fmap (HsInstDecl s c n ts) $ mapM transformInstDecl idecls
    -- Class declarations can contain regular patterns in the
    -- declarations of automatically instantiated functions
    HsClassDecl s c n ns ds cdecls ->
        fmap (HsClassDecl s c n ns ds) $ mapM transformClassDecl cdecls
    -- Type signatures, type, newtype or data declarations, infix declarations
    -- and default declarations; none can contain regular patterns
    _ -> return d

transformInstDecl :: HsInstDecl -> HsxM HsInstDecl
transformInstDecl d = case d of
    HsInsDecl decl -> fmap HsInsDecl $ transformDecl decl
    _ -> return d

transformClassDecl :: HsClassDecl -> HsxM HsClassDecl
transformClassDecl d = case d of
    HsClsDecl decl -> fmap HsClsDecl $ transformDecl decl
    _ -> return d



-- | Transform a function "match" by generating pattern guards and
-- declarations representing regular patterns in the argument list.
-- Subterms, such as guards and the right-hand side, are also traversed
-- transformed.
transformMatch :: HsMatch -> HsxM HsMatch
transformMatch (HsMatch srcloc name pats rhs decls) = do
    -- Preserve semantics of irrefutable regular patterns by postponing
    -- their evaluation to a let-expression on the right-hand side
    let (pats', rnpss) = unzip $ renameIrrPats pats
    -- Transform the patterns that stand as arguments to the function
    (pats'', attrGuards, guards, decls'') <- transformPatterns srcloc pats'
    -- Transform the right-hand side, and add any generated guards
    -- and let expressions to it
    rhs' <- mkRhs srcloc (attrGuards ++ guards) (concat rnpss) rhs
    -- Transform declarations in the where clause, adding any generated
    -- declarations to it
    decls' <- case decls of
           HsBDecls ds -> do ds' <- transformLetDecls ds
                             return $ HsBDecls $ decls'' ++ ds'
           _           -> error "Cannot bind implicit parameters in the \
                     \ \'where\' clause of a function using regular patterns."

    return $ HsMatch srcloc name pats'' rhs' decls'
-- | Transform and update guards and right-hand side of a function or
-- pattern binding. The supplied list of guards is prepended to the 
-- original guards, and subterms are traversed and transformed.
mkRhs :: SrcLoc -> [Guard] -> [(HsName, HsPat)] -> HsRhs -> HsxM HsRhs
mkRhs srcloc guards rnps (HsUnGuardedRhs rhs) = do
    -- Add the postponed patterns to the right-hand side by placing
    -- them in a let-expression to make them lazily evaluated.
    -- Then transform the whole right-hand side as an expression.
    rhs' <- transformExp $ addLetDecls srcloc rnps rhs
    case guards of 
     -- There were no guards before, and none should be added,
     -- so we still have an unguarded right-hand side
     [] -> return $ HsUnGuardedRhs rhs'
     -- There are guards to add. These should be added as pattern
     -- guards, i.e. as statements.
     _  -> return $ HsGuardedRhss [HsGuardedRhs srcloc (map mkStmtGuard guards) rhs']
mkRhs _ guards rnps (HsGuardedRhss gdrhss) = fmap HsGuardedRhss $ mapM (mkGRhs guards rnps) gdrhss
  where mkGRhs :: [Guard] -> [(HsName, HsPat)] -> HsGuardedRhs -> HsxM HsGuardedRhs
        mkGRhs gs rnps (HsGuardedRhs s oldgs rhs) = do
            -- Add the postponed patterns to the right-hand side by placing
            -- them in a let-expression to make them lazily evaluated.
            -- Then transform the whole right-hand side as an expression.
            rhs' <- transformExp $ addLetDecls s rnps rhs
            -- Now there are guards, so first we need to transform those
            oldgs' <- fmap concat $ mapM (transformStmt Guard) oldgs
            -- ... and then prepend the newly generated ones, as statements
            return $ HsGuardedRhs s ((map mkStmtGuard gs) ++ oldgs') rhs'

-- | Place declarations of postponed regular patterns in a let-expression to
-- make them lazy, in order to make them behave as irrefutable patterns.
addLetDecls :: SrcLoc -> [(HsName, HsPat)] -> HsExp -> HsExp
addLetDecls s []   e = e    -- no declarations to add
addLetDecls s rnps e = 
    -- Place all postponed patterns in the same let-expression
    letE (map (mkDecl s) rnps) e

-- | Make pattern binds from postponed regular patterns
mkDecl :: SrcLoc -> (HsName, HsPat) -> HsDecl
mkDecl srcloc (n,p) = patBind srcloc p (var n)

------------------------------------------------------------------------------------
-- Expressions
                 
-- | Transform expressions by traversing subterms.
-- Of special interest are expressions that contain patterns as subterms,
-- i.e. @let@, @case@ and lambda expressions, and also list comprehensions
-- and @do@-expressions. All other expressions simply transform their
-- sub-expressions, if any.
-- Of special interest are of course also any xml expressions.
transformExp :: HsExp -> HsxM HsExp
transformExp e = case e of
    -- A standard xml tag should be transformed into an element of the
    -- XML datatype. Attributes should be made into a set of mappings, 
    -- and children should be transformed.
    HsXTag _ name attrs mattr cs -> do
        -- Hey Pluto, look, we have XML in our syntax tree!
        setXmlTransformed
        let -- ... make tuples of the attributes
            as = map mkAttr attrs
        -- ... transform the children
        cs' <- mapM transformChild cs
        -- ... and lift the values into the XML datatype.
        return $ paren $ metaGenElement name as mattr cs'

      where
        -- | Transform expressions appearing in child position of an xml tag.
        -- Expressions are first transformed, then wrapped in a call to
        -- @toXml@.
        transformChild :: HsExp -> HsxM HsExp
        transformChild e = do
            -- Transform the expression
            te <- transformExp e
            -- ... and apply the overloaded toXMLs to it
            return $ metaAsChild te
            
    -- An empty xml tag should be transformed just as a standard tag,
    -- only that there are no children,
    HsXETag _ name attrs mattr -> do
        -- ... 'tis the season to be jolly, falalalalaaaa....
        setXmlTransformed
        let -- ... make tuples of the attributes   
            as = map mkAttr attrs
            -- ... and lift the values into the XML datatype.
        return $ paren $ metaGenEElement name as mattr
    -- PCDATA should be lifted as a string into the XML datatype.
    HsXPcdata pcdata    -> do setXmlTransformed
                              return $ strE pcdata
    -- Escaped expressions should be treated as just expressions.
    HsXExpTag e     -> do setXmlTransformed
                          e' <- transformExp e
                          return $ paren $ metaAsChild e'
    -- Patterns as arguments to a lambda expression could be regular,
    -- but we cannot put the evaluation here since a lambda expression
    -- can have neither guards nor a where clause. Thus we must postpone 
    -- them to a case expressions on the right-hand side.
    HsLambda s pats rhs -> do
        let -- First rename regular patterns
            (ps, rnpss)  = unzip $ renameRPats pats
            -- ... group them up to one big tuple
            (rns, rps) = unzip (concat rnpss)
            alt1 = alt s (pTuple rps) rhs
            texp = varTuple rns
            -- ... and put it all in a case expression, which
            -- can then be transformed in the normal way.
            e = if null rns then rhs else caseE texp [alt1]
        rhs' <- transformExp e
        return $ HsLambda s ps rhs'
    -- A let expression can contain regular patterns in the declarations, 
    -- or in the expression that makes up the body of the let.
    HsLet (HsBDecls ds) e -> do
        -- Declarations appearing in a let expression must be transformed
        -- in a special way due to scoping, see later documentation.
        -- The body is transformed as a normal expression.
        ds' <- transformLetDecls ds
        e'  <- transformExp e
        return $ letE ds' e'
    -- Bindings of implicit parameters can appear either in ordinary let
    -- expressions (GHC), in dlet expressions (Hugs) or in a with clause
    -- (both). Such bindings are transformed in a special way. The body 
    -- is transformed as a normal expression in all cases.
    HsLet (HsIPBinds is) e -> do
        is' <- mapM transformIPBind is
        e'  <- transformExp e
        return $ HsLet (HsIPBinds is') e'
    HsDLet ipbs e -> do
        ipbs' <- mapM transformIPBind ipbs
        e'    <- transformExp e
        return $ HsDLet ipbs' e'
    HsWith e ipbs -> do
        ipbs' <- mapM transformIPBind ipbs
        e'    <- transformExp e
        return $ HsWith e' ipbs'
    -- A case expression can contain regular patterns in the expression
    -- that is the subject of the casing, or in either of the alternatives.
    HsCase e alts -> do
        e'    <- transformExp e
        alts' <- mapM transformAlt alts
        return $ HsCase e' alts'
    -- A do expression can contain regular patterns in its statements.
    HsDo stmts -> do
        stmts' <- fmap concat $ mapM (transformStmt Do) stmts
        return $ HsDo stmts'
    HsMDo stmts -> do
        stmts' <- fmap concat $ mapM (transformStmt Do) stmts
        return $ HsMDo stmts'
    -- A list comprehension can contain regular patterns in the result 
    -- expression, or in any of its statements.
    HsListComp e stmts  -> do
        e'     <- transformExp e
        stmts' <- fmap concat $ mapM (transformStmt ListComp) stmts
        return $ HsListComp e' stmts'
    -- All other expressions simply transform their immediate subterms.
    HsInfixApp e1 op e2 -> transform2exp e1 e2 
                                (\e1 e2 -> HsInfixApp e1 op e2)
    HsApp e1 e2         -> transform2exp e1 e2 HsApp
    HsNegApp e          -> fmap HsNegApp $ transformExp e
    HsIf e1 e2 e3       -> transform3exp e1 e2 e3 HsIf
    HsTuple es          -> fmap HsTuple $ mapM transformExp es
    HsList es           -> fmap HsList $ mapM transformExp es
    HsParen e           -> fmap HsParen $ transformExp e
    HsLeftSection e op  -> do e' <- transformExp e
                              return $ HsLeftSection e' op
    HsRightSection op e -> fmap (HsRightSection op) $ transformExp e
    HsRecConstr n fus   -> fmap (HsRecConstr n) $ mapM transformFieldUpdate fus
    HsRecUpdate e fus   -> do e'   <- transformExp e
                              fus' <- mapM transformFieldUpdate fus
                              return $ HsRecUpdate e' fus'
    HsEnumFrom e        -> fmap HsEnumFrom $ transformExp e
    HsEnumFromTo e1 e2  -> transform2exp e1 e2 HsEnumFromTo
    HsEnumFromThen e1 e2      -> transform2exp e1 e2 HsEnumFromThen
    HsEnumFromThenTo e1 e2 e3 -> transform3exp e1 e2 e3 HsEnumFromThenTo
    HsExpTypeSig s e t  -> do e' <- transformExp e
                              return $ HsExpTypeSig s e' t
    _           -> return e -- Warning! Does not work with TH bracketed expressions ([| ... |])

  where transformFieldUpdate :: HsFieldUpdate -> HsxM HsFieldUpdate
        transformFieldUpdate (HsFieldUpdate n e) =
                fmap (HsFieldUpdate n) $ transformExp e
        
        transform2exp :: HsExp -> HsExp -> (HsExp -> HsExp -> HsExp) -> HsxM HsExp
        transform2exp e1 e2 f = do e1' <- transformExp e1
                                   e2' <- transformExp e2
                                   return $ f e1' e2'
    
        transform3exp :: HsExp -> HsExp -> HsExp -> (HsExp -> HsExp -> HsExp -> HsExp) -> HsxM HsExp
        transform3exp e1 e2 e3 f = do e1' <- transformExp e1
                                      e2' <- transformExp e2
                                      e3' <- transformExp e3
                                      return $ f e1' e2' e3'

        mkAttr :: HsXAttr -> HsExp
        mkAttr (HsXAttr name e) = 
            paren (metaMkName name `metaAssign` e)


-- | Transform pattern bind declarations inside a @let@-expression by transforming 
-- subterms that could appear as regular patterns, as well as transforming the bound
-- pattern itself. The reason we need to do this in a special way is scoping, i.e.
-- in the expression @let a | Just b <- match a = list in b@ the variable b will not
-- be in scope after the @in@. And besides, we would be on thin ice even if it was in
-- scope since we are referring to the pattern being bound in the guard that will
-- decide if the pattern will be bound... yikes, why does Haskell allow guards on 
-- pattern binds to refer to the patterns being bound, could that ever lead to anything
-- but an infinite loop??
transformLetDecls :: [HsDecl] -> HsxM [HsDecl]
transformLetDecls ds = do
    -- We need to rename regular patterns in pattern bindings, since we need to
    -- separate the generated declaration sets. This since we need to add them not
    -- to the actual binding but rather to the declaration that will be the guard
    -- of the binding.
    let ds' = renameLetDecls ds 
    transformLDs 0 0 ds'
  where transformLDs :: Int -> Int -> [HsDecl] -> HsxM [HsDecl]
        transformLDs k l ds = case ds of
            []     -> return []
            (d:ds) -> case d of
                HsPatBind srcloc pat rhs decls -> do
                    -- We need to transform all pattern bindings in a set of
                    -- declarations in the same context w.r.t. generating fresh
                    -- variable names, since they will all be in scope at the same time.
                    ([pat'], ags, gs, ws, k', l') <- runTrFromTo k l (trPatterns srcloc [pat])
                    decls' <- case decls of
                        -- Any declarations already in place should be left where they
                        -- are since they probably refer to the generating right-hand
                        -- side of the pattern bind. If they don't, we're in trouble...
                        HsBDecls decls -> fmap HsBDecls $ transformLetDecls decls
                        -- If they are implicit parameter bindings we simply transform
                        -- them as such.
                        HsIPBinds decls -> fmap HsIPBinds $ mapM transformIPBind decls
                    -- The generated guard, if any, should be a declaration, and the
                    -- generated declarations should be associated with it.
                    let gs' = case gs of
                           []  -> []
                           [g] -> [mkDeclGuard g ws]
                           _   -> error "This should not happen since we have called renameLetDecls already!"
                        -- Generated attribute guards should also be added as declarations,
                        -- but with no where clauses.
                        ags' = map (flip mkDeclGuard $ []) ags
                    -- We must transform the right-hand side as well, but there are
                    -- no new guards, nor any postponed patterns, to supply at this time.
                    rhs' <- mkRhs srcloc [] [] rhs
                    -- ... and then we should recurse with the new gensym argument.
                    ds' <- transformLDs k' l' ds
                    -- The generated guards, which should be at most one, should be
                    -- added as declarations rather than as guards due to the
                    -- scoping issue described above.
                    return $ (HsPatBind srcloc pat' rhs' decls') : ags' ++ gs' ++ ds'

                    -- We only need to treat pattern binds separately, other declarations
                    -- can be transformed normally.
                d -> do d'  <- transformDecl d 
                        ds' <- transformLDs k l ds
                        return $ d':ds'


-- | Transform binding of implicit parameters by transforming the expression on the 
-- right-hand side. The left-hand side can only be an implicit parameter, so no
-- regular patterns there...
transformIPBind :: HsIPBind -> HsxM HsIPBind
transformIPBind (HsIPBind s n e) =
    fmap (HsIPBind s n) $ transformExp e

------------------------------------------------------------------------------------
-- Statements of various kinds

-- | A simple annotation datatype for statement contexts.
data StmtType = Do | Guard | ListComp

-- | Transform statements by traversing and transforming subterms.
-- Since generator statements have slightly different semantics 
-- depending on their context, statements are annotated with their
-- context to ensure that the semantics of the resulting statement
-- sequence is correct. The return type is a list since generated
-- guards will be added as statements on the same level as the
-- statement to be transformed.
transformStmt :: StmtType -> HsStmt -> HsxM [HsStmt]
transformStmt t s = case s of
    -- Generators can have regular patterns in the result pattern on the
    -- left-hand side and in the generating expression.
    HsGenerator s p e -> do
        let -- We need to treat generated guards differently depending
            -- on the context of the statement.
            guardFun = case t of
                Do   -> monadify
                ListComp -> monadify
                Guard    -> mkStmtGuard
            -- Preserve semantics of irrefutable regular patterns by postponing
            -- their evaluation to a let-expression on the right-hand side
            ([p'], rnpss) = unzip $ renameIrrPats [p]
        -- Transform the pattern itself
        ([p''], ags, gs, ds) <- transformPatterns s [p']
        -- Put the generated declarations in a let-statement
        let lt  = case ds of
               [] -> []
               _  -> [letStmt ds]
            -- Perform the designated trick on the generated guards.
            gs' = map guardFun (ags ++ gs)
        -- Add the postponed patterns to the right-hand side by placing
        -- them in a let-expression to make them lazily evaluated.
        -- Then transform the whole right-hand side as an expression.
        e' <- transformExp $ addLetDecls s (concat rnpss) e
        return $ HsGenerator s p'' e':lt ++ gs'
      where monadify :: Guard -> HsStmt
            -- To monadify is to create a statement guard, only that the
            -- generation must take place in a monad, so we need to "return"
            -- the value gotten from the guard.
            monadify (s,p,e) = genStmt s p (metaReturn $ paren e)
    -- Qualifiers are simply wrapped expressions and are treated as such.
    HsQualifier e -> fmap (\e -> [HsQualifier $ e]) $ transformExp e
    -- Let statements suffer from the same problem as let expressions, so
    -- the declarations should be treated in the same special way.
    HsLetStmt (HsBDecls ds)  -> 
        fmap (\ds -> [letStmt ds]) $ transformLetDecls ds
    -- If the bindings are of implicit parameters we simply transform them as such.
    HsLetStmt (HsIPBinds is) -> 
        fmap (\is -> [HsLetStmt (HsIPBinds is)]) $ mapM transformIPBind is


------------------------------------------------------------------------------------------
-- Case alternatives

-- | Transform alternatives in a @case@-expression. Patterns are
-- transformed, while other subterms are traversed further.
transformAlt :: HsAlt -> HsxM HsAlt
transformAlt (HsAlt srcloc pat rhs decls) = do
    -- Preserve semantics of irrefutable regular patterns by postponing
    -- their evaluation to a let-expression on the right-hand side
    let ([pat'], rnpss) = unzip $ renameIrrPats [pat]
    -- Transform the pattern itself
    ([pat''], attrGuards, guards, decls'') <- transformPatterns srcloc [pat']
    -- Transform the right-hand side, and add any generated guards
    -- and let expressions to it.
    rhs' <- mkGAlts srcloc (attrGuards ++ guards) (concat rnpss) rhs
    -- Transform declarations in the where clause, adding any generated
    -- declarations to it.
    decls' <- case decls of
           HsBDecls ds -> do ds' <- mapM transformDecl ds
                             return $ HsBDecls $ decls'' ++ ds
           _           -> error "Cannot bind implicit parameters in the \
                     \ \'where\' clause of a function using regular patterns."

    return $ HsAlt srcloc pat'' rhs' decls'
    
    -- Transform and update guards and right-hand side of a case-expression.
    -- The supplied list of guards is prepended to the original guards, and 
    -- subterms are traversed and transformed.
  where mkGAlts :: SrcLoc -> [Guard] -> [(HsName, HsPat)] -> HsGuardedAlts -> HsxM HsGuardedAlts
        mkGAlts s guards rnps (HsUnGuardedAlt rhs) = do
            -- Add the postponed patterns to the right-hand side by placing
            -- them in a let-expression to make them lazily evaluated.
            -- Then transform the whole right-hand side as an expression.
            rhs' <- transformExp $ addLetDecls s rnps rhs
            case guards of
             -- There were no guards before, and none should be added,
             -- so we still have an unguarded right-hand side
             [] -> return $ HsUnGuardedAlt rhs'
             -- There are guards to add. These should be added as pattern
             -- guards, i.e. as statements.
             _  -> return $ HsGuardedAlts [HsGuardedAlt s (map mkStmtGuard guards) rhs']
        mkGAlts s gs rnps (HsGuardedAlts galts) =
            fmap HsGuardedAlts $ mapM (mkGAlt gs rnps) galts
          where mkGAlt :: [Guard] -> [(HsName, HsPat)] -> HsGuardedAlt -> HsxM HsGuardedAlt
                mkGAlt gs rnps (HsGuardedAlt s oldgs rhs) = do
                    -- Add the postponed patterns to the right-hand side by placing
                    -- them in a let-expression to make them lazily evaluated.
                    -- Then transform the whole right-hand side as an expression.
                    rhs'   <- transformExp $ addLetDecls s rnps rhs
                    -- Now there are guards, so first we need to transform those
                    oldgs' <- fmap concat $ mapM (transformStmt Guard) oldgs
                    -- ... and then prepend the newly generated ones, as statements
                    return $ HsGuardedAlt s ((map mkStmtGuard gs) ++ oldgs') rhs'

----------------------------------------------------------------------------------
-- Guards

-- In some places, a guard will be a declaration instead of the
-- normal statement, so we represent it in a generic fashion.
type Guard = (SrcLoc, HsPat, HsExp)

mkStmtGuard :: Guard -> HsStmt
mkStmtGuard (s, p, e) = genStmt s p e

mkDeclGuard :: Guard -> [HsDecl] -> HsDecl
mkDeclGuard (s, p, e) ds = patBindWhere s p e ds

----------------------------------------------------------------------------------
-- Rewriting expressions before transformation.
-- Done in a monad for gensym capability.

newtype RN a = RN (RNState -> (a, RNState))

type RNState = Int

initRNState = 0

instance Monad RN where
 return a = RN $ \s -> (a,s)
 (RN f) >>= k = RN $ \s -> let (a,s') = f s
                               (RN g) = k a
                            in g s'

instance Functor RN where
 fmap f rna = do a <- rna
                 return $ f a


runRename :: RN a -> a
runRename (RN f) = let (a,_) = f initRNState
                    in a

getRNState :: RN RNState
getRNState = RN $ \s -> (s,s)

setRNState :: RNState -> RN ()
setRNState s = RN $ \_ -> ((), s)

genVarName :: RN HsName
genVarName = do 
    k <- getRNState
    setRNState $ k+1
    return $ name $ "harp_rnvar" ++ show k


type NameBind = (HsName, HsPat)

-- Some generic functions on monads for traversing subterms

rename1pat :: a -> (b -> c) -> (a -> RN (b, [d])) -> RN (c, [d])
rename1pat p f rn = do (q, ms) <- rn p
                       return (f q, ms)

rename2pat :: a -> a -> (b -> b -> c) -> (a -> RN (b, [d])) -> RN (c, [d])
rename2pat p1 p2 f rn = do (q1, ms1) <- rn p1
                           (q2, ms2) <- rn p2
                           return $ (f q1 q2, ms1 ++ ms2)
            
renameNpat :: [a] -> ([b] -> c) -> (a -> RN (b, [d])) -> RN (c, [d])
renameNpat ps f rn = do (qs, mss) <- fmap unzip $ mapM rn ps
                        return (f qs, concat mss)




-- | Generate variables as placeholders for any regular patterns, in order
-- to place their evaluation elsewhere. We must likewise move the evaluation
-- of Tags because attribute lookups are force evaluation.
renameRPats :: [HsPat] -> [(HsPat, [NameBind])]
renameRPats ps = runRename $ mapM renameRP ps

renameRP :: HsPat -> RN (HsPat, [NameBind])
renameRP p = case p of
    -- We must rename regular patterns and Tag expressions
    HsPRPat _           -> rename p
    HsPXTag _ _ _ _ _   -> rename p
    HsPXETag _ _ _ _    -> rename p
    -- The rest of the rules simply try to rename regular patterns in
    -- their immediate subpatterns.
    HsPNeg p            -> rename1pat p HsPNeg renameRP
    HsPInfixApp p1 n p2 -> rename2pat p1 p2
                                (\p1 p2 -> HsPInfixApp p1 n p2)
                                renameRP
    HsPApp n ps         -> renameNpat ps (HsPApp n) renameRP
    HsPTuple ps         -> renameNpat ps HsPTuple renameRP
    HsPList ps          -> renameNpat ps HsPList renameRP
    HsPParen p          -> rename1pat p HsPParen renameRP
    HsPRec n pfs        -> renameNpat pfs (HsPRec n) renameRPf
    HsPAsPat n p        -> rename1pat p (HsPAsPat n) renameRP
    HsPIrrPat p         -> rename1pat p HsPIrrPat renameRP
    HsPXPatTag p        -> rename1pat p HsPXPatTag renameRP
    HsPatTypeSig s p t  -> rename1pat p (\p -> HsPatTypeSig s p t) renameRP 
    _                   -> return (p, [])

  where renameRPf :: HsPatField -> RN (HsPatField, [NameBind])
        renameRPf (HsPFieldPat n p) = rename1pat p (HsPFieldPat n) renameRP
    
        renameAttr :: HsPXAttr -> RN (HsPXAttr, [NameBind])
        renameAttr (HsPXAttr s p) = rename1pat p (HsPXAttr s) renameRP
    
        rename :: HsPat -> RN (HsPat, [NameBind])
        rename p = do -- Generate a fresh variable
              n <- genVarName
              -- ... and return that, along with the association of
              -- the variable with the old pattern
              return (pvar n, [(n,p)])

-- | Rename declarations appearing in @let@s or @where@ clauses.
renameLetDecls :: [HsDecl] -> [HsDecl]
renameLetDecls ds = 
    let -- Rename all regular patterns bound in pattern bindings.
        (ds', smss) = unzip $ runRename $ mapM renameLetDecl ds
        -- ... and then generate declarations for the associations
        gs = map (\(s,n,p) -> mkDecl s (n,p)) (concat smss)
        -- ... which should be added to the original list of declarations.
     in ds' ++ gs

  where renameLetDecl :: HsDecl -> RN (HsDecl, [(SrcLoc, HsName, HsPat)])
        renameLetDecl d = case d of
            -- We need only bother about pattern bindings.
            HsPatBind srcloc pat rhs decls -> do
                -- Rename any regular patterns that appear in the
                -- pattern being bound.
                (p, ms) <- renameRP pat
                let sms = map (\(n,p) -> (srcloc, n, p)) ms
                return $ (HsPatBind srcloc p rhs decls, sms)
            _ -> return (d, [])


-- | Move irrefutable regular patterns into a @let@-expression instead,
-- to make sure that the semantics of @~@ are preserved.
renameIrrPats :: [HsPat] -> [(HsPat, [NameBind])]
renameIrrPats ps = runRename (mapM renameIrrP ps)

renameIrrP :: HsPat -> RN (HsPat, [(HsName, HsPat)])
renameIrrP p = case p of
    -- We should rename any regular pattern appearing
    -- inside an irrefutable pattern.
    HsPIrrPat p     -> do (q, ms) <- renameRP p
                          return $ (HsPIrrPat q, ms)
    -- The rest of the rules simply try to rename regular patterns in
    -- irrefutable patterns in their immediate subpatterns.
    HsPNeg p            -> rename1pat p HsPNeg renameIrrP
    HsPInfixApp p1 n p2 -> rename2pat p1 p2
                                (\p1 p2 -> HsPInfixApp p1 n p2)
                                renameIrrP
    HsPApp n ps         -> renameNpat ps (HsPApp n) renameIrrP
    HsPTuple ps         -> renameNpat ps HsPTuple renameIrrP
    HsPList ps          -> renameNpat ps HsPList renameIrrP
    HsPParen p          -> rename1pat p HsPParen renameIrrP
    HsPRec n pfs        -> renameNpat pfs (HsPRec n) renameIrrPf
    HsPAsPat n p        -> rename1pat p (HsPAsPat n) renameIrrP
    HsPatTypeSig s p t  -> rename1pat p (\p -> HsPatTypeSig s p t) renameIrrP   

    -- Hsx
    HsPXTag s n attrs mat ps -> do (attrs', nss) <- fmap unzip $ mapM renameIrrAttr attrs
                                   (mat', ns1) <- case mat of
                                                   Nothing -> return (Nothing, [])
                                                   Just at -> do (at', ns) <- renameIrrP at
                                                                 return (Just at', ns)
                                   (q, ns) <- renameNpat ps (HsPXTag s n attrs' mat') renameIrrP
                                   return (q, concat nss ++ ns1 ++ ns)
    HsPXETag s n attrs mat  -> do (as, nss) <- fmap unzip $ mapM renameIrrAttr attrs
                                  (mat', ns1) <- case mat of
                                                  Nothing -> return (Nothing, [])
                                                  Just at -> do (at', ns) <- renameIrrP at
                                                                return (Just at', ns)
                                  return $ (HsPXETag s n as mat', concat nss ++ ns1)
    HsPXPatTag p            -> rename1pat p HsPXPatTag renameIrrP
    -- End Hsx

    _                       -> return (p, [])
    
  where renameIrrPf :: HsPatField -> RN (HsPatField, [NameBind])
        renameIrrPf (HsPFieldPat n p) = rename1pat p (HsPFieldPat n) renameIrrP
    
        renameIrrAttr :: HsPXAttr -> RN (HsPXAttr, [NameBind])
        renameIrrAttr (HsPXAttr s p) = rename1pat p (HsPXAttr s) renameIrrP
-----------------------------------------------------------------------------------
-- Transforming Patterns: the real stuff

-- | Transform several patterns in the same context, thereby
-- generating any code for matching regular patterns.
transformPatterns :: SrcLoc -> [HsPat] -> HsxM ([HsPat], [Guard], [Guard], [HsDecl])
transformPatterns s ps = runTr (trPatterns s ps)

---------------------------------------------------
-- The transformation monad

type State = (Int, Int, Int, [Guard], [Guard], [HsDecl])

newtype Tr a = Tr (State -> HsxM (a, State))

instance Monad Tr where
 return a = Tr $ \s -> return (a, s)
 (Tr f) >>= k = Tr $ \s ->
          do (a, s') <- f s
             let (Tr f') = k a
             f' s'

instance Functor Tr where
 fmap f tra = tra >>= (return . f)

liftTr :: HsxM a -> Tr a
liftTr hma = Tr $ \s -> do a <- hma
                           return (a, s)

initState = initStateFrom 0 0

initStateFrom k l = (0, k, l, [], [], [])

runTr :: Tr a -> HsxM (a, [Guard], [Guard], [HsDecl])
runTr (Tr f) = do (a, (_,_,_,gs1,gs2,ds)) <- f initState
                  return (a, reverse gs1, reverse gs2, reverse ds)


runTrFromTo :: Int -> Int -> Tr a -> HsxM (a, [Guard], [Guard], [HsDecl], Int, Int)
runTrFromTo k l (Tr f) = do (a, (_,k',l',gs1,gs2,ds)) <- f $ initStateFrom k l
                            return (a, reverse gs1, reverse gs2, reverse ds, k', l')


-- manipulating the state
getState :: Tr State
getState = Tr $ \s -> return (s,s)

setState :: State -> Tr ()
setState s = Tr $ \_ -> return ((),s)

updateState :: (State -> (a,State)) -> Tr a
updateState f = do s <- getState
                   let (a,s') = f s
                   setState s'
                   return a

-- specific state manipulating functions
pushGuard :: SrcLoc -> HsPat -> HsExp -> Tr ()
pushGuard s p e = updateState $ \(n,m,a,gs1,gs2,ds) -> ((),(n,m,a,gs1,(s,p,e):gs2,ds))
         
pushDecl :: HsDecl -> Tr ()
pushDecl d = updateState $ \(n,m,a,gs1,gs2,ds) -> ((),(n,m,a,gs1,gs2,d:ds))

pushAttrGuard :: SrcLoc -> HsPat -> HsExp -> Tr ()
pushAttrGuard s p e = updateState $ \(n,m,a,gs1,gs2,ds) -> ((),(n,m,a,(s,p,e):gs1,gs2,ds))

genMatchName :: Tr HsName
genMatchName = do k <- updateState $ \(n,m,a,gs1,gs2,ds) -> (n,(n+1,m,a,gs1,gs2,ds))
                  return $ HsIdent $ "harp_match" ++ show k

genPatName :: Tr HsName
genPatName = do k <- updateState $ \(n,m,a,gs1,gs2,ds) -> (m,(n,m+1,a,gs1,gs2,ds))
                return $ HsIdent $ "harp_pat" ++ show k

genAttrName :: Tr HsName
genAttrName = do k <- updateState $ \(n,m,a,gs1,gs2,ds) -> (m,(n,m,a+1,gs1,gs2,ds))
                 return $ HsIdent $ "hsx_attrs" ++ show k


setHarpTransformedT, setXmlTransformedT :: Tr ()
setHarpTransformedT = liftTr setHarpTransformed
setXmlTransformedT  = liftTr setXmlTransformed


-------------------------------------------------------------------
-- Some generic functions for computations in the Tr monad. Could
-- be made even more general, but there's really no point right now...

tr1pat :: a -> (b -> c) -> (a -> Tr b) -> Tr c
tr1pat p f tr = do q <- tr p
                   return $ f q

tr2pat :: a -> a -> (b -> b -> c) -> (a -> Tr b) -> Tr c
tr2pat p1 p2 f tr = do q1 <- tr p1
                       q2 <- tr p2
                       return $ f q1 q2

trNpat :: [a] -> ([b] -> c) -> (a -> Tr b) -> Tr c
trNpat ps f tr = do qs <- mapM tr ps
                    return $ f qs

-----------------------------------------------------------------------------
-- The *real* transformations
-- Transforming patterns

-- | Transform several patterns in the same context
trPatterns :: SrcLoc -> [HsPat] -> Tr [HsPat]
trPatterns s = mapM (trPattern s)

-- | Transform a pattern by traversing the syntax tree.
-- A regular pattern is translated, other patterns are 
-- simply left as is.
trPattern :: SrcLoc -> HsPat -> Tr HsPat
trPattern s p = case p of
    -- This is where the fun starts. =)
    -- Regular patterns must be transformed of course.
    HsPRPat rps -> do
        -- First we need a name for the placeholder pattern.
        n <- genPatName 
        -- A top-level regular pattern is a sequence in linear
        -- context, so we can simply translate it as if it was one.
        (mname, vars, _) <- trRPat s True (HsRPSeq rps)
        -- Generate a top level declaration.
        topmname <- mkTopDecl s mname vars
        -- Generate a pattern guard for this regular pattern,
        -- that will match the generated declaration to the 
        -- value of the placeholder, and bind all variables.
        mkGuard s vars topmname n
        -- And indeed, we have made a transformation!
        setHarpTransformedT
        -- Return the placeholder pattern.
        return $ pvar n
    -- Tag patterns should be transformed
    HsPXTag s name attrs mattr cpats -> do
        -- We need a name for the attribute list, if there are lookups
        an <- case (mattr, attrs) of
                -- ... if there is one already, and there are no lookups
                -- we can just return that
                (Just ap, []) -> return $ ap
                      -- ... if there are none, we dont' care
                (_, []) -> return wildcard
                (_, _)  -> do -- ... but if there are, we want a name for that list
                              n <- genAttrName
                              -- ... we must turn attribute lookups into guards
                              mkAttrGuards s n attrs mattr
                              -- ... and we return the pattern
                              return $ pvar n
        -- ... the pattern representing children should be transformed
        cpat' <- case cpats of
                  -- ... it's a regular pattern, so we can just go ahead and transform it
                  (p@(HsPXRPats _)):[] -> trPattern s p
                  -- ... it's an ordinary list, so we first wrap it up as such
                  _                    -> trPattern s (HsPList cpats)
        -- ...  we have made a transformation and should report that
        setHarpTransformedT
        -- ... and we return a Tag pattern.
        let (dom, n) = xNameParts name
        return $ metaTag dom n an cpat' 
    -- ... as should empty Tag patterns
    HsPXETag s name attrs mattr -> do
        -- We need a name for the attribute list, if there are lookups
        an <- case (mattr, attrs) of
                -- ... if there is a pattern already, and there are no lookups
                -- we can just return that
                (Just ap, []) -> return $ ap
                      -- ... if there are none, we dont' care
                (_, []) -> return wildcard
                (_, _)  -> do -- ... but if there are, we want a name for that list
                              n <- genAttrName
                              -- ... we must turn attribute lookups into guards
                              mkAttrGuards s n attrs mattr
                              -- ... and we return the pattern
                              return $ pvar n
        -- ...  we have made a transformation and should report that
        setHarpTransformedT
        -- ... and we return an ETag pattern.
        let (dom, n) = xNameParts name
        return $ metaTag dom n an peList
    -- PCDATA patterns are strings in the xml datatype.
    HsPXPcdata st -> setHarpTransformedT >> (return $ metaPcdata st)
    -- XML comments are likewise just treated as strings.
    HsPXPatTag p -> setHarpTransformedT >> trPattern s p
    -- Regular expression patterns over children should be translated
    -- just like HsPRPat.
    HsPXRPats rps -> trPattern s $ HsPRPat rps

    -- Transforming any other patterns simply means transforming
    -- their subparts.
    HsPVar _             -> return p
    HsPLit _             -> return p
    HsPNeg q             -> tr1pat q HsPNeg (trPattern s)
    HsPInfixApp p1 op p2 -> tr2pat p1 p2 (\p1 p2 -> HsPInfixApp p1 op p2) (trPattern s)
    HsPApp n ps          -> trNpat ps (HsPApp n) (trPattern s)
    HsPTuple ps          -> trNpat ps HsPTuple (trPattern s)
    HsPList ps           -> trNpat ps HsPList (trPattern s)
    HsPParen p           -> tr1pat p HsPParen (trPattern s)
    HsPRec n pfs         -> trNpat pfs (HsPRec n) (trPatternField s)
    HsPAsPat n p         -> tr1pat p (HsPAsPat n) (trPattern s)
    HsPWildCard          -> return p
    HsPIrrPat p          -> tr1pat p HsPIrrPat (trPattern s)
    HsPatTypeSig s p t   -> tr1pat p (\p -> HsPatTypeSig s p t) (trPattern s)

  where -- Transform a pattern field.
    trPatternField :: SrcLoc -> HsPatField -> Tr HsPatField
    trPatternField s (HsPFieldPat n p) = 
        tr1pat p (HsPFieldPat n) (trPattern s)
 
    -- Deconstruct an xml tag name into its parts.
    xNameParts :: HsXName -> (Maybe String, String)
    xNameParts n = case n of
                    HsXName s      -> (Nothing, s)
                    HsXDomName d s -> (Just d, s)

    -- | Generate a guard for looking up xml attributes.
    mkAttrGuards :: SrcLoc -> HsName -> [HsPXAttr] -> Maybe HsPat -> Tr ()
    mkAttrGuards s attrs [HsPXAttr n q] mattr = do
        -- Apply lookupAttr to the attribute name and
        -- attribute set
        let rhs = metaExtract n attrs
            -- ... catch the result
            pat = metaPJust q
            -- ... catch the remainder list
            rml = case mattr of
                   Nothing -> wildcard
                   Just ap -> ap
        -- ... and add the generated guard to the store.
        pushAttrGuard s (pTuple [pat, rml]) rhs

    mkAttrGuards s attrs ((HsPXAttr a q):xs) mattr = do
        -- Apply lookupAttr to the attribute name and
        -- attribute set
        let rhs = metaExtract a attrs
            -- ... catch the result
            pat = metaPJust q
        -- ... catch the remainder list
        newAttrs <- genAttrName
        -- ... and add the generated guard to the store.
        pushAttrGuard s (pTuple [pat, pvar newAttrs]) rhs
        -- ... and finally recurse
        mkAttrGuards s newAttrs xs mattr
            
    -- | Generate a declaration at top level that will finalise all 
    -- variable continuations, and then return all bound variables.
    mkTopDecl :: SrcLoc -> HsName -> [HsName] -> Tr HsName
    mkTopDecl s mname vars = 
        do -- Give the match function a name
           n <- genMatchName 
           -- Create the declaration and add it to the store.
           pushDecl $ topDecl s n mname vars
           -- Return the name of the match function so that the
           -- guard that will be generated can call it.
           return n

    topDecl :: SrcLoc -> HsName -> HsName -> [HsName] -> HsDecl
    topDecl s n mname vs = 
        let pat  = pTuple [wildcard, pvarTuple vs]      -- (_, (foo, bar, ...))
            g    = var mname                            -- harp_matchX
            a    = genStmt s pat g                      -- (_, (foo, ...)) <- harp_matchX
            vars = map (\v -> app (var v) eList) vs     -- (foo [], bar [], ...)
            b    = qualStmt $ metaReturn $ tuple vars   -- return (foo [], bar [], ...)
            e    = doE [a,b]                            -- do (...) <- harp_matchX
                                                        --    return (foo [], bar [], ...)
         in nameBind s n e                              -- harp_matchY = do ....

    -- | Generate a pattern guard that will apply the @runMatch@
    -- function on the top-level match function and the input list,
    -- thereby binding all variables.
    mkGuard :: SrcLoc -> [HsName] -> HsName -> HsName -> Tr ()
    mkGuard s vars mname n = do
        let tvs = pvarTuple vars                        -- (foo, bar, ...)
            ge  = appFun runMatchFun [var mname, var n] -- runMatch harp_matchX harp_patY
        pushGuard s (pApp just_name [tvs]) ge           -- Just (foo, bar, ...) , runMatch ...


--------------------------------------------------------------------------------
-- Transforming regular patterns

-- | A simple datatype to annotate return values from sub-patterns
data MType = S         -- Single element
           | L MType       -- List of ... , (/  /), *, +
           | E MType MType -- Either ... or ... , (  |  )
           | M MType       -- Maybe ... , ?


-- When transforming a regular sub-pattern, we need to know the
-- name of the function generated to match it, the names of all
-- variables it binds, and the type of its returned value.
type MFunMetaInfo = (HsName, [HsName], MType)


-- | Transform away a regular pattern, generating code
-- to replace it.
trRPat :: SrcLoc -> Bool -> HsRPat -> Tr MFunMetaInfo
trRPat s linear rp = case rp of
    -- For an ordinary Haskell pattern we need to generate a
    -- base match function for the pattern, and a declaration
    -- that lifts that function into the matcher monad.
    HsRPPat p -> mkBaseDecl s linear p
  
      where
        -- | Generate declarations for matching ordinary Haskell patterns
        mkBaseDecl :: SrcLoc -> Bool -> HsPat -> Tr MFunMetaInfo
        mkBaseDecl s linear p = case p of
            -- We can simplify a lot if the pattern is a wildcard or a variable
            HsPWildCard -> mkWCMatch s
            HsPVar v    -> mkVarMatch s linear v
            -- ... and if it is an embedded pattern tag, we can just skip it
            HsPXPatTag q -> mkBaseDecl s linear q

            -- ... otherwise we'll have to take the long way...
            p           -> do -- First do a case match on a single element
                              (name, vars, _) <- mkBasePat s linear p   
                              -- ... apply baseMatch to the case matcher to 
                              -- lift it into the matcher monad.
                              newname <- mkBaseMatch s name 
                              -- ... and return the meta-info gathered.
                              return (newname, vars, S)

        -- | Generate a basic function that cases on a single element, 
        -- returning Just (all bound variables) on a match, and
        -- Nothing on a mismatch.
        mkBasePat :: SrcLoc -> Bool -> HsPat -> Tr MFunMetaInfo
        mkBasePat s b p = 
         do -- First we need a name...
           n <- genMatchName
           -- ... and then we need to know what variables that 
           -- will be bound by this match.
           let vs = gatherPVars p
           -- ... and then we can create and store away a casing function.
           basePatDecl s b n vs p >>= pushDecl
           return (n, vs, S)

        -- | Generate a basic casing function for a given pattern.   
        basePatDecl :: SrcLoc -> Bool -> HsName -> [HsName] -> HsPat -> Tr HsDecl
        basePatDecl s linear f vs p = do
         -- We can use the magic variable harp_a since nothing else needs to
         -- be in scope at this time (we could use just a, or foo, or whatever)
         let a = HsIdent $ "harp_a"
         -- ... and we should case on that variable on the right-hand side.
         rhs <- baseCaseE s linear p a vs    -- case harp_a of ...
         -- The result is a simple function with one paramenter and
         -- the right-hand side we just generated.
         return $ simpleFun s f a rhs
           where baseCaseE :: SrcLoc -> Bool -> HsPat -> HsName -> [HsName] -> Tr HsExp
                 baseCaseE s b p a vs = do
                    -- First the alternative if we actually 
                    -- match the given pattern
                    let alt1 = alt s p                  -- foo -> Just (mf foo)
                                (app (var just_name) $ 
                                 tuple (map (retVar b) vs))
                        -- .. and finally an alternative for not matching the pattern.
                        alt2 = alt s wildcard (var nothing_name)        -- _ -> Nothing
                        -- ... and that pattern could itself contain regular patterns
                        -- so we must transform away these.
                    alt1' <- liftTr $ transformAlt alt1
                    return $ caseE (var a) [alt1', alt2]
                 retVar :: Bool -> HsName -> HsExp
                 retVar linear v
                    -- if bound in linear context, apply const
                    | linear    = metaConst (var v)
                    -- if bound in non-linear context, apply (:)
                    | otherwise = app consFun (var v)

    -- For guarded base patterns, we want to do the same as for unguarded base patterns,
    -- only with guards (doh).
    HsRPGuard p gs -> mkGuardDecl s linear p gs

     where mkGuardDecl :: SrcLoc -> Bool -> HsPat -> [HsStmt] -> Tr MFunMetaInfo
           mkGuardDecl s linear p gs = case p of
                -- If it is an embedded pattern tag, we want to skip it
                HsPXPatTag q -> mkGuardDecl s linear q gs

                -- ... otherwise we'll want to make a base pattern
                p           -> do -- First do a case match on a single element
                      (name, vars, _) <- mkGuardPat s linear p gs   
                      -- ... apply baseMatch to the case matcher to 
                      -- lift it into the matcher monad.
                      newname <- mkBaseMatch s name 
                      -- ... and return the meta-info gathered.
                      return (newname, vars, S)

           -- | Generate a basic function that cases on a single element, 
           -- returning Just (all bound variables) on a match, and
           -- Nothing on a mismatch.
           mkGuardPat :: SrcLoc -> Bool -> HsPat -> [HsStmt] -> Tr MFunMetaInfo
           mkGuardPat s b p gs = 
                do -- First we need a name...
                   n <- genMatchName
                   -- ... and then we need to know what variables that 
                   -- will be bound by this match.
                   let vs = gatherPVars p ++ concatMap gatherStmtVars gs
                   -- ... and then we can create and store away a casing function.
                   guardPatDecl s b n vs p gs >>= pushDecl
                   return (n, vs, S)

           -- | Generate a basic casing function for a given pattern.   
           guardPatDecl :: SrcLoc -> Bool -> HsName -> [HsName] -> HsPat -> [HsStmt] -> Tr HsDecl
           guardPatDecl s linear f vs p gs = do
                -- We can use the magic variable harp_a since nothing else needs to
                -- be in scope at this time (we could use just a, or foo, or whatever)
                let a = HsIdent $ "harp_a"
                -- ... and we should case on that variable on the right-hand side.
                rhs <- guardedCaseE s linear p gs a vs  -- case harp_a of ...
                -- The result is a simple function with one parameter and
                -- the right-hand side we just generated.
                return $ simpleFun s f a rhs
              where guardedCaseE :: SrcLoc -> Bool -> HsPat -> [HsStmt] -> HsName -> [HsName] -> Tr HsExp
                    guardedCaseE s b p gs a vs = do
                        -- First the alternative if we actually 
                        -- match the given pattern
                        let alt1 = altGW s p gs                 -- foo -> Just (mf foo)
                                    (app (var just_name) $ 
                                     tuple (map (retVar b) vs)) noBinds
                            -- .. and finally an alternative for not matching the pattern.
                            alt2 = alt s wildcard (var nothing_name)        -- _ -> Nothing
                            -- ... and that pattern could itself contain regular patterns
                            -- so we must transform away these.
                        alt1' <- liftTr $ transformAlt alt1
                        return $ caseE (var a) [alt1', alt2]
                    retVar :: Bool -> HsName -> HsExp
                    retVar linear v
                        -- if bound in linear context, apply const
                        | linear    = metaConst (var v)
                        -- if bound in non-linear context, apply (:)
                        | otherwise = app consFun (var v)



    -- For a sequence of regular patterns, we should transform all
    -- sub-patterns and then generate a function for sequencing them.
    HsRPSeq rps -> do 
        nvts <- mapM (trRPat s linear) rps
        mkSeqDecl s nvts
    
      where
        -- | Generate a match function for a sequence of regular patterns,
        -- flattening any special sub-patterns into normal elements of the list
        mkSeqDecl :: SrcLoc -> [MFunMetaInfo] -> Tr MFunMetaInfo
        mkSeqDecl s nvts = do
            -- First, as always, we need a name...
            name <- genMatchName
            let -- We need a generating statement for each sub-pattern.
                (gs, vals) = unzip $ mkGenExps s 0 nvts     -- (harp_valX, (foo, ...)) <- harp_matchY
                -- Gather up all variables from all sub-patterns.
                vars    = concatMap (\(_,vars,_) -> vars) nvts
                -- ... flatten all values to simple lists, and concatenate
                -- the lists to a new return value
                fldecls = flattenVals s vals                -- harp_valXf = $flatten harp_valX
                                                            -- harp_ret = foldComp [harp_val1f, ...]
                -- ... return the value along with all variables
                ret     = qualStmt $ metaReturn $           -- return (harp_ret, (foo, .....))
                            tuple [var retname, varTuple vars]
                -- ... do all these steps in a do expression
                rhs     = doE $ gs ++                       -- do (harp_valX, (foo, ...)) <- harpMatchY
                            [letStmt fldecls, ret]          --    let harp_valXf = $flatten harp_valX
                                                            --    return (harp_ret, (foo, .....))
            -- ... bind it to its name, and add the declaration
            -- to the store.
            pushDecl $ nameBind s name rhs                  -- harp_matchZ = do ....
            -- The return value of a sequence is always a list of elements.
            return (name, vars, L S)

        -- | Flatten values of all sub-patterns into normal elements of the list
        flattenVals :: SrcLoc -> [(HsName, MType)] -> [HsDecl]
        flattenVals s nts = 
            let -- Flatten the values of all sub-patterns to 
                -- lists of elements
                (nns, ds) = unzip $ map (flVal s) nts
                -- ... and concatenate their results.
                ret       = nameBind s retname $ app
                              (paren $ app foldCompFun 
                                (listE $ map var nns)) $ eList
             in ds ++ [ret]
    
    
        flVal :: SrcLoc -> (HsName, MType) -> (HsName, HsDecl)
        flVal s (name, mt) =
            let -- We reuse the old names, we just extend them a bit.
                newname = extendVar name "f"    -- harp_valXf
                -- Create the appropriate flattening function depending
                -- on the type of the value
                f       = flatten mt
                -- ... apply it to the value and bind it to its new name.
             in (newname, nameBind s newname $  -- harp_valXf = $flatten harp_valX
                    app f (var name))

        -- | Generate a flattening function for a given type structure.
        flatten :: MType -> HsExp
        flatten S = consFun                         -- (:)
        flatten (L mt) = 
            let f = flatten mt
                r = paren $ metaMap f
             in paren $ foldCompFun `metaComp` r    -- (foldComp . (map $flatten))
        flatten (E mt1 mt2) = 
            let f1 = flatten mt1
                f2 = flatten mt2
             in paren $ metaEither f1 f2            -- (either $flatten $flatten)
        flatten (M mt) = 
            let f = flatten mt
             in paren $ metaMaybe idFun f           -- (maybe id $flatten)

    -- For accumulating as-patterns we should transform the subpattern, and then generate 
    -- a declaration that supplies the value to be bound to the variable in question.
    -- The variable should be bound non-linearly.
    HsRPCAs v rp -> do 
        -- Transform the subpattern
        nvt@(name, vs, mt) <- trRPat s linear rp
        -- ... and create a declaration to bind its value.
        n <- mkCAsDecl s nvt
        -- The type of the value is unchanged.
        return (n, (v:vs), mt)

      where
        -- | Generate a declaration for a \@: binding.
        mkCAsDecl :: SrcLoc -> MFunMetaInfo -> Tr HsName
        mkCAsDecl = asDecl $ app consFun    -- should become lists when applied to []


    -- For ordinary as-patterns we should transform the subpattern, and then generate 
    -- a declaration that supplies the value to be bound to the variable in question.
    -- The variable should be bound linearly.
    HsRPAs v rp 
        | linear -> 
             do -- Transform the subpattern
                nvt@(name, vs, mt) <- trRPat s linear rp
                -- ... and create a declaration to bind its value
                n <- mkAsDecl s nvt
                -- The type of the value is unchanged.
                return (n, (v:vs), mt)
        -- We may not use an @ bind in non-linear context
        | otherwise -> case v of
                HsIdent n -> fail $ "Attempting to bind variable "++n++
                      " inside the context of a numerable regular pattern"
                _         -> fail $ "This should never ever ever happen... how the #% did you do it??!?"

      where
        -- | Generate a declaration for a \@ binding.
        mkAsDecl :: SrcLoc -> MFunMetaInfo -> Tr HsName
        mkAsDecl = asDecl metaConst     -- should be constant when applied to []


    -- For regular patterns, parentheses have no real meaning
    -- so at this point we can just skip them.
    HsRPParen rp -> trRPat s linear rp
    
    -- For (possibly non-greedy) optional regular patterns we need to
    -- transform the subpattern, and the generate a function that can
    -- choose to match or not to match, that is the question...
    HsRPOp rp HsRPOpt-> 
        do -- Transform the subpattern
           nvt <- trRPat s False rp
           -- ... and create a declaration that can optionally match it.
           mkOptDecl s False nvt
    -- ... similarly for the non-greedy version.
    HsRPOp rp HsRPOptG -> 
        do -- Transform the subpattern
           nvt <- trRPat s False rp
           -- ... and create a declaration that can optionally match it.
           mkOptDecl s True nvt


    -- For union patterns, we should transform both subexpressions,
    -- and generate a function that chooses between them.
    HsRPEither rp1 rp2 -> 
        do -- Transform the subpatterns
           nvt1 <- trRPat s False rp1
           nvt2 <- trRPat s False rp2
           -- ... and create a declaration that can choose between them.
           mkEitherDecl s nvt1 nvt2
        -- Generate declarations for either patterns, i.e. ( | )
      where mkEitherDecl :: SrcLoc -> MFunMetaInfo -> MFunMetaInfo -> Tr MFunMetaInfo
            mkEitherDecl s nvt1@(_, vs1, t1) nvt2@(_, vs2, t2) = do
                -- Eine namen, bitte!
                n <- genMatchName
                let -- Generate generators for the subpatterns
                    (g1, v1) = mkGenExp s nvt1
                    (g2, v2) = mkGenExp s nvt2          -- (harp_valX, (foo, bar, ...)) <- harp_matchY
                    -- ... gather all variables from both sides
                    allvs = vs1 `union` vs2
                    -- ... some may be bound on both sides, so we
                    -- need to check which ones are bound on each,
                    -- supplying empty value for those that are not
                    vals1 = map (varOrId vs1) allvs     
                    vals2 = map (varOrId vs2) allvs
                    -- ... apply either Left or Right to the returned value
                    ret1  = metaReturn $ tuple          -- return (Left harp_val1, (foo, id, ...))
                                [app (var left_name)
                                 (var v1), tuple vals1]
                    ret2  = metaReturn $ tuple          -- return (Right harp_val2, (id, bar, ...))
                                [app (var right_name)
                                 (var v2), tuple vals2]
                    -- ... and do all these things in do-expressions
                    exp1  = doE [g1, qualStmt ret1]
                    exp2  = doE [g2, qualStmt ret2]
                    -- ... and choose between them using the choice (+++) operator.
                    rhs   = (paren exp1) `metaChoice`       -- (do ...) +++ 
                            (paren exp2)            --  (do ...)
                -- Finally we create a declaration for this function and
                -- add it to the store.
                pushDecl $ nameBind s n rhs         -- harp_matchZ = (do ...) ...
                -- The type of the returned value is Either the type of the first
                -- or the second subpattern.
                return (n, allvs, E t1 t2)
         
            varOrId :: [HsName] -> HsName -> HsExp
            varOrId vs v = if v `elem` vs   -- the variable is indeed bound in this branch
                            then var v      -- ... so it should be added to the result
                            else idFun      -- ... else it should be empty.

    -- For (possibly non-greedy) repeating regular patterns we need to transform the subpattern,
    -- and then generate a function to handle many matches of it.
    HsRPOp rp HsRPStar -> 
        do -- Transform the subpattern
           nvt <- trRPat s False rp
           -- ... and create a declaration that can match it many times.
           mkStarDecl s False nvt
    -- ... and similarly for the non-greedy version.
    HsRPOp rp HsRPStarG-> 
        do -- Transform the subpattern
           nvt <- trRPat s False rp
           -- ... and create a declaration that can match it many times.
           mkStarDecl s True nvt

    -- For (possibly non-greedy) non-empty repeating patterns we need to transform the subpattern,
    -- and then generate a function to handle one or more matches of it.
    HsRPOp rp HsRPPlus -> 
        do -- Transform the subpattern
           nvt <- trRPat s False rp
           -- ... and create a declaration that can match it one or more times.
           mkPlusDecl s False nvt
    -- ... and similarly for the non-greedy version.
    HsRPOp rp HsRPPlusG -> 
        do -- Transform the subpattern
           nvt <- trRPat s False rp
           -- ... and create a declaration that can match it one or more times.
           mkPlusDecl s True nvt


  where -- These are the functions that must be in scope for more than one case alternative above.
  
    -- | Generate a declaration for matching a variable.
    mkVarMatch :: SrcLoc -> Bool -> HsName -> Tr MFunMetaInfo
    mkVarMatch s linear v = do
            -- First we need a name for the new match function.
            n <- genMatchName
            -- Then we need a basic matching function that always matches,
            -- and that binds the value matched to the variable in question.
            let e = paren $ lamE s [pvar v] $       -- (\v -> Just (mf v))
                              app (var just_name) 
                              (paren $ retVar linear v)
            -- Lift the function into the matcher monad, and bind it to its name,
            -- then add it the declaration to the store.
            pushDecl $ nameBind s n $
                          app baseMatchFun e    -- harp_matchX = baseMatch (\v -> Just (mf v))
            return (n, [v], S)          -- always binds v and only v

          where retVar :: Bool -> HsName -> HsExp
                retVar linear v 
                    -- if bound in linear context, apply const
                    | linear    = metaConst (var v)
                    -- if bound in non-linear context, apply (:)
                    | otherwise = app consFun (var v)   

    -- | Generate a declaration for matching a wildcard
    mkWCMatch :: SrcLoc -> Tr MFunMetaInfo
    mkWCMatch s = do 
            -- First we need a name...
            n <- genMatchName
            -- ... and then a function that always matches, discarding the result
            let e = paren $ lamE s [wildcard] $     -- (\_ -> Just ())
                                app (var just_name) unit_con
            -- ... which we lift, bind, and add to the store.
            pushDecl $ nameBind s n $       -- harp_matchX = baseMatch (\_ -> Just ())
                         app baseMatchFun e
            return (n, [], S)   -- no variables bound, hence []

    -- | Gather up the names of all variables in a pattern,
    -- using a simple fold over the syntax structure.
    gatherPVars :: HsPat -> [HsName]
    gatherPVars p = case p of
            HsPVar v             -> [v]
            HsPNeg q             -> gatherPVars q
            HsPInfixApp p1 _ p2  -> gatherPVars p1 ++
                                         gatherPVars p2
            HsPApp _ ps          -> concatMap gatherPVars ps 
            HsPTuple ps          -> concatMap gatherPVars ps 
            HsPList ps           -> concatMap gatherPVars ps 
            HsPParen p           -> gatherPVars p
            HsPRec _ pfs         -> concatMap help pfs
                where help (HsPFieldPat _ p) = gatherPVars p
            HsPAsPat n p         -> n : gatherPVars p
            HsPWildCard          -> []
            HsPIrrPat p          -> gatherPVars p
            HsPatTypeSig _ p _   -> gatherPVars p
            HsPRPat rps          -> concatMap gatherRPVars rps
            HsPXTag _ _ attrs mattr cps -> 
                concatMap gatherAttrVars attrs ++ concatMap gatherPVars cps ++
                    case mattr of
                     Nothing -> []
                     Just ap -> gatherPVars ap
            HsPXETag _ _ attrs mattr -> 
                concatMap gatherAttrVars attrs ++ 
                    case mattr of
                     Nothing -> []
                     Just ap -> gatherPVars ap
            HsPXPatTag p         -> gatherPVars p
            _                -> []

    gatherRPVars :: HsRPat -> [HsName]
    gatherRPVars rp = case rp of
            HsRPOp rq _        -> gatherRPVars rq
            HsRPEither rq1 rq2 -> gatherRPVars rq1 ++ gatherRPVars rq2
            HsRPSeq rqs        -> concatMap gatherRPVars rqs
            HsRPCAs n rq       -> n : gatherRPVars rq
            HsRPAs n rq        -> n : gatherRPVars rq
            HsRPParen rq       -> gatherRPVars rq
            HsRPGuard q gs     -> gatherPVars q ++ concatMap gatherStmtVars gs            
            HsRPPat q          -> gatherPVars q

    gatherAttrVars :: HsPXAttr -> [HsName]
    gatherAttrVars (HsPXAttr _ p) = gatherPVars p

    gatherStmtVars :: HsStmt -> [HsName]
    gatherStmtVars gs = case gs of
            HsGenerator _ p _ -> gatherPVars p
            _                 -> []

    -- | Generate a match function that lift the result of the
    -- basic casing function into the matcher monad.
    mkBaseMatch :: SrcLoc -> HsName -> Tr HsName
    mkBaseMatch s name = 
            do -- First we need a name...
               n <- genMatchName
               -- ... to which we bind the lifting function
               pushDecl $ baseMatchDecl s n name
               -- and then return for others to use.
               return n

    -- | Generate a declaration for the function that lifts a simple
    -- casing function into the matcher monad.
    baseMatchDecl :: SrcLoc -> HsName -> HsName -> HsDecl
    baseMatchDecl s newname oldname = 
            -- Apply the lifting function "baseMatch" to the casing function
            let e = app baseMatchFun (var oldname)
                -- ... and bind it to the new name.
             in nameBind s newname e        -- harp_matchX = baseMatch harp_matchY


    -- | Generate the generators that call sub-matching functions, and
    -- annotate names with types for future flattening of values.
    -- Iterate to enable gensym-like behavior.
    mkGenExps :: SrcLoc -> Int -> [MFunMetaInfo] -> [(HsStmt, (HsName, MType))]
    mkGenExps _ _ [] = []
    mkGenExps s k ((name, vars, t):nvs) = 
        let valname = mkValName k                           -- harp_valX
            pat     = pTuple [pvar valname, pvarTuple vars] -- (harp_valX, (foo, bar, ...))
            g       = var name
         in (genStmt s pat g, (valname, t)) :               -- (harp_valX, (foo, ...)) <- harp_matchY
                mkGenExps s (k+1) nvs

    -- | Create a single generator.
    mkGenExp :: SrcLoc -> MFunMetaInfo -> (HsStmt, HsName)
    mkGenExp s nvt = let [(g, (name, _t))] = mkGenExps s 0 [nvt]
                      in (g, name)

    -- | Generate a single generator with a call to (ng)manyMatch,
    -- and an extra variable name to use after unzipping. 
    mkManyGen :: SrcLoc -> Bool -> HsName -> HsStmt
    mkManyGen s greedy mname =
        -- Choose which repeater function to use, determined by greed
        let mf  = if greedy then gManyMatchFun else manyMatchFun
         -- ... and create a generator that applies it to the
         -- matching function in question.
         in genStmt s (pvar valsvarsname) $ 
            app mf (var mname)

    -- | Generate declarations for @: and @ bindings.
    asDecl :: (HsExp -> HsExp) -> SrcLoc -> MFunMetaInfo -> Tr HsName
    asDecl mf s nvt@(_, vs, _) = do
        -- A name, if you would
        n <- genMatchName                                -- harp_matchX
        let -- Generate a generator for matching the subpattern
            (g, val) = mkGenExp s nvt                    -- (harp_valY, (foo, ...)) <- harp_matchZ
            -- ... fix the old variables
            vars     = map var vs                        -- (apa, bepa, ...)
            -- ... and return the generated value, along with the
            -- new set of variables which is the old set prepended
            -- by the variable currently being bound.
            ret = qualStmt $ metaReturn $ tuple          -- return (harp_valY, ($mf harp_valY, apa, ...))
                [var val, tuple $ mf (var val) : vars]   -- mf in the line above is what separates
                                                         -- @: ((:)) from @ (const)
        -- Finally we create a declaration for this function and 
        -- add it to the store.
        pushDecl $ nameBind s n $ doE [g, ret]           -- harp_matchX = do ...
        return n

    -- | Generate declarations for optional patterns, ? and #?.
    -- (Unfortunally we must place this function here since both variations
    -- of transformations of optional patterns should be able to call it...)
    mkOptDecl :: SrcLoc -> Bool -> MFunMetaInfo -> Tr MFunMetaInfo
    mkOptDecl s greedy nvt@(_, vs, t) = do
        -- Un nome, s'il vouz plaît.
        n <- genMatchName
        let -- Generate a generator for matching the subpattern
            (g, val) = mkGenExp s nvt               -- (harp_valX, (foo, bar, ...)) <- harp_matchY
            -- ... and apply a Just to its value
            ret1 = metaReturn $ tuple               -- return (Just harp_val1, (foo, bar, ...))
                    [app (var just_name) 
                     (var val), varTuple vs]
            -- ... and do those two steps in a do-expression
            exp1 = doE [g, qualStmt ret1]           -- do ....
            -- For the non-matching branch, all the variables should be empty
            ids  = map (const idFun) vs             -- (id, id, ...)
            -- ... and the value should be Nothing.
            ret2 = metaReturn $ tuple               -- return (Nothing, (id, id, ...))
                    [var nothing_name, tuple ids]   -- i.e. no vars were bound
            -- The order of the arguments to the choice (+++) operator 
            -- is determined by greed...
            mc   = if greedy 
                    then metaChoice        -- standard order
                    else (flip metaChoice) -- reversed order
            -- ... and then apply it to the branches.
            rhs  = (paren exp1) `mc`                -- (do ....) +++ 
                    (paren ret2)                    --  (return (Nothing, .....))
        -- Finally we create a declaration for this function and
        -- add it to the store.
        pushDecl $ nameBind s n rhs                 -- harp_matchZ = (do ....) +++ (return ....)
        -- The type of the returned value will be Maybe the type
        -- of the value of the subpattern.
        return (n, vs, M t)
 
    -- | Generate declarations for star patterns, * and #*
    -- (Unfortunally we must place this function here since both variations
    -- of transformations of repeating patterns should be able to call it...)
    mkStarDecl :: SrcLoc -> Bool -> MFunMetaInfo -> Tr MFunMetaInfo
    mkStarDecl s greedy (mname, vs, t) = do
        -- Ett namn, tack!
        n <- genMatchName
        let -- Create a generator that matches the subpattern
            -- many times, either greedily or non-greedily
            g = mkManyGen s greedy mname
            -- ... and unzip the result, choosing the proper unzip
            -- function depending on the number of variables returned.
            metaUnzipK = mkMetaUnzip s (length vs)
            -- ... first unzip values from variables
            dec1    = patBind s (pvarTuple [valname, varsname])
                    (metaUnzip $ var valsvarsname)
            -- ... and then unzip the variables
            dec2    = patBind s (pvarTuple vs)
                    (metaUnzipK $ var varsname)
            -- ... fold all the values for variables
            retExps = map ((app foldCompFun) . var) vs
            -- ... and return value and variables
            ret     = metaReturn $ tuple $
                    [var valname, tuple retExps]
        -- Finally we need to generate a function that does all this,
        -- using a let-statement for the non-monadic stuff and a
        -- do-expression to wrap it all in.
        pushDecl $ nameBind s n $
            doE [g, letStmt [dec1, dec2], qualStmt ret]
        -- The type of the returned value is a list ([]) of the
        -- type of the subpattern.
        return (n, vs, L t)
        
    -- | Generate declarations for plus patterns, + and #+
    -- (Unfortunally we must place this function here since both variations
    -- of transformations of non-empty repeating patterns should be able to call it...)
    mkPlusDecl :: SrcLoc -> Bool -> MFunMetaInfo -> Tr MFunMetaInfo
    mkPlusDecl s greedy nvt@(mname, vs, t) = do
        -- and now I've run out of languages...
        n <- genMatchName
        let k = length vs
            -- First we want a generator to match the
            -- subpattern exactly one time
            (g1, val1) = mkGenExp s nvt                     -- (harp_valX, (foo, ...)) <- harpMatchY
            -- ... and then one that matches it many times.
            g2         = mkManyGen s greedy mname           -- harp_vvs <- manyMatch harpMatchY
            -- ... we want to unzip the result, using
            -- the proper unzip function
            metaUnzipK = mkMetaUnzip s k
            -- ... first unzip values from variables
            dec1    = patBind s                             -- (harp_vals, harp_vars) = unzip harp_vvs
                        (pvarTuple [valsname, varsname])
                        (metaUnzip $ var valsvarsname)
            -- .. now we need new fresh names for variables
            -- since the ordinary ones are already taken.
            vlvars  = genNames "harp_vl" k
            -- ... and then we can unzip the variables
            dec2    = patBind s (pvarTuple vlvars)          -- (harp_vl1, ...) = unzipK harp_vars
                        (metaUnzipK $ var varsname)
            -- .. and do the unzipping in a let-statement
            letSt   = letStmt [dec1, dec2]
            -- ... fold variables from the many-match,
            -- prepending the variables from the single match
            retExps = map mkRetFormat $ zip vs vlvars       -- foo . (foldComp harp_vl1), ...
            -- ... prepend values from the single match to
            -- those of the many-match.
            retVal  = (var val1) `metaCons` 
                        (var valsname)                      -- harp_valX : harp_vals
            -- ... return all values and variables
            ret     = metaReturn $ tuple $                  -- return (harp_valX:harpVals, 
                        [retVal, tuple retExps]             --   (foo . (...), ...))
            -- ... and wrap all of it in a do-expression.
            rhs     = doE [g1, g2, letSt, qualStmt ret]
        -- Finally we create a declaration for this function and
        -- add it to the store.
        pushDecl $ nameBind s n rhs
        -- The type of the returned value is a list ([]) of the
        -- type of the subpattern.
        return (n, vs, L t)

      where mkRetFormat :: (HsName, HsName) -> HsExp
            mkRetFormat (v, vl) =
                -- Prepend variables using function composition.
                (var v) `metaComp`
                  (paren $ (app foldCompFun) $ var vl)


--------------------------------------------------------------------------
-- HaRP-specific functions and ids

-- | Functions and ids from the @Match@ module, 
-- used in the generated matching functions
runMatchFun, baseMatchFun, manyMatchFun, gManyMatchFun :: HsExp
runMatchFun = match_qual runMatch_name
baseMatchFun = match_qual baseMatch_name
manyMatchFun = match_qual manyMatch_name
gManyMatchFun = match_qual gManyMatch_name

runMatch_name, baseMatch_name, manyMatch_name, gManyMatch_name :: HsName
runMatch_name = HsIdent "runMatch"
baseMatch_name = HsIdent "baseMatch"
manyMatch_name = HsIdent "manyMatch"
gManyMatch_name = HsIdent "gManyMatch"

match_mod, match_qual_mod :: Module
match_mod = Module "Harp.Match"
match_qual_mod = Module "HaRPMatch"

match_qual :: HsName -> HsExp
match_qual = qvar match_qual_mod

choiceOp :: HsQOp
choiceOp = HsQVarOp $ Qual match_qual_mod choice

appendOp :: HsQOp
appendOp = HsQVarOp $ UnQual append

-- foldComp = foldl (.) id, i.e. fold by composing
foldCompFun :: HsExp
foldCompFun = match_qual $ HsIdent "foldComp"

mkMetaUnzip :: SrcLoc -> Int -> HsExp -> HsExp
mkMetaUnzip s k | k <= 7 = let n = "unzip" ++ show k
                            in (\e -> matchFunction n [e])
                | otherwise = 
                   let vs      = genNames "x" k
                       lvs     = genNames "xs" k
                       uz      = name $ "unzip" ++ show k
                       ys      = name "ys"
                       xs      = name "xs"
                       alt1    = alt s peList $ tuple $ replicate k eList   -- [] -> ([], [], ...)
                       pat2    = (pvarTuple vs) `metaPCons` (pvar xs)       -- (x1, x2, ...)
                       ret2    = tuple $ map appCons $ zip vs lvs           -- (x1:xs1, x2:xs2, ...)
                       rhs2    = app (var uz) (var xs)                      -- unzipK xs
                       dec2    = patBind s (pvarTuple lvs) rhs2             -- (xs1, xs2, ...) = unzipK xs
                       exp2    = letE [dec2] ret2
                       alt2    = alt s pat2 exp2
                       topexp  = lamE s [pvar ys] $ caseE (var ys) [alt1, alt2]
                       topbind = nameBind s uz topexp
                    in app (paren $ letE [topbind] (var uz))
  where appCons :: (HsName, HsName) -> HsExp
        appCons (x, xs) = metaCons (var x) (var xs)

matchFunction :: String -> [HsExp] -> HsExp
matchFunction s es = mf s (reverse es)
  where mf s []     = match_qual $ HsIdent s
        mf s (e:es) = app (mf s es) e

-- | Some 'magic' gensym-like functions, and functions
-- with related functionality.
retname :: HsName
retname = name "harp_ret"

varsname :: HsName
varsname = name "harp_vars"

valname :: HsName
valname = name "harp_val"

valsname :: HsName
valsname = name "harp_vals"

valsvarsname :: HsName
valsvarsname = name "harp_vvs"

mkValName :: Int -> HsName
mkValName k = name $ "harp_val" ++ show k

extendVar :: HsName -> String -> HsName
extendVar (HsIdent n) s = HsIdent $ n ++ s
extendVar n _ = n

xNameParts :: HsXName -> (Maybe String, String)
xNameParts n = case n of
                HsXName s      -> (Nothing, s)
                HsXDomName d s -> (Just d, s)

---------------------------------------------------------
-- meta-level functions, i.e. functions that represent functions, 
-- and that take arguments representing arguments... whew!

metaReturn, metaConst, metaMap, metaUnzip :: HsExp -> HsExp
metaReturn e = metaFunction "return" [e]
metaConst e  = metaFunction "const" [e]
metaMap e    = metaFunction "map" [e]
metaUnzip e  = metaFunction "unzip" [e]

metaEither, metaMaybe :: HsExp -> HsExp -> HsExp
metaEither e1 e2 = metaFunction "either" [e1,e2]
metaMaybe e1 e2 = metaFunction "maybe" [e1,e2]

metaConcat :: [HsExp] -> HsExp
metaConcat es = metaFunction "concat" [listE es]

metaAppend :: HsExp -> HsExp -> HsExp
metaAppend l1 l2 = infixApp l1 appendOp l2

-- the +++ choice operator
metaChoice :: HsExp -> HsExp -> HsExp
metaChoice e1 e2 = infixApp e1 choiceOp e2

metaPCons :: HsPat -> HsPat -> HsPat
metaPCons p1 p2 = HsPInfixApp p1 cons p2

metaCons, metaComp :: HsExp -> HsExp -> HsExp
metaCons e1 e2 = infixApp e1 (HsQConOp cons) e2
metaComp e1 e2 = infixApp e1 (op fcomp) e2

metaPJust :: HsPat -> HsPat
metaPJust p = pApp just_name [p]

metaPNothing :: HsPat
metaPNothing = pvar nothing_name

metaPMkMaybe :: Maybe HsPat -> HsPat
metaPMkMaybe mp = case mp of
    Nothing -> metaPNothing
    Just p  -> pParen $ metaPJust p

metaJust :: HsExp -> HsExp
metaJust e = app (var just_name) e

metaNothing :: HsExp
metaNothing = var nothing_name

metaMkMaybe :: Maybe HsExp -> HsExp
metaMkMaybe me = case me of
    Nothing -> metaNothing
    Just e  -> paren $ metaJust e

---------------------------------------------------
-- some other useful functions at abstract level
consFun, idFun :: HsExp
consFun = HsCon cons
idFun = function "id"

cons :: HsQName
cons = Special HsCons

fcomp, choice, append :: HsName
fcomp = HsSymbol "."
choice = HsSymbol "+++"
append = HsSymbol "++"

just_name, nothing_name, left_name, right_name :: HsName
just_name = HsIdent "Just"
nothing_name = HsIdent "Nothing"
left_name = HsIdent "Left"
right_name = HsIdent "Right"

------------------------------------------------------------------------
-- Help functions for meta programming xml

{- No longer used.
hsx_data_mod :: Module
hsx_data_mod = Module "HSP.Data"

-- Also no longer used, literal PCDATA should be considered a string.
-- | Create an xml PCDATA value
metaMkPcdata :: String -> HsExp
metaMkPcdata s = metaFunction "pcdata" [strE s]
-}

-- | Create an xml tag, given its domain, name, attributes and
-- children.
metaGenElement :: HsXName -> [HsExp] -> Maybe HsExp -> [HsExp] -> HsExp
metaGenElement name ats mat cs = 
    let (d,n) = xNameParts name
        ne    = tuple [metaMkMaybe $ fmap strE d, strE n]
        m = maybe id (\x y -> paren $ y `metaAppend` (metaMap $ metaAsAttr x)) mat
        attrs = m $ listE $ map metaAsAttr ats
     in metaFunction "genElement" [ne, attrs, listE cs]

-- | Create an empty xml tag, given its domain, name and attributes.
metaGenEElement :: HsXName -> [HsExp] -> Maybe HsExp -> HsExp
metaGenEElement name ats mat = 
    let (d,n) = xNameParts name
        ne    = tuple [metaMkMaybe $ fmap strE d, strE n]
        m = maybe id (\x y -> paren $ y `metaAppend` (metaMap $ metaAsAttr x)) mat
        attrs = m $ listE $ map metaAsAttr ats
     in metaFunction "genEElement" [ne, attrs]

-- | Create an attribute by applying the overloaded @asAttr@
metaAsAttr :: HsExp -> HsExp
metaAsAttr e = metaFunction "asAttr" [e]

-- | Create a property from an attribute and a value.
metaAssign :: HsExp -> HsExp -> HsExp
metaAssign e1 e2 = infixApp e1 assignOp e2
  where assignOp = HsQVarOp $ UnQual $ HsSymbol ":="

-- | Make xml out of some expression by applying the overloaded function
-- @asChild@.
metaAsChild :: HsExp -> HsExp
metaAsChild e = metaFunction "asChild" [paren e]


-- TODO: We need to fix the stuff below so pattern matching on XML could also be overloaded.
-- Right now it only works on HSP XML, or anything that is syntactically identical to it.

-- | Lookup an attribute in the set of attributes.
metaExtract :: HsXName -> HsName -> HsExp
metaExtract name attrs = 
    let (d,n) = xNameParts name
        np    = tuple [metaMkMaybe $ fmap strE d, strE n]
     in metaFunction "extract" [np, var attrs]

-- | Generate a pattern under the Tag data constructor.
metaTag :: (Maybe String) -> String -> HsPat -> HsPat -> HsPat
metaTag dom name ats cpat =
    let d = metaPMkMaybe $ fmap strP dom
        n = pTuple [d, strP name]
     in metaConPat "Element" [n, ats, cpat]
     
-- | Generate a pattern under the PCDATA data constructor.
metaPcdata :: String -> HsPat
metaPcdata s = metaConPat "CDATA" [strP s]

metaMkName :: HsXName -> HsExp
metaMkName n = case n of
    HsXName s      -> strE s
    HsXDomName d s -> tuple [strE d, strE s]
