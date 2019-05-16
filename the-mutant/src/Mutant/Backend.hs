-- | Backend enumeration.
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies   #-}
module Mutant.Backend where


-- | Available backends.
data Backend
  = BackendJS
  | BackendSDL


-- | An instantiation of a mutant backend.
-- Backend modules will define this and provide a function
-- for creating a value of @MutantInstance i@ which can then
-- be used to fetch any needed APIs.
data family MutantInstance (i :: Backend)
