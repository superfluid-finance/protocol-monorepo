{-# LANGUAGE GADTs #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Money.Systems.Superfluid.Integrations.Show where

import           Money.Systems.Superfluid.Concepts.Agreement (AnyAgreementAccountData (..))

instance Show (AnyAgreementAccountData sft) where
    show (MkAgreementAccountData g) = show g
