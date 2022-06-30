{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE GADTs #-}

module Money.Systems.Superfluid.Instances.Simple.Integrations.Show where

import           Text.Printf                                                      (printf)

import           Money.Systems.Superfluid.Concepts.SuperfluidTypes                (SuperfluidTypes)
--
import qualified Money.Systems.Superfluid.Agreements.ConstantFlowAgreement        as CFA
import qualified Money.Systems.Superfluid.Agreements.DecayingFlowAgreement        as DFA
import qualified Money.Systems.Superfluid.Agreements.TransferableBalanceAgreement as TBA


instance SuperfluidTypes sft => Show (TBA.TBAAccountData sft) where
    show x = printf "{ t = %s, uliq = %s, mliq = %s }"
        (show $ TBA.settledAt x)
        (show $ TBA.untappedLiquidity x)
        (show $ TBA.mintedLiquidity x)

-- instance SuperfluidTypes sft => Show (CFA.CFAContractData sft) where
--     show x = printf "{ flowLastUpdatedAt = %s, flowRate = %s, flowBuffer = %s }"
--         (show $ CFA.flowLastUpdatedAt x) (show $ CFA.flowRate x) (show $ CFA.flowBuffer x)

instance SuperfluidTypes sft => Show (CFA.CFAAccountData sft) where
    show x = printf "{ t = %s, uliq = %s, buf = %s, fr = %s }"
        (show $ CFA.settledAt x)
        (show $ CFA.settledUntappedLiquidity x)
        (show $ CFA.settledBufferLiquidity x)
        (show $ CFA.netFlowRate x)

-- instance SuperfluidTypes sft => Show (DFAContractData sft) where
--     show x = printf "{ t_u = %s, δ = %s, λ = %s }"
--         (show $ flowLastUpdatedAt x) (show $ distributionLimit x) (show $ decayingFactor x)

instance SuperfluidTypes sft => Show (DFA.DFAAccountData sft) where
    show x = printf "{ t_s = %s, α = %s, ε = %s, buf = %s }"
        (show $ DFA.settledAt x)
        (show $ DFA.αVal x)
        (show $ DFA.εVal x)
        (show $ DFA.settledBuffer x)
