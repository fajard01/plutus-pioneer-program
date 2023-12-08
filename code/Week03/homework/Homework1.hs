{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeFamilies      #-}

module Homework1 where

import           Plutus.V1.Ledger.Interval  (contains, from, to)
import           Plutus.V2.Ledger.Api       (BuiltinData, POSIXTime, PubKeyHash,
                                             ScriptContext (scriptContextTxInfo), 
                                             TxInfo (txInfoValidRange),
                                             Validator,
                                             mkValidatorScript)
import           Plutus.V2.Ledger.Contexts  (txSignedBy)
import           PlutusTx                   (compile, unstableMakeIsData)
import           PlutusTx.Prelude           (Bool (..), ($), (&&), (||), (+), traceIfFalse)
import           Utilities                  (wrapValidator)

---------------------------------------------------------------------------------------------------
----------------------------------- ON-CHAIN / VALIDATOR ------------------------------------------

data VestingDatum = VestingDatum
    { beneficiary1 :: PubKeyHash
    , beneficiary2 :: PubKeyHash
    , deadline     :: POSIXTime
    }

unstableMakeIsData ''VestingDatum

{-# INLINABLE mkVestingValidator #-}
-- This should validate if either beneficiary1 has signed the transaction and the current slot is before or at the deadline
-- or if beneficiary2 has signed the transaction and the deadline has passed.
mkVestingValidator :: VestingDatum -> () -> ScriptContext -> Bool
mkVestingValidator _dat () _ctx = 
    ( 
      traceIfFalse "beneficiary2 signature missing" signedByBeneficiary1 &&
      traceIfFalse "deadline has passed" onOrBeforeDeadline
    ) ||
    (
      traceIfFalse "beneficiary2 signature missing" signedByBeneficiary2 &&
      traceIfFalse "deadline not yet reached" afterDeadline
    )
       where
          info :: TxInfo
          info = scriptContextTxInfo _ctx

          signedByBeneficiary1 :: Bool
          signedByBeneficiary1 = txSignedBy info $ beneficiary1 _dat

          signedByBeneficiary2 :: Bool
          signedByBeneficiary2 = txSignedBy info $ beneficiary2 _dat

          onOrBeforeDeadline :: Bool
          onOrBeforeDeadline = contains (to $ deadline _dat) $ txInfoValidRange info

          afterDeadline :: Bool 
          afterDeadline = contains (from $ (deadline _dat) + 1) $ txInfoValidRange info

          
{-# INLINABLE  mkWrappedVestingValidator #-}
mkWrappedVestingValidator :: BuiltinData -> BuiltinData -> BuiltinData -> ()
mkWrappedVestingValidator = wrapValidator mkVestingValidator

validator :: Validator
validator = mkValidatorScript $$(compile [|| mkWrappedVestingValidator ||])
