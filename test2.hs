{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NamedFieldPuns #-}

module LaunchPad where

import Control.Monad (void)
import Plutus.Contract as Contract
import Plutus.V1.Ledger.Value as Value
import PlutusTx.Prelude hiding (Semigroup(..), unless)
import Plutus.V1.Ledger.Tx as Tx
import Ledger.Typed.Scripts as Scripts
import Ledger.Typed.Tx as TypedTx
import Ledger.Typed.Scripts.Validators as Validators
import Ledger.Typed.Scripts.MonetaryPolicies as MonetaryPolicies
import PlutusTx

-- Define the data type for the contract's parameters
data LaunchPadParams = LaunchPadParams
    { lppOwnerPkh     :: PubKeyHash
    , lppTokenName    :: ByteString
    , lppTokenSymbol  :: ByteString
    , lppTotalSupply  :: Integer
    , lppExchangeRate :: Integer
    }

-- Define the contract's instance
instance Scripts.ValidatorTypes LaunchPad where
    type instance RedeemerType LaunchPad = ()
    type instance DatumType LaunchPad = LaunchPadParams

-- Define the contract's validator script
{-# INLINABLE mkValidator #-}
mkValidator :: LaunchPadParams -> () -> ScriptContext -> Bool
mkValidator LaunchPadParams{lppOwnerPkh, lppTokenName, lppTokenSymbol, lppTotalSupply, lppExchangeRate} () ctx =
    let 
        -- Get transaction information
        info = scriptContextTxInfo ctx
        ownInput = Tx.txInInfoOutRef $ findOwnInput ctx
        inValue = Tx.txOutValue $ Tx.txInInfoResolved ownInput
        outValue = Tx.txOutValue $ Tx.txInfoOutputs info !! 0

        -- Compute token amounts and the fee
        adaRaised = Value.flattenValue (Value.assetClassValueOf inValue $ Value.Ada $ Value.adaSymbol)
        tokensIssued = adaRaised * lppExchangeRate
        fee = adaRaised `div` 100

        -- Define token currency symbol and asset class
        policy = MonetaryPolicies.mkMonetaryPolicyScript $
                    MonetaryPolicies.defaultMonetaryPolicyParams {
                        MonetaryPolicies.mppScriptHash = Validators.validatorHash lppOwnerPkh
                    }
        curSymbol = Scripts.scriptCurrencySymbol policy
        token = Value.assetClass curSymbol lppTokenSymbol

        -- Check token issuance and fee conditions
        tokenCondition = Value.assetClassValueOf outValue token == tokensIssued
        feeCondition = Value.assetClassValueOf outValue (Value.Ada $ Value.adaSymbol) == fee
    in 
        tokenCondition && feeCondition

-- Compile the validator script
validator :: LaunchPad -> Scripts.TypedValidator LaunchPad
validator = Scripts.mkTypedValidator @Launch
