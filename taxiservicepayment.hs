{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Taxi Transportation Payment Gateway Smart Contract

module InstrumentCalibrationServiceChargesPaymentFilter where

-- Import Dependency Data Environment 
import           Control.Monad        (void)
import           Data.Aeson           (ToJSON, FromJSON)
import           Data.List.NonEmpty   (NonEmpty (..))
import           Data.Map (Map)
import           Data.Map                  as Map
import           Data.Maybe (catMaybes)
import qualified Data.ByteString.Char8     as C
import           Data.Text            (pack, Text)
import           GHC.Generics         (Generic)

-- Import Dependency Plutus Environment 
import qualified PlutusTx                  as PlutusTx
import           PlutusTx.Prelude 
import           Plutus.Contract
import PlutusTx.Prelude hiding (pure, (<$>))
import Prelude qualified as Haskell

-- Import Dependency Ledger Environment 
import           Ledger                    (Address, Validator, ScriptContext, Value, scriptAddress, getCardanoTxId, ChainIndexTxOut(..), Datum (Datum), dataHash, Datum (..), DatumHash (..), PaymentPubKeyHash, TxInfo, scriptContextTxInfo, txSignedBy,unPaymentPubKeyHash)
import           Ledger                    hiding (singleton)
import           Ledger.Tx (ChainIndexTxOut (..))
import qualified Ledger.Constraints        as Constraints
import qualified Ledger.Typed.Scripts      as Scripts
import           Ledger.Value              as Value
import           Ledger.Ada                as Ada

-- Import Dependency Playground Environment 
import           Playground.Contract
import qualified Prelude
import qualified Prelude              as P
import Prelude (String)
import           Text.Printf          (printf)

-- Functor Definition
newtype HashedString = HashedString BuiltinByteString deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
PlutusTx.makeLift ''HashedString


--------------------------------------------------------------------------------
-- | Taxi Transportation Payment Gateway Smart Contract Validation On-Chain Code
--------------------------------------------------------------------------------
minLovelace :: Integer
minLovelace = 2000000

-- | Taxi Transportation Payment Gateway Spesification Target Datum On-Chain Code
data TaxiServiceDatum = TaxiServiceDatum
    { taxi_service_beneficiary       :: !PaymentPubKeyHash           -- Public Hash Key Taxi Transportation Service Provider Beneficiary Address
    , set_TotalDistance              :: !Integer                     -- Distance of Services Utilized
    , set_ServiceChargesAmount       :: !Integer                     -- Taxi Transportation Service Charges Amount 
    , set_pin                        :: !Integer                     -- Pin Reward Service Charges Withdrawl Authorization 
    } deriving Show
       
PlutusTx.unstableMakeIsData ''TaxiServiceDatum
PlutusTx.makeLift ''TaxiServiceDatum

-- | Taxi Transportation Payment Gateway Validation Redeemer On-Chain Code
data ServiceChargesRedeemer = ServiceChargesRedeemer
    { val_TotalDistance          :: !Integer			-- Validated Value of Total Distances Utilized
    , val_pin                    :: !Integer			-- Validated Reward Service Charges Withdrawl Pin Authorization 
    } deriving Show

PlutusTx.unstableMakeIsData ''ServiceChargesRedeemer
PlutusTx.makeLift ''ServiceChargesRedeemer


-- | Taxi Transportation Payment Gateway Validation On-Chain Code
-- Transaction Validation to released Taxi Service Charges Charges Payment through On Spesification Requirements Distance of Services Validation,
-- beneficiarry address matching, and Service Charges Withdrawl Authorization authorization matching to released the Service Charges Amount

{-# INLINABLE validateCCEmission #-}
validateCCEmission :: TaxiServiceDatum -> ServiceChargesRedeemer -> ScriptContext -> Bool
validateCCEmission (TaxiServiceDatum phk stdm samt spin) (ServiceChargesRedeemer vtdm vpin) ctx = 
                                                                        traceIfFalse "Validation Services Distance Not Matched !!!" $  stdm >= vtdm  &&
                                                                        (traceIfFalse "Wrong Service Charges Withdrawl Authorization !!!" $ spin == vpin) &&
                                                                         traceIfFalse "Service Provider Beneficiary's Signature Not Matched" signedByBeneficiary
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    signedByBeneficiary :: Bool
    signedByBeneficiary = txSignedBy info $ unPaymentPubKeyHash $ phk


-- | Taxi Transportation Payment Gateway Datum and Redeemer Parameters
data TaxiServiceRewards
instance Scripts.ValidatorTypes TaxiServiceRewards where
    type instance DatumType TaxiServiceRewards = TaxiServiceDatum
    type instance RedeemerType TaxiServiceRewards = ServiceChargesRedeemer

-- | The Script Instance to compile validator (ready to go onto the chain)
taxiServiceRewardsInstance :: Scripts.TypedValidator TaxiServiceRewards
taxiServiceRewardsInstance = Scripts.mkTypedValidator @TaxiServiceRewards
  $$(PlutusTx.compile [|| validateCCEmission ||])
  $$(PlutusTx.compile [|| wrap ||])
    where
      wrap = Scripts.wrapValidator @TaxiServiceDatum @ServiceChargesRedeemer


------------------------------------------------------------------------------
-- | Taxi Transportation Payment Gateway  Validation Off-Chain Code
------------------------------------------------------------------------------

-- | The Address of the Taxi Transportation Services Providers
taxiServiceRewardsAddress :: Address
taxiServiceRewardsAddress = Ledger.scriptAddress (Scripts.validatorScript taxiServiceRewardsInstance)

-- | Parameters for the "Taxi Transportation Payment Gateway On Spesification Target" endpoint
data TaxiSpecParams = TaxiSpecParams
    { taxiServiceBeneficiary     :: !PaymentPubKeyHash          -- Public Hash Key Transport Service Provider Beneficiary Address
    , set_Total_Distance         :: !Integer                    -- Distance of Services Utilized by Customers       
    , serviceChargesAmount       :: !Integer                    -- Services Charges Amount             
    , pin                        :: !Integer                    -- Pin Reward Service Charges Withdrawl Authorization 
    }
    deriving (Generic, ToJSON, FromJSON, ToSchema)


--  | Parameters for the "Instrument Calibration Service Validation Report" endpoint
data CalSpecValParams = CalSpecValParams
    { val_Total_Distance           :: !Integer    -- Validation of Distance of Services in Km 
    , pin_validation               :: !Integer    -- Validated Pin Reward Withdrawl Authorization
    }
    deriving stock (Prelude.Eq, Prelude.Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema, ToArgument)

-- | The schema of the contract, with one endpoint to publish the problem with Taxi Transportation Payment Gateway
--   and submit Taxi Transportation Payment  On Spesification Validation Value
type TaxiServiceRewardsSchema =
            Endpoint "Taxi Transportation Services On Spesification Target and Service Charges Funds" TaxiSpecParams    
        .\/ Endpoint "Taxi Transportation Services Validation" CalSpecValParams

-- | The "Taxi Transportation Service On Spesification Target" contract endpoint.
taxisvcrewards :: AsContractError e => TaxiSpecParams -> Contract () TaxiServiceRewardsSchema e ()
taxisvcrewards (TaxiSpecParams bnf tdist svcAmt pnt ) = do
    let datDatum = TaxiServiceDatum
                { taxi_service_beneficiary         = bnf               -- Public Hash Key Transport Service Provider Beneficiary Address
                , set_TotalDistance                = tdist             -- Distance of Services Utilized by Customers        
                , set_ServiceChargesAmount         = svcAmt            -- Services Charges Amount
                , set_pin                          = pnt               -- Pin Reward Service Charges Withdrawl Authorization  
                }

    let tx   = Constraints.mustPayToTheScript datDatum $ Ada.lovelaceValueOf svcAmt
    ledgerTx <- submitTxConstraints taxiServiceRewardsInstance tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "Taxi Transportation Services Request " 
    logInfo @String $ printf "Taxi Transportation Services Order with distance  %d Kilometers " tdist 
    logInfo @String $ printf "Transportation Service Charges of %d ADA will  be credited to Taxi Service Provider when the performance meet the Specified Services Orders" svcAmt

-- | The "Taxi Transportation Services On Spesification Validation" contract endpoint.
calvalidationupdate :: AsContractError e => CalSpecValParams -> Contract () TaxiServiceRewardsSchema e ()
calvalidationupdate (CalSpecValParams valdist pin_validation) = do
    onow   <- currentTime
    opkh   <- ownPaymentPubKeyHash
    -- filter all incorrect datum calibration service Rewards scripts
    unspentOutputs <- Map.filter hasCorrectDatum <$> utxosAt taxiServiceRewardsAddress
    let datRedeemer = ServiceChargesRedeemer
                { val_TotalDistance     = valdist                    -- Validated Total Distances
                , val_pin               = pin_validation             -- Validated Reward Service Charges Withdrawl Authorization 
                }

    let tx = collectFromScript unspentOutputs datRedeemer
    ledgerTx <- submitTxConstraintsSpending taxiServiceRewardsInstance unspentOutputs tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
    logInfo @String $ printf "Taxi Transportation Services Validation Reports" 
    logInfo @String $ printf "Taxi Transportation Services has reached destination and achieved distance %d Kilometers " valdist 
      where
        hasCorrectDatum :: ChainIndexTxOut -> Bool
        hasCorrectDatum (ScriptChainIndexTxOut _ _ (Right (Datum datum)) _)    =
          case PlutusTx.fromBuiltinData datum of
          Just d  -> valdist >= (set_TotalDistance d)  && pin_validation == (set_pin d)
          Nothing -> False
        hasCorrectDatum _ = False


-- | Taxi Transportation Services Rewards endpoints.
endpoints :: AsContractError e => Contract () TaxiServiceRewardsSchema e ()
endpoints = awaitPromise (taxisvcrewards' `select` calvalidationupdate') >> endpoints
  where
    taxisvcrewards'       = endpoint @"Taxi Transportation Services On Spesification Target and Service Charges Funds" taxisvcrewards
    calvalidationupdate' = endpoint @"Taxi Transportation Services Validation" calvalidationupdate

mkSchemaDefinitions ''TaxiServiceRewardsSchema

