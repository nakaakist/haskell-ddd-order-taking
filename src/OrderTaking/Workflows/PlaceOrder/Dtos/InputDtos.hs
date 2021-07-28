{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Workflows.PlaceOrder.Dtos.InputDtos
  ( UnvalidatedCustomerInfoDto
  , UnvalidatedAddressDto
  , UnvalidatedOrderLineDto
  , UnvalidatedOrderDto
  ) where

import           OrderTaking.Workflows.PlaceOrder.DomainTypes.Inputs
                                               as Inputs


-- public input DTO types

type UnvalidatedOrderDto = UnvalidatedOrder

type UnvalidatedCustomerInfoDto = UnvalidatedCustomerInfo

type UnvalidatedAddressDto = UnvalidatedAddress

type UnvalidatedOrderLineDto = UnvalidatedOrderLine
