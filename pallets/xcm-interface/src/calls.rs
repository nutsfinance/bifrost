// This file is part of Bifrost.

// Copyright (C) 2019-2022 Liebi Technologies (UK) Ltd.
// SPDX-License-Identifier: GPL-3.0-or-later WITH Classpath-exception-2.0

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program. If not, see <https://www.gnu.org/licenses/>.

use codec::{Decode, Encode};
use frame_support::{sp_runtime::MultiSignature, RuntimeDebug};
use sp_std::vec::Vec;

use crate::ChainId;

#[derive(Encode, Decode, RuntimeDebug)]
pub enum UtilityCall<RelayChainCall> {
	#[codec(index = 1)]
	AsDerivative(u16, RelayChainCall),
	#[codec(index = 2)]
	BatchAll(Vec<RelayChainCall>),
}

#[derive(Encode, Decode, RuntimeDebug)]
pub enum StakingCall {
	#[codec(index = 3)]
	WithdrawUnbonded(u32),
}

pub mod rococo {

	pub use crate::calls::*;

	#[derive(Encode, Decode, RuntimeDebug)]
	pub enum RelaychainCall<BalanceOf, AccountIdOf, BlockNumberOf> {
		#[codec(index = 28)]
		Crowdloan(ContributeCall<BalanceOf, AccountIdOf>),
		#[codec(index = 30)]
		StableAsset(StableAssetCall<BalanceOf, AccountIdOf>),
		#[codec(index = 91)]
		Proxy(ProxyCall<AccountIdOf, BlockNumberOf>),
	}
}

pub mod kusama {

	pub use crate::calls::*;

	#[derive(Encode, Decode, RuntimeDebug)]
	pub enum RelaychainCall<BalanceOf, AccountIdOf, BlockNumberOf> {
		#[codec(index = 73)]
		Crowdloan(ContributeCall<BalanceOf, AccountIdOf>),
		#[codec(index = 30)]
		StableAsset(StableAssetCall<BalanceOf, AccountIdOf>),
		#[codec(index = 30)]
		Proxy(ProxyCall<AccountIdOf, BlockNumberOf>),
	}
}

pub mod polkadot {

	pub use crate::calls::*;

	#[derive(Encode, Decode, RuntimeDebug)]
	pub enum RelaychainCall<BalanceOf, AccountIdOf, BlockNumberOf> {
		#[codec(index = 73)]
		Crowdloan(ContributeCall<BalanceOf, AccountIdOf>),
		#[codec(index = 30)]
		StableAsset(StableAssetCall<BalanceOf, AccountIdOf>),
		#[codec(index = 29)]
		Proxy(ProxyCall<AccountIdOf, BlockNumberOf>),
	}
}

#[derive(Encode, Decode, RuntimeDebug)]
pub enum ContributeCall<BalanceOf, AccountIdOf> {
	#[codec(index = 1)]
	Contribute(Contribution<BalanceOf>),
	#[codec(index = 2)]
	Withdraw(Withdraw<AccountIdOf>),
	#[codec(index = 6)]
	AddMemo(AddMemo),
}

#[derive(PartialEq, Encode, Decode, RuntimeDebug)]
pub struct Contribution<BalanceOf> {
	#[codec(compact)]
	pub index: ChainId,
	#[codec(compact)]
	pub value: BalanceOf,
	pub signature: Option<MultiSignature>,
}

#[derive(PartialEq, Encode, Decode, RuntimeDebug)]
pub struct Withdraw<AccountIdOf> {
	pub who: AccountIdOf,
	#[codec(compact)]
	pub index: ChainId,
}

#[derive(PartialEq, Encode, Decode, RuntimeDebug)]
pub struct AddMemo {
	pub index: ChainId,
	pub memo: Vec<u8>,
}

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Encode, Decode, RuntimeDebug)]
pub enum ProxyType {
	Any,
	NonTransfer,
	Governance,
	Staking,
	IdentityJudgement,
	CancelProxy,
}

#[derive(Encode, Decode, RuntimeDebug)]
pub enum ProxyCall<AccountIdOf, BlockNumberOf> {
	#[codec(index = 1)]
	Add(AddProxy<AccountIdOf, BlockNumberOf>),
	#[codec(index = 2)]
	Remove(RemoveProxy<AccountIdOf, BlockNumberOf>),
}

#[derive(PartialEq, Encode, Decode, RuntimeDebug)]
pub struct AddProxy<AccountIdOf, BlockNumberOf> {
	pub delegate: AccountIdOf,
	pub proxy_type: ProxyType,
	pub delay: BlockNumberOf,
}

#[derive(PartialEq, Encode, Decode, RuntimeDebug)]
pub struct RemoveProxy<AccountIdOf, BlockNumberOf> {
	pub delegate: AccountIdOf,
	pub proxy_type: ProxyType,
	pub delay: BlockNumberOf,
}

#[derive(Encode, Decode, RuntimeDebug)]
pub enum StableAssetCall<BalanceOf, AccountIdOf> {
	#[codec(index = 9)]
	Mint(Mint<BalanceOf, AccountIdOf>),
	#[codec(index = 11)]
	ReceiveMint(ReceiveMint<BalanceOf, AccountIdOf>),
	#[codec(index = 12)]
	RedeemSingle(RedeemSingle<BalanceOf, AccountIdOf>),
	#[codec(index = 14)]
	ReceiveRedeemSingle(ReceiveRedeemSingle<BalanceOf, AccountIdOf>),
}

#[derive(PartialEq, Encode, Decode, RuntimeDebug)]
pub struct Mint<BalanceOf, AccountIdOf> {
	pub account_id: AccountIdOf,
	pub target_pool_id: u32,
	pub amounts: Vec<BalanceOf>,
	pub min_mint_amount: BalanceOf,
	pub source_pool_id: u32,
}

#[derive(PartialEq, Encode, Decode, RuntimeDebug)]
pub struct ReceiveMint<BalanceOf, AccountIdOf> {
	pub account_id: AccountIdOf,
	pub source_pool_id: u32,
	pub mint_amount: Option<BalanceOf>,
	pub amounts: Vec<BalanceOf>,
}

#[derive(PartialEq, Encode, Decode, RuntimeDebug)]
pub struct RedeemSingle<BalanceOf, AccountIdOf> {
	pub account_id: AccountIdOf,
	pub target_pool_id: u32,
	pub amount: BalanceOf,
	pub i: u32,
	pub min_redeem_amount: BalanceOf,
	pub asset_length: u32,
	pub source_pool_id: u32,
}

#[derive(PartialEq, Encode, Decode, RuntimeDebug)]
pub struct ReceiveRedeemSingle<BalanceOf, AccountIdOf> {
	pub account_id: AccountIdOf,
	pub source_pool_id: u32,
	pub redeem_amount: Option<BalanceOf>,
	pub burn_amount: BalanceOf,
}
