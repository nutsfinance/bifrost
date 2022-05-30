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

#![cfg_attr(not(feature = "std"), no_std)]
#![allow(clippy::unused_unit)]

pub mod calls;
pub mod traits;
pub use calls::*;
use orml_traits::MultiCurrency;
pub use pallet::*;
pub use traits::{ChainId, MessageId, Nonce};
pub use nutsfinance_stable_asset;

macro_rules! use_relay {
    ({ $( $code:tt )* }) => {
        if T::RelayNetwork::get() == NetworkId::Polkadot {
            use polkadot::RelaychainCall;

			$( $code )*
        } else if T::RelayNetwork::get() == NetworkId::Kusama {
            use kusama::RelaychainCall;

			$( $code )*
        } else if T::RelayNetwork::get() == NetworkId::Any {
            use rococo::RelaychainCall;

			$( $code )*
        } else {
            unreachable!()
        }
    }
}

pub(crate) type AccountIdOf<T> = <T as frame_system::Config>::AccountId;

pub(crate) type CurrencyIdOf<T> =
	<<T as Config>::MultiCurrency as MultiCurrency<AccountIdOf<T>>>::CurrencyId;

#[allow(type_alias_bounds)]
pub(crate) type BalanceOf<T: Config> =
	<<T as Config>::MultiCurrency as MultiCurrency<AccountIdOf<T>>>::Balance;

#[allow(type_alias_bounds)]
pub(crate) type StableAssetBalanceOf<T: Config> = <T as nutsfinance_stable_asset::Config>::Balance;

#[frame_support::pallet]
pub mod pallet {
	use frame_support::{pallet_prelude::*, transactional, weights::Weight};
	use frame_system::pallet_prelude::*;
	use orml_traits::{currency::TransferAll, MultiCurrency, MultiReservableCurrency};
	use scale_info::TypeInfo;
	use sp_runtime::{
		traits::{Convert, Zero},
		DispatchError,
	};
	use sp_std::{convert::From, prelude::*, vec, vec::Vec};
	use xcm::{latest::prelude::*, DoubleEncoded, VersionedXcm};

	use super::*;
	use crate::traits::*;

	#[derive(Encode, Decode, Eq, PartialEq, Copy, Clone, RuntimeDebug, TypeInfo)]
	pub enum XcmInterfaceOperation {
		UmpContributeTransact,
		StatemineTransfer,
		StableAssetCall,
	}

	#[pallet::config]
	pub trait Config: frame_system::Config + pallet_xcm::Config + nutsfinance_stable_asset::Config {
		type Event: From<Event<Self>> + IsType<<Self as frame_system::Config>::Event>;

		type MultiCurrency: TransferAll<AccountIdOf<Self>>
			+ MultiCurrency<AccountIdOf<Self>>
			+ MultiReservableCurrency<AccountIdOf<Self>>;

		/// Origin represented Governance
		type UpdateOrigin: EnsureOrigin<<Self as frame_system::Config>::Origin>;

		/// The currency id of the RelayChain
		#[pallet::constant]
		type RelaychainCurrencyId: Get<CurrencyIdOf<Self>>;

		/// The account of parachain on the relaychain.
		#[pallet::constant]
		type ParachainSovereignAccount: Get<AccountIdOf<Self>>;

		/// XCM executor.
		type XcmExecutor: ExecuteXcm<<Self as frame_system::Config>::Call>;

		/// Convert `T::AccountId` to `MultiLocation`.
		type AccountIdToMultiLocation: Convert<AccountIdOf<Self>, MultiLocation>;

		#[pallet::constant]
		type RelayNetwork: Get<NetworkId>;

		#[pallet::constant]
		type StatemineTransferFee: Get<BalanceOf<Self>>;

		#[pallet::constant]
		type StatemineTransferWeight: Get<Weight>;

		#[pallet::constant]
		type ContributionFee: Get<BalanceOf<Self>>;

		#[pallet::constant]
		type ContributionWeight: Get<Weight>;
	}

	#[pallet::error]
	pub enum Error<T> {
		FeeConvertFailed,
		XcmExecutionFailed,
		XcmSendFailed,
	}

	#[pallet::event]
	#[pallet::generate_deposit(pub(crate) fn deposit_event)]
	pub enum Event<T: Config> {
		/// Xcm dest weight has been updated. \[xcm_operation, new_xcm_dest_weight\]
		XcmDestWeightUpdated(XcmInterfaceOperation, Weight),
		/// Xcm dest weight has been updated. \[xcm_operation, new_xcm_dest_weight\]
		XcmFeeUpdated(XcmInterfaceOperation, BalanceOf<T>),
		TransferredStatemineMultiAsset(AccountIdOf<T>, BalanceOf<T>),
	}

	/// The dest weight limit and fee for execution XCM msg sended by XcmInterface. Must be
	/// sufficient, otherwise the execution of XCM msg on relaychain will fail.
	///
	/// XcmDestWeightAndFee: map: XcmInterfaceOperation => (Weight, Balance)
	#[pallet::storage]
	#[pallet::getter(fn xcm_dest_weight_and_fee)]
	pub type XcmDestWeightAndFee<T: Config> =
		StorageMap<_, Twox64Concat, XcmInterfaceOperation, (Weight, BalanceOf<T>), OptionQuery>;

	/// Tracker for the next nonce index
	#[pallet::storage]
	#[pallet::getter(fn current_nonce)]
	pub(super) type CurrentNonce<T: Config> =
		StorageMap<_, Blake2_128Concat, ChainId, Nonce, ValueQuery>;

	#[pallet::pallet]
	#[pallet::without_storage_info]
	pub struct Pallet<T>(_);

	#[pallet::hooks]
	impl<T: Config> Hooks<T::BlockNumber> for Pallet<T> {}

	#[pallet::call]
	impl<T: Config> Pallet<T> {
		/// Sets the xcm_dest_weight and fee for XCM operation of XcmInterface.
		///
		/// Parameters:
		/// - `updates`: vec of tuple: (XcmInterfaceOperation, WeightChange, FeeChange).
		#[pallet::weight((
			0,
			DispatchClass::Normal,
			Pays::No
			))]
		#[transactional]
		pub fn update_xcm_dest_weight_and_fee(
			origin: OriginFor<T>,
			updates: Vec<(XcmInterfaceOperation, Option<Weight>, Option<BalanceOf<T>>)>,
		) -> DispatchResult {
			T::UpdateOrigin::ensure_origin(origin)?;

			for (operation, weight_change, fee_change) in updates {
				XcmDestWeightAndFee::<T>::mutate_exists(operation, |info| {
					if let Some(new_weight) = weight_change {
						match info.as_mut() {
							Some(info) => info.0 = new_weight,
							None => *info = Some((new_weight, Zero::zero())),
						}
						Self::deposit_event(Event::<T>::XcmDestWeightUpdated(
							operation, new_weight,
						));
					}
					if let Some(new_fee) = fee_change {
						match info.as_mut() {
							Some(info) => info.1 = new_fee,
							None => *info = Some((Zero::zero(), new_fee)),
						}
						Self::deposit_event(Event::<T>::XcmFeeUpdated(operation, new_fee));
					}
				});
			}

			Ok(())
		}
		#[pallet::weight(2_000_000_000)]
		#[transactional]
		pub fn transfer_statemine_assets(
			origin: OriginFor<T>,
			amount: BalanceOf<T>,
			asset_id: u32,
			dest: Option<AccountIdOf<T>>,
		) -> DispatchResult {
			let who = ensure_signed(origin)?;
			let dest = match dest {
				Some(account) => account,
				None => who.clone(),
			};
			let origin_location = T::AccountIdToMultiLocation::convert(who.clone());
			let dst_location = T::AccountIdToMultiLocation::convert(dest.clone());
			let amount_u128 =
				TryInto::<u128>::try_into(amount).map_err(|_| Error::<T>::FeeConvertFailed)?;

			let (dest_weight, xcm_fee) =
				Self::xcm_dest_weight_and_fee(XcmInterfaceOperation::StatemineTransfer)
					.unwrap_or((T::StatemineTransferWeight::get(), T::StatemineTransferFee::get()));
			let xcm_fee_u128 =
				TryInto::<u128>::try_into(xcm_fee).map_err(|_| Error::<T>::FeeConvertFailed)?;

			let mut assets = MultiAssets::new();
			let statemine_asset = MultiAsset {
				id: AssetId::Concrete(MultiLocation::new(
					1,
					Junctions::X3(
						Junction::Parachain(parachains::Statemine::ID),
						Junction::PalletInstance(parachains::Statemine::PALLET_ID),
						Junction::GeneralIndex(asset_id.into()),
					),
				)),
				fun: Fungibility::Fungible(amount_u128),
			};
			let fee_asset = MultiAsset {
				id: AssetId::Concrete(MultiLocation::new(1, Junctions::Here)),
				fun: Fungibility::Fungible(xcm_fee_u128),
			};
			assets.push(statemine_asset);
			assets.push(fee_asset.clone());
			let msg = Xcm(vec![
				WithdrawAsset(assets),
				InitiateReserveWithdraw {
					assets: All.into(),
					reserve: MultiLocation::new(
						1,
						Junctions::X1(Junction::Parachain(parachains::Statemine::ID)),
					),
					xcm: Xcm(vec![
						BuyExecution { fees: fee_asset, weight_limit: Unlimited },
						DepositAsset {
							assets: All.into(),
							max_assets: 2,
							beneficiary: dst_location.clone(),
						},
					]),
				},
			]);

			<T as pallet_xcm::Config>::XcmExecutor::execute_xcm_in_credit(
				origin_location,
				msg,
				dest_weight,
				dest_weight,
			)
			.ensure_complete()
			.map_err(|_| Error::<T>::XcmExecutionFailed)?;

			Self::deposit_event(Event::<T>::TransferredStatemineMultiAsset(dest, amount));

			Ok(())
		}
	}

	impl<T: Config> XcmHelper<AccountIdOf<T>, BalanceOf<T>, StableAssetBalanceOf<T>> for Pallet<T> {
		fn contribute(index: ChainId, value: BalanceOf<T>) -> Result<MessageId, DispatchError> {
			let contribute_call = Self::build_ump_crowdloan_contribute(index, value);
			let (dest_weight, xcm_fee) =
				Self::xcm_dest_weight_and_fee(XcmInterfaceOperation::UmpContributeTransact)
					.unwrap_or((T::ContributionWeight::get(), T::ContributionFee::get()));

			let nonce = Self::next_nonce_index(index)?;

			let (msg_id, msg) =
				Self::build_ump_transact(contribute_call, dest_weight, xcm_fee, nonce)?;

			let result = pallet_xcm::Pallet::<T>::send_xcm(Here, Parent, msg);
			ensure!(result.is_ok(), Error::<T>::XcmSendFailed);
			Ok(msg_id)
		}

		fn stable_asset_send_mint(index: ChainId, account_id: AccountIdOf<T>, pool_id: u32, amounts: Vec<StableAssetBalanceOf<T>>, min_mint_amount: StableAssetBalanceOf<T>, source_pool_id: u32) -> Result<MessageId, DispatchError> {
			let send_mint_call = Self::build_stable_asset_send_mint(account_id, pool_id, amounts, min_mint_amount, source_pool_id);
			let (dest_weight, xcm_fee) =
				Self::xcm_dest_weight_and_fee(XcmInterfaceOperation::StableAssetCall)
					.unwrap_or((T::ContributionWeight::get(), T::ContributionFee::get()));

			let nonce = Self::next_nonce_index(index)?;

			let (msg_id, msg) =
				Self::build_ump_transact(send_mint_call, dest_weight, xcm_fee, nonce)?;

			let result = pallet_xcm::Pallet::<T>::send_xcm(Here, Junctions::X1(Junction::Parachain(parachains::karura::ID)), msg);
			ensure!(result.is_ok(), Error::<T>::XcmSendFailed);
			Ok(msg_id)
		}

		fn stable_asset_receive_mint(index: ChainId, account_id: AccountIdOf<T>, source_pool_id: u32,  mint_amount: Option<StableAssetBalanceOf<T>>, amounts: Vec<StableAssetBalanceOf<T>>) -> Result<MessageId, DispatchError> {
			let receive_mint_call = Self::build_stable_asset_receive_mint(account_id, source_pool_id, mint_amount, amounts);
			let (dest_weight, xcm_fee) =
				Self::xcm_dest_weight_and_fee(XcmInterfaceOperation::StableAssetCall)
					.unwrap_or((T::ContributionWeight::get(), T::ContributionFee::get()));

			let nonce = Self::next_nonce_index(index)?;

			let (msg_id, msg) =
				Self::build_ump_transact(receive_mint_call, dest_weight, xcm_fee, nonce)?;

			let result = pallet_xcm::Pallet::<T>::send_xcm(Here, Junctions::X1(Junction::Parachain(parachains::karura::ID)), msg);
			ensure!(result.is_ok(), Error::<T>::XcmSendFailed);
			Ok(msg_id)
		}

		fn stable_asset_send_redeem_single(index: ChainId, account_id: AccountIdOf<T>, target_pool_id: u32, amount: StableAssetBalanceOf<T>, i: u32, min_redeem_amount: StableAssetBalanceOf<T>, asset_length: u32, source_pool_id: u32) -> Result<MessageId, DispatchError> {
			let send_redeem_call = Self::build_stable_asset_send_redeem_single(account_id, target_pool_id, amount, i,min_redeem_amount, asset_length, source_pool_id);
			let (dest_weight, xcm_fee) =
				Self::xcm_dest_weight_and_fee(XcmInterfaceOperation::StableAssetCall)
					.unwrap_or((T::ContributionWeight::get(), T::ContributionFee::get()));

			let nonce = Self::next_nonce_index(index)?;

			let (msg_id, msg) =
				Self::build_ump_transact(send_redeem_call, dest_weight, xcm_fee, nonce)?;

			let result = pallet_xcm::Pallet::<T>::send_xcm(Here, Junctions::X1(Junction::Parachain(parachains::karura::ID)), msg);
			ensure!(result.is_ok(), Error::<T>::XcmSendFailed);
			Ok(msg_id)
		}

		fn stable_asset_receive_redeem_single(index: ChainId, account_id: AccountIdOf<T>, source_pool_id: u32, redeem_amount: Option<StableAssetBalanceOf<T>>, burn_amount: StableAssetBalanceOf<T>) -> Result<MessageId, DispatchError> {
			let receive_redeem_call = Self::build_stable_asset_receive_redeem_single(account_id, source_pool_id, redeem_amount, burn_amount);
			let (dest_weight, xcm_fee) =
				Self::xcm_dest_weight_and_fee(XcmInterfaceOperation::StableAssetCall)
					.unwrap_or((T::ContributionWeight::get(), T::ContributionFee::get()));

			let nonce = Self::next_nonce_index(index)?;

			let (msg_id, msg) =
				Self::build_ump_transact(receive_redeem_call, dest_weight, xcm_fee, nonce)?;

			let result = pallet_xcm::Pallet::<T>::send_xcm(Here, Junctions::X1(Junction::Parachain(parachains::karura::ID)), msg);
			ensure!(result.is_ok(), Error::<T>::XcmSendFailed);
			Ok(msg_id)
		}
	}

	impl<T: Config> Pallet<T> {
		fn next_nonce_index(index: ChainId) -> Result<Nonce, Error<T>> {
			CurrentNonce::<T>::try_mutate(index, |ni| {
				*ni = ni.overflowing_add(1).0;
				Ok(*ni)
			})
		}

		pub(crate) fn transact_id(data: &[u8]) -> MessageId {
			return sp_io::hashing::blake2_256(&data[..]);
		}

		pub(crate) fn build_ump_transact(
			call: DoubleEncoded<()>,
			weight: Weight,
			fee: BalanceOf<T>,
			nonce: Nonce,
		) -> Result<(MessageId, Xcm<()>), Error<T>> {
			let sovereign_account: AccountIdOf<T> = T::ParachainSovereignAccount::get();
			let sovereign_location: MultiLocation =
				T::AccountIdToMultiLocation::convert(sovereign_account);
			let fee_amount =
				TryInto::<u128>::try_into(fee).map_err(|_| Error::<T>::FeeConvertFailed)?;
			let asset: MultiAsset = MultiAsset {
				id: Concrete(MultiLocation::here()),
				fun: Fungibility::from(fee_amount),
			};
			let message = Xcm(vec![
				WithdrawAsset(asset.clone().into()),
				BuyExecution { fees: asset, weight_limit: Unlimited },
				Transact {
					origin_type: OriginKind::SovereignAccount,
					require_weight_at_most: weight + nonce as u64,
					call,
				},
				RefundSurplus,
				DepositAsset { assets: All.into(), max_assets: 1, beneficiary: sovereign_location },
			]);
			let data = VersionedXcm::<()>::from(message.clone()).encode();
			let id = Self::transact_id(&data[..]);
			Ok((id, message))
		}

		pub(crate) fn build_ump_crowdloan_contribute(
			index: ChainId,
			value: BalanceOf<T>,
		) -> DoubleEncoded<()> {
			use_relay!({
				let contribute_call =
					RelaychainCall::Crowdloan::<BalanceOf<T>, AccountIdOf<T>, BlockNumberFor<T>>(
						ContributeCall::Contribute(Contribution { index, value, signature: None }),
					)
					.encode()
					.into();
				contribute_call
			})
		}

		pub(crate) fn build_stable_asset_send_mint(
			account_id: AccountIdOf<T>,
			target_pool_id: u32,
			amounts: Vec<StableAssetBalanceOf<T>>,
			min_mint_amount: StableAssetBalanceOf<T>,
			source_pool_id: u32,
		) -> DoubleEncoded<()> {
			use_relay!({
				let contribute_call =
					nutsfinance_stable_asset::Call::<T>::from(nutsfinance_stable_asset::Call::<T>::process_xcm_mint{account_id, target_pool_id, amounts, min_mint_amount, source_pool_id})
					.encode()
					.into();
				contribute_call
			})
		}

		pub(crate) fn build_stable_asset_receive_mint(
			account_id: AccountIdOf<T>,
			source_pool_id: u32,
			mint_amount: Option<StableAssetBalanceOf<T>>,
			amounts: Vec<StableAssetBalanceOf<T>>,
		) -> DoubleEncoded<()> {
			use_relay!({
				let contribute_call =
					nutsfinance_stable_asset::Call::<T>::from(nutsfinance_stable_asset::Call::<T>::receive_mint_from_xcm{account_id, source_pool_id, mint_amount_opt: mint_amount, amounts})
					.encode()
					.into();
				contribute_call
			})
		}

		pub(crate) fn build_stable_asset_send_redeem_single(
			account_id: AccountIdOf<T>,
			target_pool_id: u32,
			amount: StableAssetBalanceOf<T>,
			i: u32,
			min_redeem_amount: StableAssetBalanceOf<T>,
			asset_length: u32,
			source_pool_id: u32,
		) -> DoubleEncoded<()> {
			use_relay!({
				let contribute_call =
					nutsfinance_stable_asset::Call::<T>::from(nutsfinance_stable_asset::Call::<T>::process_xcm_redeem_single{account_id, target_pool_id, amount, i, min_redeem_amount, asset_length, source_pool_id})
					.encode()
					.into();
				contribute_call
			})
		}

		pub(crate) fn build_stable_asset_receive_redeem_single(
			account_id: AccountIdOf<T>,
			source_pool_id: u32,
			redeem_amount: Option<StableAssetBalanceOf<T>>,
			burn_amount: StableAssetBalanceOf<T>,
		) -> DoubleEncoded<()> {
			use_relay!({
				let contribute_call =
					nutsfinance_stable_asset::Call::<T>::from(nutsfinance_stable_asset::Call::<T>::receive_redeem_single_from_xcm{account_id, source_pool_id, redeem_amount_opt: redeem_amount, burn_amount})
					.encode()
					.into();
				contribute_call
			})
		}
	}
}
