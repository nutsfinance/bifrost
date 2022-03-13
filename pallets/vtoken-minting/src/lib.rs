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

// Ensure we're `no_std` when compiling for Wasm.
#![cfg_attr(not(feature = "std"), no_std)]

#[cfg(test)]
mod mock;

#[cfg(test)]
mod tests;

#[cfg(feature = "runtime-benchmarks")]
mod benchmarking;

pub mod weights;

use frame_support::{
	pallet_prelude::*,
	sp_runtime::{
		traits::{AccountIdConversion, CheckedAdd, CheckedSub, Saturating, Zero},
		SaturatedConversion,
	},
	transactional, BoundedVec, PalletId,
};
use frame_system::pallet_prelude::*;
use node_primitives::{Balance, CurrencyId};
use orml_traits::MultiCurrency;
pub use pallet::*;
use sp_std::vec::Vec;
pub use weights::WeightInfo;

#[allow(type_alias_bounds)]
pub type AccountIdOf<T> = <T as frame_system::Config>::AccountId;

#[allow(type_alias_bounds)]
pub type CurrencyIdOf<T> = <<T as Config>::MultiCurrency as MultiCurrency<
	<T as frame_system::Config>::AccountId,
>>::CurrencyId;

#[allow(type_alias_bounds)]
type BalanceOf<T: Config> =
	<<T as Config>::MultiCurrency as MultiCurrency<AccountIdOf<T>>>::Balance;

pub type MintId = u32;

#[derive(Encode, Decode, Clone, RuntimeDebug, PartialEq, Eq, TypeInfo, MaxEncodedLen)]
pub enum TimeUnit {
	Era(u32),
	// SlashingSpan(u32),
}

impl Default for TimeUnit {
	fn default() -> Self {
		TimeUnit::Era(1u32)
	}
}
#[frame_support::pallet]
pub mod pallet {
	// use sp_runtime::traits::CheckedAdd;
	// use orml_traits::arithmetic::CheckedSub;
	use frame_support::traits::tokens::currency;

	use super::*;

	#[pallet::pallet]
	#[pallet::generate_store(pub(super) trait Store)]
	pub struct Pallet<T>(_);

	#[pallet::config]
	pub trait Config: frame_system::Config {
		type Event: From<Event<Self>> + IsType<<Self as frame_system::Config>::Event>;

		type MultiCurrency: MultiCurrency<AccountIdOf<Self>, CurrencyId = CurrencyId>;
		// + MultiReservableCurrency<AccountIdOf<Self>, CurrencyId = CurrencyId>;

		/// The only origin that can edit token issuer list
		type ControlOrigin: EnsureOrigin<Self::Origin>;

		/// The amount of mint
		#[pallet::constant]
		type MaximumMintId: Get<u32>;

		#[pallet::constant]
		type EntranceAccount: Get<PalletId>;

		#[pallet::constant]
		type ExitAccount: Get<PalletId>;

		#[pallet::constant]
		type FeeAccount: Get<Self::AccountId>;

		/// Set default weight.
		type WeightInfo: WeightInfo;
	}

	#[pallet::event]
	#[pallet::generate_deposit(pub(super) fn deposit_event)]
	pub enum Event<T: Config> {
		Minted {
			token: CurrencyIdOf<T>,
			token_amount: BalanceOf<T>,
		},
		Redeemed {
			token: CurrencyIdOf<T>,
			vtoken_amount: BalanceOf<T>,
		},
		Rebonded {
			token: CurrencyIdOf<T>,
			token_amount: BalanceOf<T>,
		},
		UnlockDurationSet {
			token: CurrencyIdOf<T>,
			era_count: u32,
		},
		MinimumMintSet {
			token: CurrencyIdOf<T>,
			amount: BalanceOf<T>,
		},
		MinimumRedeemSet {
			token: CurrencyIdOf<T>,
			amount: BalanceOf<T>,
		},
		SupportRebondTokenAdded {
			token: CurrencyIdOf<T>,
		},
		SupportRebondTokenRemoved {
			token: CurrencyIdOf<T>,
		},
		/// Several fees has been set.
		FeeSet {
			mint_fee: BalanceOf<T>,
			redeem_fee: BalanceOf<T>,
			// hosting_fee: BalanceOf<T>,
		},
	}

	#[pallet::error]
	pub enum Error<T> {
		/// Number of user unlocking chunks exceed MaxUserUnlockingChunks
		TooManyUserUnlockingChunks,
		/// Number of era unlocking chunks exceed MaxEraUnlockingChunks
		TooManyEraUnlockingChunks,
		/// Invalid token to rebond.
		InvalidRebondToken,
		/// Invalid token.
		InvalidToken,
		/// Token type not support.
		NotSupportTokenType,
		NotEnoughBalanceToUnlock,
		TokenToRebondNotZero,
		MaxUserUnlockingChunksNotSet,
		MaxEraUnlockingChunksNotSet,
		OngoingTimeUnitNotSet,
		UserUnlockLedgerNotFound,
		Unexpected,
	}

	#[pallet::storage]
	#[pallet::getter(fn fees)]
	pub type Fees<T: Config> = StorageValue<_, (BalanceOf<T>, BalanceOf<T>), ValueQuery>;

	#[pallet::storage]
	#[pallet::getter(fn token_pool)]
	pub type TokenPool<T: Config> =
		StorageMap<_, Twox64Concat, CurrencyIdOf<T>, BalanceOf<T>, ValueQuery>;

	#[pallet::storage]
	#[pallet::getter(fn unlock_duration)]
	pub type UnlockDuration<T: Config> =
		StorageMap<_, Twox64Concat, CurrencyIdOf<T>, u32, ValueQuery>; // time_unit

	#[pallet::storage]
	#[pallet::getter(fn ongoing_time_unit)]
	pub type OngoingTimeUnit<T: Config> = StorageMap<_, Twox64Concat, CurrencyIdOf<T>, TimeUnit>;

	#[pallet::storage]
	#[pallet::getter(fn minimum_mint)]
	pub type MinimumMint<T: Config> =
		StorageMap<_, Twox64Concat, CurrencyIdOf<T>, BalanceOf<T>, ValueQuery>;

	#[pallet::storage]
	#[pallet::getter(fn minimum_redeem)]
	pub type MinimumRedeem<T: Config> =
		StorageMap<_, Twox64Concat, CurrencyIdOf<T>, BalanceOf<T>, ValueQuery>;

	#[pallet::storage]
	#[pallet::getter(fn token_unlock_next_id)]
	pub type TokenUnlockNextId<T: Config> =
		StorageMap<_, Twox64Concat, CurrencyIdOf<T>, u32, ValueQuery>;

	#[pallet::storage]
	#[pallet::getter(fn token_unlock_ledger)]
	pub(crate) type TokenUnlockLedger<T: Config> = StorageDoubleMap<
		_,
		Blake2_128Concat,
		CurrencyIdOf<T>,
		Blake2_128Concat,
		MintId,
		(T::AccountId, BalanceOf<T>, TimeUnit),
		OptionQuery,
	>;

	#[pallet::storage]
	#[pallet::getter(fn user_unlock_ledger)]
	pub(crate) type UserUnlockLedger<T: Config> = StorageDoubleMap<
		_,
		Blake2_128Concat,
		AccountIdOf<T>,
		Blake2_128Concat,
		CurrencyIdOf<T>,
		(BalanceOf<T>, BoundedVec<MintId, T::MaximumMintId>),
		OptionQuery,
	>;

	#[pallet::storage]
	#[pallet::getter(fn era_unlock_ledger)]
	pub(crate) type EraUnlockLedger<T: Config> = StorageDoubleMap<
		_,
		Blake2_128Concat,
		TimeUnit,
		Blake2_128Concat,
		CurrencyIdOf<T>,
		(BalanceOf<T>, BoundedVec<MintId, T::MaximumMintId>, CurrencyIdOf<T>),
		OptionQuery,
	>;

	// #[pallet::storage]
	// #[pallet::getter(fn max_user_unlocking_chunks)]
	// pub type MaxUserUnlockingChunks<T: Config> = StorageMap<_, Twox64Concat, CurrencyIdOf<T>,
	// u32>;

	// #[pallet::storage]
	// #[pallet::getter(fn max_era_unlocking_chuncks)]
	// pub type MaxEraUnlockingChunks<T: Config> = StorageMap<_, Twox64Concat, CurrencyIdOf<T>,
	// u32>;

	#[pallet::storage]
	#[pallet::getter(fn token_to_deduct)]
	pub type TokenToDeduct<T: Config> =
		StorageMap<_, Twox64Concat, CurrencyIdOf<T>, BalanceOf<T>, ValueQuery>;

	#[pallet::storage]
	#[pallet::getter(fn token_to_add)]
	pub type TokenToAdd<T: Config> =
		StorageMap<_, Twox64Concat, CurrencyIdOf<T>, BalanceOf<T>, ValueQuery>;

	#[pallet::storage]
	#[pallet::getter(fn token_to_rebond)]
	pub type TokenToRebond<T: Config> = StorageMap<_, Twox64Concat, CurrencyIdOf<T>, BalanceOf<T>>;

	#[pallet::storage]
	#[pallet::getter(fn min_time_unit)]
	pub type MinTimeUnit<T: Config> = StorageValue<_, TimeUnit, ValueQuery>; // ValueQuery

	#[pallet::hooks]
	impl<T: Config> Hooks<T::BlockNumber> for Pallet<T> {
		fn on_initialize(n: T::BlockNumber) -> Weight {
			// let mut entrance_account_balance =
			// 	T::MultiCurrency::free_balance(token_id, T::EntranceAccount::get().into_account());

			let time_unit = MinTimeUnit::<T>::get();
			EraUnlockLedger::<T>::iter_prefix_values(time_unit).for_each(
				|(total_locked, ledger_list, token_id)| {
					let mut entrance_account_balance = T::MultiCurrency::free_balance(
						token_id,
						&T::EntranceAccount::get().into_account(),
					);
					for index in ledger_list.iter() {
						if let Some((account, unlock_amount, time_unit)) =
							Self::token_unlock_ledger(token_id, index)
						{
							if entrance_account_balance >= unlock_amount {
								entrance_account_balance.saturating_sub(unlock_amount);
							};
							T::MultiCurrency::transfer(
								token_id,
								&T::EntranceAccount::get().into_account(),
								&account,
								unlock_amount,
							);

							EraUnlockLedger::<T>::mutate(&time_unit, &token_id, |value| {
								if let Some((total_locked, ledger_list, token_id)) = value {
									total_locked.saturating_sub(unlock_amount);
									ledger_list.retain(|x| x != index);
								}
							});

							TokenUnlockLedger::<T>::remove(&token_id, &index);

							UserUnlockLedger::<T>::mutate(&account, &token_id, |value| {
								if let Some((total_locked, ledger_list)) = value {
									total_locked.saturating_sub(unlock_amount);
									ledger_list.retain(|x| x != index);
								}
							});
						}
					}
				},
			); // .collect::<Vec<_>>();
			T::WeightInfo::on_initialize()
		}
	}

	#[pallet::call]
	impl<T: Config> Pallet<T> {
		// #[pallet::weight(T::WeightInfo::mint())]
		#[transactional]
		#[pallet::weight(10000)]
		pub fn mint(
			origin: OriginFor<T>,
			token_id: CurrencyIdOf<T>,
			token_amount: BalanceOf<T>,
		) -> DispatchResult {
			// Check origin
			let exchanger = ensure_signed(origin)?;
			let token_pool_amount = Self::token_pool(token_id);
			let vtoken_id = token_id.to_vtoken().map_err(|_| Error::<T>::NotSupportTokenType)?;
			let vtoken_total_issuance = T::MultiCurrency::total_issuance(vtoken_id);
			let vtoken_amount = token_amount.saturating_mul(vtoken_total_issuance.into()) /
				token_pool_amount.into();
			// Transfer the user's token to EntranceAccount.
			T::MultiCurrency::transfer(
				token_id,
				&exchanger,
				&T::EntranceAccount::get().into_account(),
				token_amount,
			)?;
			// Issue the corresponding vtoken to the user's account.
			T::MultiCurrency::deposit(vtoken_id, &exchanger, vtoken_amount)?;
			TokenPool::<T>::mutate(token_id, |pool| pool.saturating_add(token_pool_amount));
			TokenToAdd::<T>::mutate(token_id, |pool| pool.saturating_add(token_pool_amount));

			Self::deposit_event(Event::Minted { token: token_id, token_amount });
			Ok(())
		}

		#[transactional]
		#[pallet::weight(10000)]
		pub fn redeem(
			origin: OriginFor<T>,
			token_id: CurrencyIdOf<T>,
			vtoken_amount: BalanceOf<T>,
		) -> DispatchResult {
			let exchanger = ensure_signed(origin)?;
			let token_pool_amount = Self::token_pool(token_id);
			let vtoken_id = token_id.to_vtoken().map_err(|_| Error::<T>::NotSupportTokenType)?;
			let vtoken_total_issuance = T::MultiCurrency::total_issuance(vtoken_id);

			// if let Some((user_unlock_amount, ledger_list)) =
			// 	Self::user_unlock_ledger(&exchanger, token_id)
			// {
			// 	if let Some(user_chunks) = MaxUserUnlockingChunks::<T>::get(token_id) {
			// 		ensure!(
			// 			ledger_list.len() as u32 <= user_chunks,
			// 			Error::<T>::TooManyUserUnlockingChunks
			// 		);
			// 	} else {
			// 		return Err(Error::<T>::MaxUserUnlockingChunksNotSet.into());
			// 	}
			// }

			if let Some(era) = OngoingTimeUnit::<T>::get(token_id) {
				// if let Some((era_unlock_amount, ledger_list)) =
				// 	Self::era_unlock_ledger(&era, token_id)
				// {
				// 	if let Some(era_chunks) = MaxEraUnlockingChunks::<T>::get(token_id) {
				// 		ensure!(
				// 			ledger_list.len() as u32 <= era_chunks,
				// 			Error::<T>::TooManyUserUnlockingChunks
				// 		);
				// 	} else {
				// 		return Err(Error::<T>::MaxEraUnlockingChunksNotSet.into());
				// 	}
				// }
				T::MultiCurrency::withdraw(vtoken_id, &exchanger, vtoken_amount)?;
				TokenPool::<T>::mutate(token_id, |pool| pool.saturating_sub(token_pool_amount));
				TokenToDeduct::<T>::mutate(token_id, |pool| pool.saturating_add(token_pool_amount));

				let next_id = Self::token_unlock_next_id(token_id);
				TokenUnlockLedger::<T>::insert(
					&token_id,
					&next_id,
					(&exchanger, vtoken_amount, &era),
				);

				UserUnlockLedger::<T>::mutate(&exchanger, &token_id, |value| {
					if let Some((total_locked, ledger_list)) = value {
						total_locked.saturating_add(vtoken_amount);
						ledger_list.try_push(next_id);
					}
				});

				EraUnlockLedger::<T>::mutate(&era, &token_id, |value| {
					if let Some((total_locked, ledger_list, token_id)) = value {
						total_locked.saturating_add(vtoken_amount);
						ledger_list.try_push(next_id);
					}
				});
			} else {
				return Err(Error::<T>::OngoingTimeUnitNotSet.into());
			}

			Self::deposit_event(Event::Redeemed { token: token_id, vtoken_amount });
			Ok(())
		}

		#[transactional]
		#[pallet::weight(10000)]
		pub fn rebond(
			origin: OriginFor<T>,
			token_id: CurrencyIdOf<T>,
			token_amount: BalanceOf<T>,
		) -> DispatchResult {
			let exchanger = ensure_signed(origin)?;

			let vtoken_id = token_id.to_vtoken().map_err(|_| Error::<T>::NotSupportTokenType)?;
			let token_amount_to_rebond =
				Self::token_to_rebond(token_id).ok_or(Error::<T>::InvalidRebondToken)?;
			if let Some((user_unlock_amount, mut ledger_list)) =
				Self::user_unlock_ledger(&exchanger, token_id)
			{
				ensure!(user_unlock_amount >= token_amount, Error::<T>::NotEnoughBalanceToUnlock);
				let mut tmp_amount = token_amount;
				ledger_list.retain(|index| {
					// todo: order
					if let Some((account, unlock_amount, time_unit)) =
						Self::token_unlock_ledger(token_id, index)
					{
						if tmp_amount >= unlock_amount {
							tmp_amount.saturating_sub(unlock_amount);
							if let Some((_, total_locked, time_unit)) =
								TokenUnlockLedger::<T>::take(&token_id, &index)
							{
								EraUnlockLedger::<T>::mutate(&time_unit, &token_id, |value| {
									if let Some((total_locked, ledger_list, token_id)) = value {
										total_locked.saturating_sub(unlock_amount); // checked_add checked_sub
										ledger_list.retain(|x| x != index);
									}
								});
							}
							// *tmp_amount = tmp_amount.checked_sub()

							return false;
						} else {
							unlock_amount.saturating_sub(tmp_amount);
							TokenUnlockLedger::<T>::mutate(&token_id, &index, |value| {
								if let Some((_, total_locked, _)) = value {
									total_locked.saturating_sub(tmp_amount);
								}
							});
							// EraUnlockLedger
							return true;
						}
					} else {
						return true;
					}
				});

				UserUnlockLedger::<T>::mutate(&exchanger, &token_id, |value| {
					if let Some((total_locked_old, ledger_list_old)) = value {
						total_locked_old.saturating_sub(token_amount);
						*ledger_list_old = ledger_list;
					}
				});
			} else {
				return Err(Error::<T>::UserUnlockLedgerNotFound.into());
			}

			let token_pool_amount = Self::token_pool(token_id);
			let vtoken_total_issuance = T::MultiCurrency::total_issuance(vtoken_id);
			let vtoken_amount = token_amount.saturating_mul(vtoken_total_issuance.into()) /
				token_pool_amount.into();
			// Issue the corresponding vtoken to the user's account.
			T::MultiCurrency::deposit(vtoken_id, &exchanger, vtoken_amount)?;
			TokenPool::<T>::mutate(token_id, |pool| pool.saturating_add(token_pool_amount));
			TokenToAdd::<T>::mutate(token_id, |pool| pool.saturating_add(token_pool_amount));

			Self::deposit_event(Event::Minted { token: token_id, token_amount });

			TokenToRebond::<T>::mutate(&token_id, |value| {
				if let Some(value_info) = value {
					*value_info = value_info.saturating_add(token_amount);
				}
			});
			TokenPool::<T>::mutate(token_id, |pool| pool.saturating_add(token_amount));
			Self::deposit_event(Event::Rebonded { token: token_id, token_amount });

			Ok(())
		}

		#[pallet::weight(0)]
		pub fn set_unlock_duration(
			origin: OriginFor<T>,
			token: CurrencyIdOf<T>,
			era_count: u32,
		) -> DispatchResult {
			ensure_root(origin)?;

			if !UnlockDuration::<T>::contains_key(token) {
				UnlockDuration::<T>::insert(token, era_count);
			} else {
				UnlockDuration::<T>::mutate(token, |old_era_count| {
					*old_era_count = era_count;
				});
			}

			Self::deposit_event(Event::UnlockDurationSet { token, era_count });

			Ok(())
		}

		#[pallet::weight(0)]
		pub fn set_minimum_mint(
			origin: OriginFor<T>,
			token: CurrencyIdOf<T>,
			amount: BalanceOf<T>,
		) -> DispatchResult {
			ensure_root(origin)?;

			if !MinimumMint::<T>::contains_key(token) {
				// mutate_exists
				MinimumMint::<T>::insert(token, amount);
			} else {
				MinimumMint::<T>::mutate(token, |old_amount| {
					*old_amount = amount;
				});
			}

			Self::deposit_event(Event::MinimumMintSet { token, amount });

			Ok(())
		}

		#[pallet::weight(0)]
		pub fn set_minimum_redeem(
			origin: OriginFor<T>,
			token: CurrencyIdOf<T>,
			amount: BalanceOf<T>,
		) -> DispatchResult {
			ensure_root(origin)?;

			if !MinimumRedeem::<T>::contains_key(token) {
				MinimumRedeem::<T>::insert(token, amount);
			} else {
				MinimumRedeem::<T>::mutate(token, |old_amount| {
					*old_amount = amount;
				});
			}

			Self::deposit_event(Event::MinimumRedeemSet { token, amount });
			Ok(())
		}

		#[pallet::weight(0)]
		pub fn add_support_rebond_token(
			origin: OriginFor<T>,
			token: CurrencyIdOf<T>,
		) -> DispatchResult {
			ensure_root(origin)?;

			if !TokenToRebond::<T>::contains_key(token) {
				TokenToRebond::<T>::insert(token, BalanceOf::<T>::zero());
				Self::deposit_event(Event::SupportRebondTokenAdded { token });
			}

			Ok(())
		}

		#[pallet::weight(0)]
		pub fn remove_support_rebond_token(
			origin: OriginFor<T>,
			token: CurrencyIdOf<T>,
		) -> DispatchResult {
			ensure_root(origin)?;

			if TokenToRebond::<T>::contains_key(token) {
				let token_amount_to_rebond =
					Self::token_to_rebond(token).ok_or(Error::<T>::InvalidRebondToken)?;
				ensure!(
					token_amount_to_rebond == BalanceOf::<T>::zero(),
					Error::<T>::TokenToRebondNotZero
				);

				TokenToRebond::<T>::remove(token);
				Self::deposit_event(Event::SupportRebondTokenRemoved { token });
			}
			Ok(())
		}

		#[pallet::weight(0)]
		pub fn set_fees(
			origin: OriginFor<T>,
			mint_fee: BalanceOf<T>,
			redeem_fee: BalanceOf<T>,
			// hosting_fee: BalanceOf<T>,
		) -> DispatchResult {
			ensure_root(origin)?;

			Fees::<T>::mutate(|fees| *fees = (mint_fee, redeem_fee));

			Self::deposit_event(Event::FeeSet { mint_fee, redeem_fee });
			Ok(())
		}
	}
}

/// The interface to call VtokneMinting module functions.
pub trait VtokenMintingOperator<CurrencyId, Balance, AccountId, TimeUnit> {
	/// Increase the token amount for the storage "token_pool" in the VtokenMining module.
	fn increase_token_pool(currency_id: CurrencyId, token_amount: Balance) -> DispatchResult;

	/// Decrease the token amount for the storage "token_pool" in the VtokenMining module.
	fn decrease_token_pool(currency_id: CurrencyId, token_amount: Balance) -> DispatchResult;

	/// Update the ongoing era for a CurrencyId.
	fn update_ongoing_time_unit(currency_id: CurrencyId, time_unit: TimeUnit) -> DispatchResult;

	/// Get the current era of a CurrencyId.
	fn get_ongoing_time_unit(currency_id: CurrencyId) -> Option<TimeUnit>;

	/// Get the the unlocking records of a certain time unit.
	fn get_unlock_records(
		currency_id: CurrencyId,
		time_unit: TimeUnit,
	) -> Option<(Balance, Vec<u32>)>;

	/// Revise the currency indexed unlocking record by some amount.
	fn deduct_unlock_amount(
		currency_id: CurrencyId,
		index: u32,
		deduct_amount: Balance,
	) -> DispatchResult;

	/// Get currency Entrance and Exit accounts.【entrance_account, exit_account】
	fn get_entrance_and_exit_accounts() -> (AccountId, AccountId);

	/// Get the token_unlock_ledger storage info to refund to the due era unlocking users.
	fn get_token_unlock_ledger(
		currency_id: CurrencyId,
		index: u32,
	) -> Option<(AccountId, Balance, TimeUnit)>;
}

impl<T: Config> VtokenMintingOperator<CurrencyId, BalanceOf<T>, AccountIdOf<T>, TimeUnit>
	for Pallet<T>
{
	fn increase_token_pool(currency_id: CurrencyId, token_amount: BalanceOf<T>) -> DispatchResult {
		TokenPool::<T>::mutate(currency_id, |pool| -> Result<(), Error<T>> {
			*pool = pool.saturating_add(token_amount);
			Ok(())
		})?;

		Ok(())
	}

	fn decrease_token_pool(currency_id: CurrencyId, token_amount: BalanceOf<T>) -> DispatchResult {
		TokenPool::<T>::mutate(currency_id, |pool| -> Result<(), Error<T>> {
			*pool = pool.saturating_sub(token_amount);
			Ok(())
		})?;

		Ok(())
	}

	fn update_ongoing_time_unit(currency_id: CurrencyId, time_unit: TimeUnit) -> DispatchResult {
		OngoingTimeUnit::<T>::mutate(currency_id, |time_unit_old| -> Result<(), Error<T>> {
			*time_unit_old = Some(time_unit);
			Ok(())
		})?;

		Ok(())
	}

	fn get_ongoing_time_unit(currency_id: CurrencyId) -> Option<TimeUnit> {
		Self::ongoing_time_unit(currency_id)
	}

	fn get_unlock_records(
		currency_id: CurrencyId,
		time_unit: TimeUnit,
	) -> Option<(BalanceOf<T>, Vec<u32>)> {
		if let Some((balance, list, _)) = Self::era_unlock_ledger(&time_unit, currency_id) {
			Some((balance, list.into_inner()))
		} else {
			None
		}
	}

	fn deduct_unlock_amount(
		currency_id: CurrencyId,
		index: u32,
		deduct_amount: BalanceOf<T>,
	) -> DispatchResult {
		if let Some((who, unlock_amount, time_unit)) = Self::token_unlock_ledger(currency_id, index)
		{
			ensure!(unlock_amount <= deduct_amount, Error::<T>::NotEnoughBalanceToUnlock);

			TokenPool::<T>::mutate(currency_id, |pool| pool.saturating_add(deduct_amount));

			EraUnlockLedger::<T>::mutate(&time_unit, &currency_id, |value| {
				if let Some((total_locked, ledger_list, token_id)) = value {
					total_locked.saturating_sub(unlock_amount);
					ledger_list.retain(|&x| x != index);
				}
			});

			UserUnlockLedger::<T>::mutate(&who, &currency_id, |value| {
				if let Some((total_locked, ledger_list)) = value {
					total_locked.saturating_sub(deduct_amount);
					ledger_list.retain(|&x| x != index);
				}
			});

			TokenUnlockLedger::<T>::remove(&currency_id, &index);
		}
		Ok(())
	}

	fn get_entrance_and_exit_accounts() -> (AccountIdOf<T>, AccountIdOf<T>) {
		(T::EntranceAccount::get().into_account(), T::ExitAccount::get().into_account())
	}

	fn get_token_unlock_ledger(
		currency_id: CurrencyId,
		index: u32,
	) -> Option<(AccountIdOf<T>, BalanceOf<T>, TimeUnit)> {
		Self::token_unlock_ledger(currency_id, index)
	}
}
