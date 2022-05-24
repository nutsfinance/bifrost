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

#![cfg(test)]

//! Cross-chain transfer tests within Kusama network.

use bifrost_polkadot_runtime::PolkadotXcm;
use bifrost_slp::{
	primitives::{
		SubstrateLedgerUpdateEntry, SubstrateValidatorsByDelegatorUpdateEntry, UnlockChunk,
	},
	Delays, Ledger, LedgerUpdateEntry, MinimumsMaximums, SubstrateLedger,
	ValidatorsByDelegatorUpdateEntry, XcmOperation,
};
use cumulus_primitives_core::relay_chain::HashT;
use frame_support::{assert_ok, BoundedVec};
use node_primitives::TimeUnit;
use orml_traits::MultiCurrency;
use pallet_staking::{Nominations, StakingLedger};
use pallet_xcm::QueryStatus;
use sp_core::H160;
use xcm::{latest::prelude::*, VersionedMultiAssets, VersionedMultiLocation};
use xcm_emulator::TestExt;

use crate::{integration_tests::*, kusama_test_net::*};

/// ****************************************************
/// *********  Preparation section  ********************
/// ****************************************************
// parachain 2001, subaccount index 0 and index 1
fn para_h160_and_account_id_20_for_2001() -> (H160, [u8; 20], MultiLocation) {
	// 5Ec4AhPV91i9yNuiWuNunPf6AQCYDhFTTA4G5QCbtqYApH9E
	let para_account_2001: H160 =
		hex_literal::hex!["7369626cd1070000000000000000000000000000"].into();
	let account_id_20: [u8; 20] =
		hex_literal::hex!["7369626cd1070000000000000000000000000000"].into();
	let para_account_location = MultiLocation {
		parents: 1,
		interior: X2(Parachain(2023u32), AccountKey20 { network: Any, key: account_id_20 }),
	};

	(para_account_2001, account_id_20, para_account_location)
}

fn subaccount_0_h160_and_account_id_20() -> (H160, [u8; 20], MultiLocation) {
	// subaccountId0: 0x863c1faef3c3b8f8735ecb7f8ed18996356dd3de
	let subaccount_0: H160 = hex_literal::hex!["863c1faef3c3b8f8735ecb7f8ed18996356dd3de"].into();
	let account_id_20_0: [u8; 20] =
		hex_literal::hex!["863c1faef3c3b8f8735ecb7f8ed18996356dd3de"].into();
	let subaccount_0_location = MultiLocation {
		parents: 1,
		interior: X2(Parachain(2023u32), AccountKey20 { network: Any, key: account_id_20_0 }),
	};

	(subaccount_0, account_id_20_0, subaccount_0_location)
}

fn subaccount_1_h160_and_account_id_20() -> (H160, [u8; 20], MultiLocation) {
	// subaccountId1: 0x3afe20b0c85801b74e65586fe7070df827172574
	let subaccount_1: H160 = hex_literal::hex!["3afe20b0c85801b74e65586fe7070df827172574"].into();
	let account_id_20_1: [u8; 20] =
		hex_literal::hex!["3afe20b0c85801b74e65586fe7070df827172574"].into();
	let subaccount_1_location = MultiLocation {
		parents: 1,
		interior: X2(Parachain(2023u32), AccountKey20 { network: Any, key: account_id_20_1 }),
	};

	(subaccount_1, account_id_20_1, subaccount_1_location)
}

// // Preparation: register sub-account index 0.
// fn register_subaccount_index_0() {
// 	let subaccount_0 = subaccount_0_h160_and_account_id_20();

// 	Bifrost::execute_with(|| {
// 		let subaccount_0_20: [u8; 20] = subaccount_0.1;

// 		let subaccount_0_location: MultiLocation = MultiLocation {
// 			parents: 1,
// 			interior: X2(
// 				Parachain(2023u32),
// 				AccountKey20 { network: _network_id, key: subaccount_0_20 },
// 			),
// 		};

// 		// Initialize ongoing timeunit as 0.
// 		assert_ok!(Slp::update_ongoing_time_unit(
// 			Origin::root(),
// 			RelayCurrencyId::get(),
// 			TimeUnit::Era(0)
// 		));

// 		// Initialize currency delays.
// 		let delay = Delays { unlock_delay: TimeUnit::Era(10) };
// 		assert_ok!(Slp::set_currency_delays(Origin::root(), RelayCurrencyId::get(), Some(delay)));

// 		// First to setup index-multilocation relationship of subaccount_0
// 		assert_ok!(Slp::add_delegator(
// 			Origin::root(),
// 			RelayCurrencyId::get(),
// 			0u16,
// 			subaccount_0_location.clone(),
// 		));

// 		// Register Operation weight and fee
// 		assert_ok!(Slp::set_xcm_dest_weight_and_fee(
// 			Origin::root(),
// 			RelayCurrencyId::get(),
// 			XcmOperation::TransferTo,
// 			Some((20_000_000_000, 10_000_000_000)),
// 		));

// 		assert_ok!(Slp::set_xcm_dest_weight_and_fee(
// 			Origin::root(),
// 			RelayCurrencyId::get(),
// 			XcmOperation::Bond,
// 			Some((20_000_000_000, 10_000_000_000)),
// 		));

// 		assert_ok!(Slp::set_xcm_dest_weight_and_fee(
// 			Origin::root(),
// 			RelayCurrencyId::get(),
// 			XcmOperation::BondExtra,
// 			Some((20_000_000_000, 10_000_000_000)),
// 		));

// 		assert_ok!(Slp::set_xcm_dest_weight_and_fee(
// 			Origin::root(),
// 			RelayCurrencyId::get(),
// 			XcmOperation::Unbond,
// 			Some((20_000_000_000, 10_000_000_000)),
// 		));

// 		assert_ok!(Slp::set_xcm_dest_weight_and_fee(
// 			Origin::root(),
// 			RelayCurrencyId::get(),
// 			XcmOperation::Rebond,
// 			Some((20_000_000_000, 10_000_000_000)),
// 		));

// 		assert_ok!(Slp::set_xcm_dest_weight_and_fee(
// 			Origin::root(),
// 			RelayCurrencyId::get(),
// 			XcmOperation::Delegate,
// 			Some((20_000_000_000, 10_000_000_000)),
// 		));

// 		assert_ok!(Slp::set_xcm_dest_weight_and_fee(
// 			Origin::root(),
// 			RelayCurrencyId::get(),
// 			XcmOperation::Payout,
// 			Some((20_000_000_000, 10_000_000_000)),
// 		));

// 		assert_ok!(Slp::set_xcm_dest_weight_and_fee(
// 			Origin::root(),
// 			RelayCurrencyId::get(),
// 			XcmOperation::Liquidize,
// 			Some((20_000_000_000, 10_000_000_000)),
// 		));

// 		assert_ok!(Slp::set_xcm_dest_weight_and_fee(
// 			Origin::root(),
// 			RelayCurrencyId::get(),
// 			XcmOperation::Chill,
// 			Some((20_000_000_000, 10_000_000_000)),
// 		));

// 		assert_ok!(Slp::set_xcm_dest_weight_and_fee(
// 			Origin::root(),
// 			RelayCurrencyId::get(),
// 			XcmOperation::TransferBack,
// 			Some((20_000_000_000, 10_000_000_000)),
// 		));

// 		let mins_and_maxs = MinimumsMaximums {
// 			delegator_bonded_minimum: 100_000_000_000,
// 			bond_extra_minimum: 0,
// 			unbond_minimum: 0,
// 			rebond_minimum: 0,
// 			unbond_record_maximum: 32,
// 			validators_back_maximum: 36,
// 			delegator_active_staking_maximum: 200_000_000_000_000,
// 			validators_reward_maximum: 0,
// 		};

// 		// Set minimums and maximums
// 		assert_ok!(Slp::set_minimums_and_maximums(
// 			Origin::root(),
// 			RelayCurrencyId::get(),
// 			Some(mins_and_maxs)
// 		));
// 	});
// }
