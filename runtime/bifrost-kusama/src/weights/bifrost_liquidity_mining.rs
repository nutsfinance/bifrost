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

//! Autogenerated weights for `bifrost_liquidity_mining`
//!
//! THIS FILE WAS AUTO-GENERATED USING THE SUBSTRATE BENCHMARK CLI VERSION 4.0.0-dev
//! DATE: 2021-10-28, STEPS: `50`, REPEAT: 20, LOW RANGE: `[]`, HIGH RANGE: `[]`
//! EXECUTION: Some(Wasm), WASM-EXECUTION: Compiled, CHAIN: Some("bifrost-local"), DB CACHE: 128

// Executed Command:
// target/release/bifrost
// benchmark
// --chain=bifrost-local
// --steps=50
// --repeat=20
// --pallet=bifrost_liquidity_mining
// --extrinsic=*
// --execution=wasm
// --wasm-execution=compiled
// --heap-pages=4096
// --header=./HEADER-GPL3
// --output=./runtime/bifrost/src/weights/bifrost_liquidity_mining.rs


#![cfg_attr(rustfmt, rustfmt_skip)]
#![allow(unused_parens)]
#![allow(unused_imports)]

use frame_support::{traits::Get, weights::Weight};
use sp_std::marker::PhantomData;

/// Weight functions for `bifrost_liquidity_mining`.
pub struct WeightInfo<T>(PhantomData<T>);
impl<T: frame_system::Config> bifrost_liquidity_mining::WeightInfo for WeightInfo<T> {
	// Storage: LiquidityMining ChargedPoolIds (r:1 w:1)
	// Storage: LiquidityMining TotalPoolInfos (r:1 w:1)
	// Storage: Tokens Accounts (r:2 w:2)
	// Storage: System Account (r:1 w:1)
	// Storage: System Number (r:1 w:0)
	// Storage: System ExecutionPhase (r:1 w:0)
	// Storage: System EventCount (r:1 w:1)
	// Storage: System Events (r:1 w:1)
	fn charge() -> Weight {
		(269_857_000 as Weight)
			.saturating_add(T::DbWeight::get().reads(9 as Weight))
			.saturating_add(T::DbWeight::get().writes(7 as Weight))
	}
	// Storage: LiquidityMining TotalPoolInfos (r:1 w:1)
	// Storage: LiquidityMining TotalDepositData (r:1 w:1)
	// Storage: Tokens Accounts (r:4 w:4)
	// Storage: System Account (r:1 w:1)
	// Storage: System Number (r:1 w:0)
	// Storage: System ExecutionPhase (r:1 w:0)
	// Storage: System EventCount (r:1 w:1)
	// Storage: System Events (r:1 w:1)
	fn deposit() -> Weight {
		(290_036_000 as Weight)
			.saturating_add(T::DbWeight::get().reads(11 as Weight))
			.saturating_add(T::DbWeight::get().writes(9 as Weight))
	}
	// Storage: LiquidityMining TotalPoolInfos (r:1 w:1)
	// Storage: System Number (r:1 w:0)
	// Storage: LiquidityMining TotalDepositData (r:1 w:1)
	// Storage: Tokens Accounts (r:7 w:6)
	// Storage: System Account (r:2 w:1)
	// Storage: System ExecutionPhase (r:1 w:0)
	// Storage: System EventCount (r:1 w:1)
	// Storage: System Events (r:1 w:1)
	fn redeem() -> Weight {
		(514_172_000 as Weight)
			.saturating_add(T::DbWeight::get().reads(15 as Weight))
			.saturating_add(T::DbWeight::get().writes(11 as Weight))
	}
	// Storage: LiquidityMining TotalPoolInfos (r:1 w:1)
	// Storage: System Number (r:1 w:0)
	// Storage: LiquidityMining TotalDepositData (r:1 w:1)
	// Storage: Tokens Accounts (r:7 w:6)
	// Storage: System Account (r:2 w:1)
	// Storage: System ExecutionPhase (r:1 w:0)
	// Storage: System EventCount (r:1 w:1)
	// Storage: System Events (r:1 w:1)
	fn redeem_all() -> Weight {
		(467_531_000 as Weight)
			.saturating_add(T::DbWeight::get().reads(15 as Weight))
			.saturating_add(T::DbWeight::get().writes(11 as Weight))
	}
	// Storage: LiquidityMining TotalPoolInfos (r:1 w:1)
	// Storage: System Number (r:1 w:0)
	// Storage: LiquidityMining TotalDepositData (r:2 w:1)
	// Storage: Tokens Accounts (r:7 w:6)
	// Storage: System Account (r:2 w:1)
	// Storage: System ExecutionPhase (r:1 w:0)
	// Storage: System EventCount (r:1 w:1)
	// Storage: System Events (r:1 w:1)
	fn volunteer_to_redeem() -> Weight {
		(509_736_000 as Weight)
			.saturating_add(T::DbWeight::get().reads(16 as Weight))
			.saturating_add(T::DbWeight::get().writes(11 as Weight))
	}
	// Storage: LiquidityMining TotalPoolInfos (r:1 w:1)
	// Storage: System Number (r:1 w:0)
	// Storage: LiquidityMining TotalDepositData (r:1 w:1)
	// Storage: Tokens Accounts (r:2 w:2)
	// Storage: System Account (r:1 w:1)
	// Storage: System ExecutionPhase (r:1 w:0)
	// Storage: System EventCount (r:1 w:1)
	// Storage: System Events (r:1 w:1)
	fn claim() -> Weight {
		(260_399_000 as Weight)
			.saturating_add(T::DbWeight::get().reads(9 as Weight))
			.saturating_add(T::DbWeight::get().writes(7 as Weight))
	}

	// By Hand
	fn unlock() -> Weight {
		1_000
	}

	// By Hand
	fn cancel_unlock() -> Weight {
		1_000
	}
}
