#  Copyright (c) 2025 Merck & Co., Inc., Rahway, NJ, USA and its affiliates.
#  All rights reserved.
#
#  This file is part of the dynacem program.
#
#  dynacem is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
#  =====================================================================

## Code to prepare `oncpsm` dataset

## Create demonstration cost-effectiveness models on which to apply dynamic calculations
## One example is provided, built using the heemod package
## =====================================================================================

# Use heemod
rlang::check_installed("heemod")
library(heemod)

# Time constants
days_in_year <- 365.25
days_in_week <- 7
cycle_years <- days_in_week / days_in_year # Duration of a one week cycle in years

# Time horizon (years) and number of cycles
thoz <- 20
Ncycles <- ceiling(thoz/cycle_years)

# Real discounting
disc_year <- 0.03 # Per year
disc_cycle <- (1+disc_year)^(cycle_years) - 1 # Per cycle

# Price inflation
infl_year <- 0.025 # Per year
infl_cycle <- (1+infl_year)^(cycle_years) - 1 # Per cycle

# Nominal discounting
nomdisc_year <- (1+disc_year)*(1+infl_year) - 1
nomdisc_cycle <- (1+nomdisc_year)^(cycle_years) - 1 # Per cycle

# State names
state_names = c(
  progression_free = "PF",
  progression = "PD",
  death = "Death"
  )

# PFS distribution for SoC with Exp() distribution and mean of 50 weeks
surv_pfs_soc <- heemod::define_surv_dist(
  distribution = "exp",
  rate = 1/50
)

# OS distribution for SoC with Lognorm() distribution, meanlog = 4.5, sdlog = 1
# This implies a mean of exp(4 + 0.5 * 1^2) = exp(4.5) = 90 weeks
surv_os_soc <- heemod::define_surv_dist(
  distribution = "lnorm",
  meanlog = 4,
  sdlog = 1
)

# PFS and OS distributions for new
surv_pfs_new <- heemod::apply_hr(surv_pfs_soc, hr=0.5)
surv_os_new <- heemod::apply_hr(surv_os_soc, hr=0.6)

# Define partitioned survival model, soc
psm_soc <- heemod::define_part_surv(
  pfs = surv_pfs_soc,
  os = surv_os_soc,
  terminal_state = FALSE,
  state_names = state_names
  )

# Define partitioned survival model, soc
psm_new <- heemod::define_part_surv(
  pfs = surv_pfs_new,
  os = surv_os_new,
  terminal_state = FALSE,
  state_names = state_names
  )

# Parameters
params <- heemod::define_parameters(
  # Discount rate
  disc = disc_cycle,
  # Disease management costs
  cman_pf = 80,
  cman_pd = 20,
  # Drug acquisition costs - the SoC regime only uses SoC drug, the New regime only uses New drug
  cdaq_soc = dispatch_strategy(
    soc = 400,
    new = 0
  ),
  cdaq_new = dispatch_strategy(
    soc = 0,
    new = 1500
  ),
  # Drug administration costs
  cadmin = dispatch_strategy(
    soc = 50,
    new = 75
  ),
  # Adverse event risks
  risk_ae = dispatch_strategy(
    soc = 0.08,
    new = 0.1
  ),
  # Adverse event average costs
  uc_ae = 2000,
  # Subsequent treatments
  csubs = dispatch_strategy(
    soc = 1200,
    new = 300
  ),
  # Health state utilities
  u_pf = 0.8,
  u_pd = 0.6,
)

# Define PF states
state_PF <- heemod::define_state(
  # Costs for the state
  cost_daq_soc = discount(cdaq_soc, disc_cycle),
  cost_daq_new = discount(cdaq_new, disc_cycle),
  cost_dadmin = discount(cadmin, disc_cycle),
  cost_dman = discount(cman_pf, disc_cycle),  
  cost_ae = risk_ae * uc_ae,
  cost_subs = 0,
  cost_total = cost_daq_soc + cost_daq_new + cost_dadmin + cost_dman + cost_ae + cost_subs,
  # Health utility, QALYs and life years
  pf_year = discount(cycle_years, disc_cycle),
  life_year = discount(cycle_years, disc_cycle),
  qaly = discount(cycle_years * u_pf, disc_cycle)
  )

# Define PD states
state_PD <- define_state(
  # Costs for the state
  cost_daq_soc = 0,
  cost_daq_new = 0,
  cost_dadmin = 0,
  cost_dman = discount(cman_pd, disc_cycle),  
  cost_ae = 0,
  cost_subs = discount(csubs, disc_cycle),
  cost_total = cost_daq_soc + cost_daq_new + cost_dadmin + cost_dman + cost_ae + cost_subs,
  # Health utility, QALYs and life years
  pf_year = 0,
  life_year = heemod::discount(cycle_years, disc_cycle),
  qaly = heemod::discount(cycle_years * u_pd, disc_cycle)
  )

# Define Death state
state_Death <- heemod::define_state(
  # Costs are zero
  cost_daq_soc = 0,
  cost_daq_new = 0,
  cost_dadmin = 0,
  cost_dman = 0,
  cost_ae = 0,
  cost_subs = 0,
  cost_total = cost_daq_soc + cost_daq_new + cost_dadmin + cost_dman + cost_ae + cost_subs,
  # Health outcomes are zero
  pf_year = 0,
  life_year = 0,
  qaly = 0,
)

# Define strategy for SoC
strat_soc <- heemod::define_strategy(
    transition = psm_soc,
    "PF" = state_PF,
    "PD" = state_PD,
    "Death" = state_Death
  )

# Define strategy for new
strat_new <- heemod::define_strategy(
  transition = psm_new,
  "PF" = state_PF,
  "PD" = state_PD,
  "Death" = state_Death
)

# Create heemod model
oncpsm <- heemod::run_model(
  soc = strat_soc,
  new = strat_new,
  parameters = params,
  cycles = Ncycles,
  cost = cost_total,
  effect = qaly,
  init = c(1, 0, 0),
  method = "life-table"
)

# Create the package data
usethis::use_data(oncpsm, overwrite = TRUE)
