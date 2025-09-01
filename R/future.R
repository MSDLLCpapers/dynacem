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

## Convenience function to calculate 'future' cost-effectiveness results
## ========================================================================

#' Calculate present value for a payoff in a single cohort with dynamic pricing across multiple timepoints
#' 
#' Present value of a series of payoffs for a single given cohort, entering at given future time, allowing for dynamic pricing. This function is a wrapper for [dynpv()] restricted to evaluation of a single cohort.
#' @inheritParams dynpv
#' @seealso [dynpv()]
#' @returns A list containing:
#' - `inputs`: list contains a list of the following parameters called with the function: `uptakes, payoffs, horizon, tzero, prices`, and `discrate`. 
#' - `calc`: Tibble of calculation results
#' - `pv`: Present value
#' @export
#' @examples
#' # Obtain dataset
#' democe <- get_dynfields(
#'    heemodel = oncpsm,
#'    payoffs = c("cost_daq_new", "cost_total", "qaly"),
#'    discount = "disc"
#'    )
#' 
#' # Obtain discount rate
#' discrate <- get_param_value(oncpsm, "disc")
#' 
#' # Obtain payoff vector of interest
#' payoffs <- democe |>
#'    dplyr::filter(int=="new") |>
#'    dplyr::mutate(cost_oth_rup = cost_total_rup - cost_daq_new_rup)
#' Nt <- nrow(payoffs)
#' 
#' # Run calculation for times 0-9
#' fpv <- futurepv(
#'   tzero = (0:9)*52,
#'   payoffs = payoffs$cost_oth_rup,
#'   prices = 1.001^(1:(2*Nt)-1), # Approx 5.3% every 52 steps
#'   discrate = 0.001 + discrate
#' )
#' fpv$calc
#' fpv$pv
futurepv <- function(tzero=0, payoffs, prices, discrate){
  # Wrapper for dynpv with uptakes=1 and horizon=length(payoffs)
  dpv <- dynpv(
    uptakes = 1,
    payoffs = payoffs,
    horizon = length(payoffs),
    tzero = tzero,
    prices = prices,
    discrate = discrate
  )
  # Only return useful outputs
  list(
    inputs = dpv$inputs,
    calc = dpv$results$calc,
    pv = dpv$results$mean
  )
}