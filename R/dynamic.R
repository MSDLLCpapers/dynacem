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

## Collection of functions to calculate cost-effectiveness model results
## with dynamic pricing and uptake
## =====================================================================

#' Trim the tailing zeroes from a long vector
#'
#' @param vec Vector. Final elements may be zero.
#' @returns A vector whose length is shorter than the original, if there were trailing zero elements
#' @export
#' @examples
#' trim_vec(c(1:10, rep(0,3)))
trim_vec <- function(vec){
  # Identify the last element before vector elements are zero
  trimto <- which(rev(cumsum(rev(vec)))==vec)[1]
  # Return trimmed vector
  return(vec[1:trimto])
}

#' Calculate present value for a payoff with dynamic pricing and dynamic uptake
#' 
#' Present value of a payoff affected by dynamic pricing, with uptake across multiple cohorts (dynamic uptake)
#' @param uptakes Vector of patient uptake over time
#' @param horizon Time horizon for the calculation (length must be less than or equal to the length of payoffs)
#' @param tzero Time at the date of calculation, to be used in lookup in prices vector
#' @param payoffs Vector of payoffs of interest (numeric vector) 
#' @param prices Vector of price indices through the time horizon of interest
#' @param discrate Discount rate per timestep, corresponding to price index
#' @returns List including
#' - calc: Tibble of calculation results
#' - cohpv: Tibble of summarized calculation results for each uptake cohort
#' - total: Total present value
#' - ncoh: Number of cohorts of uptaking patients
#' - uptake: Total number of uptaking patients
#' - mean: Average present value per uptaking patient (=total/uptake)
#' @export
#' @importFrom rlang .data
#' @examples
#' # Obtain dataset
#' democe <- get_dynfields(
#'    heemodel = oncpsm,
#'    payoffs = c("cost_daq_new", "cost_total", "qaly"),
#'    discount = "disc"
#'    )
#' 
#' # Obtain payoff vector of interest
#' payoffs <- democe |>
#'    dplyr::filter(int=="new") |>
#'    dplyr::mutate(cost_oth = cost_total - cost_daq_new)
#' Nt <- nrow(payoffs)
#' 
#' # Example calculation
#' dynpv(
#'    uptakes = rep(1, Nt),
#'    payoffs = payoffs$cost_oth,
#'    prices = 1 + (1:Nt)*0.05/52,
#'    discrate = (0.05 + 0.03)/52 
#' )
dynpv <- function(
    uptakes = 1,
    payoffs,
    horizon = length(payoffs),
    tzero = 0,
    prices = rep(1, length(payoffs)+tzero),
    discrate = 0
    ){
  # Trim
  uptakes <- trim_vec(uptakes)
  payoffs <- trim_vec(payoffs)
  # Create a dataset for each combination of time
  ds <- tidyr::expand_grid(j=1:length(uptakes), k=1:length(payoffs), l=tzero) |>
    dplyr::mutate(t= j + k - 1) |>
    # Remove time entries that are outside the time horizon
    dplyr::filter(t <= horizon) |>
    dplyr::mutate(
      uj = uptakes[j],
      pk = payoffs[k],
      R = prices[l + t],
      v = (1+discrate)^(1 - t),
      pv = uj * pk * R * v
    )
  # Summarize over each cohort (sum over k)
  sds <- ds |>
    dplyr::summarize(spv = sum(pv), .by=c(j, l)) |>
    dplyr::rename(tzero = l)
  # Summarize again by tzero / l
  ssds <- sds |>
    dplyr::summarize(total = sum(spv), .by=c(tzero)) |>
    dplyr::mutate(
      suptakes = sum(uptakes),
      mean = total/suptakes
      )
  # Total and mean
  restot <- ssds |> dplyr::select(tzero, total)
  resmean <- ssds |> dplyr::select(tzero, mean)
  if (length(tzero)==1) {
    restot <- restot$total
    resmean <- resmean$mean
    }
  # Return
  return(list(
    inputs = list(
      uptakes = uptakes,
      payoffs = payoffs,
      horizon = horizon,
      tzero = tzero,
      prices = prices,
      discrate = discrate
    ),
    results = list(
      ncoh = length(uptakes),
      uptake = sum(uptakes),
      calc = ds,
      cohpv = sds,
      total = restot,
      mean = resmean
    )
  ))
}

