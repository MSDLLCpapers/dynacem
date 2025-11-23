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
#' @param dpv_name Name to be given to Dynamic Present Value object created by this function call
#' @returns A list containing `inputs` and `results`.
#' The `inputs` list contains a list of the following parameters called with the function: `uptakes, payoffs, horizon, tzero, prices`, and `discrate`. 
#' The `results` output is a `class_dynpv` S7 object that contains the following elements:
#' - `name`: Name given to the object
#' - `df`: Tibble of calculation results
#' - `ncoh`: Number of cohorts of uptaking patients
#' - `ntimes`: Number of times (unique values of tzero) at which calculations are performed
#' - `uptake`: Total number of uptaking patients
#' - `total`: Total present value
#' - `mean`: Average present value per uptaking patient (=total/uptake)
#' - `sum_by_coh`: Tibble of summarized calculation results for each uptake cohort
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
#' # Obtain short payoff vector of interest
#' payoffs <- democe |>
#'    dplyr::filter(int=="new", model_time<11) |>
#'    dplyr::mutate(cost_oth = cost_total - cost_daq_new)
#' Nt <- nrow(payoffs)
#' 
#' # Example calculation
#' dynpv(
#'    uptakes = rep(1, Nt),
#'    payoffs = payoffs$cost_oth,
#'    prices = 1 + (0:(Nt-1))*0.05,
#'    discrate = 0.08 
#' )
dynpv <- function(
    uptakes = 1,
    payoffs,
    horizon = length(payoffs),
    tzero = 0,
    prices = rep(1, length(payoffs)+tzero),
    discrate = 0,
    dpv_name = NA_character_
    ){
  # Avoid no visible binding note
  j <- k <- l <- uj <- pk <- R <- v <- pv <- spv <- total <- suptakes <- NULL
  # Trim
  uptakes <- trim_vec(uptakes)
  payoffs <- trim_vec(payoffs)
  # Create a dataset for each combination of time
  df <- tidyr::expand_grid(j=1:length(uptakes), k=1:length(payoffs), l=tzero) |>
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
  # Put dataset into a dynpv_class object
  cds <- class_dynpv(name = dpv_name, df = df)  
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
    results = cds
  ))
}

