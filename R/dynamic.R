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

#' Present values with dynamic pricing and dynamic uptake
#' 
#' Calculate present value for a payoff with dynamic (lifecycle) pricing and dynamic uptake (stacked cohorts).
#'
#' Let us partition time as follows.
#' 
#' - Suppose \eqn{j=1,...,T} indexes the time at which the patient begins treatment, where \eqn{T} is the time horizon of the decision-maker.
#' - Suppose \eqn{k=1,...,T} indexes time since initiating treatment.
#' 
#' In general, \eqn{t=j+k-1}, and we are interested in \eqn{t=1,...,T}.
#' 
#' The [Present Value](https://en.wikipedia.org/wiki/Present_value) of a cashflow \eqn{p_k} for the \eqn{u_j} patients who began treatment at time \eqn{j} and who are in their \eqn{k}th timestep of treatment is as follows
#' \deqn{PV(j,k,l) = u_j \cdot p_k \cdot R_{j+k+l-1} \cdot (1+i)^{2-j-k}}
#' where \eqn{i} is the risk-free discount rate per timestep, \eqn{p_k} is the cashflow amount in today’s money, and \eqn{p_k \cdot R_{j+k+l-1}} is the nominal amount of the cashflow at the time it is incurred, allowing for an offset of \eqn{l = tzero}.
#' 
#' The total present value, \eqn{TPV(l)}, is therefore the sum over all \eqn{j} and \eqn{k} within the time horizon \eqn{T}, namely:
#' \deqn{TPV(l) = \sum_{j=1}^{T} \sum_{k=1}^{T-j+1} PV(j,k, l) \\
#' \;
#' = \sum_{j=1}^{T} \sum_{k=1}^{T-j+1} u_j \cdot p_k \cdot R_{l+j+k-1} \cdot (1+i)^{2-j-k}}
#' 
#' This function calculates \eqn{PV(j,k,l)} for all values of \eqn{j}, \eqn{k} and \eqn{l}, and returns this in a tibble.
#' @param uptakes Vector of patient uptake over time
#' @param horizon Time horizon for the calculation (length must be less than or equal to the length of payoffs)
#' @param tzero Time at the date of calculation, to be used in lookup in prices vector
#' @param payoffs Vector of payoffs of interest (numeric vector)
#' @param prices Vector of price indices through the time horizon of interest
#' @param discrate Discount rate per timestep, corresponding to price index
#' @returns A tibble of class "dynpv" with the following columns:
#' - `j`: Time at which patients began treatment
#' - `k`: Time since patients began treatment
#' - `l`: Time offset for the price index (from `tzero`)
#' - `t`: Equals \eqn{j+k-1}
#' - `uj`: Uptake of patients beginning treatment at time \eqn{j} (from `uptakes`)
#' - `pk`: Cashflow amount in today's money in respect of patients at time \eqn{k} since starting treatment (from `payoffs`)
#' - `R`: Index of prices over time \eqn{l+t} (from `prices`)
#' - `v`: Discounting factors, \eqn{(1+i)^{1-t}}, where `i` is the discount rate per timestep
#' - `pv`: Present value, \eqn{PV(j,k,l)}
#' @export
#' @examples
#' # Simple example
#' pv1 <- dynpv(
#'    uptakes = (1:10), # 1 patient uptakes in timestep 1, 2 patients in timestep 2, etc
#'    tzero = c(0,5), # Calculations are performed with prices at times 0 and 5
#'    payoffs = 90 + 10*(1:10), # Payoff vector of length 10 = (100, 110, ..., 190)
#'    prices = 1.02^((1:15)-1), # Prices increase at 2\% per timestep in future
#'    discrate = 0.05 # The nominal discount rate is 5\% per timestep;
#'                    # the real discount rate per timestep is 3\% (=5\% - 3\%)
#' )
#' pv1
#' summary(pv1)
#' 
#' # More complex example, using cashflow output from a heemod model
#'
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
#'
#' # Example calculation
#' pv2 <- dynpv(
#'    uptakes = rep(1, nrow(payoffs)),
#'    payoffs = payoffs$cost_oth,
#'    prices = 1.05^(0:(nrow(payoffs)-1)),
#'    discrate = 0.08
#' )
#' summary(pv2)
dynpv <- function(
    uptakes = 1,
    payoffs,
    horizon = length(payoffs),
    tzero = 0,
    prices = rep(1, length(payoffs)+tzero),
    discrate = 0
    ){
  # Avoid no visible binding note
  j <- k <- l <- uj <- pk <- R <- v <- NULL
  # Trim
  uptakes <- trim_vec(uptakes)
  payoffs <- trim_vec(payoffs)
  # Create a dataset for each combination of time
  df <- expand_grid(j=1:length(uptakes), k=1:length(payoffs), l=tzero) |>
    mutate(t= j + k - 1) |>
    # Remove time entries that are outside the time horizon
    filter(t <= horizon) |>
    mutate(
      uj = uptakes[j],
      pk = payoffs[k],
      R = prices[l + t],
      v = (1+discrate)^(1 - t),
      pv = uj * pk * R * v
    )
  class(df) <- c("dynpv", class(df))
  return(df)
}

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
