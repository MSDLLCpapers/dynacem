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

## Collection of functions to extract payoffs from other systems, e.g. heemod
## ==========================================================================

#' Helper function to get a tibble of the relevant fields from heemod output
#' @param heemodel A health economic model object from the *heemod* package (see [heemod::heemod-package]).
#' @param payoffs Field names of payoffs of interest (string vector)
#' @param discount Name of parameter providing discount rate per cycle (string)
#' @param fname Export data to a .CSV file of this name, if given (character)
#' @returns Tibble of payoffs taken from the heemod model, by intervention and model timestep (`model_time`).
#' 
#' The field `vt` is calculated as `(1+i)^(1-model_time)`, where `i` is the discount rate per model timestep set in the *heemod* model through the parameter `disc_cycle`. This can be useful in 'rolling-up' payoff values to the timestep in which they were incurred.
#' 
#' An additional set of payoffs (identified with a "_rup" suffix) provides calculations of the payoffs as at the start of the timestep in which they were incurred, i.e. original payoff / `vt`.
#' @importFrom rlang .data
#' @seealso [heemod::heemod-package]
#' @export
#' @examples
#' democe <- get_dynfields(
#'    heemodel = oncpsm,
#'    payoffs = c("cost_daq_new", "cost_total", "qaly"),
#'    discount = "disc"
#'    )
#' head(democe)
get_dynfields <- function(heemodel, payoffs, discount, fname=NA){
  # Set certain variables to NULL to avoid 'no visible binding' note
  model_time <- vt_rup <- NULL
  # Pull out interventions
  int <- names(heemodel$eval_strategy_list)
  # Pull out discount rate
  discrate <- get_param_value(heemodel, discount)
  # Create a list to store data for each intervention
  ds <- vector(mode = "list", length = length(int))
  # Pull in tibble into list for each intervention
  for (i in seq(int)){
    ds[[i]] <- heemodel$eval_strategy_list[[int[i]]]$values |>
      dplyr::as_tibble() |>
      dplyr::select(c("model_time", dplyr::all_of(payoffs))) |>
      dplyr::mutate(int = int[i])
  }
  # Unnest
  ds <- dplyr::bind_rows(ds) |>
    # Add discounting variable
    dplyr::mutate(vt = (1 + discrate)^(1 - model_time))
  # Create rolled-up variables, with "r" prefix to their name
  rs <- ds |>
    dplyr::mutate(dplyr::across(dplyr::all_of(payoffs), ~.x/vt))
  # Join the rolled-up data to the original data
  ds <- ds |>
    dplyr::left_join(rs, by=c("model_time", "int"), suffix=c("", "_rup")) |>
    dplyr::select(-vt_rup)
  # Export as CSV
  if (!is.na(fname)) {readr::write_csv(ds, file=paste0(fname, ".csv"))}
  # Return
  return(ds)
}

#' Obtain parameter value(s) from a heemod output
#' @inheritParams get_dynfields
#' @param param Name of parameter to extract from the heemod model
#' @returns Value of the parameter from the heemod model
#' @seealso [heemod::heemod-package]
#' @export
#' @examples
#' get_param_value(
#'    heemodel = oncpsm,
#'    param = "disc"
#'    )
get_param_value <- function(heemodel, param){
  rlang::eval_tidy(heemodel$parameters[[param]])
}