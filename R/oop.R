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

# Classes and methods for dynpv
# =============================

#' Method to add two dynpv objects together
#'
#' Add together two objects each of class "dynpv"
#'
#' @inherit addprod params details return
#'
#' @export
"+.dynpv" <- function(e1, e2) addprod(e1, e2, mult=1)

#' Method to subtract one dynpv object from another
#'
#' Subtract one object of S7 class "dynpv" from another
#'
#' Present value of `e1-e2` is the present values from `e1` less that from `e2`.
#' Total uptake of `e1-e2` is the uptake from `e1` less that from `e2`. Take
#' care of this when using `$mean` of the summed object.
#'
#' @inherit addprod params return
#' @export
"-.dynpv" <- function(e1, e2) addprod(e1, e2, mult=-1)

#' Method to add two dynpv objects together
#'
#' Add together two objects each of S3 class "dynpv": e1 + mult * e2
#'
#' @param e1 First "dynpv" object
#' @param e2 Second "dynpv" object
#' @param mult Numeric
#'
#' Present value is present value from `e1` plus `mult` times the present value
#' from `e2`. Total uptake is the uptake from `e1` plus `mult` times the uptake
#' from `e2`. Take care of this when using `$mean` of the summed object.
#'
#' @returns S3 object of class "dynpv"
#'
#' @export
addprod <- function(e1, e2, mult) {
  # Avoid no visible binding note
  j <- k <- l <- dpvno <- uj <- pv <- uj_x <- uj_y <- pv_x <- pv_y <- NULL
  # Pull out xdata and subset of ydata; add dpvno=1 or 2 depending on source
  xdata <- e1 |> mutate(dpvno="x")
  ydata <- e2 |> mutate(dpvno="y")
  # Check that j, k and l vectors align
  if (length(xdata$j) != length(ydata$j)) {warning("Uptake vectors differ in length")}
  stopifnot("Length of payoffs vectors differ" = max(xdata$k) == max(ydata$k))
  stopifnot("Length of time-offset vectors (tzero) differ" = max(xdata$l) == max(ydata$l))
  # Combine data
  jdata <- bind_rows(xdata, ydata) |>
    # Spread
    pivot_wider(
      id_cols = c(j, k, l, t),
      names_from = dpvno,
      values_from = c(uj, pv),
      values_fill = 0
    ) |>
    # Sum the present values from both objects
    mutate(
      uj = uj_x + mult * uj_y,
      pv = pv_x + mult * pv_y
    ) |>
    select(-uj_x, -uj_y, -pv_x, -pv_y) |>
    as_tibble()
  class(jdata) <- c("dynpv", class(jdata))
  return(jdata)
}

#' Number of cohorts of uptaking patients
#' 
#' Number of cohorts of uptaking patients, calculated as the length of the `uptakes` input to [dynpv()]
#'  
#' @param df Tibble of class "dynpv" created by [dynpv()] or [futurepv()]
#'
#' @return A number
#'
#' @export
ncoh <- function(df) max(df$k)

#' Number of times at which present value calculations are performed
#'
#' Number of times at which present value calculations are performed, calculated as the length of the `tzero` input to [dynpv()]
#' 
#' @inherit ncoh params return
#'
#' @export
ntimes <- function(df) length(unique(df$l))

#' Total number of uptaking patients
#'
#' Total number of uptaking patients, calculated as the sum of the `uptake` input to [dynpv()], by time at which the calculation is performed (`tzero` input to [dynpv()])
#' 
#' @inherit ncoh params
#'
#' @return A number or tibble
#'
#' @export
uptake <- function(df) {
  # Avoid no visible binding note
  uj <- sd <- j <- l <- tzero <- NULL
  tempout1 <- df |>
    summarize(mean=mean(uj), sd=sd(uj), .by=c(j, l)) |>
    rename(tzero = l) |>
    summarize(uptake=sum(mean), .by=tzero)
  result <- if (nrow(tempout1)==1) tempout1$uptake else tempout1
  return(result)
}

#' Present value for each uptake cohort and calculation time
#'
#' Calculates the sum of the Present Value by uptake cohort (`j`) and time at which the calculation is performed (`tzero` input to [dynpv()])
#' 
#' The [Present Value](https://en.wikipedia.org/wiki/Present_value) of a cashflow \eqn{p_k} for the \eqn{u_j} patients who began treatment at time \eqn{j} and who are in their \eqn{k}th timestep of treatment is as follows
#' \deqn{PV(j,k,l) = u_j \cdot p_k \cdot R_{j+k+l-1} \cdot (1+i)^{2-j-k}}
#' where \eqn{i} is the risk-free discount rate per timestep, \eqn{p_k} is the cashflow amount in today’s money, and \eqn{p_k \cdot R_{j+k+l-1}} is the nominal amount of the cashflow at the time it is incurred, allowing for an offset of \eqn{l = tzero}.
#' 
#' This method returns \eqn{\sum_{k=1}^{T-j+1} PV(j,k,l)} for each value of \eqn{j} and \eqn{l}, where \eqn{T} is the time horizon of the calculation.
#' 
#' @inherit uptake params return
#' @seealso [dynpv()], [futurepv()]
#' @return A number or tibble
#' @export
sum_by_coh <- function(df) {
  # Avoid no visible binding note
  pv <- j <- l <- NULL
  tempout2 <- df |>
    # Summing over k, where uj does not vary by k
    summarize(spv = sum(pv), .by=c(j, l)) |>
    rename(tzero = l)
  result <- if (nrow(tempout2)==1) tempout2$spv else tempout2
  return(result)
}

#' Total present value
#' 
#' Sum of the Present Value, by time at which the calculation is performed (`tzero` input to [dynpv()])
#' 
#' The [Present Value](https://en.wikipedia.org/wiki/Present_value) of a cashflow \eqn{p_k} for the \eqn{u_j} patients who began treatment at time \eqn{j} and who are in their \eqn{k}th timestep of treatment is as follows
#' \deqn{PV(j,k,l) = u_j \cdot p_k \cdot R_{j+k+l-1} \cdot (1+i)^{2-j-k}}
#' where \eqn{i} is the risk-free discount rate per timestep, \eqn{p_k} is the cashflow amount in today’s money, and \eqn{p_k \cdot R_{j+k+l-1}} is the nominal amount of the cashflow at the time it is incurred, allowing for an offset of \eqn{l = tzero}.
#' 
#' The total present value by time at which the calculation is performed, \eqn{TPV(l)}, is therefore the sum of \eqn{PV(j,k,l)} over all \eqn{j} and \eqn{k} within the time horizon \eqn{T}, namely:
#' \deqn{TPV(l) = \sum_{j=1}^{T} \sum_{k=1}^{T-j+1} PV(j,k, l) \\
#' \;
#' = \sum_{j=1}^{T} \sum_{k=1}^{T-j+1} u_j \cdot p_k \cdot R_{l+j+k-1} \cdot (1+i)^{2-j-k}}
#'
#' @inherit uptake params return
#' @seealso [dynpv()], [futurepv()]
#' @return A number or tibble
#' @export
total <- function(df) {
  # Avoid no visible binding note
  pv <- l <- NULL
  tempout3 <- df |>
    summarize(total = sum(pv), .by=c(l)) |>
    rename(tzero = l)
  result <- if (nrow(tempout3)==1) tempout3$total else tempout3
  return(result)
}

#' Mean present value per uptaking patient
#' 
#' Mean of the Present Value per uptaking patient, by time at which the calculation is performed (`tzero` input to [dynpv()]).
#' 
#' This is equal to [total()] divided by [uptake()].
#'
#' @param x Tibble of class "dynpv" created by [dynpv()] or [futurepv()]
#' @param ... Currently unused
#'
#' @inherit uptake return
#'
#' @export
mean.dynpv <- function(x, ...) {
  # Avoid no visible binding note
  tzero <- NULL
  total <- total(x)
  uptake <- uptake(x)
  if (length(total)==1) {
    total / uptake
  } else {
    left_join(total, uptake, join_by(tzero)) |>
      mutate(mean = total / uptake) |>
      select(-total, -uptake)
  }
}

#' Summarize a dynpv object
#'
#' @param object Tibble of class "dynpv" created by [dynpv()] or [futurepv()]
#' @param ... Currently unused
#'
#' @return A list of class "dynpv_summary" with the following elements:
#' - `ncoh`: Number of cohorts of uptaking patients, from [ncoh()]
#' - `ntimes`: Number of times at which present value calculations are performed, from [ntimes()]
#' - `uptake`: Total number of uptaking patients, from [uptake()]
#' - `sum_by_coh`: Present value for each uptake cohort and calculation time, from [sum_by_coh()]
#' - `total`: Total present value, from [total()]
#' - `mean`: Mean present value per uptaking patient, from [mean()], equal to `total`/`uptake`.
#'
#' @export
summary.dynpv <- function(object, ...) {
  structure(
    class = "dynpv_summary",
    list(
      ncoh = ncoh(object),
      ntimes = ntimes(object),
      uptake = uptake(object),
      sum_by_coh = sum_by_coh(object),
      total = total(object),
      mean = mean(object)
    )
  )
}

#' @export
print.dynpv_summary <- function(x, ...) {
  cat("Summary of Dynamic Pricing and Uptake\n")
  cat("     Number of cohorts:            ", x$ncoh, "\n")
  cat("     Number of times:              ", x$ntimes, "\n")
  # Output depends on whether $ntimes>1
  if (x$ntimes>1) {
    # Avoid no visible binding note
    tzero <- NULL
    # Create a tibble
    tib <- x$uptake |>
      left_join(x$total, join_by(tzero)) |>
      left_join(x$mean, join_by(tzero))
    cat("\n Uptake, total and mean present values by timepoint: \n")
    print(tib)
  }
  else {
    cat("     Total uptake:                 ", x$uptake, "\n")
    cat("     Total present value:          ", x$total, "\n")
    cat("     Mean present value:           ", x$mean, "\n")
  }
  return(invisible(x))
}
