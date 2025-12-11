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
  # Pull out xdata and subset of ydata; add dpvno=1 or 2 depending on source
  xdata <- e1 |> mutate(dpvno="x")
  ydata <- e2 |> mutate(dpvno="y")
  # Check that j, k and l vectors align
  if (length(xdata$j) != length(ydata$j)) {warning("Uptake vectors differ in length after trimming")}
  stopifnot(max(xdata$k) == max(ydata$k))
  stopifnot(max(xdata$l) == max(ydata$l))
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


#' Number of cohorts of uptaking patients (ncoh)
#'
#' @param df Tibble of class "dynpv" created by [dynpv()] or [futurepv()]
#'
#' @return A number
#'
#' @export
ncoh <- function(df) max(df$k)

#' Number of times (unique values of tzero) at which calculations are performed
#'
#' @inherit ncoh params return
#'
#' @export
ntimes <- function(df) length(unique(df$l))

#' Total number of uptaking patients (uptake)
#'
#' @inherit ncoh params
#'
#' @return A number or tibble
#'
#' @export
uptake <- function(df) {
  tempout1 <- df |>
    summarize(mean=mean(uj), sd=sd(uj), .by=c(j, l)) |>
    rename(tzero = l) |>
    summarize(uptake=sum(mean), .by=tzero)
  result <- if (nrow(tempout1)==1) tempout1$uptake else tempout1
  return(result)
}
#' Tibble of summarized calculation results for each uptake cohort (sum_by_coh)
#'
#' @inherit uptake params return
#'
#' @export
sum_by_coh <- function(df) {
  tempout2 <- df |>
    # Summing over k, where uj does not vary by k
    summarize(spv = sum(pv), .by=c(j, l)) |>
    rename(tzero = l)
  result <- if (nrow(tempout2)==1) tempout2$spv else tempout2
  return(result)
}

#' Total present value (total)
#'
#' @inherit uptake params return
#'
#' @export
total <- function(df) {
  tempout3 <- df |>
    summarize(total = sum(pv), .by=c(l)) |>
    rename(tzero = l)
  result <- if (nrow(tempout3)==1) tempout3$total else tempout3
  return(result)
}

#' Average present value per uptaking patient (mean=total/uptake)
#'
#' @param x Tibble of class "dynpv" created by [dynpv()] or [futurepv()]
#' @param ... Currently unused
#'
#' @inherit uptake return
#'
#' @export
mean.dynpv <- function(x, ...) {
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
#' - `ncoh`: Number of cohorts of uptaking patients
#' - `ntimes`: Number of times (unique values of tzero) at which calculations are performed
#' - `uptake`: Total number of uptaking patients
#' - `sum_by_coh`: Tibble of summarized calculation results for each uptake cohort
#' - `total`: Total present value
#' - `mean`: Average present value per uptaking patient (=total/uptake)
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
