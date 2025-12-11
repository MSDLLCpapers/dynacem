# Test the future calculations

# Get data
democe <- get_dynfields(
   heemodel = oncpsm,
   payoffs = c("cost_daq_new", "cost_total", "qaly"),
   discount = "disc"
   )

# Get discount rate
discrate <- get_param_value(oncpsm, "disc")

# Obtain payoff vector of interest
payoffs <- democe |>
   dplyr::filter(int=="new") |>
   dplyr::mutate(cost_oth = cost_total_rup - cost_daq_new_rup)
Nt <- nrow(payoffs)

# Simple calculation - no discounting
test_that("Simple calculation works", {
  # Actual
  act_val1 <- futurepv(
      tzero = 0:9,
      payoffs = payoffs$cost_oth,
      prices = rep(1, 2*Nt),
      discrate = discrate
      )
  # Expected
  exp_val1 <- rep(sum(payoffs$cost_total - payoffs$cost_daq_new), 10)
  # Test
  expect_equal(mean(act_val1)[[2]], exp_val1)
})

# More complex calculations - different time horizon
test_that("Complex calculation works", {
  # Actual
  act_val2 <- futurepv(
    tzero = 0,
    payoffs = payoffs$cost_oth[1:(Nt/2)],
    prices = rep(1, 2*Nt),
    discrate = discrate
  )
  # Expected
  exp_val2 <- sum((payoffs$cost_total - payoffs$cost_daq_new)[1:(Nt/2)])
  # Tests
  expect_equal(mean(act_val2), exp_val2)
})

# 10. No error when running print
test_that("No error when applying print summary", {
  # Some sample dynpv values
  fpv1 <- futurepv(
    tzero = 0,
    payoffs = payoffs$cost_oth[1:(Nt/2)],
    prices = rep(1, 2*Nt),
    discrate = discrate
  )
  fpv2 <- futurepv(
      tzero = 0:9,
      payoffs = payoffs$cost_oth,
      prices = rep(1, 2*Nt),
      discrate = discrate
      )
  # Check no error
  expect_no_error(print(summary(fpv1)))
  expect_no_error(print(summary(fpv2)))
})
