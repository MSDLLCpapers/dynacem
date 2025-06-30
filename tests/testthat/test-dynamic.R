# Test the dynamic calculations

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

# 1. Simple calculation - one off uptake, no price escalation, no discounting
test_that("Simple calculation", {
  # Actual
  act_dval1 <- dynpv(
    uptakes = c(1, rep(0, Nt-1)),
    payoffs = payoffs$cost_oth,
    prices = rep(1, Nt),
    discrate = discrate
  )
  # Expected
  exp_dval1 <- sum(payoffs$cost_total - payoffs$cost_daq_new)
  # Test
  expect_equal(act_dval1$results$total, exp_dval1)
})

# 2. As (1) but with 5 x the uptake
test_that("5x the uptake of (1)", {
  # Actual
  act_dval2 <- dynpv(
    uptakes = c(5, rep(0, Nt-1)),
    payoffs = payoffs$cost_oth,
    prices = rep(1, Nt),
    discrate = discrate
    )
  # Expected
  exp_dval2 <- 5 * sum(payoffs$cost_total - payoffs$cost_daq_new)
  # Test
  expect_equal(act_dval2$results$total, exp_dval2)
})

# 3. As (1) but with discounting at 1.5%
test_that("As (1) but 1.5% discounting", {
  # Actual
  act_dval3 <- dynpv(
    uptakes = c(1, rep(0, Nt-1)),
    payoffs = payoffs$cost_oth,
    prices = rep(1, Nt),
    discrate = 0.015
    )
  # Expect
  vt <- (1.015)^(1-(1:Nt))
  exp_dval3 <- sum(vt * (payoffs$cost_total_rup - payoffs$cost_daq_new_rup))
  # Test
  expect_equal(act_dval3$results$total, exp_dval3)
})

# 4. As (3) but with 1% increasing price index
test_that("As (1) but 1% price increases", {
  # Actual
  prices <- 1.01^((1:Nt)-1)
  act_dval4 <- dynpv(
    uptakes = c(1, rep(0, Nt-1)),
    payoffs = payoffs$cost_oth,
    prices = prices,
    discrate = 0.015
    )
  # Expected
  vt <- (1.015)^(1-(1:Nt))
  exp_dval4 <- sum(vt* prices * (payoffs$cost_total_rup - payoffs$cost_daq_new_rup))
  # Test
  expect_equal(act_dval4$results$total, exp_dval4)  
})

# 5. As (1) but with flat incidence
test_that("As (1) but flat incidence", {
  # Actual
  act_dval5 <- dynpv(
    uptakes = rep(1, Nt),
    payoffs = payoffs$cost_oth,
    prices = rep(1, Nt),
    discrate = 0
    )
  # Expected
  exp_dval5 <- sum((Nt:1) * payoffs$cost_oth)
  # Test
  expect_equal(act_dval5$results$total, exp_dval5)
})

# 6. As (5) but with price increases
test_that("As (5) but with price increases", {
  # Actual
  prices <- 1.01^((1:Nt)-1)
  act_dval6 <- dynpv(
    uptakes = rep(1, Nt),
    payoffs = payoffs$cost_oth,
    prices = prices,
    discrate = discrate
    )
  # Expected lower bound - exp_dval5
  exp_dval6 <- sum((Nt:1) * payoffs$cost_oth)
  # Test
  expect_gt(act_dval6$results$mean, exp_dval6)
})


# 7. As (6) but with multiple time points
test_that("As (5) but with price increases", {
  # Actual
  prices <- 1.01^((1:(2*Nt))-1)
  tzero <- (0:4)*52
  act_dval6 <- dynpv(
    uptakes = rep(1, Nt),
    payoffs = payoffs$cost_oth,
    prices = prices,
    tzero = tzero,
    discrate = discrate
  )
  # Expected
  exp_dval6_0 <- dynpv(
    uptakes = rep(1, Nt),
    payoffs = payoffs$cost_oth,
    prices = prices,
    tzero = 0,
    discrate = discrate
  )
  exp_dval6_1 <- dynpv(
    uptakes = rep(1, Nt),
    payoffs = payoffs$cost_oth,
    prices = prices,
    tzero = 1*52,
    discrate = discrate
  )
  exp_dval6_2 <- dynpv(
    uptakes = rep(1, Nt),
    payoffs = payoffs$cost_oth,
    prices = prices,
    tzero = 2*52,
    discrate = discrate
  )
  exp_dval6_3 <- dynpv(
    uptakes = rep(1, Nt),
    payoffs = payoffs$cost_oth,
    prices = prices,
    tzero = 3*52,
    discrate = discrate
  )
  exp_dval6_4 <- dynpv(
    uptakes = rep(1, Nt),
    payoffs = payoffs$cost_oth,
    prices = prices,
    tzero = 4*52,
    discrate = discrate
  )
  # Test
  expect_equal(act_dval6$results$mean$mean,
        c(exp_dval6_0$results$mean, exp_dval6_1$results$mean, exp_dval6_2$results$mean, exp_dval6_3$results$mean, exp_dval6_4$results$mean))
})