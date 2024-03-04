mock_house =
  tibble(
    purchase_price = c(100000, 200000),
    purchase_price_unindexed = c(50000,100000),
    value_estimate_based_on_woz = c(50000,100000),
    debt = c(25000,50000)
  )

testthat::test_that(
  "check valid result for valid input",
  {
    res <- interest_constant_interest_rate(mock_house, 0.5, 0.1)
    expect_length(res$interest_payments, 2)
    expect_equal(res$interest_payments, c(-2500, -5000))
  }
)
