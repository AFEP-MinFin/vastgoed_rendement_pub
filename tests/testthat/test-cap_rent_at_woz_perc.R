mock_houses <-
  tibble(
    rent_category = c('middle', 'social', 'free'),
    woz_indexed = 100000
  )

mock_rent_func = function(x) return(tibble(a = c(10000, 5000, 15000)))

test_that("Valid result for valid input", {
  res <- cap_rent_at_woz_perc(mock_houses, mock_rent_func, rent_categories = c('middle'), cap_perc = 0.05)[[1]]
  expect_length(res, 3)
  expect_equal(res, c(5000, 5000, 15000)) # To define
})
