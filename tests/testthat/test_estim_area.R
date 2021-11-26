test_that("estimate area input",{
  expect_error(estimate_area(B = "a string"))
  expect_error(estimate_area(B = -100))
  expect_error(estimate_area(B = 100, seed = "a string"))
})
test_that("estimate area output",{
  expect_type(estimate_area(B = 100), "list")
})

