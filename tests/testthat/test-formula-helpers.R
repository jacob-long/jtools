test_that("formula helpers do not depend on a global debug flag", {
  expect_true(two_sided(y ~ x))
  expect_false(two_sided(~x))
  expect_equal(all_vars(y ~ x + z), c("y", "x", "z"))
  expect_false(any_transforms(y ~ x + z))
  expect_true(any_transforms(y ~ log(x)))
})

test_that("formula helpers handle Formula objects", {
  skip_if_not_installed("Formula")
  expect_true(two_sided(Formula::Formula(y ~ x | z)))
  expect_false(two_sided(Formula::Formula(~x | z)))
})
