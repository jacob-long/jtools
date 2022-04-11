context("Colors tests")
test_that("Color classes are returned", {
  expect_silent(get_colors("CUD", 6))
  expect_silent(get_colors("Qual1", 6))
  expect_silent(get_colors("Qual2", 6))
  expect_silent(get_colors("Qual3", 6))
  expect_silent(get_colors("Rainbow", 6))
  expect_silent(get_colors("Blues", 6))
  expect_silent(get_colors("BrBG", 6))
  expect_silent(get_colors("viridis", 6))
  expect_silent(get_colors("Blues", 2, gradient = TRUE))
  expect_silent(get_colors("Oranges", 2, gradient = TRUE))
  expect_silent(get_colors("Greens", 2, gradient = TRUE))
  expect_silent(get_colors("Reds", 2, gradient = TRUE))
  expect_silent(get_colors("Purples", 2, gradient = TRUE))
  expect_silent(get_colors("Greys", 2, gradient = TRUE))
  expect_silent(get_colors("blue", 2, gradient = TRUE))
  expect_silent(get_colors("green", 2, gradient = TRUE))
  expect_silent(get_colors("red", 2, gradient = TRUE))
  expect_silent(get_colors("purple", 2, gradient = TRUE))
  expect_silent(get_colors("seagreen", 2, gradient = TRUE))
  expect_silent(get_colors("blue2", 2, gradient = TRUE))
  expect_silent(get_colors(c("red", "blue", "black"), 2))
  expect_silent(get_colors(c("red", "blue", "black"), 3))
  expect_silent(get_colors(c("red", "blue", "black"), 2, reverse = TRUE))
})

context("Survey tools")
if (requireNamespace("boot")) {
   states <- as.data.frame(state.x77)
   set.seed(100)
   states$wts <- runif(50, 0, 3)
   fit <- lm(Murder ~ Illiteracy + Frost, data = states)
   expect_s3_class(
     weights_tests(model = fit, data = states, weights = wts, sims = 100),
     "weights_tests"
   )
}

 if (requireNamespace("survey")) {
  library(survey, quietly = TRUE)
  data(api)
  # Create survey design object
  dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw,
                      data = apistrat, fpc = ~fpc)
  # Save the results, extract correlation matrix
  out <- svycor(~api00 + api99 + dnum, design = dstrat)
  expect_s3_class(out, "svycor")
}