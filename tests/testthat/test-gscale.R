context("gscale")

x <- rnorm(100, 2, 1)
x2 <- rbinom(100, 1, .5)
wts <- runif(100, 0, 1)
mtcars$weights <- wts[1:32]

test_that("gscale: binary vars are scaled", {
  expect_length(gscale(x2), 100)
  expect_length(gscale(x2, binary.inputs = "full"), 100)
  expect_length(gscale(x2, binary.inputs = "0/1"), 100)
  expect_length(gscale(x2, binary.inputs = "-0.5/0.5"), 100)
})

test_that("gscale: continuous vars are scaled", {
  expect_length(gscale(x), 100)
})

test_that("gscale: continuous vars are centered", {
  expect_length(gscale(x, center.only = TRUE), 100)
})

test_that("gscale: continuous vars are scaled without centering", {
  expect_length(gscale(x, scale.only = TRUE), 100)
})

test_that("gscale: dataframes are scaled", {
  expect_length(gscale(data = mtcars), length(mtcars))
  expect_length(gscale(data = mtcars, binary.inputs = "full"),
                length(mtcars))
  expect_length(gscale(data = mtcars, binary.inputs = "0/1"),
                length(mtcars))
  expect_length(gscale(data = mtcars, binary.inputs = "-0.5/0.5"),
                length(mtcars))
})

test_that("gscale: dataframes are scaled without centering", {
  expect_length(gscale(data = mtcars, scale.only = TRUE), length(mtcars))
})

test_that("gscale: dataframes are centered without scaling", {
  expect_length(gscale(data = mtcars, center.only = TRUE), length(mtcars))
})

test_that("gscale: selected vars in dataframes are scaled", {
  expect_length(gscale(vars = c("hp", "wt", "vs"), data = mtcars),
                length(mtcars))
  expect_length(gscale(vars = c("hp", "wt", "vs"), data = mtcars,
                       binary.inputs = "full"),
                length(mtcars))
  expect_length(gscale(vars = c("hp", "wt", "vs"), data = mtcars,
                       binary.inputs = "0/1"),
                length(mtcars))
  expect_length(gscale(vars = c("hp", "wt", "vs"), data = mtcars,
                       binary.inputs = "-0.5/0.5"),
                length(mtcars))
})

test_that("gscale: deprecated warnings work", {
  expect_warning(gscale(x = c("hp", "wt", "vs"), data = mtcars,
         binary.inputs = "-0.5/0.5"))
  expect_warning(gscale(x = mtcars$hp))
})

test_that("gscale: selected vars in dataframes are scaled without centering", {
  expect_length(gscale(vars = c("hp", "wt", "vs"), data = mtcars,
                       scale.only = TRUE),
                length(mtcars))
})

test_that("gscale: selected vars in dataframes are centered without scaling", {
  expect_length(gscale(vars = c("hp", "wt", "vs"), data = mtcars,
                       center.only = TRUE),
                length(mtcars))
})

if (requireNamespace("survey")) {
  suppressMessages(library(survey, quietly = TRUE))
  data(api)
  ## Create survey design object
  dstrat <- svydesign(id = ~1,strata = ~stype, weights = ~pw, data = apistrat,
                        fpc=~fpc)
  dstrat$variables$binary <- rbinom(200, 1, 0.5) # Creating test binary variable
  # This kills that warning that I don't want
  dstrat$variables <- dstrat$variables[,!(names(dstrat$variables) %in% "flag")]

  test_that("gscale: surveys work as expected", {
    expect_is(gscale(data = dstrat, binary.inputs = "full"),
              c("survey.design2","survey.design"))
    expect_is(gscale(data = dstrat, binary.inputs = "center"),
              c("survey.design2","survey.design"))
    expect_is(gscale(data = dstrat, binary.inputs = "0/1"),
              c("survey.design2","survey.design"))
    expect_is(gscale(data = dstrat, binary.inputs = "-0.5/0.5"),
              c("survey.design2","survey.design"))
    expect_is(gscale(data = dstrat, binary.inputs = "full", center.only = TRUE),
              c("survey.design2","survey.design"))
    expect_is(gscale(data = dstrat, binary.inputs = "full", scale.only = TRUE),
              c("survey.design2","survey.design"))
    expect_is(gscale(c("api00","meals","binary"), data = dstrat,
                     binary.inputs = "-0.5/0.5"),
              c("survey.design2","survey.design"))
    expect_is(gscale(c("api00","meals","binary"), data = dstrat,
                     binary.inputs = "full"),
              c("survey.design2","survey.design"))
    expect_is(gscale(c("api00","meals","binary"), data = dstrat,
                     binary.inputs = "0/1"),
              c("survey.design2","survey.design"))
    expect_is(gscale(c("api00","meals","binary"), data = dstrat,
                     binary.inputs = "full", center.only = TRUE),
              c("survey.design2","survey.design"))
    expect_is(gscale(c("api00","meals","binary"), data = dstrat,
                     binary.inputs = "full", scale.only = TRUE),
              c("survey.design2","survey.design"))
  })
}

test_that("gscale: binary vars are scaled with weights", {
  expect_length(gscale(x2, weights = wts), 100)
  expect_length(gscale(x2, binary.inputs = "full", weights = wts), 100)
  expect_length(gscale(x2, binary.inputs = "0/1", weights = wts), 100)
  expect_length(gscale(x2, binary.inputs = "-0.5/0.5", weights = wts), 100)
})

test_that("gscale: continuous vars are scaled with weights", {
  expect_length(gscale(x, weights = wts), 100)
  expect_length(gscale(x, binary.inputs = "full", weights = wts), 100)
  expect_length(gscale(x, binary.inputs = "0/1", weights = wts), 100)
  expect_length(gscale(x, binary.inputs = "-0.5/0.5", weights = wts), 100)
})

test_that("gscale: dataframes are scaled with weights", {
  expect_length(gscale(data = mtcars, weights = weights), length(mtcars))
  expect_length(gscale(data = mtcars, binary.inputs = "full", weights = weights),
                length(mtcars))
  expect_length(gscale(data = mtcars, binary.inputs = "0/1", weights = weights),
                length(mtcars))
  expect_length(gscale(data = mtcars, binary.inputs = "-0.5/0.5", weights = weights),
                length(mtcars))
})

test_that("gscale: variables in dataframes are scaled with weights", {
  expect_length(gscale(c("hp", "wt", "vs"), data = mtcars, weights = weights),
                length(mtcars))
  expect_length(gscale(c("hp", "wt", "vs"), data = mtcars,
                       binary.inputs = "full", weights = weights), length(mtcars))
  expect_length(gscale(c("hp", "wt", "vs"), data = mtcars,
                       binary.inputs = "0/1", weights = weights), length(mtcars))
  expect_length(gscale(c("hp", "wt", "vs"), data = mtcars,
                       binary.inputs = "-0.5/0.5", weights = weights),
                length(mtcars))
})

test_that("gscale: non-standard input is rejected", {
  expect_error(gscale(x2, binary.inputs = "poop"))
  expect_error(gscale(x, center.only = TRUE, scale.only = TRUE))
  expect_error(gscale(x2, binary.inputs = "full",
                      center.only = TRUE, scale.only = TRUE))
  expect_error(gscale(data = mtcars, center.only = TRUE, scale.only = TRUE))
  expect_error(gscale("wt", data = mtcars, center.only = TRUE, scale.only = TRUE))
  expect_error(gscale("poop"))
})

