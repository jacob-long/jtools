library(jtools)

set.seed(1)
output <- rpois(100, 5)
input <- log(output) + runif(100,0,1)
fitgf <- glm(output ~ input, family = poisson)

library(survey)
data(api)
dstrat <- svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
dstrat$variables$mealsdec <- dstrat$variables$meals/100
regmodel <- svyglm(mealsdec ~ ell + api00, design = dstrat,
                   family = quasibinomial)
regmodell <- svyglm(mealsdec ~ ell + api00, design = dstrat)

fit <- lm(Income ~ Frost + Illiteracy + Murder, data = as.data.frame(state.x77))

test_that("jsumm: non-linear models work", {
  expect_is(j_summ(fitgf), "j_summ.glm")
  expect_is(j_summ(fitgf, standardize = TRUE), "j_summ.glm")
  expect_is(j_summ(fitgf, center = TRUE), "j_summ.glm")
})


test_that("jsumm: non-linear svyglm models work", {
  expect_is(j_summ(regmodel), "j_summ.svyglm")
})

test_that("jsumm: svyglm vifs work", {
  expect_is(j_summ(regmodel, vifs = TRUE), "j_summ.svyglm")
})

test_that("jsumm: svyglm linear model check works", {
  expect_is(j_summ(regmodel, model.check = TRUE), "j_summ.svyglm")
})

test_that("jsumm and scale_lm: scaling works", {
  expect_is(j_summ(fitgf, standardize = TRUE, n.sd = 2), "j_summ.glm")
  expect_is(j_summ(fit, standardize = TRUE, n.sd = 2), "j_summ.lm")
})

test_that("jsumm and center_lm: centering works", {
  expect_is(j_summ(fitgf, center = TRUE, n.sd = 2), "j_summ.glm")
  expect_is(j_summ(fit, center = TRUE, n.sd = 2), "j_summ.lm")
})

test_that("jsumm: Printing isn't borked", {
  expect_output(print(j_summ(fitgf, vifs = TRUE)))
  expect_output(print(j_summ(fitgf, standardize = TRUE)))
  expect_output(print(j_summ(regmodel, standardize = TRUE, n.sd = 2)))
  expect_output(print(j_summ(regmodel, model.check = TRUE, vifs = TRUE)))
  expect_output(print(j_summ(regmodell, standardize = TRUE, n.sd = 2)))
  expect_output(print(j_summ(regmodell, model.check = TRUE, vifs = TRUE)))
  expect_output(print(j_summ(fit, standardize = TRUE, n.sd = 2)))
  expect_output(print(j_summ(fit, model.check = TRUE, vifs = TRUE)))

})
