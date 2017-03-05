library(jtools)

test_that("jsumm: non-linear models work", {
  output <- rpois(100, 5)
  input <- log(output) + runif(100,0,1)
  fitgf <- glm(output ~ input, family = poisson)

  expect_is(j_summ(fitgf), "j_summ.glm")
  expect_is(j_summ(fitgf, standardize = TRUE), "j_summ.glm")
  expect_is(j_summ(fitgf, center = TRUE), "j_summ.glm")
})

library(survey)
test_that("jsumm: non-linear svyglm models work", {
  data(api)
  dstrat <- svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
  regmodel <- svyglm(meals/100 ~ ell + api00, design = dstrat,
                     family = quasibinomial)

  expect_is(j_summ(fitgf), "j_summ.glm")
})

test_that("jsumm: svyglm vifs work", {
  data(api)
  dstrat <- svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
  regmodel <- svyglm(meals/100 ~ ell + api00, design = dstrat,
                     family = quasibinomial)

  expect_is(j_summ(fitgf, vifs = TRUE), "j_summ.glm")
})

test_that("jsumm: svyglm linear model check works", {
  data(api)
  dstrat <- svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
  regmodel <- svyglm(meals/100 ~ ell + api00, design = dstrat)

  expect_is(j_summ(fitgf, model.check = TRUE), "j_summ.glm")
})

test_that("jsumm and scale_lm: scaling works", {
  data(api)
  dstrat <- svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
  regmodel <- svyglm(meals/100 ~ ell + api00, design = dstrat,
                     family = quasibinomial)
  fit <- lm(Income ~ Frost + Illiteracy + Murder, data = as.data.frame(state.x77))


  expect_is(j_summ(fitgf, standardize = TRUE, n.sd = 2), "j_summ.glm")
  expect_is(j_summ(fit, standardize = TRUE, n.sd = 2), "j_summ.lm")
})

test_that("jsumm and center_lm: centering works", {
  data(api)
  dstrat <- svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
  regmodel <- svyglm(meals/100 ~ ell + api00, design = dstrat,
                     family = quasibinomial)
  fit <- lm(Income ~ Frost + Illiteracy + Murder, data = as.data.frame(state.x77))

  expect_is(j_summ(fitgf, center = TRUE, n.sd = 2), "j_summ.glm")
  expect_is(j_summ(fit, center = TRUE, n.sd = 2), "j_summ.lm")
})

test_that("jsumm: Printing isn't borked", {
  data(api)
  dstrat <- svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
  regmodel <- svyglm(meals/100 ~ ell + api00, design = dstrat,
                     family = quasibinomial)
  regmodell <- svyglm(meals/100 ~ ell + api00, design = dstrat)
  fit <- lm(Income ~ Frost + Illiteracy + Murder, data = as.data.frame(state.x77))
  output <- rpois(100, 5)
  input <- log(output) + runif(100,0,1)
  fitgf <- glm(output ~ input, family = poisson)

  expect_output(print(j_summ(fitgf, vifs = TRUE)))
  expect_output(print(j_summ(fitgf, standardize = TRUE)))
  expect_output(print(j_summ(regmodel, standardize = TRUE, n.sd = 2)))
  expect_output(print(j_summ(regmodel, model.check = TRUE, vifs = TRUE)))
  expect_output(print(j_summ(regmodell, standardize = TRUE, n.sd = 2)))
  expect_output(print(j_summ(regmodell, model.check = TRUE, vifs = TRUE)))
  expect_output(print(j_summ(fit, standardize = TRUE, n.sd = 2)))
  expect_output(print(j_summ(fit, model.check = TRUE, vifs = TRUE)))

})
