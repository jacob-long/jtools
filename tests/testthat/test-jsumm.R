library(jtools)

# GLM test
set.seed(1)
output <- rpois(100, 5)
input <- log(output) + runif(100,0,1)
fitgf <- glm(output ~ input, family = poisson)

# survey test
library(survey, quietly = TRUE)
data(api)
dstrat <- svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
dstrat$variables$mealsdec <- dstrat$variables$meals/100
regmodel <- svyglm(mealsdec ~ ell + api00, design = dstrat,
                   family = quasibinomial)
regmodell <- svyglm(mealsdec ~ ell + api00, design = dstrat)

# lm tests (OLS and WLS)
states <- as.data.frame(state.x77)
states$HSGrad <- states$`HS Grad`
set.seed(3)
states$wts <- runif(50, 0, 3)
fit <- lm(Income ~ HSGrad*Murder*Illiteracy, data = states)
fitw <- lm(Income ~ HSGrad*Murder*Illiteracy, data = states, weights = wts)

# merMod test
library(lme4, quietly = TRUE)
data(sleepstudy)
mv <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)

test_that("jsumm: non-linear models work", {
  expect_is(j_summ(fitgf), "j_summ.glm")
  expect_is(j_summ(fitgf, standardize = TRUE), "j_summ.glm")
  expect_is(j_summ(fitgf, center = TRUE), "j_summ.glm")
  expect_warning(j_summ(fitgf, robust = TRUE))
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

test_that("jsumm: svyglm robust std. error warning", {
  expect_warning(j_summ(regmodel, robust = TRUE))
})

test_that("jsumm and scale_lm: scaling works", {
  expect_is(j_summ(fitgf, standardize = TRUE, n.sd = 2), "j_summ.glm")
  expect_is(j_summ(fit, standardize = TRUE, n.sd = 2), "j_summ.lm")
})

test_that("jsumm and center_lm: centering works", {
  expect_is(j_summ(fitgf, center = TRUE, n.sd = 2), "j_summ.glm")
  expect_is(j_summ(fit, center = TRUE, n.sd = 2), "j_summ.lm")
})

test_that("jsumm and merMod objects: everything works", {
  expect_is(suppressWarnings(j_summ(mv, center = TRUE, n.sd = 2)), "j_summ.merMod")
  expect_is(j_summ(mv, standardize = TRUE, n.sd = 2), "j_summ.merMod")
  expect_warning(j_summ(mv, robust = TRUE))
})

test_that("jsumm can standardize weighted lms", {
  expect_is(j_summ(fitw, standardize = T, n.sd = 2, robust = TRUE), "j_summ.lm")
  expect_is(j_summ(fitw, center = T, robust = TRUE), "j_summ.lm")
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
  expect_output(print(j_summ(mv, standardize = TRUE, n.sd = 2)))

})
