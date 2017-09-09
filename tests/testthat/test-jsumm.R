library(jtools)

# GLM test
set.seed(1)
output <- rpois(100, 5)
input <- log(output) + runif(100,0,1)
fitgf <- glm(output ~ input, family = poisson)
clusters <- sample(1:5, size = 100, replace = TRUE)

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
  # expect_warning(j_summ(fitgf, robust = TRUE))
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

test_that("jsumm: lm CIs work", {
  expect_is(j_summ(fit, confint = TRUE), "j_summ.lm")
  expect_output(print(j_summ(fit, confint = TRUE)))
})

test_that("jsumm: glm CIs work", {
  expect_is(j_summ(fitgf, confint = TRUE), "j_summ.glm")
  expect_output(print(j_summ(fitgf, confint = TRUE)))
})

test_that("jsumm: svyglm CIs work", {
  expect_is(j_summ(regmodel, confint = TRUE), "j_summ.svyglm")
  expect_output(print(j_summ(regmodel, confint = TRUE)))
})

test_that("jsumm: merMod CIs work", {
  expect_is(j_summ(mv, confint = TRUE), "j_summ.merMod")
  expect_output(print(j_summ(mv, confint = TRUE)))
})

test_that("jsumm: lm dropping pvals works", {
  expect_is(j_summ(fit, pvals = FALSE), "j_summ.lm")
  expect_output(print(j_summ(fit, pvals = FALSE)))
})

test_that("jsumm: glm dropping pvals works", {
  expect_is(j_summ(fitgf, pvals = FALSE), "j_summ.glm")
  expect_output(print(j_summ(fitgf, pvals = FALSE)))
})

test_that("jsumm: svyglm dropping pvals works", {
  expect_is(j_summ(regmodel, pvals = FALSE), "j_summ.svyglm")
  expect_output(print(j_summ(regmodel, pvals = FALSE)))
})

test_that("jsumm: merMod dropping pvals works", {
  expect_is(j_summ(mv, pvals = FALSE), "j_summ.merMod")
  expect_output(print(j_summ(mv, pvals = FALSE)))
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

test_that("jsumm: lm robust SEs work", {
  expect_is(j_summ(fit, robust = T), "j_summ.lm")
  expect_is(j_summ(fit, robust = T, robust.type = "HC4m"), "j_summ.lm")
  expect_output(print(j_summ(fit, robust = T, robust.type = "HC4m")))
})

test_that("jsumm: glm robust SEs work", {
  expect_is(j_summ(fitgf, robust = T), "j_summ.glm")
  expect_is(j_summ(fitgf, robust = T, robust.type = "HC4m"), "j_summ.glm")
  expect_output(print(j_summ(fitgf, robust = T, robust.type = "HC4m")))
})

test_that("jsumm: lm cluster-robust SEs work", {
  expect_is(j_summ(fit, robust = T, cluster = "Population"), "j_summ.lm")
  expect_output(print(j_summ(fit, robust = T, cluster = "Population")))
  expect_error(j_summ(fit, robust = T, robust.type = "HC4m",
                        cluster = "Population"))
})

test_that("jsumm: glm cluster-robust SEs work", {
  expect_is(j_summ(fitgf, robust = T, cluster = clusters), "j_summ.glm")
  expect_output(print(j_summ(fitgf, robust = T, cluster = clusters)))
  expect_error(j_summ(fitgf, robust = T, robust.type = "HC4m",
                        cluster = clusters))
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
