library(jtools)

# GLM test
set.seed(1)
output <- rpois(100, 5)
input <- log(output) + runif(100,0,1)
fitgf <- glm(output ~ input, family = poisson)
clusters <- sample(1:5, size = 100, replace = TRUE)

# survey test
suppressMessages(library(survey, quietly = TRUE))
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
  expect_is(summ(fitgf), "summ.glm")
  expect_is(summ(fitgf, standardize = TRUE), "summ.glm")
  expect_is(summ(fitgf, center = TRUE), "summ.glm")
  # expect_warning(summ(fitgf, robust = TRUE))
})

test_that("jsumm: partial correlations work", {
  expect_is(summ(fit, part.corr = TRUE), "summ.lm")
  expect_output(print(summ(fit, part.corr = TRUE)))
  expect_warning(summ(fit, part.corr = TRUE, robust = TRUE))
})

test_that("jsumm: non-linear svyglm models work", {
  expect_is(summ(regmodel), "summ.svyglm")
})

test_that("jsumm: svyglm vifs work", {
  expect_is(summ(regmodel, vifs = TRUE), "summ.svyglm")
})

test_that("jsumm: svyglm linear model check works", {
  expect_is(summ(regmodel, model.check = TRUE), "summ.svyglm")
})

test_that("jsumm: lm CIs work", {
  expect_is(summ(fit, confint = TRUE), "summ.lm")
  expect_output(print(summ(fit, confint = TRUE)))
})

test_that("jsumm: glm CIs work", {
  expect_is(summ(fitgf, confint = TRUE), "summ.glm")
  expect_output(print(summ(fitgf, confint = TRUE)))
})

test_that("jsumm: svyglm CIs work", {
  expect_is(summ(regmodel, confint = TRUE), "summ.svyglm")
  expect_output(print(summ(regmodel, confint = TRUE)))
})

test_that("jsumm: merMod CIs work", {
  expect_is(summ(mv, confint = TRUE), "summ.merMod")
  expect_output(print(summ(mv, confint = TRUE)))
})

test_that("jsumm: lm dropping pvals works", {
  expect_is(summ(fit, pvals = FALSE), "summ.lm")
  expect_output(print(summ(fit, pvals = FALSE)))
})

test_that("jsumm: glm dropping pvals works", {
  expect_is(summ(fitgf, pvals = FALSE), "summ.glm")
  expect_output(print(summ(fitgf, pvals = FALSE)))
})

test_that("jsumm: svyglm dropping pvals works", {
  expect_is(summ(regmodel, pvals = FALSE), "summ.svyglm")
  expect_output(print(summ(regmodel, pvals = FALSE)))
})

test_that("jsumm: merMod dropping pvals works", {
  expect_is(summ(mv, pvals = FALSE), "summ.merMod")
  expect_output(print(summ(mv, pvals = FALSE)))
})

test_that("jsumm and scale_lm: scaling works", {
  expect_is(summ(fitgf, standardize = TRUE, n.sd = 2), "summ.glm")
  expect_is(summ(fit, standardize = TRUE, n.sd = 2), "summ.lm")
})

test_that("jsumm and center_lm: centering works", {
  expect_is(summ(fitgf, center = TRUE, n.sd = 2), "summ.glm")
  expect_is(summ(fit, center = TRUE, n.sd = 2), "summ.lm")
})

test_that("jsumm and merMod objects: everything works", {
  expect_is(suppressWarnings(summ(mv, center = TRUE, n.sd = 2)), "summ.merMod")
  expect_is(summ(mv, standardize = TRUE, n.sd = 2), "summ.merMod")
  expect_warning(summ(mv, robust = TRUE))
})

test_that("jsumm can standardize weighted lms", {
  expect_is(summ(fitw, standardize = T, n.sd = 2, robust = TRUE), "summ.lm")
  expect_is(summ(fitw, center = T, robust = TRUE), "summ.lm")
})

test_that("jsumm: lm robust SEs work", {
  expect_is(summ(fit, robust = T), "summ.lm")
  expect_is(summ(fit, robust = T, robust.type = "HC4m"), "summ.lm")
  expect_output(print(summ(fit, robust = T, robust.type = "HC4m")))
})

test_that("jsumm: glm robust SEs work", {
  expect_is(summ(fitgf, robust = T), "summ.glm")
  expect_is(summ(fitgf, robust = T, robust.type = "HC4m"), "summ.glm")
  expect_output(print(summ(fitgf, robust = T, robust.type = "HC4m")))
})

test_that("jsumm: lm cluster-robust SEs work", {
  expect_is(summ(fit, robust = T, cluster = "Population"), "summ.lm")
  expect_output(print(summ(fit, robust = T, cluster = "Population")))
  expect_error(summ(fit, robust = T, robust.type = "HC4m",
                        cluster = "Population"))
})

test_that("jsumm: glm cluster-robust SEs work", {
  expect_is(summ(fitgf, robust = T, cluster = clusters), "summ.glm")
  expect_output(print(summ(fitgf, robust = T, cluster = clusters)))
  expect_error(summ(fitgf, robust = T, robust.type = "HC4m",
                        cluster = clusters))
})

test_that("jsumm: Printing isn't borked", {
  expect_output(print(summ(fitgf, vifs = TRUE)))
  expect_output(print(summ(fitgf, standardize = TRUE)))
  expect_output(print(summ(regmodel, standardize = TRUE, n.sd = 2)))
  expect_output(print(summ(regmodel, model.check = TRUE, vifs = TRUE)))
  expect_output(print(summ(regmodell, standardize = TRUE, n.sd = 2)))
  expect_output(print(summ(regmodell, model.check = TRUE, vifs = TRUE)))
  expect_output(print(summ(fit, standardize = TRUE, n.sd = 2)))
  expect_output(print(summ(fit, model.check = TRUE, vifs = TRUE)))
  expect_output(print(summ(mv, standardize = TRUE, n.sd = 2)))

})
