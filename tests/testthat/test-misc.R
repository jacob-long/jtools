library(jtools)

states <- as.data.frame(state.x77)
states$HSGrad <- states$`HS Grad`
states$o70 <- 0
states$o70[states$`Life Exp` > 70] <- 1
set.seed(3)
states$wts <- runif(50, 0, 3)
fit <- lm(Income ~ HSGrad*Murder*Illiteracy, data = states)
fit2 <- lm(Income ~ HSGrad*o70, data = states)
fitw <- lm(Income ~ HSGrad*Murder*Illiteracy, data = states, weights = wts)


library(survey)
data(api)
dstrat <- svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
regmodel <- svyglm(api00~ell*meals*both,design=dstrat)

test_that("interact_plot works for lm", {
  expect_silent(interact_plot(model = fit,
                              pred = Murder,
                              modx = Illiteracy,
                              mod2 = HSGrad,
                              centered = "all"))
  expect_silent(interact_plot(model = fit,
                              pred = Murder,
                              modx = Illiteracy,
                              mod2 = HSGrad,
                              centered = "HSGrad"))
})

test_that("sim_slopes works for lm", {
  expect_silent(sim_slopes(model = fit,
                              pred = Murder,
                              modx = Illiteracy,
                              mod2 = HSGrad,
                              centered = "all"))
  expect_silent(sim_slopes(model = fit,
                              pred = Murder,
                              modx = Illiteracy,
                              mod2 = HSGrad,
                              centered = "HSGrad"))
})

test_that("sim_slopes works for weighted lm", {
  expect_s3_class(sim_slopes(model = fitw,
                           pred = Murder,
                           modx = Illiteracy,
                           mod2 = HSGrad,
                           centered = "all"), class = "sim_slopes")
  expect_s3_class(sim_slopes(model = fitw,
                                    pred = Murder,
                                    modx = Illiteracy,
                                    mod2 = HSGrad,
                                    centered = "all"), class = "sim_slopes")
})

test_that("interact_plot works for weighted lm", {
  expect_silent(interact_plot(model = fitw,
                             pred = Murder,
                             modx = Illiteracy,
                             mod2 = HSGrad,
                             centered = "all"))
  expect_silent(interact_plot(model = fitw,
                             pred = Murder,
                             modx = Illiteracy,
                             mod2 = HSGrad,
                             centered = "all"))
})

test_that("interact_plot accepts user-specified values and labels", {
  expect_silent(interact_plot(model = fit,
                              pred = Murder,
                              modx = Illiteracy,
                              mod2 = HSGrad,
                              centered = "all",
                              modxvals = c(0, 1, 3),
                              modx.labels = c("None","Low","High"),
                              mod2vals = c(40, 60, 80),
                              mod2.labels = c("Low","Average","High")))
  expect_silent(interact_plot(model = fit2,
                              pred = o70,
                              modx = HSGrad,
                              pred.labels = c("Under","Over")))
})

test_that("effect_plot works for lm", {
  expect_silent(effect_plot(model = fit,
                              pred = Murder,
                              centered = "all"))
  expect_silent(effect_plot(model = fit,
                              pred = Murder,
                              centered = "HSGrad"))
})

test_that("effect_plot works for weighted lm", {
  expect_silent(effect_plot(model = fitw,
                            pred = Murder,
                            centered = "all"))
  expect_silent(effect_plot(model = fitw,
                            pred = Murder,
                            centered = "HSGrad"))
})

test_that("interact_plot works for svyglm", {
  expect_silent(interact_plot(regmodel, pred = ell, modx = meals, mod2 = both,
                              centered = "all"))
  expect_silent(interact_plot(regmodel, pred = ell, modx = meals, mod2 = both,
                              centered = "ell"))
})


test_that("effect_plot works for svyglm", {
  expect_silent(effect_plot(regmodel, pred = meals,
                              centered = "all"))
  expect_silent(effect_plot(regmodel, pred = meals,
                              centered = "ell"))
})

library(lme4)
data(VerbAgg)
mv <- lmer(Anger ~ Gender*mode + btype +  (1 | item), data = VerbAgg)

test_that("interact_plot works for lme4", {
  expect_silent(interact_plot(mv, pred = mode, modx = Gender))
})

test_that("effect_plot works for lme4", {
  expect_silent(effect_plot(mv, pred = mode))
})

set.seed(100)
exposures <- rpois(50, 50)
counts <- exposures - rpois(50, 25)
money <- (counts/exposures) + rnorm(50, sd = 1)
talent <- counts*.5 + rnorm(50, sd = 3)
poisdat <- as.data.frame(cbind(exposures, counts, talent, money))
pmod <- glm(counts ~ talent*money, offset = log(exposures), data = poisdat,
            family = poisson)

test_that("interact_plot handles offsets", {
  expect_message(interact_plot(pmod, pred = talent, modx = money))
})

test_that("effect_plot handles offsets", {
  expect_message(effect_plot(pmod, pred = money))
})


