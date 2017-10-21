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


suppressMessages(library(survey, quietly = TRUE))
data(api)
dstrat <- svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
regmodel <- svyglm(api00~ell*meals*both,design=dstrat)

set.seed(100)
exposures <- rpois(50, 50)
counts <- exposures - rpois(50, 25)
money <- (counts/exposures) + rnorm(50, sd = 1)
talent <- counts*.5 + rnorm(50, sd = 3)
poisdat <- as.data.frame(cbind(exposures, counts, talent, money))
pmod <- glm(counts ~ talent*money, offset = log(exposures), data = poisdat,
            family = poisson)
pmod_a <- glm(counts ~ talent*money, offset = log(exposures), data = poisdat,
            family = poisson)

library(lme4, quietly = TRUE)
data(VerbAgg)
mv <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)

# library(broom)
# library(huxtable)

test_that("Export doesn't fail with lm", {
  expect_is(export_summs(fit,fit2,fitw), "huxtable")
})

test_that("Export accepts summ args with lm", {
  expect_is(export_summs(fit,fit2,fitw, scale = T, robust = T),
            "huxtable")
})

test_that("Export accepts huxreg args with lm", {
  expect_is(export_summs(fit,fit2,fitw, pad_decimal = ","),
            "huxtable")
})

test_that("Export accepts huxreg and summ args with lm", {
  expect_is(export_summs(fit,fit2,fitw, pad_decimal = ",",
                         robust = T), "huxtable")
})

test_that("Export doesn't fail with glm", {
  expect_is(export_summs(pmod), "huxtable")
})

test_that("Export accepts summ args with glm", {
  expect_is(export_summs(pmod, scale = T, robust = T),
            "huxtable")
})

test_that("Export accepts huxreg args with glm", {
  expect_is(export_summs(pmod, pad_decimal = ","),
            "huxtable")
})

test_that("Export accepts huxreg and summ args with glm", {
  expect_is(export_summs(pmod, pad_decimal = ",",
                         robust = T), "huxtable")
})

test_that("Export doesn't fail with svyglm", {
  expect_is(export_summs(regmodel, statistics = c(N = "nobs")), "huxtable")
})

test_that("Export accepts summ args with svyglm", {
  expect_is(export_summs(regmodel, scale = T),
            "huxtable")
})

test_that("Export accepts huxreg args with svyglm", {
  expect_is(export_summs(regmodel, pad_decimal = ",",
                         statistics = c(N = "nobs")),
            "huxtable")
})

test_that("Export accepts huxreg and summ args with svyglm", {
  expect_is(export_summs(regmodel, pad_decimal = ",",
                         scale = T), "huxtable")
})

test_that("Export doesn't fail with merMod", {
  expect_is(export_summs(mv), "huxtable")
})

test_that("Export accepts summ args with merMod", {
  expect_is(export_summs(mv, scale = T),
            "huxtable")
})

test_that("Export accepts huxreg args with merMod", {
  expect_is(export_summs(mv, pad_decimal = ","),
            "huxtable")
})

test_that("Export accepts huxreg and summ args with merMod", {
  expect_is(export_summs(mv, pad_decimal = ",",
                         scale = T), "huxtable")
})

test_that("Export can do confidence intervals (merMod)", {
  expect_is(export_summs(mv,
            error_format = "95% CI [{conf.low}, {conf.high}]"), "huxtable")
})

test_that("Export can do confidence intervals (lm)", {
  expect_is(export_summs(fit, fitw,
                         error_format = "95% CI [{conf.low}, {conf.high}]"),
            "huxtable")
})

test_that("Export can do confidence intervals (glm)", {
  expect_is(export_summs(pmod,
                         error_format = "95% CI [{conf.low}, {conf.high}]"),
            "huxtable")
})

test_that("Export can do confidence intervals (svyglm)", {
  expect_is(export_summs(regmodel,
                         error_format = "95% CI [{conf.low}, {conf.high}]"),
            "huxtable")
})

test_that("Export can take manual coefficient names", {
  expect_is(export_summs(fit,fit2,fitw,
                         coefs = c("HS Grad %" = "HSGrad",
                          "Murder Rate" = "Murder")), "huxtable")
})

#### plot_summs ############################################################

test_that("plot_summs doesn't fail with lm", {
  expect_is(p <- plot_summs(fit,fit2,fitw), "ggplot")
  expect_silent(print(p))
})

test_that("plot_summs accepts summ args with lm", {
  expect_is(p <- plot_summs(fit,fit2,fitw, scale = T, robust = T),
   "ggplot")
  expect_silent(print(p))
})

test_that("plot_summs works with glm", {
  expect_is(p <- plot_summs(pmod, pmod_a), "ggplot")
  expect_silent(print(p))
})

test_that("plot_summs accepts summ args with glm", {
  expect_is(p <- plot_summs(pmod, pmod_a, scale = T, robust = T),
   "ggplot")
  expect_silent(print(p))
})

test_that("plot_summs accepts odds ratios with glm", {
  expect_is(p <- plot_summs(pmod, pmod_a, scale = T, robust = T),
   "ggplot")
  expect_silent(print(p))
})

test_that("plot_summs works with svyglm", {
  expect_is(p <- plot_summs(regmodel), "ggplot")
  expect_silent(print(p))
})

test_that("plot_summs accepts summ args with svyglm", {
  expect_is(p <- plot_summs(regmodel, scale = T), "ggplot")
  expect_silent(print(p))
})

# The message expected is the "calculating confidence intervals..." from lme4
test_that("plot_summs works with lmer", {
  expect_is(p <- plot_summs(mv), "ggplot")
  expect_silent(print(p))
})

test_that("plot_summs accepts summ args with lmer", {
  expect_is(p <- plot_summs(mv, scale = T), "ggplot")
  expect_silent(print(p))
})

test_that("plot_summs can take manual coefficient names", {
  expect_is(p <- plot_summs(fit,fit2,fitw,
                         coefs = c("HS Grad %" = "HSGrad",
                          "Murder Rate" = "Murder")), "ggplot")
  expect_silent(print(p))
})

test_that("plot_summs can omit coefficients", {
  expect_is(p <- plot_summs(fit,fit2,fitw,
                         coefs = c("HSGrad","Murder")), "ggplot")
  expect_silent(print(p))
})

test_that("plot_coefs can take manual coefficient names", {
  expect_is(p <- plot_coefs(fit,fit2,fitw,
                         coefs = c("HS Grad %" = "HSGrad",
                          "Murder Rate" = "Murder")), "ggplot")
  expect_silent(print(p))
})

test_that("plot_coefs can omit coefficients", {
  expect_is(p <- plot_coefs(fit,fit2,fitw,
                         coefs = c("HSGrad","Murder")), "ggplot")
  expect_silent(print(p))
})

test_that("plot_coefs works", {
  expect_is(p <- plot_coefs(fit, pmod), "ggplot")
  expect_silent(print(p))
})
