context("effect_plot")

library(vdiffr)
library(ggplot2)
states <- as.data.frame(state.x77)
states$HSGrad <- states$`HS Grad`
states$o70 <- 0
states$o70[states$`Life Exp` > 70] <- 1
states$o70n <- states$o70
states$o70 <- factor(states$o70)
states$o70l <- states$`Life Exp` > 70
states$o70c <- ifelse(states$o70l, yes = "yes", no = "no")
set.seed(3)
states$wts <- runif(50, 0, 3)
fit <- lm(Income ~ HSGrad*Murder*Illiteracy + o70 + Area, data = states)
fit2 <- lm(Income ~ HSGrad*o70, data = states)
fit2n <- lm(Income ~ HSGrad*o70n, data = states)
fitw <- lm(Income ~ HSGrad*Murder*Illiteracy + o70 + Area, data = states,
           weights = wts)
fitl <- lm(Income ~ HSGrad*o70l, data = states)
fitc <- lm(Income ~ HSGrad*Murder + o70c, data = states)

test_that("effect_plot works for lm", {
  p <- effect_plot(model = fit, pred = Murder,centered = "all") +
    ggtitle("All centered")
  expect_doppelganger("lm-all-centered", p)
  p <- effect_plot(model = fit, pred = Murder,centered = "all") +
    ggtitle("All centered APA") + theme_apa()
  expect_doppelganger("lm-all-centered-apa", p)
  p <- effect_plot(model = fit,pred = Murder, centered = "HSGrad") +
    ggtitle("HSGrad centered")
  expect_doppelganger("lm-one-centered", p)
})

test_that("effect_plot: robust intervals works", {
  p <- effect_plot(model = fit, pred = Murder, centered = "HSGrad", 
    robust = TRUE) + 
    ggtitle("Robust intervals")
  expect_doppelganger("lm-robust", p)
})

test_that("effect_plot: rug plots work", {
  p <- effect_plot(model = fit, pred = Murder, centered = "HSGrad", rug = TRUE) +
    ggtitle("Rug default")
  expect_doppelganger("lm-rug", p)
  p <- effect_plot(model = fit, pred = Murder, centered = "HSGrad", rug = TRUE,
    rug.sides = "lb") + ggtitle("Rug sides 'lb'")
  expect_doppelganger("lm-rug-sides-lb", p)
})

test_that("effect_plot: plot.points works", {
  p <- effect_plot(model = fit, pred = Murder, centered = "HSGrad", 
    plot.points = TRUE) +
    ggtitle("Plot points HSGrad centered")
  expect_doppelganger("lm-points-one-centered", p)
  p <- effect_plot(model = fit, pred = Murder, centered = "all", 
    plot.points = TRUE) +
    ggtitle("Plot points all centered")
  expect_doppelganger("lm-points-all-centered", p)
})

test_that("effect_plot: partial residuals work", {
  p <- effect_plot(model = fit, pred = Murder, centered = "HSGrad", 
    partial.residuals = TRUE) +
    ggtitle("Partial residuals HSGrad centered")
  expect_doppelganger("lm-partials-one-centered", p)
  p <- effect_plot(model = fit, pred = Murder, centered = "all", 
    partial.residuals = TRUE) +
    ggtitle("Partial residuals all centered")
  expect_doppelganger("lm-partials-all-centered", p)
})

test_that("effect_plot works for weighted lm", {
  p <- effect_plot(model = fitw, pred = Murder, centered = "all") +
    ggtitle("Weighted lm")
  expect_doppelganger("wlm-all-centered", p)
  p <- effect_plot(model = fitw, pred = Murder, centered = "HSGrad") +
    ggtitle("Weighted lm HSGrad centered")
  expect_doppelganger("wlm-one-centered", p)
  p <- effect_plot(model = fitw, pred = Murder, centered = "HSGrad",
    robust = TRUE, interval = TRUE) + 
    ggtitle("Weighted lm HSGrad centered and robust intervals")
  expect_doppelganger("wlm-one-centered-robust", p)
  p <- effect_plot(model = fitw, pred = Murder, centered = "HSGrad", 
    robust = TRUE, interval = TRUE, plot.points = TRUE) +
    ggtitle("Weighted lm HSGrad centered, robust, plot points")
  expect_doppelganger("wlm-one-centered-robust-points", p)
  p <- effect_plot(model = fitw, pred = Murder, centered = "HSGrad", 
    robust = TRUE, interval = TRUE, partial.residuals = TRUE) + 
    ggtitle("Weighted lm HSGrad centered, robust, partial residuals")
  expect_doppelganger("wlm-one-centered-robust-partials", p)
})

if (requireNamespace("survey")) {
  context("effect_plot survey")
  library(survey, quietly = TRUE)
  data(api)
  dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw,
                      data = apistrat, fpc = ~fpc)
  regmodel <- svyglm(api00 ~ ell * meals * both + sch.wide, design = dstrat)
  svyqb <- svyglm(sch.wide~ell+meals+mobility, design=dstrat,
                  family=quasibinomial)
  
  test_that("effect_plot works for svyglm", {
    p <- effect_plot(regmodel, pred = meals, centered = "all") + 
      ggtitle("svyglm")
    expect_doppelganger("svyglm", p)
    p <- effect_plot(regmodel, pred = meals, centered = "ell") +
      ggtitle("svyglm ell centered")
    expect_doppelganger("svyglm-one-centered", p)
    p <- effect_plot(regmodel, pred = meals, centered = "all", 
      plot.points = TRUE) + 
      ggtitle("svyglm plot points")
    expect_doppelganger("svyglm-all-centered-points", p)
    p <- effect_plot(regmodel, pred = meals, centered = "all",
      partial.residuals = TRUE) + 
      ggtitle("svyglm partial residuals")
    expect_doppelganger("svyglm-all-centered-partials", p)
    p <- effect_plot(svyqb, pred = mobility, centered = "all") + 
      ggtitle("svyglm binomial")
    expect_doppelganger("svyglm-binomial", p)
  })
}

if (requireNamespace("lme4")) {
  context("effect_plot lme4")
  library(lme4, quietly = TRUE)
  data(VerbAgg)
  va <- VerbAgg[sample(seq_len(nrow(VerbAgg)), 500),]
  fmVA0 <- glmer(r2 ~ Anger + Gender + btype + situ + (1|id) + (1|item), 
                 family = binomial, data = va, nAGQ=0L)
  lmVA0 <- lmer(as.numeric(r2 == "Y") ~ Anger + Gender + btype + situ + (1|id) + (1|item), 
                data = va)
  gm <- glmer(incidence ~ as.numeric(period) + (1 | herd), family = poisson,
              data = cbpp, offset = log(size))
  test_that("effect_plot works for lme4", {
    p <- effect_plot(lmVA0, pred = Anger, data = va) +
      ggtitle("lmer test")
    expect_doppelganger("lmer", p)
    p <- effect_plot(lmVA0, pred = Anger, plot.points = T, data = va) +
      ggtitle("lmer test + plot points")
    expect_doppelganger("lmer-points", p)
    p <- effect_plot(lmVA0, pred = Anger, partial.residuals = T, 
      data = va) +
      ggtitle("lmer test + partial residuals")
    expect_doppelganger("lmer-partials", p)
    p <- effect_plot(fmVA0, pred = Anger, data = va) +
      ggtitle("glmer test")
    expect_doppelganger("glmer", p)
    expect_message(p <- effect_plot(gm, pred = period, data = cbpp) +
                    ggtitle("glmer + offset test"))
    expect_doppelganger("glmer-offset", p)
  })
  context("lme4 predictions")
  test_that("predict_mer works with random effects", {
    expect_equal(head(jtools:::predict_mer(lmVA0, newdata = va, 
                                           type = "response")), 
                 head(predict(lmVA0, newdata = va, type = "response")))
  })
}

context("effect_plot offsets")

set.seed(1)
n <- 50
cov <- 10
x <- rnorm(n, 0, 0.2)
p <- 0.4 + 0.2*x
y <- rbinom(n, cov, p)
bindat <- data.frame(cbind(x, p, y, t = 10))
binglm <- glm(cbind(y, t-y) ~ x, data = bindat, family = binomial)

set.seed(100)
exposures <- rpois(50, 50)
counts <- exposures - rpois(50, 25)
money <- (counts/exposures) + rnorm(50, sd = 1)
talent <- counts*.5 + rnorm(50, sd = 3)
talent_f <- rbinom(50, 1, .5)
poisdat <- as.data.frame(cbind(exposures, counts, talent, talent_f, money))
poisdat$talent_f <- factor(poisdat$talent_f)
pmod <- glm(counts ~ talent*money + talent_f, offset = log(exposures),
            data = poisdat, family = poisson)

test_that("effect_plot handles two-column DVs", {
  p <- effect_plot(binglm, x, interval = T)
  expect_doppelganger("glm-bin-2col", p)
})

test_that("effect_plot handles offsets", {
  expect_message(p <- effect_plot(pmod, pred = money) + ggtitle("offset"))
  expect_doppelganger("glm-offset", p)
})

test_that("effect_plot handles plot points with offsets", {
  expect_message(p <- effect_plot(pmod, pred = money, plot.points = TRUE) +
                   ggtitle("offset plus plot points"))
  expect_doppelganger("glm-offset-points", p)
})


test_that("effect_plot handles partial residuals with offsets", {
  expect_message(p <- effect_plot(pmod, pred = money, partial.residuals = TRUE) +
                   ggtitle("offset plus partials"))
  expect_doppelganger("glm-offset-partials", p)
})


context("effect_plot categorical predictors")

test_that("effect_plot handles offsets w/ categorical predictors", {
  p <- effect_plot(pmod, pred = talent_f) + 
    ggtitle("categorical (plus offset)")
  expect_doppelganger("glm-cat-offset", p)
  expect_error(p <- effect_plot(pmod, pred = talent_f, int.type = "prediction"))
  p <- effect_plot(pmod, pred = talent_f, plot.points = TRUE) +
    ggtitle("categorical, offset, plot points")
  expect_doppelganger("glm-cat-offset-points", p)
  p <- effect_plot(pmod, pred = talent_f, partial.residuals = TRUE) +
    ggtitle("categorical, offset, partial residuals")
  expect_doppelganger("glm-cat-offset-partials", p)
})

test_that("effect_plot does line plots", {
  p <- effect_plot(pmod, pred = talent_f, cat.geom = "line") +
    ggtitle("categorical line plot")
  expect_doppelganger("glm-cat-line-offset", p)
  p <- effect_plot(pmod, pred = talent_f, cat.geom = "line", interval = FALSE) +
    ggtitle("categorical line plot no intervals")
  expect_doppelganger("glm-cat-line-offset-no-int", p)
})

test_that("effect_plot does bar plots", {
  p <- effect_plot(pmod, pred = talent_f, cat.geom = "bar") +
    ggtitle("categorical bar plot")
  expect_doppelganger("glm-cat-bar-offset", p)
  p <- effect_plot(pmod, pred = talent_f, cat.geom = "bar", interval = FALSE) +
    ggtitle("categorical bar plot no intervals")
  expect_doppelganger("glm-cat-bar-offset-no-int", p)
})

### Code used to create brmsfit and stanreg test objects
# library(brms)
# data(epilepsy)
# bprior1 <- prior(student_t(5,0,10), class = b) +
#   prior(cauchy(0,2), class = sd)
# fit1 <- brm(count ~ zAge + zBase * Trt + (1|patient),
#             data = epilepsy, family = poisson(), prior = bprior1,
#             cores = 2, chains = 1, iter = 500, save_dso = FALSE)
# saveRDS(fit1, "brmfit.rds", version = 2)
#
# library(rstanarm)
# fitrs <- stan_glmer(incidence ~ size * as.numeric(period) + (1 | herd),
#                   data = lme4::cbpp, family = poisson,
#                   # this next line is only to keep the example small in size!
#                   chains = 2, cores = 2, seed = 12345, iter = 500)
# saveRDS(fitrs, "rsafit.rds")
# fitmv <- brm(
#   bf(mpg ~ cyl + wt, sigma ~ cyl + wt) + bf(wt ~ cyl + mpg, sigma ~ cyl + mpg) +
#     set_rescor(FALSE),
#   data = mtcars, chains = 2, cores = 2, iter = 500
# )
# saveRDS(fitmv, "mvfit.rds")

#### brms and rstanarm tests #################################################

if (requireNamespace("brms")) {
  context("brmsfit plots")
  bfit <- readRDS("brmfit.rds")
  mvfit <- readRDS("mvfit.rds")
  test_that("brmsfit objects are supported", {
    p <- effect_plot(bfit, pred = "zBase", interval = TRUE) + ggtitle("brms")
    expect_doppelganger("brm", p)
    expect_is(make_predictions(bfit, pred = "zBase", at = list("Trt" = c(0,1)),
                               interval = TRUE, estimate = "median"),
                               "data.frame")
  })
  test_that("brmsfit multivariate models work", {
    p <- effect_plot(mvfit, pred = "cyl", interval = TRUE) + 
      ggtitle("mv default dv mpg")
    expect_doppelganger("brm-multiv-default-int", p)
    p <- effect_plot(mvfit, pred = "cyl", interval = TRUE, resp = "wt") +
      ggtitle("mv selected dv wt")
    expect_doppelganger("brm-multiv-selected-int", p)
  })
  test_that("brmsfit distributional models work", {
    p <- effect_plot(mvfit, pred = "cyl", interval = TRUE, dpar = "sigma") +
      ggtitle("mv default dv mpg sigma")
    expect_doppelganger("brm-multiv-default-sigma-int", p)
    p <- effect_plot(mvfit, pred = "cyl", interval = TRUE, resp = "wt", 
      dpar = "sigma") +
      ggtitle("mv selected dv wt sigma")
    expect_doppelganger("brm-multiv-selected-sigma-int", p)
    expect_warning(p <- effect_plot(mvfit, pred = "cyl", interval = TRUE,
                                    dpar = "sigma", resp = "mpg", 
                                    plot.points = TRUE) + 
                          ggtitle("mv select dv mpg sigma warning"))
    expect_error(make_predictions(mvfit, pred = "cyl", interval = TRUE,
                                  resp = "mpg", dpar = "sigma",
                                  partial.residuals = TRUE))
  })
}

if (requireNamespace("rstanarm") & requireNamespace("lme4")) {
  context("stanreg plots")
  set.seed(100)
  rsfit <- readRDS("rsafit.rds")
  library(lme4)
  data(cbpp)
  test_that("stanreg objects are supported", {
    p <- effect_plot(rsfit, pred = "size", interval = TRUE, data = lme4::cbpp) +
      ggtitle("stanreg")
    expect_doppelganger("rstanarm", p)
    expect_s3_class(make_predictions(rsfit, pred = "size", interval = TRUE,
      estimate = "median", data = cbpp), "data.frame")
  })
  test_that("facet.by works with rstanarm", {
    p <- effect_plot(rsfit, pred = "size", interval = TRUE, data = lme4::cbpp,
                     facet.by = "herd") +
         ggtitle("stanreg facet.by")
    expect_doppelganger("rstanarm-facet", p)
  })
}

