context("interactions lm")

device <- getOption("device")
options(device = "pdf")

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

if (requireNamespace("survey")) {
  suppressMessages(library(survey, quietly = TRUE))
  data(api)
  dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat,
                      fpc = ~fpc)
  regmodel <- svyglm(api00 ~ ell * meals * both + sch.wide, design = dstrat)
}

context("effect_plot svyglm")

if (requireNamespace("survey")) {

  test_that("effect_plot works for svyglm", {
    expect_silent(p <- effect_plot(regmodel, pred = meals,
                                centered = "all"))
    expect_silent(print(p))
    expect_silent(p <- effect_plot(regmodel, pred = meals,
                                centered = "ell"))
    expect_silent(print(p))
  })
}

context("effect_plot merMod")

if (requireNamespace("lme4")) {
  library(lme4, quietly = TRUE)
  data(VerbAgg)
  VerbAgg$mode_numeric <- as.numeric(VerbAgg$mode)
  mve <- lmer(Anger ~ Gender * mode + btype +  (1 | item), data = VerbAgg)
  mv <- lmer(Anger ~ Gender * mode_numeric + btype +  (1 | item),
             data = VerbAgg)
  gm <- glmer(incidence ~ period + (1 | herd), family = poisson, data = cbpp,
              offset = log(size))

  test_that("effect_plot works for lme4", {
    # expect_error(effect_plot(mve, pred = mode))
    expect_silent(p <- effect_plot(mv, pred = mode_numeric))
    expect_silent(print(p))
    expect_message(p <- effect_plot(mv, pred = mode_numeric, interval = TRUE))
    expect_silent(print(p))
  })

}

context("interactions offsets")

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

### Code used to create brmsfit and stanreg test objects
# library(brms)
# fit1 <- brm(count ~ log_Age_c + log_Base4_c * Trt
#                    + (1 | patient) + (1 | obs),
#                  data = epilepsy, family = poisson(),
#                  prior = c(prior(student_t(5,0,10), class = b),
#                            prior(cauchy(0,2), class = sd)),
#                  cores = 2, chains = 2, iter = 2000)
# saveRDS(fit1, "brmfit.rds")
#
# library(rstanarm)
# fitrs <- stan_glmer(incidence ~ size * as.numeric(period) + (1 | herd),
#                   data = lme4::cbpp, family = poisson,
#                   # this next line is only to keep the example small in size!
#                   chains = 2, cores = 2, seed = 12345, iter = 500)
# saveRDS(fitrs, "rsafit.rds")

#### brms and rstanarm tests #################################################

if (requireNamespace("brms")) {
  context("brmsfit plots")
  bfit <- readRDS("brmfit.rds")
  test_that("brmsfit objects are supported", {
    expect_silent(print(effect_plot(bfit, pred = "log_Base4_c",
                  interval = TRUE)))
    # expect_silent(print(cat_plot(bfit, pred = "Trt",
    #               interval = TRUE)))
    # expect_silent(print(interact_plot(bfit, pred = "log_Base4_c", modx = "Trt",
    #               interval = TRUE)))
    expect_is(make_predictions(bfit, pred = "log_Base4_c", modx = "Trt",
                               interval = TRUE, estimate = "median"),
                               "data.frame")
  })
}

if (requireNamespace("rstanarm") & requireNamespace("lme4")) {
  context("stanreg plots")
  rsfit <- readRDS("rsafit.rds")
  library(lme4)
  data(cbpp)
  test_that("stanreg objects are supported", {
    expect_message(print(effect_plot(rsfit, pred = "size", interval = TRUE)))
    # expect_silent(print(interact_plot(rsfit, pred = "size",
    #   modx = "period", interval = TRUE, data = cbpp)))
    # expect_is(make_predictions(rsfit, pred = "size", interval = TRUE,
    #   estimate = "median", data = cbpp), "predictions")
  })
}

### effect_plot ###############################################################

context("effect_plot")

test_that("effect_plot works for lm", {
  expect_silent(p <- effect_plot(model = fit,
                                 pred = Murder,
                                 centered = "all"))
  expect_silent(print(p))
  expect_silent(p <- effect_plot(model = fit,
                                 pred = Murder,
                                 centered = "HSGrad"))
  expect_silent(print(p))
})

test_that("effect_plot: robust intervals works", {
  expect_silent(p <- effect_plot(model = fit,
                                 pred = Murder,
                                 centered = "HSGrad",
                                 robust = TRUE))
  expect_silent(print(p))
})

test_that("effect_plot: rug plots work", {
  expect_silent(p <- effect_plot(model = fit,
                                 pred = Murder,
                                 centered = "HSGrad",
                                 rug = TRUE))
  expect_silent(print(p))
  expect_silent(p <- effect_plot(model = fit,
                                 pred = Murder,
                                 centered = "HSGrad",
                                 rug = TRUE,
                                 rug.sides = "lb"))
  expect_silent(print(p))
})

test_that("effect_plot works for weighted lm", {
  expect_silent(p <- effect_plot(model = fitw,
                                 pred = Murder,
                                 centered = "all"))
  expect_silent(print(p))
  expect_silent(p <- effect_plot(model = fitw,
                                 pred = Murder,
                                 centered = "HSGrad"))
  expect_silent(print(p))
  expect_silent(p <- effect_plot(model = fitw,
                                 pred = Murder,
                                 centered = "HSGrad",
                                 robust = TRUE))
  expect_silent(print(p))
})

if (requireNamespace("survey")) {
  test_that("effect_plot works for svyglm", {
    expect_silent(p <- effect_plot(regmodel, pred = meals, centered = "all"))
    expect_silent(print(p))
    expect_silent(p <- effect_plot(regmodel, pred = meals, centered = "ell"))
    expect_silent(print(p))
  })
}

if (requireNamespace("lme4")) {
  test_that("effect_plot works for lme4", {
    expect_silent(p <- effect_plot(mv, pred = mode_numeric))
    expect_silent(print(p))
  })
}

test_that("effect_plot handles offsets", {
  expect_message(p <- effect_plot(pmod, pred = money))
  expect_silent(print(p))
})

context("effect_plot with categorical variables")
library(ggplot2)
diamond <- diamonds
diamond <- diamond[diamond$color != "D",]
set.seed(10)
samps <- sample(1:nrow(diamond), 2000)
diamond <- diamond[samps,]
fit <- lm(price ~ cut * color, data = diamond)

test_that("effect_plot handles offsets w/ categorical predictors", {
  expect_s3_class(p <- effect_plot(pmod, pred = talent_f), "gg")
  expect_silent(print(p))
  expect_s3_class(p <- effect_plot(pmod, pred = talent_f, plot.points = TRUE), "gg")
  expect_silent(print(p))
})

test_that("effect_plot does line plots", {
  expect_s3_class(p <- effect_plot(pmod, pred = talent_f, cat.geom = "line"),
                  "gg")
  expect_silent(print(p))
  expect_s3_class(p <- effect_plot(pmod, pred = talent_f, cat.geom = "line",
                                   interval = FALSE), "gg")
  expect_silent(print(p))
})

test_that("effect_plot does bar plots", {
  expect_s3_class(p <- effect_plot(pmod, pred = talent_f, cat.geom = "bar"),
                  "gg")
  expect_silent(print(p))
  expect_s3_class(p <- effect_plot(pmod, pred = talent_f, cat.geom = "bar",
                                   interval = FALSE), "gg")
  expect_silent(print(p))
})

options(device = device)
# dev.off()
