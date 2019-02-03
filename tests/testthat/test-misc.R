context("effect_plot")

device <- getOption("device")
options(device = "pdf")
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
  expect_silent(p <- effect_plot(model = fit,
                                 pred = Murder,
                                 centered = "all") + ggtitle("All centered"))
  expect_silent(print(p))
  expect_silent(p <- effect_plot(model = fit,
                                 pred = Murder,
                                 centered = "HSGrad") +
                  ggtitle("HSGrad centered"))
  expect_silent(print(p))
})

test_that("effect_plot: robust intervals works", {
  expect_silent(p <- effect_plot(model = fit,
                                 pred = Murder,
                                 centered = "HSGrad",
                                 robust = TRUE) + ggtitle("Robust intervals"))
  expect_silent(print(p))
})

test_that("effect_plot: rug plots work", {
  expect_silent(p <- effect_plot(model = fit,
                                 pred = Murder,
                                 centered = "HSGrad",
                                 rug = TRUE) + ggtitle("Rug default"))
  expect_silent(print(p))
  expect_silent(p <- effect_plot(model = fit,
                                 pred = Murder,
                                 centered = "HSGrad",
                                 rug = TRUE,
                                 rug.sides = "lb") + ggtitle("Rug sides 'lb'"))
  expect_silent(print(p))
})

test_that("effect_plot: plot.points works", {
  expect_silent(p <- effect_plot(model = fit,
                                 pred = Murder,
                                 centered = "HSGrad",
                                 plot.points = TRUE) +
                  ggtitle("Plot points HSGrad centered"))
  expect_silent(print(p))
  expect_silent(p <- effect_plot(model = fit,
                                 pred = Murder,
                                 centered = "all",
                                 plot.points = TRUE) +
                  ggtitle("Plot points all centered"))
  expect_silent(print(p))
})

test_that("effect_plot: partial residuals work", {
  expect_silent(p <- effect_plot(model = fit,
                                 pred = Murder,
                                 centered = "HSGrad",
                                 partial.residuals = TRUE) +
                  ggtitle("Partial residuals HSGrad centered"))
  expect_silent(print(p))
  expect_silent(p <- effect_plot(model = fit,
                                 pred = Murder,
                                 centered = "all",
                                 partial.residuals = TRUE) +
                  ggtitle("Partial residuals all centered"))
  expect_silent(print(p))
})

test_that("effect_plot works for weighted lm", {
  expect_silent(p <- effect_plot(model = fitw,
                                 pred = Murder,
                                 centered = "all") + ggtitle("Weighted lm"))
  expect_silent(print(p))
  expect_silent(p <- effect_plot(model = fitw,
                                 pred = Murder,
                                 centered = "HSGrad") + 
                  ggtitle("Weighted lm HSGrad centered"))
  expect_silent(print(p))
  expect_silent(p <- effect_plot(model = fitw,
                                 pred = Murder,
                                 centered = "HSGrad",
                                 robust = TRUE,
                                 interval = TRUE) + 
                  ggtitle("Weighted lm HSGrad centered and robust intervals"))
  expect_silent(print(p))
  expect_silent(p <- effect_plot(model = fitw,
                                 pred = Murder,
                                 centered = "HSGrad",
                                 robust = TRUE,
                                 interval = TRUE,
                                 plot.points = TRUE) + 
                  ggtitle("Weighted lm HSGrad centered, robust, plot points"))
  expect_silent(print(p))
  expect_silent(p <- effect_plot(model = fitw,
                                 pred = Murder,
                                 centered = "HSGrad",
                                 robust = TRUE,
                                 interval = TRUE,
                                 partial.residuals = TRUE) + 
                  ggtitle("Weighted lm HSGrad centered, robust, partial residuals"))
  expect_silent(print(p))
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
    expect_silent(p <- effect_plot(regmodel, pred = meals, centered = "all") + 
                    ggtitle("svyglm"))
    expect_silent(print(p))
    expect_silent(p <- effect_plot(regmodel, pred = meals, centered = "ell") +
                    ggtitle("svyglm ell centered"))
    expect_silent(print(p))
    expect_silent(p <- effect_plot(regmodel, pred = meals, centered = "all",
                                   plot.points = TRUE) + 
                    ggtitle("svyglm plot points"))
    expect_silent(print(p))
    expect_silent(p <- effect_plot(regmodel, pred = meals, centered = "all",
                                   partial.residuals = TRUE) + 
                    ggtitle("svyglm partial residuals"))
    expect_silent(print(p))
    expect_silent(p <- effect_plot(svyqb, pred = mobility, centered = "all") + 
                    ggtitle("svyglm binomial"))
    expect_silent(print(p))
  })
}

if (requireNamespace("lme4")) {
  context("effect_plot lme4")
  library(lme4, quietly = TRUE)
  data(VerbAgg)
  fmVA0 <- glmer(r2 ~ Anger + Gender + btype + situ + (1|id) + (1|item), 
                 family = binomial, data = VerbAgg, nAGQ=0L)
  lmVA0 <- lmer(as.numeric(r2 == "Y") ~ Anger + Gender + btype + situ + (1|id) + (1|item), 
                data = VerbAgg)
  gm <- glmer(incidence ~ as.numeric(period) + (1 | herd), family = poisson,
              data = cbpp, offset = log(size))
  test_that("effect_plot works for lme4", {
    expect_silent(p <- effect_plot(lmVA0, pred = Anger, data = VerbAgg) +
                    ggtitle("lmer test"))
    expect_silent(print(p))
    expect_silent(p <- effect_plot(lmVA0, pred = Anger, plot.points = T,
                                   data = VerbAgg) +
                    ggtitle("lmer test + plot points"))
    expect_silent(print(p))
    expect_silent(p <- effect_plot(lmVA0, pred = Anger, partial.residuals = T,
                                   data = VerbAgg) +
                    ggtitle("lmer test + partial residuals"))
    expect_silent(print(p))
    expect_silent(p <- effect_plot(fmVA0, pred = Anger, data = VerbAgg) +
                    ggtitle("glmer test"))
    expect_silent(print(p))
    expect_message(p <- effect_plot(gm, pred = period, data = cbpp) +
                    ggtitle("glmer + offset test"))
    expect_silent(print(p))
  })
  context("lme4 predictions")
  test_that("predict_mer works with random effects", {
    expect_equal(head(jtools:::predict_mer(lmVA0, newdata = VerbAgg, 
                                           type = "response")), 
                 head(predict(lmVA0, newdata = VerbAgg, type = "response")))
  })
}

context("effect_plot offsets")

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


test_that("effect_plot handles offsets", {
  expect_message(p <- effect_plot(pmod, pred = money) + ggtitle("offset"))
  expect_silent(print(p))
})

test_that("effect_plot handles plot points with offsets", {
  expect_message(p <- effect_plot(pmod, pred = money, plot.points = TRUE) +
                   ggtitle("offset plus plot points"))
  expect_silent(print(p))
})


test_that("effect_plot handles partial residuals with offsets", {
  expect_message(p <- effect_plot(pmod, pred = money, partial.residuals = TRUE) +
                   ggtitle("offset plus partials"))
  expect_silent(print(p))
})


context("effect_plot categorical predictors")

test_that("effect_plot handles offsets w/ categorical predictors", {
  expect_s3_class(p <- effect_plot(pmod, pred = talent_f) + 
                    ggtitle("categorical (plus offset)"), "gg")
  expect_silent(print(p))
  expect_s3_class(p <- effect_plot(pmod, pred = talent_f, plot.points = TRUE) +
                    ggtitle("categorical, offset, plot points"), "gg")
  expect_silent(print(p))
  expect_s3_class(p <- effect_plot(pmod, pred = talent_f,
                                   partial.residuals = TRUE) +
                    ggtitle("categorical, offset, partial residuals"), "gg")
  expect_silent(print(p))
})

test_that("effect_plot does line plots", {
  expect_s3_class(p <- effect_plot(pmod, pred = talent_f, cat.geom = "line") +
                    ggtitle("categorical line plot"),
                  "gg")
  expect_silent(print(p))
  expect_s3_class(p <- effect_plot(pmod, pred = talent_f, cat.geom = "line",
                                   interval = FALSE) +
                    ggtitle("categorical line plot no intervals"), "gg")
  expect_silent(print(p))
})

test_that("effect_plot does bar plots", {
  expect_s3_class(p <- effect_plot(pmod, pred = talent_f, cat.geom = "bar") +
                    ggtitle("categorical bar plot"),
                  "gg")
  expect_silent(print(p))
  expect_s3_class(p <- effect_plot(pmod, pred = talent_f, cat.geom = "bar",
                                   interval = FALSE) +
                    ggtitle("categorical bar plot no intervals"), "gg")
  expect_silent(print(p))
})

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
                  interval = TRUE) + ggtitle("brms")))
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
    expect_message(print(effect_plot(rsfit, pred = "size", interval = TRUE) + 
                           ggtitle("stanreg")))
    # expect_silent(print(interact_plot(rsfit, pred = "size",
    #   modx = "period", interval = TRUE, data = cbpp)))
    # expect_is(make_predictions(rsfit, pred = "size", interval = TRUE,
    #   estimate = "median", data = cbpp), "predictions")
  })
}

options(device = device)
# dev.off()
