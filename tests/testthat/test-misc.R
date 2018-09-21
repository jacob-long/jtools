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

test_that("interact_plot works for lm", {
  expect_silent(p <- interact_plot(model = fit,
                              pred = Murder,
                              modx = Illiteracy,
                              mod2 = HSGrad,
                              centered = "all"))
  expect_silent(print(p))
  expect_warning(p <- interact_plot(model = fit,
                              pred = Murder,
                              modx = Illiteracy,
                              mod2 = HSGrad,
                              centered = "HSGrad"))
  expect_silent(print(p))
  expect_silent(p <- interact_plot(model = fit,
                                   pred = Murder,
                                   modx = Illiteracy,
                                   mod2 = HSGrad,
                                   centered = "Area"))
  expect_silent(print(p))
  expect_silent(p <- interact_plot(model = fit,
                                   pred = Murder,
                                   modx = Illiteracy,
                                   mod2 = HSGrad,
                                   centered = "none"))
  expect_silent(print(p))
})

test_that("interact_plot: robust standard errors work", {
  expect_silent(p <- interact_plot(model = fit,
                                   pred = Murder,
                                   modx = Illiteracy,
                                   mod2 = HSGrad,
                                   centered = "all",
                                   robust = TRUE))
  expect_silent(print(p))
})

test_that("rug plots work", {
  expect_silent(p <- interact_plot(model = fit,
                                   pred = Murder,
                                   modx = Illiteracy,
                                   mod2 = HSGrad,
                                   centered = "all",
                                   rug = TRUE))
  expect_silent(print(p))

  expect_silent(p <- interact_plot(model = fit,
                                   pred = Murder,
                                   modx = Illiteracy,
                                   mod2 = HSGrad,
                                   centered = "all",
                                   rug = TRUE,
                                   rug.sides = "lb"))
  expect_silent(print(p))
})

test_that("sim_slopes works for lm", {
  expect_silent(sim_slopes(model = fit,
                              pred = Murder,
                              modx = Illiteracy,
                              mod2 = HSGrad,
                              centered = "all"))
  expect_warning(sim_slopes(model = fit,
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
  expect_silent(p <- interact_plot(model = fitw,
                             pred = Murder,
                             modx = Illiteracy,
                             mod2 = HSGrad,
                             centered = "all"))
  expect_silent(print(p))
})

test_that("sim_slopes and interact_plot work for lm w/ logical", {
  expect_silent(sim_slopes(model = fitl,
                           pred = HSGrad,
                           modx = o70l,
                           johnson_neyman = FALSE))
  expect_silent(p <- interact_plot(model = fitl,
                           pred = HSGrad,
                           modx = o70l))
  expect_silent(print(p))
})

test_that("sim_slopes and interact_plot work for lm w/ non-focal character", {
  expect_silent(sim_slopes(model = fitc,
                           pred = HSGrad,
                           modx = Murder,
                           johnson_neyman = FALSE))
  expect_silent(p <- interact_plot(model = fitc,
                                   pred = HSGrad,
                                   modx = Murder))
  expect_silent(print(p))
})

test_that("interact_plot accepts user-specified values and labels", {
  expect_silent(p <- interact_plot(model = fit,
                              pred = Murder,
                              modx = Illiteracy,
                              mod2 = HSGrad,
                              centered = "all",
                              modxvals = c(0, 1, 3),
                              modx.labels = c("None","Low","High"),
                              mod2vals = c(40, 60, 80),
                              mod2.labels = c("Low","Average","High")))
  expect_silent(print(p))
  expect_error(p <- interact_plot(model = fit2,
                              pred = o70,
                              modx = HSGrad,
                              pred.labels = c("Under","Over")))
  expect_silent(p <- interact_plot(model = fit2n,
                                  pred = o70n,
                                  modx = HSGrad,
                                  pred.labels = c("Under","Over")))
  expect_silent(print(p))
})

test_that("interact_plot terciles modxval/mod2val works", {
  expect_message(p <- interact_plot(model = fit,
                              pred = Murder,
                              modx = Illiteracy,
                              mod2 = HSGrad,
                modxvals = "terciles",
                mod2vals = "terciles",
                centered = "none"))
  expect_silent(print(p))
})

test_that("interact_plot linearity.check works", {
  expect_message(p <- interact_plot(model = fit,
                              pred = Murder,
                              modx = Illiteracy,
                              modxvals = "terciles",
                              linearity.check = TRUE,
                              plot.points = TRUE))
  expect_silent(print(p))
  expect_silent(p <- interact_plot(model = fit,
                              pred = Murder,
                              modx = Illiteracy,
                              linearity.check = TRUE))
  expect_silent(print(p))
})

if (requireNamespace("huxtable") && requireNamespace("broom")) {
  test_that("as_huxtable.sim_slopes works", {
    ss3 <- sim_slopes(model = fit, pred = Murder, modx = Illiteracy,
                      mod2 = HSGrad)
    ss <- sim_slopes(model = fit, pred = Murder, modx = Illiteracy)
    expect_is(as_huxtable.sim_slopes(ss3), "huxtable")
    expect_is(as_huxtable.sim_slopes(ss), "huxtable")
  })
}

if (requireNamespace("ggstance") && requireNamespace("broom")) {
  test_that("plot.sim_slopes works", {
    ss3 <- sim_slopes(model = fit, pred = Murder, modx = Illiteracy,
                      mod2 = HSGrad)
    ss <- sim_slopes(model = fit, pred = Murder, modx = Illiteracy)
    expect_is(plot(ss3), "ggplot")
    expect_is(plot(ss), "ggplot")
  })
}

context("interactions svyglm")

if (requireNamespace("survey")) {
  test_that("interact_plot works for svyglm", {
    expect_silent(p <- interact_plot(regmodel, pred = ell, modx = meals,
                                     mod2 = both,
                                centered = "all"))
    expect_silent(print(p))
    expect_warning(p <- interact_plot(regmodel, pred = ell, modx = meals,
                                     mod2 = both,
                                centered = "ell"))
    expect_silent(print(p))
  })


  test_that("effect_plot works for svyglm", {
    expect_silent(p <- effect_plot(regmodel, pred = meals,
                                centered = "all"))
    expect_silent(print(p))
    expect_silent(p <- effect_plot(regmodel, pred = meals,
                                centered = "ell"))
    expect_silent(print(p))
  })
}

context("interactions merMod")

if (requireNamespace("lme4")) {
  library(lme4, quietly = TRUE)
  data(VerbAgg)
  VerbAgg$mode_numeric <- as.numeric(VerbAgg$mode)
  mve <- lmer(Anger ~ Gender * mode + btype +  (1 | item), data = VerbAgg)
  mv <- lmer(Anger ~ Gender * mode_numeric + btype +  (1 | item),
             data = VerbAgg)
  gm <- glmer(incidence ~ period + (1 | herd), family = poisson, data = cbpp,
              offset = log(size))

  test_that("interact_plot works for lme4", {
    expect_error(p <- interact_plot(mve, pred = mode, modx = Gender))
    expect_silent(p <- interact_plot(mv, pred = mode_numeric, modx = Gender))
    expect_silent(print(p))
    expect_message(p <- interact_plot(mv, pred = mode_numeric, modx = Gender,
                                     interval = TRUE))
    expect_silent(print(p))
  })

  test_that("effect_plot works for lme4", {
    expect_error(p <- effect_plot(mve, pred = mode))
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
poisdat <- as.data.frame(cbind(exposures, counts, talent, money))
pmod <- glm(counts ~ talent*money, offset = log(exposures), data = poisdat,
            family = poisson)

test_that("interact_plot handles offsets", {
  expect_message(p <- interact_plot(pmod, pred = talent, modx = money))
  expect_silent(print(p))
})

test_that("interact_plot handles offsets with robust SE", {
  expect_message(p <- interact_plot(pmod, pred = talent, modx = money,
                                    robust = TRUE))
  expect_silent(print(p))
})

test_that("sim_slopes handles offsets", {
  expect_s3_class(sim_slopes(pmod, pred = talent, modx = money), "sim_slopes")
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
#                   chains = 2, cores = 2, seed = 12345, iter = 2000)
# saveRDS(fitrs, "rsafit.rds")

#### brms and rstanarm tests #################################################

if (requireNamespace("brms")) {
  context("brmsfit plots")
  bfit <- readRDS("brmfit.rds")
  test_that("brmsfit objects are supported", {
    expect_silent(print(effect_plot(bfit, pred = "log_Base4_c",
                  interval = TRUE)))
    expect_silent(print(cat_plot(bfit, pred = "Trt",
                  interval = TRUE)))
    expect_silent(print(interact_plot(bfit, pred = "log_Base4_c", modx = "Trt",
                  interval = TRUE)))
    expect_is(make_predictions(bfit, pred = "log_Base4_c", modx = "Trt",
                               interval = TRUE, estimate = "median"),
                               "predictions")
  })
}

if (requireNamespace("rstanarm") & requireNamespace("lme4")) {
  context("stanreg plots")
  rsfit <- readRDS("rsafit.rds")
  library(lme4)
  data(cbpp)
  test_that("stanreg objects are supported", {
    expect_warning(print(effect_plot(rsfit, pred = "size", interval = TRUE)))
    expect_silent(print(interact_plot(rsfit, pred = "size",
      modx = "period", interval = TRUE, data = cbpp)))
    expect_is(make_predictions(rsfit, pred = "size", interval = TRUE,
      estimate = "median", data = cbpp), "predictions")
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

### johnson_neyman ###########################################################

context("j_n specific")

test_that("johnson_neyman control.fdr argument works", {
  expect_s3_class(johnson_neyman(fit, pred = Murder, modx = Illiteracy,
                                 control.fdr = TRUE), "johnson_neyman")
})

test_that("johnson_neyman critical.t argument works", {
  expect_s3_class(johnson_neyman(fit, pred = Murder, modx = Illiteracy,
                                 critical.t = 2.1), "johnson_neyman")
})

test_that("johnson_neyman color arguments work", {
  expect_silent(johnson_neyman(fit, pred = Murder, modx = Illiteracy,
                               sig.color = "black", insig.color = "grey")$plot)
})

test_that("johnson_neyman mod.range argument works", {
  expect_silent(johnson_neyman(fit, pred = Murder, modx = Illiteracy,
                               mod.range = c(1, 2))$plot)
})

### cat_plot ##################################################################

context("cat_plot")

if (requireNamespace("lme4", quietly = TRUE)) {
  mv <- lmer(Anger ~ Gender*mode + btype +  (1 | item), data = VerbAgg)
}
library(ggplot2)
diamond <- diamonds
diamond <- diamond[diamond$color != "D",]
set.seed(10)
samps <- sample(1:nrow(diamond), 2000)
diamond <- diamond[samps,]
fit <- lm(price ~ cut * color, data = diamond)

test_that("cat_plot handles simple plot (bar)", {
  expect_silent(p <- cat_plot(fit, pred = color, modx = cut))
  expect_silent(print(p))
})

test_that("cat_plot handles intervals (bar)", {
  expect_silent(p <- cat_plot(fit, pred = color, modx = cut, interval = TRUE))
  expect_silent(print(p))
})

test_that("cat_plot handles plotted points (bar)", {
  expect_silent(p <- cat_plot(fit, pred = color, modx = cut, interval = TRUE,
                         plot.points = TRUE))
  expect_silent(print(p))
})

test_that("cat_plot handles simple plot (line)", {
  expect_silent(p <- cat_plot(fit, pred = color, modx = cut, geom = "line"))
  expect_silent(print(p))
})

test_that("cat_plot handles intervals (line)", {
  expect_silent(p <- cat_plot(fit, pred = color, modx = cut, interval = TRUE,
                         geom = "line"))
  expect_silent(print(p))
})

test_that("cat_plot handles plotted points (line)", {
  expect_silent(p <- cat_plot(fit, pred = color, modx = cut, interval = TRUE,
                         plot.points = TRUE, geom = "line"))
  expect_silent(print(p))
})

test_that("cat_plot handles point.shape (line)", {
  expect_silent(p <- cat_plot(fit, pred = color, modx = cut, interval = TRUE,
                         plot.points = TRUE, geom = "line", point.shape = TRUE))
  expect_silent(print(p))
})

test_that("cat_plot handles point.shape (line)", {
  expect_silent(p <- cat_plot(fit, pred = color, modx = cut, interval = TRUE,
                         plot.points = TRUE, geom = "line", point.shape = TRUE,
                         vary.lty = TRUE))
  expect_silent(print(p))
})

test_that("cat_plot handles simple plot (point)", {
  expect_silent(p <- cat_plot(fit, pred = color, modx = cut, geom = "point"))
  expect_silent(print(p))
})

test_that("cat_plot handles intervals (point)", {
  expect_silent(p <- cat_plot(fit, pred = color, modx = cut, interval = TRUE,
                         geom = "point"))
  expect_silent(print(p))
})

test_that("cat_plot handles plotted points (point)", {
  expect_silent(p <- cat_plot(fit, pred = color, modx = cut, interval = TRUE,
                         plot.points = TRUE, geom = "point"))
  expect_silent(print(p))
})

test_that("cat_plot handles point.shape (point)", {
  expect_silent(p <- cat_plot(fit, pred = color, modx = cut, interval = TRUE,
                         plot.points = TRUE, geom = "point",
                         point.shape = TRUE))
  expect_silent(print(p))
})

test_that("cat_plot handles simple plot (boxplot)", {
  expect_silent(p <- cat_plot(fit, pred = color, modx = cut, geom = "boxplot"))
  expect_silent(print(p))
})

test_that("cat_plot handles plotted points (boxplot)", {
  expect_silent(p <- cat_plot(fit, pred = color, modx = cut, interval = TRUE,
                         plot.points = TRUE, geom = "boxplot"))
  expect_silent(print(p))
})

set.seed(100)
exposures <- rpois(50, 50)
counts <- exposures - rpois(50, 25)
money <- (counts/exposures) + rnorm(50, sd = 1)
talent <- rbinom(50, 1, .5)
poisdat <- as.data.frame(cbind(exposures, counts, talent, money))
poisdat$talent <- factor(poisdat$talent)
pmod <- glm(counts ~ talent*money, offset = log(exposures), data = poisdat,
            family = poisson)

test_that("cat_plot handles offsets", {
  expect_s3_class(p <- cat_plot(pmod, pred = talent), "gg")
  expect_silent(print(p))
})

if (requireNamespace("survey")) {
test_that("cat_plot handles svyglm", {
  expect_silent(p <- cat_plot(regmodel, pred = both))
  expect_silent(print(p))
})
}

test_that("cat_plot handles merMod", {
  expect_silent(p <- cat_plot(mv, pred = mode, modx = Gender, interval = FALSE))
  expect_silent(print(p))
})

# 3-way interaction

## Will first create a couple dichotomous factors to ensure full rank
mpg2 <- mpg
mpg2$auto <- "auto"
mpg2$auto[mpg2$trans %in% c("manual(m5)", "manual(m6)")] <- "manual"
mpg2$auto <- factor(mpg2$auto)
mpg2$fwd <- "2wd"
mpg2$fwd[mpg2$drv == "4"] <- "4wd"
mpg2$fwd <- factor(mpg2$fwd)
## Drop the two cars with 5 cylinders (rest are 4, 6, or 8)
mpg2 <- mpg2[mpg2$cyl != "5",]
mpg2$cyl <- factor(mpg2$cyl)
## Fit the model
fit3 <- lm(cty ~ cyl * fwd * auto, data = mpg2)


test_that("cat_plot does 3-way interactions (bar)", {
  expect_silent(p <- cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "bar"))
  expect_silent(print(p))
})

test_that("cat_plot does intervals w/ 3-way interactions (bar)", {
  expect_silent(p <- cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "bar",
           interval = TRUE))
  expect_silent(print(p))
})

test_that("cat_plot does plot.points w/ 3-way interactions (bar)", {
  expect_silent(p <- cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "bar",
           interval = TRUE, plot.points = TRUE))
  expect_silent(print(p))
})

test_that("cat_plot does point.shape w/ 3-way interactions (bar)", {
  expect_silent(p <- cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "bar",
           interval = TRUE, plot.points = TRUE, point.shape = TRUE))
  expect_silent(print(p))
})

test_that("cat_plot does 3-way interactions (line)", {
  expect_silent(p <- cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "line"))
  expect_silent(print(p))
})

test_that("cat_plot does intervals w/ 3-way interactions (line)", {
  expect_silent(p <- cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "line",
           interval = TRUE))
  expect_silent(print(p))
})

test_that("cat_plot does plot.points w/ 3-way interactions (line)", {
  expect_silent(p <- cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "line",
           interval = TRUE, plot.points = TRUE))
  expect_silent(print(p))
})

test_that("cat_plot does point.shape w/ 3-way interactions (line)", {
  expect_silent(p <- cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "line",
           interval = TRUE, plot.points = TRUE, point.shape = TRUE))
  expect_silent(print(p))
})

test_that("cat_plot does vary.lty w/ 3-way interactions (line)", {
  expect_silent(p <- cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "line",
           interval = TRUE, plot.points = TRUE, point.shape = TRUE,
           vary.lty = TRUE))
  expect_silent(print(p))
})

test_that("cat_plot does 3-way interactions (point)", {
  expect_silent(p <- cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "point"))
  expect_silent(print(p))
})

test_that("cat_plot does intervals w/ 3-way interactions (point)", {
  expect_silent(p <- cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "point",
           interval = TRUE))
  expect_silent(print(p))
})

test_that("cat_plot does plot.points w/ 3-way interactions (point)", {
  expect_silent(p <- cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "point",
           interval = TRUE, plot.points = TRUE))
  expect_silent(print(p))
})

test_that("cat_plot does point.shape w/ 3-way interactions (point)", {
  expect_silent(p <- cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "point",
           interval = TRUE, plot.points = TRUE, point.shape = TRUE))
  expect_silent(print(p))
})

test_that("cat_plot does 3-way interactions (line)", {
  expect_silent(p <- cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "line"))
  expect_silent(print(p))
})

test_that("cat_plot does plot.points w/ 3-way interactions (line)", {
  expect_silent(p <- cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "line",
           plot.points = TRUE))
  expect_silent(print(p))
})

test_that("cat_plot does point.shape w/ 3-way interactions (boxplot)", {
  expect_silent(p <- cat_plot(fit3, pred = cyl, modx = fwd, mod2 = auto, geom = "boxplot",
           plot.points = TRUE, point.shape = TRUE))
  expect_silent(print(p))
})

### No interaction cat_plot ##################################################

test_that("cat_plot handles simple plot w/ no mod. (bar)", {
  expect_silent(p <- cat_plot(fit, pred = color))
  expect_silent(print(p))
})

test_that("cat_plot handles intervals w/ no mod. (bar)", {
  expect_silent(p <- cat_plot(fit, pred = color, interval = TRUE))
  expect_silent(print(p))
})

test_that("cat_plot handles plotted points w/ no mod. (bar)", {
  expect_silent(p <- cat_plot(fit, pred = color, interval = TRUE,
                         plot.points = TRUE))
  expect_silent(print(p))
})

test_that("cat_plot handles simple plot w/ no mod. (point)", {
  expect_silent(p <- cat_plot(fit, pred = color, geom = "point"))
  expect_silent(print(p))
})

test_that("cat_plot handles intervals w/ no mod. (point)", {
  expect_silent(p <- cat_plot(fit, pred = color, interval = TRUE,
                         geom = "point"))
  expect_silent(print(p))
})

test_that("cat_plot handles plotted points w/ no mod. (point)", {
  expect_silent(p <- cat_plot(fit, pred = color, interval = TRUE,
                         plot.points = TRUE, geom = "point"))
  expect_silent(print(p))
})

test_that("cat_plot handles point.shape w/ no mod. (point)", {
  expect_silent(p <- cat_plot(fit, pred = color, interval = TRUE,
                         plot.points = TRUE, geom = "point",
                         point.shape = TRUE))
  expect_silent(print(p))
})

test_that("cat_plot handles simple plot w/ no mod. (boxplot)", {
  expect_silent(p <- cat_plot(fit, pred = color, geom = "boxplot"))
  expect_silent(print(p))
})

test_that("cat_plot handles plotted points w/ no mod. (boxplot)", {
  expect_silent(p <- cat_plot(fit, pred = color, interval = TRUE,
                         plot.points = TRUE, geom = "boxplot"))
  expect_silent(print(p))
})

if (requireNamespace("lme4", quietly = TRUE)) {
  test_that("make_predictions.merMod bootstrap intervals work", {
    mp <- make_predictions(gm, pred = "period", interval = TRUE, boot = TRUE,
                           sims = 10, progress = "none")
    expect_silent(p <- plot_predictions(mp, interval = TRUE))
    expect_silent(print(p))
  })

  test_that("glmer works", {
    expect_message(p <- cat_plot(gm, pred = period, interval = TRUE))
    expect_silent(print(p))
  })
}

options(device = device)
# dev.off()
