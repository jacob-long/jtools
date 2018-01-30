context("interactions lm")

states <- as.data.frame(state.x77)
states$HSGrad <- states$`HS Grad`
states$o70 <- 0
states$o70[states$`Life Exp` > 70] <- 1
states$o70n <- states$o70
states$o70 <- factor(states$o70)
set.seed(3)
states$wts <- runif(50, 0, 3)
fit <- lm(Income ~ HSGrad*Murder*Illiteracy + o70 + Area, data = states)
fit2 <- lm(Income ~ HSGrad*o70, data = states)
fit2n <- lm(Income ~ HSGrad*o70n, data = states)
fitw <- lm(Income ~ HSGrad*Murder*Illiteracy + o70 + Area, data = states,
           weights = wts)


suppressMessages(library(survey, quietly = TRUE))
data(api)
dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw, data = apistrat,
                    fpc = ~fpc)
regmodel <- svyglm(api00 ~ ell * meals * both + sch.wide + pcttest,
                   design = dstrat)

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
  expect_silent(p <- interact_plot(model = fit,
                              pred = Murder,
                              modx = Illiteracy,
                              mod2 = HSGrad,
                modxvals = "terciles",
                mod2vals = "terciles",
                centered = "none"))
  expect_silent(print(p))
})

test_that("interact_plot linearity.check works", {
  expect_silent(p <- interact_plot(model = fit,
                              pred = Murder,
                              modx = Illiteracy,
                              modxvals = "terciles",
                              linearity.check = TRUE))
  expect_silent(print(p))
  expect_silent(p <- interact_plot(model = fit,
                              pred = Murder,
                              modx = Illiteracy,
                              linearity.check = TRUE))
  expect_silent(print(p))
})

context("interactions svyglm")

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

context("interactions merMod")

library(lme4, quietly = TRUE)
data(VerbAgg)
VerbAgg$nmode <- as.numeric(VerbAgg$mode)
mv <- lmer(Anger ~ Gender*nmode + btype +  (1 | item), data = VerbAgg)

test_that("interact_plot works for lme4", {
  expect_silent(p <- interact_plot(mv, pred = nmode, modx = Gender))
  expect_silent(print(p))
})

# data("cake")
#
# data("grouseticks")
# g <- grouseticks
# g$YEAR <- as.numeric(g$YEAR)
# lf <- lmer(TICKS ~ HEIGHT * YEAR + (1 | BROOD), data = g)

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

test_that("sim_slopes handles offsets", {
  expect_s3_class(sim_slopes(pmod, pred = talent, modx = money), "sim_slopes")
})


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

test_that("effect_plot works for weighted lm", {
  expect_silent(p <- effect_plot(model = fitw,
                                 pred = Murder,
                                 centered = "all"))
  expect_silent(print(p))
  expect_silent(p <- effect_plot(model = fitw,
                                 pred = Murder,
                                 centered = "HSGrad"))
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

test_that("effect_plot works for lme4", {
  expect_silent(p <- effect_plot(mv, pred = nmode))
  expect_silent(print(p))
})

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

### cat_plot ##################################################################

context("cat_plot")

mv <- lmer(Anger ~ Gender*mode + btype +  (1 | item), data = VerbAgg)
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

test_that("cat_plot handles svyglm", {
  expect_silent(p <- cat_plot(regmodel, pred = both))
  expect_silent(print(p))
})

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
