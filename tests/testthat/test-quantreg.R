context("quantreg compatibility")

device <- getOption("device")
options(device = "pdf")

if (requireNamespace("quantreg", quietly = TRUE)) {

library(quantreg)

stackloss2 <- stackloss
stackloss2[5, 4] <- NA
rfit <- rq(formula = stack.loss ~ Air.Flow + Water.Temp + Acid.Conc.,
            tau = 0.5, data = stackloss2)
rfiti <- rq(formula = stack.loss ~ Air.Flow * Water.Temp * Acid.Conc.,
            tau = 0.5, data = stackloss2)

test_that("summ.rq works", {
  expect_is(summ(rfit), "summ.rq")
  expect_is(summ(rfiti), "summ.rq")
  expect_is(summ(rfit, scale = TRUE), "summ.rq")
  expect_is(suppressWarnings(summ(rfiti, center = TRUE)), "summ.rq")
  expect_is(summ(rfit, se = "boot", boot.sims = 100), "summ.rq")
  expect_is(summ(rfit, vifs = TRUE), "summ.rq")
  expect_warning(summ(rfit, confint = TRUE, stars = TRUE, se = "iid"))
  expect_is(jtools:::knit_print.summ.rq(summ(rfit)), "knit_asis")
})

data(mpg, package = "ggplot2")
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
suppressWarnings(fit3r <- rq(cty ~ cyl * auto, data = mpg2, tau = 0.5))

test_that("rq plotters work", {
  expect_silent(print(effect_plot(rfit, pred = "Air.Flow", interval = TRUE)))
  # expect_silent(interact_plot(rfiti, pred = "Air.Flow", modx = "Water.Temp",
  #                             interval = TRUE))
  expect_silent(print(effect_plot(fit3r, pred = cyl, geom = "line",
                interval = TRUE)))
  expect_is(make_predictions(rfiti, pred = "Air.Flow", modx = "Water.Temp",
                             se = "ker"), "data.frame")
})

test_that("export_summs et al.", {
  expect_is(export_summs(rfit, rfiti), "huxtable")
  expect_is(suppressWarnings(export_summs(rfit, rfiti, scale = TRUE)), "huxtable")
  expect_silent(plot_summs(rfit, rfiti))
  expect_silent(suppressWarnings(plot_summs(rfit, rfiti, scale = TRUE)))
})

}

options(device = device)
dev.off()
