## ----echo=FALSE----------------------------------------------------------
required <- c("survey", "huxtable", "sandwich", "cowplot")
if (!all(sapply(required, requireNamespace, quietly = TRUE)))
  knitr::opts_chunk$set(eval = FALSE)
knitr::opts_chunk$set(message = F, warning = F, fig.width = 6, fig.height = 5,
                      fig.options = list(type = "cairo"))
library(jtools)

## ------------------------------------------------------------------------
states <- as.data.frame(state.x77)
fiti <- lm(Income ~ Illiteracy * Murder, data = states)
summ(fiti)

## ------------------------------------------------------------------------
summ(fiti, center = TRUE)

## ------------------------------------------------------------------------
interact_plot(fiti, pred = "Illiteracy", modx = "Murder")

## ------------------------------------------------------------------------
fitiris <- lm(Petal.Length ~ Petal.Width * Species, data = iris)
interact_plot(fitiris, pred = "Petal.Width", modx = "Species")

## ------------------------------------------------------------------------
interact_plot(fitiris, pred = "Petal.Width", modx = "Species",
              modx.values = c("versicolor", "virginica"))

## ------------------------------------------------------------------------
interact_plot(fiti, pred = "Illiteracy", modx = "Murder", plot.points = TRUE)

## ------------------------------------------------------------------------
interact_plot(fitiris, pred = "Petal.Width", modx = "Species",
              plot.points = TRUE)

## ------------------------------------------------------------------------
interact_plot(fitiris, pred = "Petal.Width", modx = "Species",
              plot.points = TRUE, jitter = 0.1, point.shape = TRUE)

## ------------------------------------------------------------------------
fiti <- lm(Income ~ Illiteracy * Murder, data = states,
           weights = Population)
interact_plot(fiti, pred = "Illiteracy", modx = "Murder", plot.points = TRUE)

## ------------------------------------------------------------------------
interact_plot(fiti, pred = "Illiteracy", modx = "Murder", interval = TRUE,
              int.width = 0.8)

## ------------------------------------------------------------------------
set.seed(99)
x <- rnorm(n = 200, mean = 3, sd = 1)
err <- rnorm(n = 200, mean = 0, sd = 4)
w <- rbinom(n = 200, size = 1, prob = 0.5)

y_1 <- 5 - 4*x - 9*w + 3*w*x + err

## ------------------------------------------------------------------------
model_1 <- lm(y_1 ~ x * w)
summ(model_1)

## ------------------------------------------------------------------------
interact_plot(model_1, pred = "x", modx = "w", linearity.check = TRUE, 
              plot.points = TRUE)

## ------------------------------------------------------------------------
x_2 <- runif(n = 200, min = -3, max = 3)
y_2 <- 2.5 - x_2^2 - 5*w + 2*w*(x_2^2) + err
data_2 <- as.data.frame(cbind(x_2, y_2, w))

model_2 <- lm(y_2 ~ x_2 * w, data = data_2)
summ(model_2)

## ------------------------------------------------------------------------
interact_plot(model_2, pred = "x_2", modx = "w", linearity.check = TRUE, 
              plot.points = TRUE)

## ------------------------------------------------------------------------
model_3 <- lm(y_2 ~ poly(x_2, 2) * w, data = data_2)
summ(model_3)

## ------------------------------------------------------------------------
interact_plot(model_3, pred = "x_2", modx = "w", data = data_2)

## ------------------------------------------------------------------------
interact_plot(model_3, pred = "x_2", modx = "w", data = data_2,
              linearity.check = TRUE, plot.points = TRUE)

## ------------------------------------------------------------------------
interact_plot(fiti, pred = "Illiteracy", modx = "Murder",
              x.label = "Custom X Label", y.label = "Custom Y Label",
              main.title = "Sample Plot",  legend.main = "Custom Legend Title",
              color.class = "Oranges")

## ------------------------------------------------------------------------
interact_plot(fitiris, pred = "Petal.Width", modx = "Species") + theme_apa()

## ------------------------------------------------------------------------
sim_slopes(fiti, pred = Illiteracy, modx = Murder, johnson_neyman = FALSE)

## ------------------------------------------------------------------------
sim_slopes(fiti, pred = Illiteracy, modx = Murder, modx.values = c(0, 5, 10),
           johnson_neyman = FALSE)

## ------------------------------------------------------------------------
ss <- sim_slopes(fiti, pred = Illiteracy, modx = Murder, modx.values = c(0, 5, 10))
plot(ss)

## ------------------------------------------------------------------------
ss <- sim_slopes(fiti, pred = Illiteracy, modx = Murder, modx.values = c(0, 5, 10))
library(huxtable)
as_huxtable(ss)

## ------------------------------------------------------------------------
sim_slopes(fiti, pred = Illiteracy, modx = Murder, johnson_neyman = TRUE)

## ------------------------------------------------------------------------
johnson_neyman(fiti, pred = Illiteracy, modx = Murder, alpha = 0.01)

## ------------------------------------------------------------------------
sim_slopes(fiti, pred = Illiteracy, modx = Murder, johnson_neyman = TRUE,
           control.fdr = TRUE)

## ------------------------------------------------------------------------
sim_slopes(fiti, pred = Illiteracy, modx = Murder, cond.int = TRUE)

## ------------------------------------------------------------------------
sim_slopes(fiti, pred = Illiteracy, modx = Murder, robust = "HC3")

## ------------------------------------------------------------------------
library(survey)
data(api)
dstrat <- svydesign(id = ~1, strata = ~stype, weights = ~pw,
                    data = apistrat, fpc = ~fpc)
regmodel <- svyglm(api00 ~ avg.ed * growth, design = dstrat)

probe_interaction(regmodel, pred = growth, modx = avg.ed, cond.int = TRUE,
                  interval = TRUE,  jnplot = TRUE)

## ------------------------------------------------------------------------
out <- probe_interaction(regmodel, pred = growth, modx = avg.ed,
                         cond.int = TRUE, interval = TRUE, jnplot = TRUE)
names(out)

## ------------------------------------------------------------------------
fita3 <- lm(rating ~ privileges * critical * learning, data = attitude)
probe_interaction(fita3, pred = critical, modx = learning, mod2 = privileges,
                  alpha = .1)

## ------------------------------------------------------------------------
mtcars$cyl <- factor(mtcars$cyl,
                     labels = c("4 cylinder", "6 cylinder", "8 cylinder"))
fitc3 <- lm(mpg ~ hp * wt * cyl, data = mtcars)
interact_plot(fitc3, pred = hp, modx = wt, mod2 = cyl) + 
  theme_apa(legend.pos = "bottomright")

## ----fig.height = 8------------------------------------------------------
regmodel3 <- svyglm(api00 ~ avg.ed * growth * enroll, design = dstrat)
sim_slopes(regmodel3, pred = growth, modx = avg.ed, mod2 = enroll,
          jnplot = TRUE)

## ------------------------------------------------------------------------
ss3 <- sim_slopes(regmodel3, pred = growth, modx = avg.ed, mod2 = enroll)
plot(ss3)

## ------------------------------------------------------------------------
as_huxtable(ss3)

## ------------------------------------------------------------------------
set.seed(5)
x <- rnorm(100)
m <- rnorm(100)
prob <- boot::inv.logit(.25 + .3*x + .3*m + -.5*(x*m) + rnorm(100))
y <- rep(0, 100)
y[prob >= .5] <- 1
logit_fit <- glm(y ~ x * m, family = binomial)

## ------------------------------------------------------------------------
summ(logit_fit)

## ------------------------------------------------------------------------
interact_plot(logit_fit, pred = x, modx = m)

