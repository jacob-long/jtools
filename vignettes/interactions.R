## ----echo=FALSE----------------------------------------------------------
knitr::opts_chunk$set(message=F, warning=F, fig.width = 6, fig.height = 4)
library(jtools)

## ------------------------------------------------------------------------
fiti <- lm(Income ~ Illiteracy*Murder, data = as.data.frame(state.x77))
j_summ(fiti)

## ------------------------------------------------------------------------
j_summ(fiti, standardize = TRUE)

## ------------------------------------------------------------------------
sim_slopes(fiti, pred = Illiteracy, modx = Murder, johnson_neyman = FALSE)

## ------------------------------------------------------------------------
sim_slopes(fiti, pred = Illiteracy, modx = Murder, modxvals = c(0, 5, 10),
           johnson_neyman = FALSE)

## ------------------------------------------------------------------------
sim_slopes(fiti, pred = Illiteracy, modx = Murder, modxvals = c(0, 5, 10),
           johnson_neyman = TRUE)

## ------------------------------------------------------------------------
johnson_neyman(fiti, pred = Illiteracy, modx = Murder, alpha = 0.01)

## ------------------------------------------------------------------------
sim_slopes(fiti, pred = Illiteracy, modx = Murder, cond.int = TRUE)

## ------------------------------------------------------------------------
sim_slopes(fiti, pred = Illiteracy, modx = Murder, robust = TRUE)

## ------------------------------------------------------------------------
sim_slopes(fiti, pred = Illiteracy, modx = Murder, standardize = TRUE, 
           centered = "all")

## ------------------------------------------------------------------------
interact_plot(fiti, pred = "Illiteracy", modx = "Murder")

## ------------------------------------------------------------------------
interact_plot(fiti, pred = "Illiteracy", modx = "Murder", modxvals = "plus-minus")

## ------------------------------------------------------------------------
fitiris <- lm(Petal.Length ~ Petal.Width*Species, data = iris)
interact_plot(fitiris, pred = "Petal.Width", modx = "Species")

## ------------------------------------------------------------------------
interact_plot(fiti, pred = "Illiteracy", modx = "Murder", plot.points = TRUE)

## ------------------------------------------------------------------------
interact_plot(fitiris, pred = "Petal.Width", modx = "Species", plot.points = TRUE)

## ------------------------------------------------------------------------
interact_plot(fiti, pred = "Illiteracy", modx = "Murder", interval = TRUE, int.width = 0.8)

## ------------------------------------------------------------------------
interact_plot(fiti, pred = "Illiteracy", modx = "Murder", x.label = "Custom X Label",
              y.label = "Custom Y Label", main.title = "Sample Plot", 
              legend.main = "Custom Legend Title")

## ------------------------------------------------------------------------
interact_plot(fitiris, pred = "Petal.Width", modx = "Species") + theme_apa()

## ------------------------------------------------------------------------
library(survey)
data(api)
dstrat <- svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)
regmodel <- svyglm(api00 ~ avg.ed*growth, design = dstrat)
probe_interaction(regmodel, pred = growth, modx = avg.ed, cond.int = TRUE, interval = TRUE, 
                  jnplot = TRUE)

## ------------------------------------------------------------------------
out <- probe_interaction(regmodel, pred = growth, modx = avg.ed, cond.int = TRUE,
                         interval = TRUE, jnplot = TRUE)
names(out)

## ------------------------------------------------------------------------
fita3 <- lm(rating ~ privileges*critical*learning, data = attitude)
probe_interaction(fita3, pred = critical, modx = learning, mod2 = privileges)

## ------------------------------------------------------------------------
mtcars$cyl <- factor(mtcars$cyl, labels = c("4 cylinder", "6 cylinder", "8 cylinder"))
fitc3 <- lm(mpg ~ hp*wt*cyl, data=mtcars)
interact_plot(fitc3, pred = hp, modx = wt, mod2 = cyl) + 
  theme_apa(legend.pos = "bottomright")

## ------------------------------------------------------------------------
regmodel3 <- svyglm(api00 ~ avg.ed*growth*enroll, design = dstrat)
probe_interaction(regmodel3, pred = growth, modx = avg.ed, mod2 = enroll,
                  johnson_neyman = TRUE, jnplot = TRUE, interval = T)

