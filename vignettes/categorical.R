## ----echo=FALSE----------------------------------------------------------
knitr::opts_chunk$set(message = F, warning = F, fig.width = 6, fig.height = 5,
                      fig.options = list(type = "cairo"))
library(jtools)

## ------------------------------------------------------------------------
library(ggplot2)
mpg2 <- mpg
mpg2$cyl <- factor(mpg2$cyl)
mpg2["auto"] <- "auto"
mpg2$auto[mpg2$trans %in% c("manual(m5)", "manual(m6)")] <- "manual"
mpg2$auto <- factor(mpg2$auto)
mpg2["fwd"] <- "2wd"
mpg2$fwd[mpg2$drv == "4"] <- "4wd"
mpg2$fwd <- factor(mpg2$fwd)
## Drop the two cars with 5 cylinders (rest are 4, 6, or 8)
mpg2 <- mpg2[mpg2$cyl != "5",]
## Fit the model
fit3 <- lm(cty ~ cyl * fwd * auto, data = mpg2)

## ------------------------------------------------------------------------
summ(fit3)

## ------------------------------------------------------------------------
cat_plot(fit3, pred = cyl, modx = fwd)

## ------------------------------------------------------------------------
cat_plot(fit3, pred = cyl, modx = fwd, plot.points = TRUE)

## ------------------------------------------------------------------------
cat_plot(fit3, pred = cyl, modx = fwd, geom = "line")

## ------------------------------------------------------------------------
cat_plot(fit3, pred = cyl, modx = fwd, geom = "line", point.shape = TRUE)

## ------------------------------------------------------------------------
cat_plot(fit3, pred = cyl, modx = fwd, geom = "line", point.shape = TRUE,
         vary.lty = TRUE)

## ------------------------------------------------------------------------
cat_plot(fit3, pred = cyl, modx = fwd, geom = "line", point.shape = TRUE,
         color.class = "Set2")

## ------------------------------------------------------------------------
cat_plot(fit3, pred = cyl, modx = fwd, geom = "bar")

## ------------------------------------------------------------------------
cat_plot(fit3, pred = cyl, modx = fwd, geom = "bar", interval = FALSE)

## ------------------------------------------------------------------------
cat_plot(fit3, pred = cyl, modx = fwd, geom = "bar", interval = FALSE,
         plot.points = TRUE)

