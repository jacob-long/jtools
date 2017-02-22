## ----echo=FALSE----------------------------------------------------------
knitr::opts_chunk$set(message=F, warning=F)
library(jtools)

## ------------------------------------------------------------------------
library(survey)
data(api)
dstrat <- svydesign(id = ~1,strata = ~stype, weights = ~pw, data = apistrat, fpc=~fpc)

## ------------------------------------------------------------------------
svycor(~api00 + api99, design = dstrat)

## ------------------------------------------------------------------------
svycor(~api00 + api99, design = dstrat, digits = 4)

## ------------------------------------------------------------------------
svycor(~api00 + api99, design = dstrat, digits = 4, sig.stats = TRUE, bootn = 2000, mean1 = TRUE)

## ------------------------------------------------------------------------
c <- svycor(~api00 + api99, design = dstrat, digits = 4, sig.stats = TRUE, bootn = 2000, mean1 = TRUE)

c$cors

c$p.values

c$std.err

## ------------------------------------------------------------------------
svyvar(~api00 + api99, design = dstrat)

## ------------------------------------------------------------------------
var <- svyvar(~api00 + api99, design = dstrat)
var <- as.matrix(var)
var

## ------------------------------------------------------------------------
cor <- cov2cor(var)
cor

## ------------------------------------------------------------------------
cor <- cor[1:nrow(cor), 1:nrow(cor)]
cor

## ------------------------------------------------------------------------
out <- svycor(~api99 + api00, design = dstrat)
out$cors

