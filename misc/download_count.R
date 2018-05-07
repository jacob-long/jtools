dsa <-
  adjustedcranlogs::adj_cran_downloads(packages = "jtools",
                                       from = "2014-02-28", to = Sys.Date())
dsa <- dsa[which(dsa$total_downloads > 0)[1]:nrow(dsa),]
ds <-
  cranlogs::cran_downloads(packages = "jtools",
                                       from = "2017-02-28", to = Sys.Date())
library(ggplot2)
dsa$avg_7 <- filter(dsa$count, rep(1, 7), sides = 1)/7
dsa$adj_avg_7 <- filter(dsa$adjusted_downloads, rep(1, 7), sides = 1)/7

dst <- dsa[c("date", "avg_7", "adj_avg_7")]
library(tidyr)
dst <- gather(dst, key = "type", value = "downloads", -date)

ggplot(data = dst) +
  geom_line(aes(x = date, y = downloads, colour = type)) +
  xlab("") + ylab("Daily downloads (7-day average)") +
  scale_colour_hue(breaks = c("adj_avg_7" ,"avg_7"),
                   labels = c("Adjusted", "Total"),
                   name = "Counting method") +
  geom_vline(xintercept = dsa$date[dsa$updateday], alpha = 0.5, linetype = "dashed") +
  jtools::theme_apa(legend.pos = "topleft", legend.use.title = TRUE,
                    remove.y.gridlines = FALSE, remove.x.gridlines = FALSE,
                    legend.font.size = 10) +
  theme(legend.title = element_text(size = 11)) +
  ggtitle("jtools daily download trends") +
  labs(caption = "Dashed vertical lines show dates of updates")

total_dls <- sum(dsa$count)
