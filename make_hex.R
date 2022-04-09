library(showtext)
library(jtools)
library(ggplot2)
library(hexSticker)

font_add("Fantasque Sans Mono", "FantasqueSansMono-Regular.ttf")
font_add("monofur", "monof55.ttf")

# states <- as.data.frame(state.x77)
# states$HSGrad <- states$`HS Grad`
# fit <- lm(Income ~ HSGrad + Murder,
#   data = states)
# p <- effect_plot(model = fit, pred = Murder, x.label = "x", y.label = "y")

x <- rnorm(1000)
y <- 25*x + 20*(x^2) + rnorm(1000, sd=.1)
d <- data.frame(cbind(x, y))
p <- effect_plot(fit <- lm(y ~ poly(x, 2), data = d), "x")

p <- p +  theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
 axis.title.x = element_text(color = "white", size = 5), axis.title.y = element_text(color = "white", size = 5), 
 panel.grid.major = element_line(linetype='longdash', size = 0.25))

plot(sticker(p + theme_transparent(), package="jtools", p_family = "Fantasque Sans Mono",
 s_width = 1.25, s_height = 1.3, s_x = 0.93, s_y = 0.83, p_size = 5, p_y = 1.6, h_color = "#570008", 
 h_fill = "#844247", url = "jtools.jacob-long.com", u_size = 1.6,
  u_family = "Fantasque Sans Mono", filename = "hex.png", dpi = 500))


# atlantic: #466A9F
# garnet: #73000a
# sandstorm: #FFF2E3
# azalea: #844247
# dark garnet: #570008



