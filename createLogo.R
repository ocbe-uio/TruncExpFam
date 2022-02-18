library(hexSticker)
library(ggplot2)
library(TruncExpFam)
set.seed(31898546)
lo <- -4
hi <- 2
x <- rtrunc(1e4, mean = 0, sd = 2, a = -5, b = 5)
dfx <- data.frame(x = as.numeric(x))
dfx$cut <- dfx$x < lo | dfx$x > hi
p <- ggplot(dfx, aes(x, fill = cut)) +
	geom_histogram(breaks = seq(-5, 5, .2), col = "#f4f6ff", size = .3)
p <- p +
	theme_transparent() +
	theme_void() +
	theme(legend.position = "none") +
	scale_fill_manual(values = c("#000000", "#f4f6ff"))
sticker(
	subplot    = p,
	package    = "TruncExpFam",
	filename   = "logo.png",
	s_x        = 1,
	s_y        = 0.85,
	s_width    = 1.4,
	s_height   = 0.8,
	p_color    = "#f4f6ff",
	p_family   = "sans",
	p_fontface = "bold",
	p_size     = 16,
	h_size     = 2,
	h_fill     = "#B60000",
	h_color    = "#f4f6ff",
)
