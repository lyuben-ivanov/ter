# Production possibility frontier plot
#
# This is an example function named 'ppf_plot'
# which takes as an input the names of two products
# and the production per hour for each product and
# produces a labelled plot of a production
# possibility frontier for the respective country

par(mar = c(4, 5, 1, 1),     # setting the margins (bottom, left, top, right)
    yaxs = "i",
    xaxs = "i",
    tcl = -0.3,
    mgp = c(2, 0.5, 0)
    # plt = c(0.4, 0.6, 0.1, 0.9)  # option to change the proportion of the axes
)

plot.new()                   # starting a blank plot

plot.window(xlim = c(-1,9), ylim = c(-1, 36))     # setting the plot limits

xticks <- seq(0, 8)         # setting x-locations of tick marks on x-axis

yticks <- seq(0, 32, 4)     # setting y-locations of tick marks on y-axis

axis(side = 1,
     at = xticks,
     labels = xticks,
     pos = 0)

arrows(x0 = 8,                  # adding arrow at the end of the x-axis
       y0 = 0,
       x1 = 9,
       y1 = 0,
       angle = 45,
       length = 0.05)

axis(side = 2,
     at = yticks,
     labels = yticks,
     pos = 0,
     las = 2)

arrows(x0 = 0,                  # adding arrow at the end of the y-axis
       y0 = 32,
       x1 = 0,
       y1 = 36,
       angle = 45,
       length = 0.05)

lines(seq(0,8), seq(32, 0, -4), type = "o", pch = 16)

text(seq(0.25,8.25), seq(34, 2, -4), labels = LETTERS[1:9])

segments(x0 = 0,
         y0 = seq(28, 4, -4),
         x1 = seq(1, 7),
         y1 = seq(28, 4, -4),
         lty = "dashed")

segments(x0 = c(1, 2, 3, 4, 5, 6, 7),
         y0 = c(28, 24, 20, 16, 12, 8, 4),
         x1 = c(1, 2, 3, 4, 5, 6, 7),
         y1 = c(0, 0, 0, 0, 0, 0, 0),
         lty = "dashed")

mtext(text = "Quantity of\npotatoes\nproduced",
      side = 2,
      las = 2,
      at = 33,
      cex = 0.9)

mtext(text = "Quantity of meat produced",
      side = 1,
      line = 1.5,
      adj = 1,
      cex = 0.9)
