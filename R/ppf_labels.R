# Production possibility frontier plot
#
# This is a function named 'ppf_plot' which takes as an input the
# maximum amount of each of two products that a country can produce  and
# produces a labelled plot of a production
# possibility frontier for the respective country

ppf_plot <- function(
    maximum_quantity_of_product_1 = 50,
    maximum_quantity_of_product_2 = 30,
    new_maximum_quantity_of_product_1 = 50,
    new_maximum_quantity_of_product_2 = 30,
    product_1 = "meat",
    product_2 = "potatoes",
    unit_of_product_1 = "kg",
    unit_of_product_2 = "kg",
    time_period = "month"
) {

  xmax <- maximum_quantity_of_product_1
  ymax <- maximum_quantity_of_product_2
  nxmax <- new_maximum_quantity_of_product_1
  nymax <- new_maximum_quantity_of_product_2

  par(mar = c(5, 4, 4, 2) + 0.1,     # setting the margins (bottom, left, top, right)
      yaxs = "i",
      xaxs = "i",
      tcl = -0.3,
      mgp = c(3, 1, 0)
      # plt = c(0.4, 0.6, 0.1, 0.9)  # option to change the proportion of the axes
  )

  plot.new()                   # starting a blank plot

  plot.window(xlim = c(-1,110), ylim = c(-1, 55), asp = 1)     # setting the plot limits

  xticks <- seq(0, 100, 10)         # setting x-locations of tick marks on x-axis

  yticks <- seq(0, 50, 10)     # setting y-locations of tick marks on y-axis

  axis(side = 1,
       at = xticks,
       labels = xticks,
       pos = 0)

  arrows(x0 = 100,                  # adding arrow at the end of the x-axis
         y0 = 0,
         x1 = 110,
         y1 = 0,
         angle = 45,
         length = 0.05)

  axis(side = 2,
       at = yticks,
       labels = yticks,
       pos = 0,
       las = 2)

  arrows(x0 = 0,                  # adding arrow at the end of the y-axis
         y0 = 50,
         x1 = 0,
         y1 = 55,
         angle = 45,
         length = 0.05)

  title(xlab = paste0('Quantity of ', product_1, " (", unit_of_product_1, "s per ", time_period, ")"),
        ylab = paste0('Quantity of ', product_2, " (", unit_of_product_2, "s per ", time_period, ")"))

  f = expression(ymax - (ymax/(xmax^2))*(x^2))

  curve(ymax - (ymax/(xmax^2))*(x^2),
        add = TRUE,
        from = 0,
        to = xmax
  )

  curve(nymax - (nymax/(nxmax^2))*(x^2),
        add = TRUE,
        from = 0,
        to = nxmax,
        lty = "dashed"
  )

  # mtext(text = "Quantity of\npotatoes\nproduced",
  #       side = 2,
  #       las = 2,
  #       at = 55,
  #       cex = 1)
  #
  # mtext(text = "Quantity of meat produced",
  #       side = 1,
  #       line = 0,
  #       adj = 1,
  #       cex = 1)
}








# lines(seq(0,8), seq(32, 0, -4), type = "o", pch = 16)
#
# text(seq(0.25,8.25), seq(34, 2, -4), labels = LETTERS[1:9])
#
# segments(x0 = 0,
#          y0 = seq(28, 4, -4),
#          x1 = seq(1, 7),
#          y1 = seq(28, 4, -4),
#          lty = "dashed")
#
# segments(x0 = c(1, 2, 3, 4, 5, 6, 7),
#          y0 = c(28, 24, 20, 16, 12, 8, 4),
#          x1 = c(1, 2, 3, 4, 5, 6, 7),
#          y1 = c(0, 0, 0, 0, 0, 0, 0),
#          lty = "dashed")


