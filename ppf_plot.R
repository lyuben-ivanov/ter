# Production possibility frontier plot
#
# This is a function named 'ppf_plot' which takes as an input the maximum
# amounts of two products that a country can produce given its resources and
# the available technology. The function produces as an output a plot showing
# the production possibility frontier for the respective economy

ppf_plot <- function(
  maximum_quantity_of_product_1 = 50,
  maximum_quantity_of_product_2 = 30,
  plot_new_frontier = FALSE,
  new_maximum_quantity_of_product_1 = 50,
  new_maximum_quantity_of_product_2 = 30,
  plot_point = FALSE,
  point_label = "A",
  quantity_of_product_1 = 10,
  quantity_of_product_2 = 28.75,
  plot_new_point = FALSE,
  new_point_label = "B",
  new_quantity_of_product_1 = 15,
  new_quantity_of_product_2 = 15,
  x_axis_label = "Production of pizzas (mlns per month)",
  y_axis_label = "Production of robots (thousands per month)",
  linear = FALSE
) {

  xmax <- maximum_quantity_of_product_1             # simplify variables
  ymax <- maximum_quantity_of_product_2             # simplify variables
  nxmax <- new_maximum_quantity_of_product_1        # simplify variables
  nymax <- new_maximum_quantity_of_product_2        # simplify variables




  plot.new()                                        # start blank plot
  plot.window(xlim = c(-1,110), ylim = c(-1, 55),   # set plot limits
              asp = 1                               # set axes aspect ratio
  )

  xticks <- seq(0, 100, 10)         # set locations of tick marks on x-axis
  yticks <- seq(0, 50, 10)          # set locations of tick marks on y-axis

  axis(side = 1,                    # add x-axis
       at = xticks,
       labels = xticks,
       xaxs = "i",
       tck = -0.01,
       pos = 0)

  axis(side = 1,                    # add x-axis
       at = xticks,
       labels = FALSE,
       xaxs = "i",
       tck = 0.01,
       pos = 0)

  # add arrow at the end of x-axis
  arrows(x0 = 100,
         y0 = 0,
         x1 = 110,
         y1 = 0,
         angle = 30,
         length = 0.1)

  axis(side = 2,                    # add y-axis
       at = yticks,
       labels = yticks,
       pos = 0,
       yaxs = "i",
       tck = -0.01,
       las = 2)

  axis(side = 2,                    # add y-axis
       at = yticks,
       pos = 0,
       yaxs = "i",
       tck = 0.01,
       las = 2)

  # add arrow at the end of y-axis
  arrows(x0 = 0,
         y0 = 50,
         x1 = 0,
         y1 = 55,
         angle = 30,
         length = 0.1)

  if (linear == FALSE) {

    curve(
      expr = ymax - (ymax/(xmax^2))*(x^2),
      add = TRUE,
      from = 0,
      to = xmax
    )

    if (plot_new_frontier == TRUE) {
      curve(
        expr = nymax - (nymax/(nxmax^2))*(x^2),
        add = TRUE,
        from = 0,
        to = nxmax,
        lty = "dashed"
      )
    }

  } else {

    curve(
      expr = ymax - (ymax/xmax)*(x),
      add = TRUE,
      from = 0,
      to = xmax
    )

    if (plot_new_frontier == TRUE) {

    curve(
      expr = nymax - (nymax/nxmax)*(x),
      add = TRUE,
      from = 0,
      to = nxmax,
      lty = "dashed"
    )
   }
  }

  if (plot_point == TRUE) {
    points(
      x = quantity_of_product_1,
      y = quantity_of_product_2,
      pch = 19,                         # set plotting character to solid circle
      cex = 0.66                        # make the solid circle smaller
    )

    text(
      x = quantity_of_product_1 + 1.5,
      y = quantity_of_product_2 + 1.5,
      labels = point_label,
      adj = c(0, 0)
    )
  }

  if (plot_new_point == TRUE) {
    points(
      x = new_quantity_of_product_1,
      y = new_quantity_of_product_2,
      pch = 19,                         # set plotting character to solid circle
      cex = 0.66                       # make the solid circle smaller
    )

    text(
      x = new_quantity_of_product_1 + 1.5,
      y = new_quantity_of_product_2 + 1.5,
      labels = new_point_label,
      adj = c(0, 0)
    )
  }

  mtext(
    text = x_axis_label,
    side = 1,
    line = 1.8
  )

  mtext(
    text = y_axis_label,
    side = 2,
    line = 1.5
  )
  # par(new = TRUE)
  # title(xlab = x_axis_label, line = 1.5)
  #
  # par(new = TRUE, las = 1)
  # title(ylab = y_axis_label, line = 1.8)


}


