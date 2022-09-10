# Production possibility frontier plot
#
# This is a function named 'ppf_plot' which takes as an input the maximum
# amounts of two products that a country can produce given its resources and
# the available technology. The function produces as an output a plot showing
# the production possibility frontier for the respective economy

ppf_plot <- function(
    maximum_quantity_of_product_1 = 50,
    maximum_quantity_of_product_2 = 30,
    new_maximum_quantity_of_product_1 = 50,
    new_maximum_quantity_of_product_2 = 30,
    linear = FALSE,
    plot_point_1 = FALSE,
    point_label_1 = "A",
    quantity_product_1 = 10,
    quantity_product_2 = 28.75,
    plot_point_2 = FALSE,
    point_label_2 = "B",
    quantity_product_1_point_2 = 15,
    quantity_product_2_point_2 = 15,
    x_axis_label = "Production of pizzas (mlns per month)",
    y_axis_label = "Production of robots (thousands per month)"
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

  arrows(x0 = 100,                  # add arrow at the end of x-axis
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

  arrows(x0 = 0,                  # add arrow at the end of y-axis
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

    curve(
      expr = nymax - (nymax/(nxmax^2))*(x^2),
      add = TRUE,
      from = 0,
      to = nxmax,
      lty = "dashed"
    )

  } else {

    curve(
      expr = ymax - (ymax/xmax)*(x),
      add = TRUE,
      from = 0,
      to = xmax
    )

    curve(
      expr = nymax - (nymax/nxmax)*(x),
      add = TRUE,
      from = 0,
      to = nxmax,
      lty = "dashed"
    )
  }

  if (plot_point_1 == TRUE) {
    points(
      x = quantity_product_1,
      y = quantity_product_2,
      pch = 19,                         # set plotting character to solid circle
      cex = 0.66                        # make the solid circle smaller
    )
      
    text(
      x = quantity_product_1 + 2.1,
      y = quantity_product_2 + 2.1,
      labels = point_label_1
    )
  }  
      
  if (plot_point_2 == TRUE) {
    points(
      x = quantity_product_1_point_2,
      y = quantity_product_2_point_2,
      pch = 19,                         # set plotting character to solid circle
      cex = 0.66                        # make the solid circle smaller
    )    

    text(
      x = quantity_product_1_point_2 + 2.1,
      y = quantity_product_2_point_2 + 2.1,
      labels = point_label_2
    )
  }


par(new = TRUE, mgp = c(1.5, 1, 0))
title(xlab = x_axis_label)

par(new = TRUE, mgp = c(1.8, 1, 0))
title(ylab = y_axis_label)


}




