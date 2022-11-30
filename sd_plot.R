# Supply and demand plot
#
# This is a function named 'sd_plot' which takes as an input points from
# the supply and demand curves and produces as an output a plot showing
# the supply and demand curves for the respective economy

sd_plot <- function(
  price_1 = 50,
  quantity_demanded_1 = 20,
  quantity_supplied_1 = 60,
  price_2 = 10,
  quantity_demanded_2 = 60,
  quantity_supplied_2 = 20,
  x_axis_label = "Quantity of avocados (tonnes per month)",
  y_axis_label = "Price of avocados (USD per kg)",
  equilbrium_price_label_1 = expression(P[1]^e),
  equilbrium_price_label_2 = expression(P[2]^e),
  equilbrium_quantity_label_1 = expression(Q[1]^e),
  equilbrium_quantity_label_2 = expression(Q[2]^e),
  demand_curve_label = "Demand",
  supply_curve_label = "Supply",
  tick_marks = FALSE,
  demand_change = FALSE,
  supply_change = FALSE
) {

# simplify variables' names
  p1 <- price_1
  qd1 <- quantity_demanded_1
  qs1 <- quantity_supplied_1
  p2 <- price_2
  qd2 <- quantity_demanded_2
  qs2 <- quantity_supplied_2
  epl1 <- equilbrium_price_label_1
  eql1 <- equilbrium_quantity_label_1
  epl2 <- equilbrium_price_label_2
  eql2 <- equilbrium_quantity_label_2
  dql <- demand_curve_label
  sql <- supply_curve_label


# calculate equilibrium

ds <- ((p2 - p1)/(qd2 - qd1))
a <- (p1 - ds*qd1)

ss <- ((p2 - p1)/(qs2 - qs1))
b <- (p1 - ss*qs1)

qe1 <- ((a - b)/(ss - ds))
pe1 <- (a + ds*qe1)

# start blank plot
  plot.new()
# set plot margins (bottom, left, top, right)
  par(mar = c(2,1,1,1))
  plot.window(xlim = c(0,90), ylim = c(0, 55),   # set plot limits
              asp = 1                            # set axes aspect ratio
  )

# set locations of tick marks on x-axis
  xticks <- seq(0, 80, 10)

# set locations of tick marks on y-axis
  yticks <- seq(0, 50, 10)

# add x-axis
if (tick_marks == TRUE) {
  axis(side = 1,
       at = xticks,
       labels = xticks,
       xaxs = "i",
       tck = -0.01,
       pos = 0)

  axis(side = 1,
       at = xticks,
       labels = FALSE,
       xaxs = "i",
       tck = 0.01,
       pos = 0)
} else {
  axis(side = 1,
       at = xticks,
       labels = FALSE,
       tick = TRUE,
       xaxs = "i",
       tck = 0,
       pos = 0)
}

# add arrow at the end of x-axis
  arrows(x0 = 80,
         y0 = 0,
         x1 = 90,
         y1 = 0,
         angle = 30,
         length = 0.1)

# add y-axis
  if (tick_marks == TRUE) {
    axis(side = 2,
         at = yticks,
         labels = yticks,
         pos = 0,
         yaxs = "i",
         tck = -0.01,
         las = 2)

    axis(side = 2,
         at = yticks,
         pos = 0,
         yaxs = "i",
         tck = 0.01,
         las = 2)
  } else {
    axis(side = 2,
         at = yticks,
         labels = FALSE,
         pos = 0,
         yaxs = "i",
         tck = 0,
         las = 2)
  }

# add arrow at the end of y-axis
  arrows(x0 = 0,
         y0 = 50,
         x1 = 0,
         y1 = 55,
         angle = 30,
         length = 0.1)

# draw demand curve
segments(
  x0 = qd1,
  y0 = p1,
  x1 = qd2,
  y1 = p2
)

text(
  x = qd2 + 1.5,
  y = p2,
  labels = expression(D[1]),
  adj = c(0, 0)
)

# draw supply curve
segments(
  x0 = qs1,
  y0 = p1,
  x1 = qs2,
  y1 = p2
)

text(
  x = qs1 + 1.5,
  y = p1,
  labels = expression(S[1]),
  adj = c(0, 0)
)

# label axes
title(xlab = x_axis_label, line = 1)
title(ylab = y_axis_label, line = -2)


# draw paths to equilibrium
segments(
  x0 = 0,
  y0 = pe1,
  x1 = qe1,
  y1 = pe1,
  lty = "dashed"
)

segments(
  x0 = qe1,
  y0 = 0,
  x1 = qe1,
  y1 = pe1,
  lty = "dashed"
)

mtext(text = epl1, at = pe1, side = 2, las = 1, line = -3.1)
mtext(text = eql1, at = qe1, side = 1)

if (demand_change |> is.numeric()) {

# draw new demand curve
  segments(
    x0 = qd1 + demand_change,
    y0 = p1,
    x1 = qd2 + demand_change,
    y1 = p2,
    lty = "dotted"
  )

# label the new demand curve
  text(
    x = qd2 + demand_change + 1.5,
    y = p2,
    labels = expression(D[2]),
    adj = c(0, 0)
  )

  # calculate new equilibrium
  a2 <- a + demand_change
  qe2 <- ((a2 - b)/(ss - ds))
  pe2 <- (a2 + ds*qe2)

  # draw paths to new equilibrium
  segments(
    x0 = 0,
    y0 = pe2,
    x1 = qe2,
    y1 = pe2,
    lty = "dashed"
  )

  segments(
    x0 = qe2,
    y0 = 0,
    x1 = qe2,
    y1 = pe2,
    lty = "dashed"
  )

  # label the new equilibrium
  mtext(text = epl2, at = pe2, side = 2, las = 1, line = -3.1)
  mtext(text = eql2, at = qe2, side = 1)

  if (demand_change > 0) {
    # add arrow towards the new demand curve
    arrows(
      x0 = qd1 + 1.5 + 3,
      y0 = p1 - 3,
      x1 = qd1 + demand_change - 1.5 + 3,
      y1 = p1 - 3,
      angle = 30,
      length = 0.05
    )

    # add arrow towards the new equilibrium quantity
    arrows(
      x0 = qe1 + 1.5,
      y0 = 1.5,
      x1 = qe2 - 1.5,
      y1 = 1.5,
      angle = 30,
      length = 0.05
    )

    # add arrow towards the new equilibrium price
    arrows(
      x0 = 1.5,
      y0 = pe1 + 1.5,
      x1 = 1.5,
      y1 = pe2 - 1.5,
      angle = 30,
      length = 0.05
    )
  } else if (demand_change < 0) {

    # add arrow towards the new demand curve
    arrows(
      x0 = qd1 - 1.5 + 3,
      y0 = p1 - 3,
      x1 = qd1 + demand_change + 1.5 + 3,
      y1 = p1 - 3,
      angle = 30,
      length = 0.05
    )

    # add arrow towards the new equilibrium quantity
    arrows(
      x0 = qe1 - 1.5,
      y0 = 1.5,
      x1 = qe2 + 1.5,
      y1 = 1.5,
      angle = 30,
      length = 0.05
    )

    # add arrow towards the new equilibrium price
    arrows(
      x0 = 1.5,
      y0 = pe1 - 1.5,
      x1 = 1.5,
      y1 = pe2 + 1.5,
      angle = 30,
      length = 0.05
    )
  }
}

if (supply_change |> is.numeric()) {

  # draw new supply curve
  segments(
    x0 = qs1 + supply_change,
    y0 = p1,
    x1 = qs2 + supply_change,
    y1 = p2,
    lty = "dotted"
  )

  # label the new supply curve
  text(
    x = qs1 + supply_change + 1.5,
    y = p1,
    labels = expression(S[2]),
    adj = c(0, 0)
  )

  # calculate new equilibrium
  b2 <- p1 - ss*(qs1 + supply_change)

  # a + ds*qe2 = b2 + ss*qe2
  # a + ds*qe2 = b2 + ss*qe2
  # ss*qe2 - ds*qe2 = a - b2


  qe2 <- ((a - b2)/(ss - ds))
  pe2 <- (b2 + ss*qe2)

  # draw paths to new equilibrium
  segments(
    x0 = 0,
    y0 = pe2,
    x1 = qe2,
    y1 = pe2,
    lty = "dashed"
  )

  segments(
    x0 = qe2,
    y0 = 0,
    x1 = qe2,
    y1 = pe2,
    lty = "dashed"
  )

  # label the new equilibrium
  mtext(text = epl2, at = pe2, side = 2, las = 1, line = -3.1)
  mtext(text = eql2, at = qe2, side = 1)

  if (supply_change > 0) {
    # add arrow towards the new supply curve
    arrows(
      x0 = qs1 + 1.5 - 3,
      y0 = p1 - 3,
      x1 = qs1 + supply_change - 1.5 - 3,
      y1 = p1 - 3,
      angle = 30,
      length = 0.05
    )

    # add arrow towards the new equilibrium quantity
    arrows(
      x0 = qe1 + 1.5,
      y0 = 1.5,
      x1 = qe2 - 1.5,
      y1 = 1.5,
      angle = 30,
      length = 0.05
    )

    # add arrow towards the new equilibrium price
    arrows(
      x0 = 1.5,
      y0 = pe1 - 1.5,
      x1 = 1.5,
      y1 = pe2 + 1.5,
      angle = 30,
      length = 0.05
    )
  } else if (supply_change < 0) {

    # add arrow towards the new supply curve
    arrows(
      x0 = qs1 - 1.5 - 3,
      y0 = p1 - 3,
      x1 = qs1 + supply_change + 1.5 - 3,
      y1 = p1 - 3,
      angle = 30,
      length = 0.05
    )

    # add arrow towards the new equilibrium quantity
    arrows(
      x0 = qe1 - 1.5,
      y0 = 1.5,
      x1 = qe2 + 1.5,
      y1 = 1.5,
      angle = 30,
      length = 0.05
    )

    # add arrow towards the new equilibrium price
    arrows(
      x0 = 1.5,
      y0 = pe1 + 1.5,
      x1 = 1.5,
      y1 = pe2 - 1.5,
      angle = 30,
      length = 0.05
    )
  }
}



}


