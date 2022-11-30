# Production function plot
#
# This is a function named 'pf_plot' which takes as an input a vector of inputs
# and a vector of outputs. It produces as an output a plot showing graphically
# the relationship between inputs and outputs

pf_plot <- function(
  inputs = c(0, 1, 2, 3, 4, 5),
  outputs = c(0, 100, 180, 240, 280, 300),
  x_axis_label = "Number of workers (L)",
  y_axis_label = "Quantity of output (Q)",
  tick_marks = TRUE
) {

# start blank plot
  plot.new()
# set plot margins (bottom, left, top, right)
  par(mar = c(3,4,1,1))
  plot.window(xlim = c(0,6), ylim = c(0, 330))   # set plot limits

# set locations of tick marks on x-axis
  xticks <- inputs

# set locations of tick marks on y-axis
  yticks <- outputs

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
  arrows(x0 = 5,
         y0 = 0,
         x1 = 6,
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
         y0 = 300,
         x1 = 0,
         y1 = 330,
         angle = 30,
         length = 0.1)

# plot production set

lines(inputs, outputs, type = "o", pch = 16, cex = 0.75)

# connect points to axes

segments(
  x0 = inputs[1],
  y0 = outputs[2:6],
  x1 = inputs[2:6],
  y1 = outputs[2:6],
  lty = "dashed"
)

segments(
  x0 = inputs[2:6],
  y0 = outputs[1],
  x1 = inputs[2:6],
  y1 = outputs[2:6],
  lty = "dashed"
)

# label axes
title(xlab = x_axis_label, line = 1.5)
title(ylab = y_axis_label, line = 2)

}


