# Time series plot
#
# This is a function named 'ts_plot'. It takes as an input the values and the
# labels for up to three lines and plots them on a nice looking plot with
# minimal ink

ts_plot <-
  function (dates = 0:10,
            line_1_values = 0:10,
            line_2_values = NULL,
            line_3_values = NULL,
            line_1_label = "Line 1",
            line_2_label = "Line 2",
            line_3_label = "Line 3",
            x_ticks = c(0,10),
            x_labels = c(0,10),
            y_ticks = 0:10,
            y_labels = 0:10,
            labels_margin = 3) {

    y_min <- min(line_1_values, line_2_values, line_3_values)

    y_max <- max(line_1_values, line_2_values, line_3_values)

    par(mar =                     # set margins around plot
        c(4, 4, 4, 4),            # (bottom, left, top, right)
        cex = 0.9,                # text magnification
        xaxs = "i"
    )

    plot(                         # start new plot
      x = dates,
      y = line_1_values,          # provide values for first line
      axes = F,                   # remove axes
      xlab = "", 		  					  # blank space for x-axis label
      ylab = "", 		  					  # blank space for y-axis label
      type = "l",                 # specify type of plot
      lwd = 1,                    # specify line width
      ylim = c(y_min, y_max*1.1), # specify limit of y-axis
      xlim =                      # specify limit of x-axis
        c(min(dates), max(dates) + labels_margin)
    )

    text(x = tail(dates, 1),
         y = tail(line_1_values, 1),
         labels = line_1_label,
         pos = 4
    )

  if (is.numeric(line_2_values) == T) {
    lines(                              # add second line
      x = dates,
      y = line_2_values,                # provide values for second line
      lty= "dashed"                     # specify type of second line
    )

    text(x = tail(dates, 1),
         y = tail(line_2_values, 1),
         labels = line_2_label,
         pos = 4
         )

  }

    if (is.numeric(line_3_values) == T) {
      lines(                              # add third line
        x = dates,
        y = line_3_values,                # provide values for third line
        lty = "dotted"                    # specify type of third line
      )

      text(x = tail(dates, 1),
           y = tail(line_2_values, 1),
           labels = line_2_label,
           pos = 4
      )
    }


    axis(						                    # add axis to plot
      side = 1, 				                # axis should be drawn below
      at = x_ticks,	                    # specify labels locations
      labels = x_labels,                # specify labels
      tick = T,				                  # no ticks
      family = "serif"                  # specify font
    )

    axis(						                    # add axis to plot
      side = 2, 				                # axis should be drawn to the left
      at = y_ticks,	                    # specify labels locations
      labels = y_labels,                # specify labels
      las = 2,                          # specify label orientation
      tick = F,				                  # no ticks
      family = "serif"	                # specify font
    )
  }


