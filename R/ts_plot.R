# Time series plot
#
# This is a function named 'ts_plot'

ts_plot <-
  function (line_1_data,
            line_2_data,
            start_date,
            end_date,
            y_locations,
            y_labels,
            labels_margin=0) {


    line_1_data <- line_1_data[line_1_data$date >= start_date & line_1_data$date <= end_date,]

    line_2_data <- line_2_data[line_2_data$date >= start_date & line_2_data$date <= end_date, ]

    y_min <- min(line_1_data$value, line_2_data$value)

    y_max <- max(line_1_data$value, line_2_data$value)

    par(mar=                            # setting the margins around the plot
          c(2, 4, 2, 0),                # (bottom, left, top, right)
        cex = 0.9                       # plotting text magnification
    )

    plot(                               # starting a new plot
      line_1_data,                # providing data for the first line
      axes = F,                   # removing the axes
      xlab="", 		  					    # blank space for the x-axis label
      ylab="", 		  					    # blank space for the y-axis label
      type="l",                   # specifying the type of the plot
      lwd=1,                      # specifying the line width
      ylim = c(y_min, y_max),     # specifying the limit of the y-axis

      xlim=                       # specifying the limit of the x-axis
        c(as.Date(start_date), as.Date(end_date) + labels_margin)
    )

    lines(                              # adding a second line
      line_2_data,                      # providing data for the second line
      lty= "dashed"                     # specifying the type of the second line
    )

    axis(						                            # adding an axis to the current plot
      side=1, 				                        	# the axis should be drawn below
      at=as.Date(c(start_date, end_date)),	    # specifying labels locations
      labels=c(format(as.Date(start_date), "%b %d, %Y"),
               format(as.Date(end_date), "%b %d, %Y")),  # specifying the labels
      tick=T,				                          	# no ticks
      family="serif"                      		  # specifying the font
    )

    axis(						                            # adding an axis to the current plot
      side=2, 				                        	# the axis should be drawn to the left
      at=y_locations,	                          # specifying labels locations
      labels=y_labels,                          # specifying the labels
      las=2,                        	      		# specifying the label orientation
      tick=F,				                          	# no ticks
      family="serif"	                      		# specifying the font
    )
  }


