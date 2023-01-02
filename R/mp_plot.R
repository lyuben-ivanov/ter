#' Profit maximization with market power plot
#'
#'

# assign function definition to mp_plot()

mp_plot <-
  function(
    profit_maximizing_q = 20,
    profit_minimizing_q = 4,
    fixed_costs = 1000,
    market_price = 250,
    demand_slope = 5,
    reservantion_price = 500,
    show_average_cost_curve = FALSE,
    show_average_variable_cost_curve = FALSE,
    show_marginal_cost_curve = FALSE,
    show_profit_maximizing_quantity = FALSE
  ) {
    a <- profit_maximizing_q
    b <- profit_minimizing_q
    d <- market_price
    f <- fixed_costs

# provide plot parameters

    par(mar =                     # set margins around plot
          c(4, 5, 4, 7),        # (bottom, left, top, right)
        cex = 0.9,                # text magnification
        yaxs = "i",
        xaxs = "i",
        tcl = 0,
        las = 1
    )

# start a new plot

    plot(
      x = seq(from = 0, to = 30, by = 5),
      type = "n",
      xlim = c(0,33),
      ylim = c(0, 595),
      bty = "l",
      xlab = "",
      ylab = "",
    )

  }
