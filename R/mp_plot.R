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
    demand_slope = -20,
    reservation_price = 500,
    show_demand = FALSE,
    show_marginal_revenue = FALSE,
    show_average_cost_curve = FALSE,
    show_average_variable_cost_curve = FALSE,
    show_marginal_cost_curve = FALSE,
    show_profit_maximizing_quantity = FALSE,
    show_values = FALSE
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

    # par(
    #   lab = c(
    #     x = 1,
    #     y = 1,
    #     len = 7
    #   )
    # )

# start a new plot

    if (show_values == TRUE) {
      plot(
        x = seq(from = 0, to = 30, by = 5),
        type = "n",
        xlim = c(0,33),
        ylim = c(0, 595),
        bty = "l",
        xlab = "",
        ylab = ""
      )
    } else {
      plot(
        x = seq(from = 0, to = 30, by = 5),
        type = "n",
        xlim = c(0,33),
        ylim = c(0, 595),
        bty = "l",
        xlab = "",
        ylab = "",
        xaxt = "n",
        yaxt = "n"
      )
    }

# add arrows to end of plot axes

    usr <- par("usr")
    arrows(
      x0 = usr[1L],
      x1 = usr[1:2],
      y0 = usr[3L],
      y1 = usr[4:3],
      length = 0.1,
      angle = 20,
      xpd = TRUE
    )

# label y-axis

    mtext(
      text = "Price\n Cost\nRevenue",
      side = 2,
      cex = 0.9,
      at = 530,
      line = 0.5
    )

# label x-axis

    mtext(
      text = "Quantity",
      side = 1,
      cex = 0.9,
      at = 30,
      line = 0.5,
      bg = "black"
    )

# draw demand curve

    if (show_demand == TRUE & demand_slope != 0) {

      abline(
        a = reservation_price,
        b = demand_slope
      )

      mtext(
        text = "D=P=AR",
        side = 1,
        at = -reservation_price/demand_slope,
        line = -1.5,
        cex = 0.9,
        adj = 0
      )
    }

    if (show_demand == TRUE & demand_slope == 0) {

      abline(
        a = reservation_price,
        b = demand_slope
      )

      mtext(
        text = "D=P=AR=MR",
        side = 4,
        at = reservation_price,
        line = 0.5,
        cex = 0.9,
        adj = 0
      )
    }

# draw average cost curve

    if (show_average_cost_curve == TRUE) {
      curve(
        expr = (x^2 - 3*(a + b)/2*x + 3*a*b + d + f/x),
        from = 1,
        to = 30,
        xlim = c(0,33),
        ylim = c(0, 595),
        lty = "longdash",
        add = TRUE
      )

      text(
        x = 30,
        y = (30^2 - 3*(a + b)/2*30 + 3*a*b + d + f/30),
        labels = "ATC",
        xpd = TRUE,
        pos = 4
      )
    }

# draw average variable cost curve

    if (show_average_variable_cost_curve == TRUE) {
      curve(
        expr = (x^2 - 3*(a + b)/2*x + 3*a*b + d),
        from = 1,
        to = 30,
        lty = "dotted",
        add = TRUE
      )

      text(
        x = 30,
        y = (30^2 - 3*(a + b)/2*30 + 3*a*b + d),
        labels = "AVC",
        xpd = TRUE,
        pos = 4
      )
    }

# draw marginal cost curve

    if (show_marginal_cost_curve == TRUE) {
      curve(
        expr = (3*x^2 - 3*(a + b)*x + 3*a*b + d),
        from = 5,
        to = 24,
        bty = "l",
        lty = "dotdash",
        add = TRUE
      )

      text(
        x = 24,
        y = (3*24^2 - 3*(a + b)*24 + 3*a*b + d),
        labels = "MC",
        xpd = TRUE,
        pos = 4
      )
    }

# draw marginal revenue curve

    if (show_marginal_revenue == TRUE & demand_slope != 0) {

      abline(
        a = reservation_price,
        b = 2*demand_slope,
        lty = "twodash"
      )

      mtext(
        text = "MR",
        side = 1,
        at = (-reservation_price/demand_slope)/2,
        line = -1.5,
        cex = 0.9,
        adj = 0
      )
    }

# show profit maximizing quantity and price

    if (show_profit_maximizing_quantity == T) {

      profit_maximizing_q <- uniroot(
        function(x) 3*x^2 - 3*(a + b)*x + 3*a*b + d
        - reservation_price - 2*demand_slope*x,
        lower = 0,
        upper = 35
      ) |> use_series(root)

      profit_maximizing_p <-
        reservation_price + demand_slope*profit_maximizing_q

      lines(
        x = c(profit_maximizing_q, profit_maximizing_q),
        y = c(profit_maximizing_p, 0),
        lty = "dashed"
      )

      mtext(
        text = "Q*",
        side = 1,
        line = 0.5,
        cex = 0.9,
        at = profit_maximizing_q,
      )

      lines(
        x = c(0, profit_maximizing_q),
        y = c(profit_maximizing_p, profit_maximizing_p),
        lty = "dashed"
      )

      mtext(
        text = "P*",
        side = 2,
        line = 0.5,
        cex = 0.9,
        at = profit_maximizing_p,
      )

    }

  }
