#' Profit maximization in competitive markets plot
#'
#'
#'
#'
#'
#'

# cost_function <- function(a=20,b=4,d=250,f=1000,x=6) {
#   x^3 - (3*(a + b)/2)*(x^2) + (3*a*b+d)*x + f
# }

# assign function definition to cm_plot()

cm_plot <-
  function(
    profit_maximizing_q = 20,
    profit_minimizing_q = 4,
    market_price = 250,
    fixed_costs = 1000,
    price_adjusment = 0,
    show_average_cost_curve = FALSE,
    show_average_variable_cost_curve = FALSE,
    show_marginal_cost_curve = FALSE,
    show_market_price = TRUE,
    show_profit_maximizing_quantity = FALSE
    ) {
    a <- profit_maximizing_q
    b <- profit_minimizing_q
    d <- market_price
    f <- fixed_costs
    h <- price_adjusment

# provide plot parameters

    par(mar =                     # set margins around plot
          c(4, 5, 4, 7),        # (bottom, left, top, right)
        cex = 0.9,                # text magnification
        yaxs = "i",
        xaxs = "i",
        tcl = 0,
        las = 1
    )

# draw average cost curve

    if (show_average_cost_curve == TRUE) {
      curve(
        expr = (x^2 - 3*(a + b)/2*x + 3*a*b + d + f/x),
        from = 1,
        to = 30,
        xlim = c(0,33),
        ylim = c(0, 595),
        bty = "l",
        xlab = "",
        ylab = "",
        lty = "longdash"
      )

      text(
        x = 30,
        y = (30^2 - 3*(a + b)/2*30 + 3*a*b + d + f/30),
        labels = "ATC",
        xpd = TRUE,
        pos = 4
      )
    } else {
      curve(
        expr = (x^2 - 3*(a + b)/2*x + 3*a*b + d + f/x),
        from = 1,
        to = 30,
        xlim = c(0,33),
        ylim = c(0, 595),
        bty = "l",
        xlab = "",
        ylab = "",
        lty = "blank"
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

# draw market price (with possible adjustment to demonstrate normal profit)

    if (show_market_price == TRUE) {
      abline(h = d + h)

      mtext(
        text = "D=P=AR=MR",
        side = 4,
        at = d + h,
        line = 0.5,
        cex = 0.9
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
      at = 645,
      line = 1
      )

# label x-axis

    mtext(
      text = "Quantity",
      side = 1,
      cex = 0.9,
      at = 35,
      line = 1,
      bg = "black"
    )

# show profit maximizing quantity

    if (show_profit_maximizing_quantity == T) {
      lines(
        x = c(profit_maximizing_q, profit_maximizing_q),
        y = c(d, 0),
        lty = "dashed"
      )
    }

  }
