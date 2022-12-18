#' Profit maximization in competitive markets plot
#'
#'
#'
#'
#'
#'

cm_plot <-
  function(
    profit_maximizing_q = 20,
    profit_minimizing_q = 4,
    market_price = 250,
    fixed_costs = 1000
    ) {
    a <- profit_maximizing_q
    b <- profit_minimizing_q
    d <- market_price
    f <- fixed_costs

    par(mar =                     # set margins around plot
          c(4, 4, 4, 4),          # (bottom, left, top, right)
        cex = 0.9,                # text magnification
        yaxs = "i",
        tcl = -0.15,
        las = 1
    )

    curve(
      expr = (x^2 - 3*(a + b)/2*x + 3*a*b + d + f/x),
      from = 1,
      to = 25,
      ylim = c(0, 500),
      bty = "l"
    )

    curve(
      expr = (x^2 - 3*(a + b)/2*x + 3*a*b + d),
      from = 1,
      to = 25,
      ylim = c(0, 500),
      bty = "l",
      lty = "dotted",
      add = TRUE
    )

    curve(
      expr = (3*x^2 - 3*(a + b)*x + 3*a*b + d),
      from = 1,
      to = 25,
      ylim = c(0, 500),
      bty = "l",
      lty = "dashed",
      add = TRUE
    )

    abline(h = d)

  }
