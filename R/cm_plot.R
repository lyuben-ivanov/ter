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

cm_plot <-
  function(
    profit_maximizing_q = 20,
    profit_minimizing_q = 4,
    market_price = 250,
    fixed_costs = 1000,
    price_adjusment = 0,
    show_average_variable_cost_curve = FALSE,
    show_marginal_cost_curve = FALSE,
    show_market_price == FALSE
    ) {
    a <- profit_maximizing_q
    b <- profit_minimizing_q
    d <- market_price
    f <- fixed_costs
    h <- price_adjusment

    par(mar =                     # set margins around plot
          c(4, 5, 4, 4),          # (bottom, left, top, right)
        cex = 0.9,                # text magnification
        yaxs = "i",
        tcl = 0,
        las = 1
    )

# draw average cost curve

    curve(
      expr = (x^2 - 3*(a + b)/2*x + 3*a*b + d + f/x),
      from = 1,
      to = 30,
      ylim = c(0, 575),
      bty = "l",
      xlab = "",
      ylab = "",
      lty = "longdash"
    )

# draw average variable cost curve

   if (average_variable_cost_curve == TRUE) {
     curve(
       expr = (x^2 - 3*(a + b)/2*x + 3*a*b + d),
       from = 1,
       to = 30,
       lty = "dotted",
       add = TRUE
     )
   }

# draw marginal cost curve

  if (show_marginal_cost_curve == TRUE) {
    curve(
      expr = (3*x^2 - 3*(a + b)*x + 3*a*b + d),
      from = 5,
      to = 24,
      bty = "l",
      lty = "dashed",
      add = TRUE
    )
  }

# draw market price (with possible adjustment to demonstrate normal profit)

    if (show_market_price == TRUE) {
      abline(h = d + h)
    }

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

    mtext(
      text = "Price\n Cost\nRevenue",
      side = 2,
      cex = 0.9,
      at = 600,
      line = 1
      )



  }
