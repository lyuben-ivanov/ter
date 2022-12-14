#' Stock prices plot
#'
#' @param start_date A date in the format "YYYY-MM-DD" (e.g. "1983-06-27")
#' @param end_date A date in the format "YYYY-MM-DD" (e.g. "1983-06-27")
#' @param S_and_P_500_benchmark Logical to indicate whether the S&P 500
#' index should be plotted as a benchmark, the default value is TRUE
#' @param stock_ticker_1 Character to provide the ticker of the first stock
#' @param stock_ticker_2 Character to provide the ticker of the second stock
#' @param freq_data Character to indicate the frequency of financial data:
#' 'daily', 'weekly', 'monthly' (default), 'yearly'
#'
#'
#'
#'

stock_plot <-
  function (start_date = "1983-06-27",
            end_date = Sys.Date(),
            stock_ticker_1 = "^GSPC",
            stock_ticker_2 = "^GSPC",
            label_1 = "SP 500",
            label_2 = "SP 500",
            freq_data = "monthly",
            annotation_x = NULL,
            annotation_y = NULL,
            annotation_text = NULL) {

    par(mar =                     # set margins around plot
          c(4, 4, 4, 4),          # (bottom, left, top, right)
        cex = 0.9,                # text magnification
        yaxs = "i",
        tcl = -0.15,
        las = 1
    )

    options(scipen = 999)

    stock_1_data <-
      yf_get(
      tickers = stock_ticker_1,
      first_date = start_date,
      last_date = end_date,
      freq_data = freq_data
           ) |>
      select(ref_date, price_close) |>
      mutate(index = price_close/price_close[1]*100)

    stock_2_data <-
      yf_get(
        tickers = stock_ticker_2,
        first_date = start_date,
        last_date = end_date,
        freq_data = freq_data
    ) |>
      select(ref_date, price_close) |>
      mutate(index = price_close/price_close[1]*100)

    y_max <-
      stock_1_data |>
      rbind(stock_2_data) |>
      use_series(index) |>
      max()

      stock_1_data |>
        select(ref_date, index) |>
        plot(
          type = "l",
          bty = "l",
          xlab = "Time",
          main = "",
          ylab = "",
          ylim = c(0, y_max)
          )

      stock_2_data |>
        select(ref_date, index) |>
        lines(
          lty = "dashed"
        )

      mtext(
        text = c(label_1, label_2),
        side = 4,
        cex = 0.9,
        at = c(
          stock_1_data |>
          use_series(index) |>
          last(),
          stock_2_data |>
            use_series(index) |>
            last()
          )
        )
  }
