# TeX Export

# This a function that exports a plot object to a tex file using the awesome
# tikzDevice engine

texport <-
  function(x, file_name = "figure_1", operating_system = "macOS") {
    if (operating_system == "macOS" | operating_system == "Linux") {
      file_path <- paste0(getwd(),"/", file_name, ".tex")
    } else{
      file_path <- paste0(getwd(),"\\", file_name, ".tex")
    }

  tikz(                                            # starting tikzDevice
    file = file_path,         # output path and file name
    width = 4.5, height = 2.5                      # the size of the figure
  )

  x

  dev.off()
}
