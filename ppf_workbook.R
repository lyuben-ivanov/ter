# Workbook downloads

# This is a function that downloads R Notebooks with exercises from GitHub

ppf_workbook <- function(operating_system = "macOS") {
  if (operating_system == "macOS") {
    download.file(url = "https://tinyurl.com/5avdyayt",
                  destfile = paste0(getwd(),"/ppf.Rmd"))
    system("open ppf.Rmd", wait = FALSE)
  } else if (operating_system == "Windows") {
    download.file(url = "https://tinyurl.com/5avdyayt",
                  destfile = paste0(getwd(),"\\ppf.Rmd"))
    shell("ppf.Rmd", wait = FALSE)
  } else {
    download.file(url = "https://tinyurl.com/5avdyayt",
                  destfile = paste0(getwd(),"/ppf.Rmd"))
    system("xdg-open ppf.Rmd", wait = FALSE)
  }
}
