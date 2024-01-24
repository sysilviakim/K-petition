renv::init()
install.packages("devtools")
install.packages("remotes")
install.packages("colorspace")
library(remotes)
install_github(
  "sysilviakim/Kmisc", INSTALL_opts = c("--no-multiarch"), dependencies = TRUE
)
Kmisc::proj_skeleton()
install_github(
  "wch/extrafont", INSTALL_opts = c("--no-multiarch"), dependencies = TRUE
)
install_github(
  "wch/fontcm", INSTALL_opts = c("--no-multiarch"), dependencies = TRUE
)

# Install typically used libraries
install.packages("plyr")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("here")
install.packages("assertthat")
install.packages("janitor")
install.packages("data.table")
install.packages("xtable")
install.packages("styler")

# Web scraping and text analysis
install.packages("xml2")
install.packages("tidytext")

renv::snapshot()