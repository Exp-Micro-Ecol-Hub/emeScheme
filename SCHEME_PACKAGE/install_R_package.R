if (!require(devtools)) {
  install.packages("devtools")
}

devtools::install_github(
  repo = "Exp-Micro-Ecol-Hub/emeScheme",
  ref = "master",
  force = TRUE,
  upgrade = "never"
)
