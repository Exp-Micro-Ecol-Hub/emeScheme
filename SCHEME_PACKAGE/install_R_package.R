rold <- options("repos")
r <- rold
r["dmdScheme"] <- "https://exp-micro-ecol-hub.github.io/dmdSchemeRMaster/"
options(repos = r)

install.packages("emeScheme")

options(repos = rold)
