# Define the list of packages
packages <- c(
  "sas7bdat", "ggpubr", "AICcmodavg",
  "OnofrAndreaPG/aomisc", "OnofrAndreaPG", "lmfor", "neuralnet",
  "MASS", "rcompanion", "rsq", "blorr", "class", "anfis", "FuzzyR",
  "frbs", "Anfis-package", "maptools", "sp", "raster", "rgdal",
  "dismo", "rJava", "PresenceAbsence", "devtools", "RColorBrewer",
  "pander", "ggplot2", "gridExtra", "systemfit", "reshape", "betareg",
  "DirichletReg", "pacman", "caTools", "psych", "tidyverse", "Metrics"
)

# Install packages with error handling
for (package in packages) {
  message(paste("Installing package:", package))
  if (!require(package, character.only = TRUE)) {
    # Install if not already installed
    install.packages(package)
  }
}

message("All packages installed (or already available).")
