#installing packages
if (!suppressPackageStartupMessages(require("pacman"))) { install.packages("pacman") }

pacman::p_load(
  # For data manipulation
  tidyr,
  dplyr,
  googlesheets4,
  readxl,
  stringr,
  
  # For testing
  assertthat,
  
  # For spatial models
  spaMM,
  
  # For PCA
  psych,
  
  # For phylogenetics models
  ape,
  geiger,
  phylolm,
  phangorn,
  
  # For plotting
  ggplot2,
  grid,
  
  # For IIR
  irr,
  lsr,
  pwr
)
