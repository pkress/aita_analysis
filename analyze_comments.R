##################-
### Author: Peter Kress
### Date: 2022/03/06
### Purpose: Analyze AITA comments and replies for aita posts
##################-

##################-
# Initialize Workspace ----
##################-
## Paths ----
setwd("~/Documents/Personal Projects/AITA/")

## Packages ----

if(!require("pacman")) install.packages("pacman")
library(pacman)
p_load(data.table, magrittr, stringr, ggplot2)

## Handy Functions ----
`%p%` = paste0

month_diff = function(d1, d2){
  12*(year(d2) - year(d1)) + (month(d2) - month(d1))
}

make_title = function(x){ str_to_title(gsub("_", " ", x))}

##################-
# Read in Data ----
##################-

aita = fread("data/raw/aita_clean.csv")
posts = fread("data/intermediate/top_1_250_posts.csv")
comments = fread("data/intermediate/top_1_250_posts_top_50_comments.csv")

##################-
# Clean Data ----
##################-



##################)
# Summary Stats ----
##################)


##################)
# Explore Sentiment ----
##################)


##################)
# Compare polarization to popularity ----
##################)