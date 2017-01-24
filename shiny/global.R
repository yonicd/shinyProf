library(igraph)
library(zoo)
library(reshape2)
library(dplyr)
library(RColorBrewer)

load('GS.rda')
source('convertStack.R')
#to load from real run: GS.net=convertStack(shiny:::.graphStack$as_list())