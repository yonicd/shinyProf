library(ggnet)
library(zoo)
library(reshape2)
library(dplyr)
library(RColorBrewer)

load('GS.rda')
GS.net$timeid=1:nrow(GS.net)