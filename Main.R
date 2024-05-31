# ABOUT -------------------------------------------------------------------
## R code for 
## AIA_NMA
##
## This is the code for primary analysis.
##

# PREPARE -----------------------------------------------------------------
## clear settings

Sys.setenv(LANGUAGE="en_US.UTF-8")
rm(list=ls())   ###clears memory
cat("\014")   ###clears console
if(!is.null(dev.list())) dev.off() ###clears plots

## library
library(dplyr)
library(openxlsx)
library(meta)
library(netmeta)
library(tidyr)

# RAW ---------------------------------------------------------------------
## load data
data <- read.xlsx("AIA_DE.xlsx",sheet = "Sheet1")

## set data
data$ep_n <- as.numeric(data$ep_n) 
data$ep_mean <- as.numeric(data$ep_mean)
data$ep_sd <- as.numeric(data$ep_sd)

data$n_randomized <- as.numeric(data$n_randomized)
data$r <- as.numeric(data$r)
data$dropout <- as.numeric(data$dropout)
data$psy_ep_n <- as.numeric(data$psy_ep_n)
data$psy_ep_mean <- as.numeric(data$psy_ep_mean)
data$psy_ep_sd <- as.numeric(data$psy_ep_sd)

# NMA-class ---------------------------------------------------------------------
# +BUILD -------------------------------------------------------------------
# NMA
## Format trial data

data_nma_class <- data %>% 
  dplyr::select(c(study, class,ep_n,ep_mean,ep_sd))
data_nma_class<-data_nma_class[data_nma_class$study!="Dumon1992",]


df_nma_class <- netmeta::pairwise(
  treat = class,
  n = ep_n,
  mean = ep_mean,
  sd = ep_sd,
  sm = "SMD",
  data = data_nma_class,
  studlab = study
)
head(df_nma_class)

# +ANALYZE -----------------------------------------------------------------
# for NMA run:
net_nma_class <- netmeta(TE, seTE, treat1, treat2, studlab,
                   data = df_nma_class, ref = "placebo",
                   sm = "SMD", fixed = FALSE, small = "desirable")
net_nma_class

# p-score (SUCRA for frequentist NMA)
netrank(x=net_nma_class, 
        small.values="good",
        sort = TRUE)

## Network graph
size_net_nma_class <- tapply(data_nma_class$ep_n, data_nma_class$class, sum)

netgraph(net_nma_class,
         seq = "optimal",
         col = "black", plastic = FALSE,
         points = TRUE, pch = 21, cex.points = 2.5,
         col.points = "black",
         bg.points = "gray",
         # thickness = "se.fixed", #alternative choice
         thickness = "number.of.studies",
         multiarm = FALSE,
         number.of.studies = TRUE)

# global
decomp.design(net_nma_class)

# local
netsplit(net_nma_class) # here you can check which (and how many) loops show inconsistencies between direct and indirect TEs.

## begin: prediction interval
net_nma_split_class<-netsplit(net_nma_class)
forest(net_nma_split_class,fontsize=6,spacing=0.5,addrow.subgroups=FALSE)

png("prediction_interval.png", width = 400, height = 1750)    # prepare device
forest(net_nma_split_class,fontsize=8,spacing=0.75,show="all", 
       only.reference=FALSE,prediction=TRUE, direct=FALSE,indirect=FALSE)
dev.off()   


## Forest plot
forest(net_nma_class, xlim = c(-3, 1.5),sortvar = -Pscore,
       smlab = paste("SMD"),
       label.left = "Favours intervention",
       label.right = "  Favors placebo",
       print.subgroup.name=TRUE)

### rearrange the order
forest(net_nma_class, xlim = c(-3, 1.5),sortvar = c("benzodiazepines", "vitamin B", 
                                                    "triptans", "beta blockers", "5-HT2A antagonists",
                                                    "anticholinergics","placebo"),
       smlab = paste("SMD"),
       label.left = "Favours intervention",
       label.right = "  Favors placebo",
       print.subgroup.name=TRUE)

## League table
# Create a CSV file with league table for random effects model
league_nma_class <- netleague(net_nma_class, digits = 2, bracket = "(", separator = " to ",seq = netrank(net_nma_class))
write.table(league_nma_class$random, file = "league_nma_class.csv",
            row.names = FALSE, col.names = FALSE, sep = ",")

