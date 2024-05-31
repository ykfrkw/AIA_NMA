
# S1 NMA-drug ---------------------------------------------------------------------
# +BUILD -------------------------------------------------------------------
# NMA
## Format trial data

data_nma <- data %>% 
  dplyr::select(c(study, drug,ep_n,ep_mean,ep_sd))
#%>% # select columns
#  dplyr::group_by(study,treatment_group) %>% # order
#  dplyr::summarise(r = sum(r), n = sum(n)) #combine the same treatment_group arms within a trial


df_nma <- netmeta::pairwise(
  treat = drug,
  n = ep_n,
  mean = ep_mean,
  sd = ep_sd,
  sm = "SMD",
  data = data_nma,
  studlab = study
)
head(df_nma)

# +ANALYZE -----------------------------------------------------------------
# for NMA run:
net_nma <- netmeta(TE, seTE, treat1, treat2, studlab,
                   data = df_nma, ref = "placebo",
                   sm = "SMD", fixed = FALSE, small = "desirable")
head(net_nma)

# p-score (SUCRA for frequentist NMA)
netrank(x=net_nma, 
        small.values="good",
        sort = TRUE)

## Network graph
size_net_nma <- tapply(data_nma$ep_n, data_nma$drug, sum)

netgraph(net_nma,
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
decomp.design(net_nma)

# local
netsplit(net_nma) # here you can check which (and how many) loops show inconsistencies between direct and indirect TEs.

## Forest plot
forest(net_nma, xlim = c(-4, 2),sortvar = -Pscore,
       smlab = paste("SMD"),
       label.left = "Favours intervention",
       label.right = "  Favors placebo",)

net_nma




# S2
data_nma_class_S2 <- data %>% 
  dplyr::select(c(study, class,ep_n,ep_mean,ep_sd))
data_nma_class_S2<-data_nma_class_S2[data_nma_class_S2$study!="Dumon1992",]
data_nma_class_S2<-data_nma_class_S2[data_nma_class_S2$study!="Baskak2007",]

df_nma_class_S2 <- netmeta::pairwise(
  treat = class,
  n = ep_n,
  mean = ep_mean,
  sd = ep_sd,
  sm = "SMD",
  data = data_nma_class_S2,
  studlab = study
)

# +ANALYZE -----------------------------------------------------------------
# for NMA run:
net_nma_class_S2 <- netmeta(TE, seTE, treat1, treat2, studlab,
                         data = df_nma_class_S2, ref = "placebo",
                         sm = "SMD", fixed = FALSE, small = "desirable")
net_nma_class_S2

########
data_nma_class_S3 <- data %>% 
  dplyr::select(c(study, class,ep_n,ep_mean,ep_sd,rob))
data_nma_class_S3<-data_nma_class_S3[data_nma_class_S3$study!="Dumon1992",]
data_nma_class_S3<-data_nma_class_S3[data_nma_class_S3$rob!="H",]



df_nma_class_S3 <- netmeta::pairwise(
  treat = class,
  n = ep_n,
  mean = ep_mean,
  sd = ep_sd,
  sm = "SMD",
  data = data_nma_class_S3,
  studlab = study
)
head(df_nma_class)

# +ANALYZE -----------------------------------------------------------------
# for NMA run:
net_nma_class_S3 <- netmeta(TE, seTE, treat1, treat2, studlab,
                         data = df_nma_class_S3, ref = "placebo",
                         sm = "SMD", fixed = FALSE, small = "desirable")
net_nma_class_S3

