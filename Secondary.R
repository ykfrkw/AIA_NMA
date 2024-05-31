# Dichotomous response----
data_nma_binary <- data %>% 
  dplyr::select(c(study, class,n_randomized,r)) 
data_nma_binary<-data_nma_binary[data_nma_binary$study!="Dumon1992",]

df_nma_binary <- netmeta::pairwise(
  treat = class,
  event = r,
  n = n_randomized,
  sm = "OR",
  data = data_nma_binary,
  studlab = study
)

net_nma_binary <- netmeta(TE, seTE, treat1, treat2, studlab,
                          data = df_nma_binary, ref = "placebo",
                          sm = "OR", fixed = FALSE, small = "undesirable")
net_nma_binary

## Forest plot
forest(net_nma_binary, xlim = c(0.1, 100),sortvar = -Pscore,
       smlab = paste("OR"),
       label.left = "Favours placebo",
       label.right = "  Favors intervention",)

# Dichotomous droupout----
data_nma_do <- data %>% 
  dplyr::select(c(study, class,n_randomized,dropout)) 
data_nma_do<-data_nma_do[data_nma_do$study!="Dumon1992",]
 

df_nma_binary_do <- netmeta::pairwise(
  treat = class,
  event = dropout,
  n = n_randomized,
  sm = "OR",
  data = data_nma_do,
  studlab = study
)

net_nma_binary <- netmeta(TE, seTE, treat1, treat2, studlab,
                          data = df_nma_binary_do, ref = "placebo",
                          sm = "OR", fixed = FALSE, small = "desirable")
net_nma_binary

## Forest plot
forest(net_nma_binary, xlim = c(0.1, 100),sortvar = -Pscore,
       smlab = paste("OR"),
       label.left = "Favours placebo",
       label.right = "  Favors intervention",)

# psychotic symptoms ----
data_nma_class_psy <- data %>% 
  dplyr::select(c(study, class,psy_ep_n,psy_ep_mean,psy_ep_sd))

df_nma_class_psy <- netmeta::pairwise(
  treat = class,
  n = psy_ep_n,
  mean = psy_ep_mean,
  sd = psy_ep_sd,
  sm = "SMD",
  data = data_nma_class_psy,
  studlab = study
)


net_nma_class_psy <- netmeta(TE, seTE, treat1, treat2, studlab,
                         data = df_nma_class_psy, ref = "placebo",
                         sm = "SMD", fixed = FALSE, small = "desirable")
net_nma_class_psy



## Forest plot
forest(net_nma_class_psy, xlim = c(-1.5, 2),sortvar = -Pscore,
       smlab = paste("SMD"),
       label.left = "Favours intervention",
       label.right = "  Favors placebo",
       print.subgroup.name=TRUE)

