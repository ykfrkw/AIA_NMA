#kappa

library(irr)
library(tidyverse)


# FT screening decision ---------------------------------------------------


df_ft <- openxlsx::read.xlsx("FTScreening.xlsx")

# two columns with R1 and R2
df_ft <- df_ft %>% 
  dplyr::select(c("Rater1","Rater2"))

# # exclude -> Exclude
# # include -> Include
df_ft[df_ft =="include"] <- "Include"
df_ft[df_ft =="exclude"] <- "Exclude"
df_ft[df_ft =="記入漏れ"] <- NA

kappa2(df_ft)
table(df_ft)
agree(df_ft)

# primary  ---------------------------------------------------
df_kappa <- openxlsx::read.xlsx("AIA_DE.xlsx", sheet = "kappa") 
df_po1 <- as.numeric(df_kappa$R1_ep_mean)
df_po2 <- as.numeric(df_kappa$R2_ep_mean)
df_po <- bind_cols(df_po1, df_po2)
icc(df_po, model = "twoway",
    type = "consistency", unit = "single")

# rob  ---------------------------------------------------
df_kappa_rob <- df_kappa %>% 
  dplyr::select(c(study,R1_rob,R2_rob))%>% # select columns
  group_by(study)%>%
  slice(1)

table(df_kappa_rob$R1_rob, df_kappa_rob$R2_rob)
kappa2(df_kappa_rob, weight = "squared")
agree(df_kappa_rob)

# # componenent  ---------------------------------------------------
# df_kappa <- openxlsx::read.xlsx("AIA_DE.xlsx") 
# #se
# df_se<-data.frame(df_kappa$se1,df_kappa$se2)
# kappa2(df_se)
# table(df_se)
# agree(df_se)
# 
# #sd
# df_sd<-data.frame(df_kappa$sd1,df_kappa$sd2)
# kappa2(df_sd)
# table(df_sd)
# agree(df_sd)
# 
# #cr1	
# df_cr<-data.frame(df_kappa$cr1,df_kappa$cr2)
# kappa2(df_cr)
# table(df_cr)
# agree(df_cr)
# 
# #th1	
# df_th<-data.frame(df_kappa$th1,df_kappa$th2)
# kappa2(df_th)
# table(df_th)
# agree(df_th)
# 
# #cw1	
# df_cw<-data.frame(df_kappa$cw1,df_kappa$cw2)
# kappa2(df_cw)
# table(df_cw)
# agree(df_cw)
# 
# #sr1	
# df_sr<-data.frame(df_kappa$sr1,df_kappa$sr2)
# kappa2(df_sr)
# table(df_sr)
# agree(df_sr)
# 
# #sc1	
# df_sc<-data.frame(df_kappa$sc1,df_kappa$sc2)
# kappa2(df_sc)
# table(df_sc)
# agree(df_sc)
# 
# #re1	
# df_re<-data.frame(df_kappa$re1,df_kappa$re2)
# kappa2(df_re)
# table(df_re)
# agree(df_re)
# 
# #pi1	
# df_pi<-data.frame(df_kappa$pi1,df_kappa$pi2)
# kappa2(df_pi)
# table(df_pi)
# agree(df_pi)
# 
# #w1	
# df_w<-data.frame(df_kappa$w1,df_kappa$w2)
# kappa2(df_w)
# table(df_w)
# agree(df_w)
# 
# #dt1	
# df_dt<-data.frame(df_kappa$dt1,df_kappa$dt2)
# kappa2(df_dt)
# table(df_dt)
# agree(df_dt)
# 
# #ns1	
# df_ns<-data.frame(df_kappa$ns1,df_kappa$ns2)
# kappa2(df_ns)
# table(df_ns)
# agree(df_ns)
# 
# #he1	
# df_he<-data.frame(df_kappa$he1,df_kappa$he2)
# kappa2(df_he)
# table(df_he)
# agree(df_he)
# 
# #tg1	
# df_tg<-data.frame(df_kappa$tg1,df_kappa$tg2)
# kappa2(df_tg)
# table(df_tg)
# agree(df_tg)
# 
# #ind1	
# df_ind<-data.frame(df_kappa$ind1,df_kappa$ind2)
# kappa2(df_ind)
# table(df_ind)
# agree(df_ind)
# 
# #gp1	
# df_gp<-data.frame(df_kappa$gp1,df_kappa$gp2)
# kappa2(df_gp)
# table(df_gp)
# agree(df_gp)
# 
# #ff1	
# df_ff<-data.frame(df_kappa$ff1,df_kappa$ff2)
# kappa2(df_ff)
# table(df_ff)
# agree(df_ff)
# 
# #ae1
# df_ae<-data.frame(df_kappa$ae1,df_kappa$ae2)
# kappa2(df_ae)
# table(df_ae)
# agree(df_ae)
# 
# # Primary outcome consistency ---------------------------------------------
# 
# ## Rater1's n + r
# df_po1   <- c(df_kappa$n1, df_kappa$remission1)
# df_po1 <- as.numeric(df_po1)
# ## Rater2's n + r
# df_po2   <- c(df_kappa$n2, df_kappa$remission2)
# df_po2 <- as.numeric(df_po2)
# ## dataframe R1 vs R2
# df_po <- bind_cols(df_po1, df_po2)
# 
# icc(df_po, model = "twoway",
#     type = "consistency", unit = "single")
# 
# # RoB2 consistency --------------------------------------------------------
# #df_kappa_rob <- openxlsx::read.xlsx("DE_20230419_kappa_rob.xlsx") 
# df_kappa_rob <- openxlsx::read.xlsx("sliced_rob_20230430.xlsx") # each domain
# 
# # df_rob2_1 <- df_rob2_1 %>%
# #   group_by(study) %>%
# #   slice(1) #chose the first line of each study
# #   # need to delete duplicates (almost by hand...)
# 
# # data_rob <- data %>%
# #   dplyr::select(c(study, D1,D2,D3,D4,D5,Overall))%>% # select columns
# #   group_by(study) %>%
# #   slice(1) #chose the first line of each study
# #   # need to delete duplicates (almost by hand...)
# # write.xlsx(data_rob,file = 'data_rob.xlsx', rowNames = F)
# 
# # RoB2
# df_kappa_rob <- df_kappa_rob %>% 
#   dplyr::select(c(rob1,rob2)) # select columns
# 
# table(df_kappa_rob$rob1, df_kappa_rob$rob2)
# kappa2(df_kappa_rob, weight = "squared")
# agree(df_kappa_rob)
# 
# # D1
# df_kappa_d1 <- df_kappa_rob %>% 
#   dplyr::select(c(D11,D12)) # select columns
# 
# table(df_kappa_d1$D11, df_kappa_d1$D12)
# kappa2(df_kappa_d1, weight = "squared")
# agree(df_kappa_d1)
# 
# # D2
# df_kappa_d2 <- df_kappa_rob %>% 
#   dplyr::select(c(D21,D22)) # select columns
# 
# table(df_kappa_d2$D21, df_kappa_d2$D22)
# kappa2(df_kappa_d2, weight = "squared")
# agree(df_kappa_d2)
# 
# # D3
# df_kappa_d3 <- df_kappa_rob %>% 
#   dplyr::select(c(D31,D32)) # select columns
# 
# table(df_kappa_d3$D31, df_kappa_d3$D32)
# kappa2(df_kappa_d3, weight = "squared")
# agree(df_kappa_d3)
# 
# # D4
# df_kappa_d4 <- df_kappa_rob %>% 
#   dplyr::select(c(D41,D42)) # select columns
# 
# table(df_kappa_d4$D41, df_kappa_d4$D42)
# kappa2(df_kappa_d4, weight = "squared")
# agree(df_kappa_d4)
# 
# # D5
# df_kappa_d5 <- df_kappa_rob %>% 
#   dplyr::select(c(D51,D52)) # select columns
# 
# table(df_kappa_d5$D51, df_kappa_d5$D52)
# kappa2(df_kappa_d5, weight = "squared")
# agree(df_kappa_d5)
# 
