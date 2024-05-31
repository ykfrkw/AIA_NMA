#Transitivity

data_transitivity <- openxlsx::read.xlsx("AIA_DE.xlsx",sheet="BoxPlot") %>%
  dplyr::group_by(study)%>%
  dplyr::slice(1)
data_transitivity$pro_akathsia <- as.numeric(data_transitivity$pro_akathsia) 
data_transitivity$bp_mean <- as.numeric(data_transitivity$bp_mean) 
data_transitivity$year <- as.numeric(data_transitivity$year) 

# year -----
gt1 <- with(data_transitivity,                       # Order boxes by median
            reorder(comparison,
                    year,
                    median))

boxplot(
  year ~ gt1,
  data = data_transitivity,
  main = "publication year",
  cex.axis=0.5,
  xlab="comparison",
  ylab="publication year"
  #col = c("pink","lightgreen" ,"lightblue")
)

# severity -----
data_transitivity2<-subset(data_transitivity,!(is.na(data_transitivity$bp_mean)))

gt2 <- with(data_transitivity2,                       # Order boxes by median
            reorder(comparison,
                    bp_mean,
                    median))
boxplot(
  bp_mean ~ gt2,
  data = data_transitivity2,
  main = "BARS global",
  cex.axis=0.5,
  ylim=c(0,5),
  xlab="comparison",
  ylab="severity (BARS global)"
  #col = c("pink","lightgreen" ,"lightblue")
)

# proportion pro_akathsia -----
data_transitivity3<-subset(data_transitivity,!(is.na(data_transitivity$pro_akathsia)))

gt3 <- with(data_transitivity3,                       # Order boxes by median
            reorder(comparison,
                    pro_akathsia,
                    median))
boxplot(
  pro_akathsia ~ gt3,
  data = data_transitivity3,
  main = "Proportion of akathisia-inducing antipsychotics",
  cex.axis=0.5,
  ylim=c(0,1),
  xlab="comparison",
  ylab="proportion"
  #col = c("pink","lightgreen" ,"lightblue")
)
