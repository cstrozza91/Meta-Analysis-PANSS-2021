### META-ANALYSIS FORENSIC PSYCHIATRY ###

# Libraries and data ------------------------------------------------------

##Libraries
library(tidyverse)
library(readxl)
library(metafor)
windowsFonts(A=windowsFont("Arial"),
             B=windowsFont("Bookman Old Style"))

##Load data
dati <- read_xlsx(path = "Meta-Analysis-PANSS-data.xlsx",
                  sheet = 1, 
                  col_names = T)

# Analysis on forensic patients -------------------------------------------

##positive PANSS domani
dati$PANSS_Positive_mean_forensic <- as.numeric(dati$PANSS_Positive_mean_forensic)
dati$PANSS_Positive_SD_forensic <- as.numeric(dati$PANSS_Positive_SD_forensic)

ma1 <- rma(mi = PANSS_Positive_mean_forensic,
           sdi = PANSS_Positive_SD_forensic,
           ni = N_forensic,
           data = dati,
           measure = "MN", method = "REML")
ma1
confint(ma1)
influence(ma1)

ranktest(ma1)
regtest(ma1)
trimfill(ma1,verbose = T)

par(mfrow=c(1,2))
funnel(trimfill(ma1), digits = 2, main = "Funnel plot", cex = .75)

forest(ma1, 
       slab = paste(dati$Author, dati$Year, sep = ", "),
       mlab = "Random effects model",
       cex = .75,
       xlab = "Raw Mean",
       main = "PANSS Positive Scale",
       refline = NA)
segments(14.67, -1, 14.67, 19, lty = 2, col = 2, lwd = 2)
text(-7, 20, c("Study"), cex = .75)
text(37, 20, c("Mean [95%CI]"), cex = .75)

##negative PANSS domani
dati$PANSS_Negative_mean_forensic <- as.numeric(dati$PANSS_Negative_mean_forensic)
dati$PANSS_Negative_SD_forensic <- as.numeric(dati$PANSS_Negative_SD_forensic)

ma2 <- rma(mi = PANSS_Negative_mean_forensic,
           sdi = PANSS_Negative_SD_forensic,
           ni = N_forensic,
           data = dati,
           measure = "MN", method = "REML")
ma2
confint(ma2)
influence(ma2)

ranktest(ma2)
regtest(ma2)
trimfill(ma2, verbose = T, control = list(maxiter=10000))

par(mfrow=c(1,2))
funnel(trimfill(ma2, verbose = T, control = list(maxiter=10000)), digits = 2, main = "Funnel plot", cex = .75)

forest(ma2, 
       slab = paste(dati$Author, dati$Year, sep = ", "),
       mlab = "Random effects model",
       cex = .75,
       xlab = "Raw Mean",
       main = "PANSS Negative Scale",
       refline = NA)
segments(16.76, -1, 16.76, 18, lty = 2, col = 2, lwd = 2)
text(-6, 19, c("Study"), cex = .75)
text(35, 19, c("Mean [95%CI]"), cex = .75)

##general PANSS domani
dati$PANSS_GeneralPsychopt_mean_forensic <- as.numeric(dati$PANSS_GeneralPsychopt_mean_forensic)
dati$PANSS_GeneralPsychopt_SD_forensic <- as.numeric(dati$PANSS_GeneralPsychopt_SD_forensic)

ma3 <- rma(mi = PANSS_GeneralPsychopt_mean_forensic,
           sdi = PANSS_GeneralPsychopt_SD_forensic,
           ni = N_forensic,
           data = dati,
           measure = "MN", method = "REML")
ma3
confint(ma3)
influence(ma3)

ranktest(ma3)
regtest(ma3)
trimfill(ma3, verbose = T, control = list(maxiter=10000))

par(mfrow=c(1,2))
funnel(trimfill(ma3, verbose = T, control = list(maxiter=10000)), digits = 2, main = "Funnel plot", cex = .75)

forest(ma3,
       slab = paste(dati$Author, dati$Year, sep = ", "),
       mlab = "Random effects model",
       cex = .75,
       xlab = "Raw Mean",
       main = "PANSS General Scale",
       refline = NA)
segments(31.345, -1, 31.345, 16, lty = 2, col = 2, lwd = 2)
text(-6, 17, c("Study"), cex = .75)
text(68, 17, c("Mean [95%CI]"), cex = .75)

##total PANSS 
dati$PANSS_Total_mean_forensic <- as.numeric(dati$PANSS_Total_mean_forensic)
dati$PANSS_Total_SD_forensic <- as.numeric(dati$PANSS_Total_SD_forensic)

ma4 <- rma(mi = PANSS_Total_mean_forensic,
           sdi = PANSS_Total_SD_forensic,
           ni = N_forensic,
           data = dati,
           measure = "MN", method = "REML")
ma4
confint(ma4)
influence(ma4)

ranktest(ma4)
regtest(ma4)
trimfill(ma4, verbose = T, control = list(maxiter=10000))

par(mfrow=c(1,2))
funnel(trimfill(ma4, verbose = T, control = list(maxiter=10000)), digits = 2, main = "Funnel plot", cex = .75)

forest(ma4,
       slab = paste(dati$Author, dati$Year, sep = ", "),
       mlab = "Random effects model",
       cex = .75,
       xlab = "Raw Mean",
       main = "PANSS Total Score",
       refline = NA)
segments(65.26, -1, 65.26, 18, lty = 2, col = 2, lwd = 2)
text(-13, 19, c("Study"), cex = .75)
text(135, 19, c("Mean [95%CI]"), cex = .75)


# Analysis on forensic and non-forensic patients --------------------------

##effect size mean difference forensic vs non-forensic

###positive PANSS domain
dati$PANSS_Positive_mean_NonForensic <- as.numeric(dati$PANSS_Positive_mean_NonForensic)
dati$PANSS_Positive_SD_NonForensic <- as.numeric(dati$PANSS_Positive_SD_NonForensic)
dati$N_NonForensic <- as.numeric(dati$N_NonForensic)

dati_nf <- escalc(measure = "MD", 
                m1i = PANSS_Positive_mean_forensic, 
                sd1i = PANSS_Positive_SD_forensic,
                n1i = N_forensic,
                m2i = PANSS_Positive_mean_NonForensic,
                sd2i = PANSS_Positive_SD_NonForensic,
                n2i = N_NonForensic,
                data = dati)

ma1_nf <- rma(yi, vi, data = dati_nf, method = "REML")
ma1_nf
confint(ma1_nf)
influence(ma1_nf)

ranktest(ma1_nf)
regtest(ma1_nf)
trimfill(ma1_nf,verbose = T)

par(mfrow=c(1,2))
funnel(trimfill(ma1_nf), digits = 2, main = "Funnel plot", cex = .75)

forest(ma1_nf,
       slab = paste(dati$Author, dati$Year, sep = ", "),
       mlab = "Random effects model",
       cex = .75,
       xlab = "Mean difference",
       main = "PANSS Positive Scale:\nforensic - non forensic")
segments(2.45, -1, 2.45, 8, lty = 2, col = 2, lwd = 2)
text(-17, 9, c("Study"), cex = .75)
text(22, 9, c("Mean [95%CI]"), cex = .75)

###negative PANSS domain
dati$PANSS_Negative_mean_NonForensic <- as.numeric(dati$PANSS_Negative_mean_NonForensic)
dati$PANSS_Negative_SD_NonForensic <- as.numeric(dati$PANSS_Negative_SD_NonForensic)

dati_nf <- escalc(measure = "MD", 
                  m1i = PANSS_Negative_mean_forensic, 
                  sd1i = PANSS_Negative_SD_forensic,
                  n1i = N_forensic,
                  m2i = PANSS_Negative_mean_NonForensic,
                  sd2i = PANSS_Negative_SD_NonForensic,
                  n2i = N_NonForensic,
                  data = dati)

ma2_nf <- rma(yi, vi, data = dati_nf, method = "REML")
ma2_nf
confint(ma2_nf)
influence(ma2_nf)

ranktest(ma2_nf)
regtest(ma2_nf)
trimfill(ma2_nf,verbose = T)

par(mfrow=c(1,2))
funnel(trimfill(ma2_nf), digits = 2, main = "Funnel plot", cex = .75)

forest(ma2_nf,
       slab = paste(dati$Author, dati$Year, sep = ", "),
       mlab = "Random effects model",
       cex = .75,
       xlab = "Mean difference",
       main = "PANSS Negative Scale:\nforensic - non forensic")
segments(1.81, -1, 1.81, 8, lty = 2, col = 2, lwd = 2)
text(-13, 9, c("Study"), cex = .75)
text(13, 9, c("Mean [95%CI]"), cex = .75)

###general PANSS domain
dati$PANSS_GeneralPsychopt_mean_NonForensic <- as.numeric(dati$PANSS_GeneralPsychopt_mean_NonForensic)
dati$PANSS_GeneralPsychopt_SD_NonForensic <- as.numeric(dati$PANSS_GeneralPsychopt_SD_NonForensic)

dati_nf <- escalc(measure = "MD", 
                  m1i = PANSS_GeneralPsychopt_mean_forensic, 
                  sd1i = PANSS_GeneralPsychopt_SD_forensic,
                  n1i = N_forensic,
                  m2i = PANSS_GeneralPsychopt_mean_NonForensic,
                  sd2i = PANSS_GeneralPsychopt_SD_NonForensic,
                  n2i = N_NonForensic,
                  data = dati)

ma3_nf <- rma(yi, vi, data = dati_nf, method = "REML")
ma3_nf
confint(ma3_nf)
influence(ma3_nf)

ranktest(ma3_nf)
regtest(ma3_nf)
trimfill(ma3_nf,verbose = T)

par(mfrow=c(1,2))
funnel(trimfill(ma3_nf), digits = 2, main = "Funnel plot", cex = .75)

forest(ma3_nf,
       slab = paste(dati$Author, dati$Year, sep = ", "),
       mlab = "Random effects model",
       cex = .75,
       xlab = "Mean difference",
       main = "PANSS General Scale:\nforensic - non forensic")
segments(3.32, -1, 3.32, 8, lty = 2, col = 2, lwd = 2)
text(-21, 9, c("Study"), cex = .75)
text(25, 9, c("Mean [95%CI]"), cex = .75)


###total PANSS 
dati$PANSS_Total_mean_NonForensic <- as.numeric(dati$PANSS_Total_mean_NonForensic)
dati$PANSS_Total_SD_NonForensic <- as.numeric(dati$PANSS_Total_SD_NonForensic)

dati_nf <- escalc(measure = "MD", 
                  m1i = PANSS_Total_mean_forensic, 
                  sd1i = PANSS_Total_SD_forensic,
                  n1i = N_forensic,
                  m2i = PANSS_Total_mean_NonForensic,
                  sd2i = PANSS_Total_SD_NonForensic,
                  n2i = N_NonForensic,
                  data = dati)

ma4_nf <- rma(yi, vi, data = dati_nf, method = "REML")
ma4_nf
confint(ma4_nf)
influence(ma4_nf)

ranktest(ma4_nf)
regtest(ma4_nf)
trimfill(ma4_nf,verbose = T)

par(mfrow=c(1,2))
funnel(trimfill(ma4_nf), digits = 2, main = "Funnel plot", cex = .75)

forest(ma4_nf,
       slab = paste(dati$Author, dati$Year, sep = ", "),
       mlab = "Random effects model",
       cex = .75,
       xlab = "Mean difference",
       main = "PANSS Total Score:\nforensic - non forensic")
segments(7.65, -1, 7.65, 4, lty = 2, col = 2, lwd = 2)
text(-45, 5, c("Study"), cex = .75)
text(50, 5, c("Mean [95%CI]"), cex = .75)


# Subgroup analysis -------------------------------------------------------

###positive PANSS domain
##SSD%
dati_ssd1 <- 
  dati %>% 
  mutate(`SSD_ %` = as.numeric(`SSD_ %`),
         SSD = as.factor(case_when(`SSD_ %` == 100 ~ 1,
                                   `SSD_ %` < 100 ~ 0))) %>% 
  filter(SSD == 1)

dati_ssd0 <- 
  dati %>% 
  mutate(`SSD_ %` = as.numeric(`SSD_ %`),
         SSD = as.factor(case_when(`SSD_ %` == 100 ~ 1,
                                   `SSD_ %` < 100 ~ 0))) %>% 
  filter(SSD == 0)

#100
ma1_ssd1 <- rma(mi = PANSS_Positive_mean_forensic,
           sdi = PANSS_Positive_SD_forensic,
           ni = N_forensic,
           data = dati_ssd1,
           measure = "MN", method = "REML")
ma1_ssd1
confint(ma1_ssd1)
influence(ma1_ssd1)

ranktest(ma1_ssd1)
regtest(ma1_ssd1)
trimfill(ma1_ssd1,verbose = T)

par(mfrow=c(1,2))
funnel(trimfill(ma1_ssd1), digits = 2, main = "Funnel plot", cex = .75)

forest(ma1_ssd1,
       slab = paste(dati_ssd1$Author, dati_ssd1$Year, sep = ", "),
       main = "Random effects model",
       cex = .75,
       refline = NA)
segments(15.06, -1, 15.06, 15, lty = 2, col = 2, lwd = 2)
text(-7,16,c("Study"), cex = .75)
text(33,16,c("Mean [95%CI]"), cex = .75)

#<100
ma1_ssd0 <- rma(mi = PANSS_Positive_mean_forensic,
               sdi = PANSS_Positive_SD_forensic,
               ni = N_forensic,
               data = dati_ssd0,
               measure = "MN", method = "REML")
ma1_ssd0
confint(ma1_ssd0)
influence(ma1_ssd0)

ranktest(ma1_ssd0)
regtest(ma1_ssd0)
trimfill(ma1_ssd0,verbose = T)

par(mfrow=c(1,2))
funnel(trimfill(ma1_ssd0), digits = 2, main = "Funnel plot", cex = .75)

forest(ma1_ssd0,
       slab = paste(dati_ssd0$Author, dati_ssd0$Year, sep = ", "),
       main = "Random effects model",
       cex = .75,
       refline = NA)
segments(12.98, -1, 12.98, 5, lty = 2, col = 2, lwd = 2)
text(2,6,c("Study"), cex = .75)
text(23,6,c("Mean [95%CI]"), cex = .75)

##setting
table(dati$Setting, useNA = "ifany")

dati_setting1 <- dati %>% filter(Setting == "I")
dati_setting0 <- dati %>% filter(Setting != "I")

#I
ma1_setting1 <- rma(mi = PANSS_Positive_mean_forensic,
                sdi = PANSS_Positive_SD_forensic,
                ni = N_forensic,
                data = dati_setting1[-c(24,25),],
                measure = "MN", method = "REML")
ma1_setting1
confint(ma1_setting1)
influence(ma1_setting1)

ranktest(ma1_setting1)
regtest(ma1_setting1)
trimfill(ma1_setting1,verbose = T)

par(mfrow=c(1,2))
funnel(trimfill(ma1_setting1), digits = 2, main = "Funnel plot", cex = .75)

forest(ma1_setting1,
       slab = paste(dati_setting1[-c(24,25),]$Author, dati_setting1[-c(24,25),]$Year, sep = ", "),
       main = "Random effects model",
       cex = .75,
       refline = NA)
segments(13.95, -1, 13.95, 16, lty = 2, col = 2, lwd = 2)
text(-5, 17, c("Study"), cex = .75)
text(30, 17, c("Mean [95%CI]"), cex = .75)

#0
ma1_setting0 <- rma(mi = PANSS_Positive_mean_forensic,
                sdi = PANSS_Positive_SD_forensic,
                ni = N_forensic,
                data = dati_setting0,
                measure = "MN", method = "REML")
ma1_setting0
confint(ma1_setting0)
influence(ma1_setting0)

ranktest(ma1_setting0)
regtest(ma1_setting0)
trimfill(ma1_setting0,verbose = T)

par(mfrow=c(1,2))
funnel(trimfill(ma1_setting0), digits = 2, main = "Funnel plot", cex = .75)

forest(ma1_setting0,
       slab = paste(dati_setting0$Author, dati_setting0$Year, sep = ", "),
       main = "Random effects model",
       cex = .75,
       refline = NA)
segments(18.18, -1, 18.18, 4, lty = 2, col = 2, lwd = 2)
text(0, 5, c("Study"), cex = .75)
text(30, 5, c("Mean [95%CI]"), cex = .75)

##male pct
table(dati$`%_male_forensic`, useNA = "ifany")

dati_malepct1 <- 
  dati %>% 
  mutate(`%_male_forensic` = as.numeric(`%_male_forensic`),
         malepct = as.factor(case_when(`%_male_forensic` == 100 ~ 1,
                                       `%_male_forensic` < 100 ~ 0))) %>% 
  filter(malepct == 1)

dati_malepct0 <- 
  dati %>% 
  mutate(`%_male_forensic` = as.numeric(`%_male_forensic`),
         malepct = as.factor(case_when(`%_male_forensic` == 100 ~ 1,
                                       `%_male_forensic` < 100 ~ 0))) %>% 
  filter(malepct == 0)

#100
ma1_malepct1 <- rma(mi = PANSS_Positive_mean_forensic,
                    sdi = PANSS_Positive_SD_forensic,
                    ni = N_forensic,
                    data = dati_malepct1,
                    measure = "MN", method = "REML")
ma1_malepct1
confint(ma1_malepct1)
influence(ma1_malepct1)

ranktest(ma1_malepct1)
regtest(ma1_malepct1)
trimfill(ma1_malepct1, verbose = T, control = list(maxiter = 10000))

par(mfrow=c(1,2))
funnel(trimfill(ma1_malepct1, verbose = T, control = list(maxiter = 10000)), digits = 2, main = "Funnel plot", cex = .75)

forest(ma1_malepct1,
       slab = paste(dati_malepct1[-16,]$Author, dati_malepct1[-16,]$Year, sep = ", "),
       main = "Random effects model",
       cex = .75,
       refline = F)
segments(13.88, -1, 13.88, 12, lty = 2, col = 2, lwd = 2)
text(-5, 13, c("Study"), cex = .75)
text(30, 13, c("Mean [95%CI]"), cex = .75)

#<100
ma1_malepct0 <- rma(mi = PANSS_Positive_mean_forensic,
                     sdi = PANSS_Positive_SD_forensic,
                     ni = N_forensic,
                     data = dati_malepct0,
                     measure = "MN", method = "REML")
ma1_malepct0
confint(ma1_malepct0)
influence(ma1_malepct0)

ranktest(ma1_malepct0)
regtest(ma1_malepct0)
trimfill(ma1_malepct0,verbose = T)

par(mfrow=c(1,2))
funnel(trimfill(ma1_malepct0), digits = 2, main = "Funnel plot", cex = .75)

forest(ma1_malepct0,
       slab = paste(dati_malepct0$Author, dati_malepct0$Year, sep = ", "),
       main = "Random effects model",
       cex = .75,
       refline = F)
segments(16.96, -1, 16.96, 6, lty = 2, col = 2, lwd = 2)
text(-5, 7, c("Study"), cex = .75)
text(32, 7, c("Mean [95%CI]"), cex = .75)

##mean age
dati_meanageP <- 
  dati %>%
  filter(!is.na(Age_mean_forensic),
         !is.na(PANSS_Positive_mean_forensic),
         !is.na(PANSS_Positive_SD_forensic),
         !is.na(N_forensic))

summary(as.numeric(dati_meanageP$Age_mean_forensic))

dati_meanage1 <- 
  dati_meanageP %>% 
  mutate(Age_mean_forensic = as.numeric(Age_mean_forensic),
         meanage = as.factor(case_when(Age_mean_forensic >= 36.62 ~ 1,
                                       Age_mean_forensic < 36.62 ~ 0))) %>% 
  filter(meanage == 1)

dati_meanage0 <- 
  dati_meanageP %>% 
  mutate(Age_mean_forensic = as.numeric(Age_mean_forensic),
         meanage = as.factor(case_when(Age_mean_forensic >= 36.62 ~ 1,
                                       Age_mean_forensic < 36.62 ~ 0))) %>% 
  filter(meanage == 0)

#>=36.62
ma1_meanage1 <- rma(mi = PANSS_Positive_mean_forensic,
                    sdi = PANSS_Positive_SD_forensic,
                    ni = N_forensic,
                    data = dati_meanage1,
                    measure = "MN", method = "REML")
ma1_meanage1
confint(ma1_meanage1)
influence(ma1_meanage1)

ranktest(ma1_meanage1)
regtest(ma1_meanage1)
trimfill(ma1_meanage1, verbose = T, control = list(maxiter = 10000))

par(mfrow=c(1,2))
funnel(trimfill(ma1_meanage1, verbose = T, control = list(maxiter = 10000)), digits = 2, main = "Funnel plot", cex = .75)

forest(ma1_meanage1,
       slab = paste(dati_meanage1$Author, dati_meanage1$Year, sep = ", "),
       main = "Random effects model",
       cex = .75,
       refline = F)
segments(14.82, -1, 14.82, 14, lty = 2, col = 2, lwd = 2)
text(-6, 15, c("Study"), cex = .75)
text(33, 15, c("Mean [95%CI]"), cex = .75)

#<36.62
ma1_meanage0 <- rma(mi = PANSS_Positive_mean_forensic,
                     sdi = PANSS_Positive_SD_forensic,
                     ni = N_forensic,
                     data = dati_meanage0,
                     measure = "MN", method = "REML")
ma1_meanage0
confint(ma1_meanage0)
influence(ma1_meanage0)

ranktest(ma1_meanage0)
regtest(ma1_meanage0)
trimfill(ma1_meanage0,verbose = T)

par(mfrow=c(1,2))
funnel(trimfill(ma1_meanage0), digits = 2, main = "Funnel plot", cex = .75)

forest(ma1_meanage0,
       slab = paste(dati_meanage0$Author, dati_meanage0$Year, sep = ", "),
       main = "Random effects model",
       cex = .75,
       refline = F)
segments(14.28, -1, 14.28, 6, lty = 2, col = 2, lwd = 2)
text(-5, 7, c("Study"), cex = .75)
text(30, 7, c("Mean [95%CI]"), cex = .75)

##quality
dati_quality1 <- 
  dati %>% 
  filter(`QUALITY_high-low` == "High")

dati_quality0 <- 
  dati %>% 
  filter(`QUALITY_high-low` == "Low")

#high
ma1_quality1 <- rma(mi = PANSS_Positive_mean_forensic,
                    sdi = PANSS_Positive_SD_forensic,
                    ni = N_forensic,
                    data = dati_quality1,
                    measure = "MN", method = "REML")
ma1_quality1
confint(ma1_quality1)
influence(ma1_quality1)

ranktest(ma1_quality1)
regtest(ma1_quality1)
trimfill(ma1_quality1, verbose = T, control = list(maxiter = 10000))

par(mfrow=c(1,2))
funnel(trimfill(ma1_quality1, verbose = T, control = list(maxiter = 10000)), digits = 2, main = "Funnel plot", cex = .75)

forest(ma1_quality1,
       slab = paste(dati_quality1[-c(25,26),]$Author, dati_quality1[-c(25,26),]$Year, sep = ", "),
       main = "Random effects model",
       cex = .75,
       refline = F)
segments(14.96, -1, 14.96, 17, lty = 2, col = 2, lwd = 2)
text(-6, 18, c("Study"), cex = .75)
text(33, 18, c("Mean [95%CI]"), cex = .75)

#low
ma1_quality0 <- rma(mi = PANSS_Positive_mean_forensic,
                     sdi = PANSS_Positive_SD_forensic,
                     ni = N_forensic,
                     data = dati_quality0,
                     measure = "MN", method = "REML")
ma1_quality0
confint(ma1_quality0)
influence(ma1_quality0)

ranktest(ma1_quality0)
regtest(ma1_quality0)
trimfill(ma1_quality0,verbose = T)

# par(mfrow=c(1,2))
# funnel(trimfill(ma1_quality0), digits = 2, main = "Funnel plot", cex = .75)
# 
# forest(ma1_quality0,
#        slab = paste(dati_quality0$Author, dati_quality0$Year, sep = ", "),
#        main = "Random effects model",
#        cex = .75,
#        refline = F)
# segments(14.82, -1, 14.82, 14, lty = 2, col = 2, lwd = 2)
# text(-6, 15, c("Study"), cex = .75)
# text(33, 15, c("Mean [95%CI]"), cex = .75)


###negative PANSS domain
##SSD%

#100
ma2_ssd1 <- rma(mi = PANSS_Negative_mean_forensic,
                sdi = PANSS_Negative_SD_forensic,
                ni = N_forensic,
                data = dati_ssd1,
                measure = "MN", method = "REML")
ma2_ssd1
confint(ma2_ssd1)
influence(ma2_ssd1)

ranktest(ma2_ssd1)
regtest(ma2_ssd1)
trimfill(ma2_ssd1,verbose = T)

par(mfrow=c(1,2))
funnel(trimfill(ma2_ssd1), digits = 2, main = "Funnel plot", cex = .75)

forest(ma2_ssd1,
       slab = paste(dati_ssd1$Author, dati_ssd1$Year, sep = ", "),
       main = "Random effects model",
       cex = .75,
       refline = F)
segments(16.55, -1, 16.55, 15, lty = 2, col = 2, lwd = 2)
text(-6, 16, c("Study"), cex = .75)
text(33, 16, c("Mean [95%CI]"), cex = .75)

#<100
ma2_ssd0 <- rma(mi = PANSS_Negative_mean_forensic,
                sdi = PANSS_Negative_SD_forensic,
                ni = N_forensic,
                data = dati_ssd0,
                measure = "MN", method = "REML")
ma2_ssd0
confint(ma2_ssd0)
influence(ma2_ssd0)

ranktest(ma2_ssd0)
regtest(ma2_ssd0)
trimfill(ma2_ssd0,verbose = T)

par(mfrow=c(1,2))
funnel(trimfill(ma2_ssd0), digits = 2, main = "Funnel plot", cex = .75)

forest(ma2_ssd0,
       slab = paste(dati_ssd0$Author, dati_ssd0$Year, sep = ", "),
       main = "Random effects model",
       cex = .75,
       refline = F)
segments(17.77, -1, 17.77, 4, lty = 2, col = 2, lwd = 2)
text(5, 5, c("Study"), cex = .75)
text(27, 5, c("Mean [95%CI]"), cex = .75)

##setting

#I
ma2_setting1 <- rma(mi = PANSS_Negative_mean_forensic,
                    sdi = PANSS_Negative_SD_forensic,
                    ni = N_forensic,
                    data = dati_setting1,
                    measure = "MN", method = "REML")
ma2_setting1
confint(ma2_setting1)
influence(ma2_setting1)

ranktest(ma2_setting1)
regtest(ma2_setting1)
trimfill(ma2_setting1,verbose = T)

par(mfrow=c(1,2))
funnel(trimfill(ma2_setting1), digits = 2, main = "Funnel plot", cex = .75)

forest(ma2_setting1,
       slab = paste(dati_setting1$Author, dati_setting1$Year, sep = ", "),
       main = "Random effects model",
       cex = .75,
       refline = F)
segments(16.87, -1, 16.87, 15, lty = 2, col = 2, lwd = 2)
text(-6, 16, c("Study"), cex = .75)
text(33, 16, c("Mean [95%CI]"), cex = .75)

#0
ma2_setting0 <- rma(mi = PANSS_Negative_mean_forensic,
                     sdi = PANSS_Negative_SD_forensic,
                     ni = N_forensic,
                     data = dati_setting0,
                     measure = "MN", method = "REML")
ma2_setting0
confint(ma2_setting0)
influence(ma2_setting0)

ranktest(ma2_setting0)
regtest(ma2_setting0)
trimfill(ma2_setting0,verbose = T)

par(mfrow=c(1,2))
funnel(trimfill(ma2_setting0), digits = 2, main = "Funnel plot", cex = .75)

forest(ma2_setting0,
       slab = paste(dati_setting0$Author, dati_setting0$Year, sep = ", "),
       main = "Random effects model",
       cex = .75,
       refline = F)
segments(16.26, -1, 16.26, 4, lty = 2, col = 2, lwd = 2)
text(1, 5, c("Study"), cex = .75)
text(25, 5, c("Mean [95%CI]"), cex = .75)

##male pct

#100
ma2_malepct1 <- rma(mi = PANSS_Negative_mean_forensic,
                    sdi = PANSS_Negative_SD_forensic,
                    ni = N_forensic,
                    data = dati_malepct1,
                    measure = "MN", method = "REML")
ma2_malepct1
confint(ma2_malepct1)
influence(ma2_malepct1)

ranktest(ma2_malepct1)
regtest(ma2_malepct1)
trimfill(ma2_malepct1, verbose = T, control = list(maxiter = 10000))

par(mfrow=c(1,2))
funnel(trimfill(ma2_malepct1, verbose = T, control = list(maxiter = 10000)), digits = 2, main = "Funnel plot", cex = .75)

forest(ma2_malepct1,
       slab = paste(dati_malepct1[-16,]$Author, dati_malepct1[-16,]$Year, sep = ", "),
       main = "Random effects model",
       cex = .75,
       refline = F)
segments(16.13, -1, 16.13, 12, lty = 2, col = 2, lwd = 2)
text(-6, 13, c("Study"), cex = .75)
text(33, 13, c("Mean [95%CI]"), cex = .75)

#<100
ma2_malepct0 <- rma(mi = PANSS_Negative_mean_forensic,
                     sdi = PANSS_Negative_SD_forensic,
                     ni = N_forensic,
                     data = dati_malepct0,
                     measure = "MN", method = "REML")
ma2_malepct0
confint(ma2_malepct0)
influence(ma2_malepct0)

ranktest(ma2_malepct0)
regtest(ma2_malepct0)
trimfill(ma2_malepct0,verbose = T)

par(mfrow=c(1,2))
funnel(trimfill(ma2_malepct0), digits = 2, main = "Funnel plot", cex = .75)

forest(ma2_malepct0,
       slab = paste(dati_malepct0$Author, dati_malepct0$Year, sep = ", "),
       main = "Random effects model",
       cex = .75,
       refline = F)
segments(17.68, -1, 17.68, 6, lty = 2, col = 2, lwd = 2)
text(0, 7, c("Study"), cex = .75)
text(30, 7, c("Mean [95%CI]"), cex = .75)

##mean age
dati_meanageN <- 
  dati %>%
  filter(!is.na(Age_mean_forensic),
         !is.na(PANSS_Negative_mean_forensic),
         !is.na(PANSS_Negative_SD_forensic),
         !is.na(N_forensic))

summary(as.numeric(dati_meanageN$Age_mean_forensic))

dati_meanage1 <- 
  dati_meanageN %>% 
  mutate(Age_mean_forensic = as.numeric(Age_mean_forensic),
         meanage = as.factor(case_when(Age_mean_forensic >= 36.60 ~ 1,
                                       Age_mean_forensic < 36.60 ~ 0))) %>% 
  filter(meanage == 1)

dati_meanage0 <- 
  dati_meanageN %>% 
  mutate(Age_mean_forensic = as.numeric(Age_mean_forensic),
         meanage = as.factor(case_when(Age_mean_forensic >= 36.60 ~ 1,
                                       Age_mean_forensic < 36.60 ~ 0))) %>% 
  filter(meanage == 0)

#>=36.6
ma2_meanage1 <- rma(mi = PANSS_Negative_mean_forensic,
                    sdi = PANSS_Negative_SD_forensic,
                    ni = N_forensic,
                    data = dati_meanage1,
                    measure = "MN", method = "REML")
ma2_meanage1
confint(ma2_meanage1)
influence(ma2_meanage1)

ranktest(ma2_meanage1)
regtest(ma2_meanage1)
trimfill(ma2_meanage1, verbose = T, control = list(maxiter = 10000))

par(mfrow=c(1,2))
funnel(trimfill(ma2_meanage1, verbose = T, control = list(maxiter = 10000)), digits = 2, main = "Funnel plot", cex = .75)

forest(ma2_meanage1,
       slab = paste(dati_meanage1$Author, dati_meanage1$Year, sep = ", "),
       main = "Random effects model",
       cex = .75,
       refline = F)
segments(17.07, -1, 17.07, 14, lty = 2, col = 2, lwd = 2)
text(-6, 15, c("Study"), cex = .75)
text(33, 15, c("Mean [95%CI]"), cex = .75)

#<36.6
ma2_meanage0 <- rma(mi = PANSS_Negative_mean_forensic,
                     sdi = PANSS_Negative_SD_forensic,
                     ni = N_forensic,
                     data = dati_meanage0,
                     measure = "MN", method = "REML")
ma2_meanage0
confint(ma2_meanage0)
influence(ma2_meanage0)

ranktest(ma2_meanage0)
regtest(ma2_meanage0)
trimfill(ma2_meanage0,verbose = T)

par(mfrow=c(1,2))
funnel(trimfill(ma2_meanage0), digits = 2, main = "Funnel plot", cex = .75)

forest(ma2_meanage0,
       slab = paste(dati_meanage0$Author, dati_meanage0$Year, sep = ", "),
       main = "Random effects model",
       cex = .75,
       refline = F)
segments(15.74, -1, 15.74, 5, lty = 2, col = 2, lwd = 2)
text(3, 6, c("Study"), cex = .75)
text(28, 6, c("Mean [95%CI]"), cex = .75)

##quality

#high
ma2_quality1 <- rma(mi = PANSS_Negative_mean_forensic,
                    sdi = PANSS_Negative_SD_forensic,
                    ni = N_forensic,
                    data = dati_quality1,
                    measure = "MN", method = "REML")
ma2_quality1
confint(ma2_quality1)
influence(ma2_quality1)

ranktest(ma2_quality1)
regtest(ma2_quality1)
trimfill(ma2_quality1, verbose = T, control = list(maxiter = 10000))

par(mfrow=c(1,2))
funnel(trimfill(ma2_quality1, verbose = T, control = list(maxiter = 10000)), digits = 2, main = "Funnel plot", cex = .75)

forest(ma2_quality1,
       slab = paste(dati_quality1$Author, dati_quality1$Year, sep = ", "),
       main = "Random effects model",
       cex = .75,
       refline = F)
segments(17.09, -1, 17.09, 16, lty = 2, col = 2, lwd = 2)
text(-6, 17, c("Study"), cex = .75)
text(33, 17, c("Mean [95%CI]"), cex = .75)

#low
ma2_quality0 <- rma(mi = PANSS_Negative_mean_forensic,
                     sdi = PANSS_Negative_SD_forensic,
                     ni = N_forensic,
                     data = dati_quality0,
                     measure = "MN", method = "REML")
ma2_quality0
confint(ma2_quality0)
influence(ma2_quality0)

ranktest(ma2_quality0)
regtest(ma2_quality0)
trimfill(ma2_quality0,verbose = T)

# par(mfrow=c(1,2))
# funnel(trimfill(ma2_quality0), digits = 2, main = "Funnel plot", cex = .75)
# 
# forest(ma2_quality0,
#        slab = paste(dati_quality0$Author, dati_quality0$Year, sep = ", "),
#        main = "Random effects model",
#        cex = .75)
# text(-85,26,c("Study"), cex = .75)
# text(115,26,c("Mean [95%CI]"), cex = .75)


###GeneralPsychopt PANSS domain
##SSD%

#100
ma3_ssd1 <- rma(mi = PANSS_GeneralPsychopt_mean_forensic,
                sdi = PANSS_GeneralPsychopt_SD_forensic,
                ni = N_forensic,
                data = dati_ssd1,
                measure = "MN", method = "REML")
ma3_ssd1
confint(ma3_ssd1)
influence(ma3_ssd1)

ranktest(ma3_ssd1)
regtest(ma3_ssd1)
trimfill(ma3_ssd1,verbose = T)

par(mfrow=c(1,2))
funnel(trimfill(ma3_ssd1), digits = 2, main = "Funnel plot", cex = .75)

forest(ma3_ssd1,
       slab = paste(dati_ssd1$Author, dati_ssd1$Year, sep = ", "),
       main = "Random effects model",
       cex = .75,
       refline = F)
segments(31.38, -1, 31.38, 13, lty = 2, col = 2, lwd = 2)
text(-5, 14, c("Study"), cex = .75)
text(60, 14, c("Mean [95%CI]"), cex = .75)

#<100
ma3_ssd0 <- rma(mi = PANSS_GeneralPsychopt_mean_forensic,
                sdi = PANSS_GeneralPsychopt_SD_forensic,
                ni = N_forensic,
                data = dati_ssd0,
                measure = "MN", method = "REML")
ma3_ssd0
confint(ma3_ssd0)
influence(ma3_ssd0)

ranktest(ma3_ssd0)
regtest(ma3_ssd0)
trimfill(ma3_ssd0,verbose = T)

par(mfrow=c(1,2))
funnel(trimfill(ma3_ssd0), digits = 2, main = "Funnel plot", cex = .75)

forest(ma3_ssd0,
       slab = paste(dati_ssd0$Author, dati_ssd0$Year, sep = ", "),
       main = "Random effects model",
       cex = .75,
       refline = F)
segments(31.28, -1, 31.28, 4, lty = 2, col = 2, lwd = 2)
text(16, 5, c("Study"), cex = .75)
text(43, 5, c("Mean [95%CI]"), cex = .75)

##setting

#I
ma3_setting1 <- rma(mi = PANSS_GeneralPsychopt_mean_forensic,
                    sdi = PANSS_GeneralPsychopt_SD_forensic,
                    ni = N_forensic,
                    data = dati_setting1,
                    measure = "MN", method = "REML")
ma3_setting1
confint(ma3_setting1)
influence(ma3_setting1)

ranktest(ma3_setting1)
regtest(ma3_setting1)
trimfill(ma3_setting1,verbose = T)

par(mfrow=c(1,2))
funnel(trimfill(ma3_setting1), digits = 2, main = "Funnel plot", cex = .75)

forest(ma3_setting1,
       slab = paste(dati_setting1$Author, dati_setting1$Year, sep = ", "),
       main = "Random effects model",
       cex = .75,
       refline = F)
segments(31.18, -1, 31.18, 13, lty = 2, col = 2, lwd = 2)
text(-5, 14, c("Study"), cex = .75)
text(60, 14, c("Mean [95%CI]"), cex = .75)

#0
ma3_setting0 <- rma(mi = PANSS_GeneralPsychopt_mean_forensic,
                     sdi = PANSS_GeneralPsychopt_SD_forensic,
                     ni = N_forensic,
                     data = dati_setting0,
                     measure = "MN", method = "REML")
ma3_setting0
confint(ma3_setting0)
influence(ma3_setting0)

ranktest(ma3_setting0)
regtest(ma3_setting0)
trimfill(ma3_setting0,verbose = T)

par(mfrow=c(1,2))
funnel(trimfill(ma3_setting0), digits = 2, main = "Funnel plot", cex = .75)

forest(ma3_setting0,
       slab = paste(dati_setting0$Author, dati_setting0$Year, sep = ", "),
       main = "Random effects model",
       cex = .75,
       refline = F)
segments(31.99, -1, 31.99, 4, lty = 2, col = 2, lwd = 2)
text(8, 5, c("Study"), cex = .75)
text(45, 5, c("Mean [95%CI]"), cex = .75)

##male pct

#100
ma3_malepct1 <- rma(mi = PANSS_GeneralPsychopt_mean_forensic,
                    sdi = PANSS_GeneralPsychopt_SD_forensic,
                    ni = N_forensic,
                    data = dati_malepct1,
                    measure = "MN", method = "REML")
ma3_malepct1
confint(ma3_malepct1)
influence(ma3_malepct1)

ranktest(ma3_malepct1)
regtest(ma3_malepct1)
trimfill(ma3_malepct1, verbose = T, control = list(maxiter = 10000))

par(mfrow=c(1,2))
funnel(trimfill(ma3_malepct1, verbose = T, control = list(maxiter = 10000)), digits = 2, main = "Funnel plot", cex = .75)

forest(ma3_malepct1,
       slab = paste(dati_malepct1$Author, dati_malepct1$Year, sep = ", "),
       main = "Random effects model",
       cex = .75,
       refline = F)
segments(30.88, -1, 30.88, 11, lty = 2, col = 2, lwd = 2)
text(-5, 12, c("Study"), cex = .75)
text(60, 12, c("Mean [95%CI]"), cex = .75)

#<100
ma3_malepct0 <- rma(mi = PANSS_GeneralPsychopt_mean_forensic,
                     sdi = PANSS_GeneralPsychopt_SD_forensic,
                     ni = N_forensic,
                     data = dati_malepct0,
                     measure = "MN", method = "REML")
ma3_malepct0
confint(ma3_malepct0)
influence(ma3_malepct0)

ranktest(ma3_malepct0)
regtest(ma3_malepct0)
trimfill(ma3_malepct0,verbose = T)

par(mfrow=c(1,2))
funnel(trimfill(ma3_malepct0), digits = 2, main = "Funnel plot", cex = .75)

forest(ma3_malepct0,
       slab = paste(dati_malepct0$Author, dati_malepct0$Year, sep = ", "),
       main = "Random effects model",
       cex = .75,
       refline = F)
segments(32.26, -1, 32.26, 6, lty = 2, col = 2, lwd = 2)
text(8, 7, c("Study"), cex = .75)
text(45, 7, c("Mean [95%CI]"), cex = .75)

##mean age

dati_meanageG <- 
  dati %>%
  filter(!is.na(Age_mean_forensic),
         !is.na(PANSS_GeneralPsychopt_mean_forensic),
         !is.na(PANSS_GeneralPsychopt_SD_forensic),
         !is.na(N_forensic))

summary(as.numeric(dati_meanageG$Age_mean_forensic))

dati_meanage1 <- 
  dati_meanageG %>% 
  mutate(Age_mean_forensic = as.numeric(Age_mean_forensic),
         meanage = as.factor(case_when(Age_mean_forensic >= 36.65 ~ 1,
                                       Age_mean_forensic < 36.65 ~ 0))) %>% 
  filter(meanage == 1)

dati_meanage0 <- 
  dati_meanageG %>% 
  mutate(Age_mean_forensic = as.numeric(Age_mean_forensic),
         meanage = as.factor(case_when(Age_mean_forensic >= 36.65 ~ 1,
                                       Age_mean_forensic < 36.65 ~ 0))) %>% 
  filter(meanage == 0)


#>=36.65
ma3_meanage1 <- rma(mi = PANSS_GeneralPsychopt_mean_forensic,
                    sdi = PANSS_GeneralPsychopt_SD_forensic,
                    ni = N_forensic,
                    data = dati_meanage1,
                    measure = "MN", method = "REML")
ma3_meanage1
confint(ma3_meanage1)
influence(ma3_meanage1)

ranktest(ma3_meanage1)
regtest(ma3_meanage1)
trimfill(ma3_meanage1, verbose = T, control = list(maxiter = 10000))

par(mfrow=c(1,2))
funnel(trimfill(ma3_meanage1, verbose = T, control = list(maxiter = 10000)), digits = 2, main = "Funnel plot", cex = .75)

forest(ma3_meanage1,
       slab = paste(dati_meanage1$Author, dati_meanage1$Year, sep = ", "),
       main = "Random effects model",
       cex = .75,
       refline = F)
segments(31.45, -1, 31.45, 12, lty = 2, col = 2, lwd = 2)
text(-5, 13, c("Study"), cex = .75)
text(60, 13, c("Mean [95%CI]"), cex = .75)

#<36.65
ma3_meanage0 <- rma(mi = PANSS_GeneralPsychopt_mean_forensic,
                     sdi = PANSS_GeneralPsychopt_SD_forensic,
                     ni = N_forensic,
                     data = dati_meanage0,
                     measure = "MN", method = "REML")
ma3_meanage0
confint(ma3_meanage0)
influence(ma3_meanage0)

ranktest(ma3_meanage0)
regtest(ma3_meanage0)
trimfill(ma3_meanage0,verbose = T)

par(mfrow=c(1,2))
funnel(trimfill(ma3_meanage0), digits = 2, main = "Funnel plot", cex = .75)

forest(ma3_meanage0,
       slab = paste(dati_meanage0$Author, dati_meanage0$Year, sep = ", "),
       main = "Random effects model",
       cex = .75,
       refline = F)
segments(31.06, -1, 31.06, 5, lty = 2, col = 2, lwd = 2)
text(12, 6, c("Study"), cex = .75)
text(45, 6, c("Mean [95%CI]"), cex = .75)

##quality

#high
ma3_quality1 <- rma(mi = PANSS_GeneralPsychopt_mean_forensic,
                    sdi = PANSS_GeneralPsychopt_SD_forensic,
                    ni = N_forensic,
                    data = dati_quality1,
                    measure = "MN", method = "REML")
ma3_quality1
confint(ma3_quality1)
influence(ma3_quality1)

ranktest(ma3_quality1)
regtest(ma3_quality1)
trimfill(ma3_quality1, verbose = T, control = list(maxiter = 10000))

par(mfrow=c(1,2))
funnel(trimfill(ma3_quality1, verbose = T, control = list(maxiter = 10000)), digits = 2, main = "Funnel plot", cex = .75)

forest(ma3_quality1,
       slab = paste(dati_quality1$Author, dati_quality1$Year, sep = ", "),
       main = "Random effects model",
       cex = .75,
       refline = F)
segments(32.07, -1, 32.07, 14, lty = 2, col = 2, lwd = 2)
text(-5, 15, c("Study"), cex = .75)
text(60, 15, c("Mean [95%CI]"), cex = .75)

#low
ma3_quality0 <- rma(mi = PANSS_GeneralPsychopt_mean_forensic,
                     sdi = PANSS_GeneralPsychopt_SD_forensic,
                     ni = N_forensic,
                     data = dati_quality0,
                     measure = "MN", method = "REML")
ma3_quality0
confint(ma3_quality0)
influence(ma3_quality0)

ranktest(ma3_quality0)
regtest(ma3_quality0)
trimfill(ma3_quality0,verbose = T)

# par(mfrow=c(1,2))
# funnel(trimfill(ma3_quality0), digits = 2, main = "Funnel plot", cex = .75)
# 
# forest(ma3_quality0,
#        slab = paste(dati_quality0$Author, dati_quality0$Year, sep = ", "),
#        main = "Random effects model",
#        cex = .75,
#        refline = F)
# segments(30.88, -1, 30.88, 11, lty = 2, col = 2, lwd = 2)
# text(-5, 12, c("Study"), cex = .75)
# text(60, 12, c("Mean [95%CI]"), cex = .75)


###Total PANSS 
##SSD%

#100
ma4_ssd1 <- rma(mi = PANSS_Total_mean_forensic,
                sdi = PANSS_Total_SD_forensic,
                ni = N_forensic,
                data = dati_ssd1,
                measure = "MN", method = "REML")
ma4_ssd1
confint(ma4_ssd1)
influence(ma4_ssd1)

ranktest(ma4_ssd1)
regtest(ma4_ssd1)
trimfill(ma4_ssd1,verbose = T)

par(mfrow=c(1,2))
funnel(trimfill(ma4_ssd1), digits = 2, main = "Funnel plot", cex = .75)

forest(ma4_ssd1,
       slab = paste(dati_ssd1$Author, dati_ssd1$Year, sep = ", "),
       main = "Random effects model",
       cex = .75,
       refline = F)
segments(67.59, -1, 67.59, 13, lty = 2, col = 2, lwd = 2)
text(-10, 14, c("Study"), cex = .75)
text(125, 14, c("Mean [95%CI]"), cex = .75)

#<100
ma4_ssd0 <- rma(mi = PANSS_Total_mean_forensic,
                sdi = PANSS_Total_SD_forensic,
                ni = N_forensic,
                data = dati_ssd0,
                measure = "MN", method = "REML")
ma4_ssd0
confint(ma4_ssd0)
influence(ma4_ssd0)

ranktest(ma4_ssd0)
regtest(ma4_ssd0)
trimfill(ma4_ssd0,verbose = T)

par(mfrow=c(1,2))
funnel(trimfill(ma4_ssd0), digits = 2, main = "Funnel plot", cex = .75)

forest(ma4_ssd0,
       slab = paste(dati_ssd0$Author, dati_ssd0$Year, sep = ", "),
       main = "Random effects model",
       cex = .75,
       refline = F)
segments(59.42, -1, 59.42, 6, lty = 2, col = 2, lwd = 2)
text(20, 7, c("Study"), cex = .75)
text(95, 7, c("Mean [95%CI]"), cex = .75)

##setting

#I
ma4_setting1 <- rma(mi = PANSS_Total_mean_forensic,
                    sdi = PANSS_Total_SD_forensic,
                    ni = N_forensic,
                    data = dati_setting1,
                    measure = "MN", method = "REML")
ma4_setting1
confint(ma4_setting1)
influence(ma4_setting1)

ranktest(ma4_setting1)
regtest(ma4_setting1)
trimfill(ma4_setting1,verbose = T)

par(mfrow=c(1,2))
funnel(trimfill(ma4_setting1), digits = 2, main = "Funnel plot", cex = .75)

forest(ma4_setting1,
       slab = paste(dati_setting1$Author, dati_setting1$Year, sep = ", "),
       main = "Random effects model",
       cex = .75,
       refline = F)
segments(64.27, -1, 64.27, 15, lty = 2, col = 2, lwd = 2)
text(-10, 16, c("Study"), cex = .75)
text(125, 16, c("Mean [95%CI]"), cex = .75)

#0
ma4_setting0 <- rma(mi = PANSS_Total_mean_forensic,
                     sdi = PANSS_Total_SD_forensic,
                     ni = N_forensic,
                     data = dati_setting0,
                     measure = "MN", method = "REML")
ma4_setting0
confint(ma4_setting0)
influence(ma4_setting0)

ranktest(ma4_setting0)
regtest(ma4_setting0)
trimfill(ma4_setting0,verbose = T)

par(mfrow=c(1,2))
funnel(trimfill(ma4_setting0), digits = 2, main = "Funnel plot", cex = .75)

forest(ma4_setting0,
       slab = paste(dati_setting0$Author, dati_setting0$Year, sep = ", "),
       main = "Random effects model",
       cex = .75,
       refline = F)
segments(69.88, -1, 69.88, 4, lty = 2, col = 2, lwd = 2)
text(0, 5, c("Study"), cex = .75)
text(110, 5, c("Mean [95%CI]"), cex = .75)

##male pct

#100
ma4_malepct1 <- rma(mi = PANSS_Total_mean_forensic,
                    sdi = PANSS_Total_SD_forensic,
                    ni = N_forensic,
                    data = dati_malepct1,
                    measure = "MN", method = "REML")
ma4_malepct1
confint(ma4_malepct1)
influence(ma4_malepct1)

ranktest(ma4_malepct1)
regtest(ma4_malepct1)
trimfill(ma4_malepct1, verbose = T, control = list(maxiter = 10000))

par(mfrow=c(1,2))
funnel(trimfill(ma4_malepct1, verbose = T, control = list(maxiter = 10000)), digits = 2, main = "Funnel plot", cex = .75)

forest(ma4_malepct1,
       slab = paste(dati_malepct1$Author, dati_malepct1$Year, sep = ", "),
       main = "Random effects model",
       cex = .75,
       refline = F)
segments(69.98, -1, 69.98, 10, lty = 2, col = 2, lwd = 2)
text(-5, 11, c("Study"), cex = .75)
text(125, 11, c("Mean [95%CI]"), cex = .75)

#<100
ma4_malepct0 <- rma(mi = PANSS_Total_mean_forensic,
                     sdi = PANSS_Total_SD_forensic,
                     ni = N_forensic,
                     data = dati_malepct0,
                     measure = "MN", method = "REML")
ma4_malepct0
confint(ma4_malepct0)
influence(ma4_malepct0)

ranktest(ma4_malepct0)
regtest(ma4_malepct0)
trimfill(ma4_malepct0,verbose = T)

par(mfrow=c(1,2))
funnel(trimfill(ma4_malepct0), digits = 2, main = "Funnel plot", cex = .75)

forest(ma4_malepct0,
       slab = paste(dati_malepct0$Author, dati_malepct0$Year, sep = ", "),
       main = "Random effects model",
       cex = .75,
       refline = F)
segments(60.32, -1, 60.32, 7, lty = 2, col = 2, lwd = 2)
text(3, 8, c("Study"), cex = .75)
text(100, 8, c("Mean [95%CI]"), cex = .75)

##mean age

dati_meanageT <- 
  dati %>%
  filter(!is.na(Age_mean_forensic),
         !is.na(PANSS_Total_mean_forensic),
         !is.na(PANSS_Total_SD_forensic),
         !is.na(N_forensic))

summary(as.numeric(dati_meanageT$Age_mean_forensic))

dati_meanage1 <- 
  dati_meanageT %>% 
  mutate(Age_mean_forensic = as.numeric(Age_mean_forensic),
         meanage = as.factor(case_when(Age_mean_forensic >= 36.7 ~ 1,
                                       Age_mean_forensic < 36.7 ~ 0))) %>% 
  filter(meanage == 1)

dati_meanage0 <- 
  dati_meanageT %>% 
  mutate(Age_mean_forensic = as.numeric(Age_mean_forensic),
         meanage = as.factor(case_when(Age_mean_forensic >= 36.7 ~ 1,
                                       Age_mean_forensic < 36.7 ~ 0))) %>% 
  filter(meanage == 0)

#>=36.7
ma4_meanage1 <- rma(mi = PANSS_Total_mean_forensic,
                    sdi = PANSS_Total_SD_forensic,
                    ni = N_forensic,
                    data = dati_meanage1,
                    measure = "MN", method = "REML")
ma4_meanage1
confint(ma4_meanage1)
influence(ma4_meanage1)

ranktest(ma4_meanage1)
regtest(ma4_meanage1)
trimfill(ma4_meanage1, verbose = T, control = list(maxiter = 10000))

par(mfrow=c(1,2))
funnel(trimfill(ma4_meanage1, verbose = T, control = list(maxiter = 10000)), digits = 2, main = "Funnel plot", cex = .75)

forest(ma4_meanage1,
       slab = paste(dati_meanage1$Author, dati_meanage1$Year, sep = ", "),
       main = "Random effects model",
       cex = .75,
       refline = F)
segments(65.39, -1, 65.39, 13, lty = 2, col = 2, lwd = 2)
text(-10, 14, c("Study"), cex = .75)
text(125, 14, c("Mean [95%CI]"), cex = .75)

#<36.7
ma4_meanage0 <- rma(mi = PANSS_Total_mean_forensic,
                     sdi = PANSS_Total_SD_forensic,
                     ni = N_forensic,
                     data = dati_meanage0,
                     measure = "MN", method = "REML")
ma4_meanage0
confint(ma4_meanage0)
influence(ma4_meanage0)

ranktest(ma4_meanage0)
regtest(ma4_meanage0)
trimfill(ma4_meanage0,verbose = T)

par(mfrow=c(1,2))
funnel(trimfill(ma4_meanage0), digits = 2, main = "Funnel plot", cex = .75)

forest(ma4_meanage0,
       slab = paste(dati_meanage0$Author, dati_meanage0$Year, sep = ", "),
       main = "Random effects model",
       cex = .75,
       refline = F)
segments(64.95, -1, 64.95, 6, lty = 2, col = 2, lwd = 2)
text(10, 7, c("Study"), cex = .75)
text(105, 7, c("Mean [95%CI]"), cex = .75)

##quality

#high
ma4_quality1 <- rma(mi = PANSS_Total_mean_forensic,
                    sdi = PANSS_Total_SD_forensic,
                    ni = N_forensic,
                    data = dati_quality1,
                    measure = "MN", method = "REML")
ma4_quality1
confint(ma4_quality1)
influence(ma4_quality1)

ranktest(ma4_quality1)
regtest(ma4_quality1)
trimfill(ma4_quality1, verbose = T, control = list(maxiter = 10000))

par(mfrow=c(1,2))
funnel(trimfill(ma4_quality1, verbose = T, control = list(maxiter = 10000)), digits = 2, main = "Funnel plot", cex = .75)

forest(ma4_quality1,
       slab = paste(dati_quality1$Author, dati_quality1$Year, sep = ", "),
       main = "Random effects model",
       cex = .75,
       refline = F)
segments(65.01, -1, 65.01, 15, lty = 2, col = 2, lwd = 2)
text(-10, 16, c("Study"), cex = .75)
text(125, 16, c("Mean [95%CI]"), cex = .75)

#low
ma4_quality0 <- rma(mi = PANSS_Total_mean_forensic,
                     sdi = PANSS_Total_SD_forensic,
                     ni = N_forensic,
                     data = dati_quality0,
                     measure = "MN", method = "REML")
ma4_quality0
confint(ma4_quality0)
influence(ma4_quality0)

ranktest(ma4_quality0)
regtest(ma4_quality0)
trimfill(ma4_quality0,verbose = T)

par(mfrow=c(1,2))
funnel(trimfill(ma4_quality0), digits = 2, main = "Funnel plot", cex = .75)

forest(ma4_quality0,
       slab = paste(dati_quality0$Author, dati_quality0$Year, sep = ", "),
       main = "Random effects model",
       cex = .75,
       refline = F)
segments(66.34, -1, 66.34, 4, lty = 2, col = 2, lwd = 2)
text(-10, 5, c("Study"), cex = .75)
text(125, 5, c("Mean [95%CI]"), cex = .75)


# Analisi moderatori ------------------------------------------------------
###positive PANSS domain
##SSD
ma1_ssd_mix <- 
  rma(mi = PANSS_Positive_mean_forensic,
      sdi = PANSS_Positive_SD_forensic,
      ni = N_forensic, mods = as.numeric(`SSD_ %`),
      data = dati,
      measure = "MN", method = "REML")

ma1_ssd_mix
ranktest(ma1_ssd_mix)
regtest(ma1_ssd_mix)

par(mfrow=c(1,1))
forest(ma1_ssd_mix,
       slab = paste(dati$Author, dati$Year, sep = ", "),
       main = "Mixed effects model",
       cex = .75,
       refline = F)
text(-5,20,c("Study"), cex = .75)
text(35,20,c("Mean [95%CI]"), cex = .75)

par(mfrow=c(1,2))
funnel(ma1, main="Random-Effects Model")
funnel(ma1_ssd_mix, main="Mixed-Effects Model adjusted for SSD%")

##setting
dati <- 
  dati %>% 
  mutate(Setting0 = ifelse(Setting != "I", 1, 0))

ma1_setting_mix <- 
  rma(mi = PANSS_Positive_mean_forensic,
      sdi = PANSS_Positive_SD_forensic,
      ni = N_forensic, mods = Setting0,
      data = dati,
      measure = "MN", method = "REML")

ma1_setting_mix
ranktest(ma1_setting_mix)
regtest(ma1_setting_mix)

par(mfrow=c(1,1))
forest(ma1_setting_mix,
       slab = paste(dati$Author, dati$Year, sep = ", "),
       main = "Mixed effects model",
       cex = .75,
       refline = F)
text(-5,20,c("Study"), cex = .75)
text(35,20,c("Mean [95%CI]"), cex = .75)

par(mfrow=c(1,2))
funnel(ma1, main="Random-Effects Model")
funnel(ma1_setting_mix, main="Mixed-Effects Model adjusted for Setting")

##male pct
ma1_malepct_mix <- 
  rma(mi = PANSS_Positive_mean_forensic,
      sdi = PANSS_Positive_SD_forensic,
      ni = N_forensic, mods = as.numeric(`%_male_forensic`),
      data = dati,
      measure = "MN", method = "REML")

ma1_malepct_mix
ranktest(ma1_malepct_mix)
regtest(ma1_malepct_mix)

par(mfrow=c(1,1))
forest(ma1_malepct_mix,
       slab = paste(dati$Author, dati$Year, sep = ", "),
       main = "Mixed effects model",
       cex = .75,
       refline = F)
text(-5,18,c("Study"), cex = .75)
text(35,18,c("Mean [95%CI]"), cex = .75)

par(mfrow=c(1,2))
funnel(ma1, main="Random-Effects Model")
funnel(ma1_malepct_mix, main="Mixed-Effects Model adjusted for % of male")

##age
ma1_age_mix <- 
  rma(mi = PANSS_Positive_mean_forensic,
      sdi = PANSS_Positive_SD_forensic,
      ni = N_forensic, mods = as.numeric(Age_mean_forensic),
      data = dati,
      measure = "MN", method = "REML")

ma1_age_mix
ranktest(ma1_age_mix)
regtest(ma1_age_mix)

par(mfrow=c(1,1))
forest(ma1_age_mix,
       slab = paste(dati$Author, dati$Year, sep = ", "),
       main = "Mixed effects model",
       cex = .75,
       refline = F)
text(-5,20,c("Study"), cex = .75)
text(35,20,c("Mean [95%CI]"), cex = .75)

par(mfrow=c(1,2))
funnel(ma1, main="Random-Effects Model")
funnel(ma1_age_mix, main="Mixed-Effects Model adjusted for mean age")

##quality
dati <- 
  dati %>% 
  mutate(qualityL = ifelse(`QUALITY_high-low` == "Low", 1, 0))

ma1_quality_mix <- 
  rma(mi = PANSS_Positive_mean_forensic,
      sdi = PANSS_Positive_SD_forensic,
      ni = N_forensic, mods = qualityL,
      data = dati,
      measure = "MN", method = "REML")

ma1_quality_mix
ranktest(ma1_quality_mix)
regtest(ma1_quality_mix)

par(mfrow=c(1,1))
forest(ma1_quality_mix,
       slab = paste(dati$Author, dati$Year, sep = ", "),
       main = "Mixed effects model",
       cex = .75,
       refline = F)
text(-5,20,c("Study"), cex = .75)
text(35,20,c("Mean [95%CI]"), cex = .75)

par(mfrow=c(1,2))
funnel(ma1, main="Random-Effects Model")
funnel(ma1_quality_mix, main="Mixed-Effects Model adjusted for mean age")


###negative PANSS domain
##SSD
ma2_ssd_mix <- 
  rma(mi = PANSS_Negative_mean_forensic,
      sdi = PANSS_Negative_SD_forensic,
      ni = N_forensic, mods = as.numeric(`SSD_ %`),
      data = dati,
      measure = "MN", method = "REML")

ma2_ssd_mix
ranktest(ma2_ssd_mix)
regtest(ma2_ssd_mix)

par(mfrow=c(1,1))
forest(ma2_ssd_mix,
       slab = paste(dati$Author, dati$Year, sep = ", "),
       main = "Mixed effects model",
       cex = .75,
       refline = F)
text(-5,20,c("Study"), cex = .75)
text(35,20,c("Mean [95%CI]"), cex = .75)

par(mfrow=c(1,2))
funnel(ma2, main="Random-Effects Model")
funnel(ma2_ssd_mix, main="Mixed-Effects Model adjusted for SSD%")

##setting
dati <- 
  dati %>% 
  mutate(Setting0 = ifelse(Setting != "I", 1, 0))

ma2_setting_mix <- 
  rma(mi = PANSS_Negative_mean_forensic,
      sdi = PANSS_Negative_SD_forensic,
      ni = N_forensic, mods = Setting0,
      data = dati,
      measure = "MN", method = "REML")

ma2_setting_mix
ranktest(ma2_setting_mix)
regtest(ma2_setting_mix)

par(mfrow=c(1,1))
forest(ma2_setting_mix,
       slab = paste(dati$Author, dati$Year, sep = ", "),
       main = "Mixed effects model",
       cex = .75,
       refline = F)
text(-5,20,c("Study"), cex = .75)
text(35,20,c("Mean [95%CI]"), cex = .75)

par(mfrow=c(1,2))
funnel(ma2, main="Random-Effects Model")
funnel(ma2_setting_mix, main="Mixed-Effects Model adjusted for Setting")

##male pct
ma2_malepct_mix <- 
  rma(mi = PANSS_Negative_mean_forensic,
      sdi = PANSS_Negative_SD_forensic,
      ni = N_forensic, mods = as.numeric(`%_male_forensic`),
      data = dati,
      measure = "MN", method = "REML")

ma2_malepct_mix
ranktest(ma2_malepct_mix)
regtest(ma2_malepct_mix)

par(mfrow=c(1,1))
forest(ma2_malepct_mix,
       slab = paste(dati$Author, dati$Year, sep = ", "),
       main = "Mixed effects model",
       cex = .75,
       refline = F)
text(-5,18,c("Study"), cex = .75)
text(35,18,c("Mean [95%CI]"), cex = .75)

par(mfrow=c(1,2))
funnel(ma2, main="Random-Effects Model")
funnel(ma2_malepct_mix, main="Mixed-Effects Model adjusted for % of male")

##age
ma2_age_mix <- 
  rma(mi = PANSS_Negative_mean_forensic,
      sdi = PANSS_Negative_SD_forensic,
      ni = N_forensic, mods = as.numeric(Age_mean_forensic),
      data = dati,
      measure = "MN", method = "REML")

ma2_age_mix
ranktest(ma2_age_mix)
regtest(ma2_age_mix)

par(mfrow=c(1,1))
forest(ma2_age_mix,
       slab = paste(dati$Author, dati$Year, sep = ", "),
       main = "Mixed effects model",
       cex = .75,
       refline = F)
text(-5,20,c("Study"), cex = .75)
text(35,20,c("Mean [95%CI]"), cex = .75)

par(mfrow=c(1,2))
funnel(ma2, main="Random-Effects Model")
funnel(ma2_age_mix, main="Mixed-Effects Model adjusted for mean age")

##quality
dati <- 
  dati %>% 
  mutate(qualityL = ifelse(`QUALITY_high-low` == "Low", 1, 0))

ma2_quality_mix <- 
  rma(mi = PANSS_Negative_mean_forensic,
      sdi = PANSS_Negative_SD_forensic,
      ni = N_forensic, mods = qualityL,
      data = dati,
      measure = "MN", method = "REML")

ma2_quality_mix
ranktest(ma2_quality_mix)
regtest(ma2_quality_mix)

par(mfrow=c(1,1))
forest(ma2_quality_mix,
       slab = paste(dati$Author, dati$Year, sep = ", "),
       main = "Mixed effects model",
       cex = .75,
       refline = F)
text(-5,20,c("Study"), cex = .75)
text(35,20,c("Mean [95%CI]"), cex = .75)

par(mfrow=c(1,2))
funnel(ma2, main="Random-Effects Model")
funnel(ma2_quality_mix, main="Mixed-Effects Model adjusted for mean age")

###General PANSS domain
##SSD
ma3_ssd_mix <- 
  rma(mi = PANSS_GeneralPsychopt_mean_forensic,
      sdi = PANSS_GeneralPsychopt_SD_forensic,
      ni = N_forensic, mods = as.numeric(`SSD_ %`),
      data = dati,
      measure = "MN", method = "REML")

ma3_ssd_mix
ranktest(ma3_ssd_mix)
regtest(ma3_ssd_mix)

par(mfrow=c(1,1))
forest(ma3_ssd_mix,
       slab = paste(dati$Author, dati$Year, sep = ", "),
       main = "Mixed effects model",
       cex = .75,
       refline = F)
text(-5,17,c("Study"), cex = .75)
text(70,17,c("Mean [95%CI]"), cex = .75)

par(mfrow=c(1,2))
funnel(ma3, main="Random-Effects Model")
funnel(ma3_ssd_mix, main="Mixed-Effects Model adjusted for SSD%")

##setting
dati <- 
  dati %>% 
  mutate(Setting0 = ifelse(Setting != "I", 1, 0))

ma3_setting_mix <- 
  rma(mi = PANSS_GeneralPsychopt_mean_forensic,
      sdi = PANSS_GeneralPsychopt_SD_forensic,
      ni = N_forensic, mods = Setting0,
      data = dati,
      measure = "MN", method = "REML")

ma3_setting_mix
ranktest(ma3_setting_mix)
regtest(ma3_setting_mix)

par(mfrow=c(1,1))
forest(ma3_setting_mix,
       slab = paste(dati$Author, dati$Year, sep = ", "),
       main = "Mixed effects model",
       cex = .75,
       refline = F)
text(-5,17,c("Study"), cex = .75)
text(70,17,c("Mean [95%CI]"), cex = .75)

par(mfrow=c(1,2))
funnel(ma3, main="Random-Effects Model")
funnel(ma3_setting_mix, main="Mixed-Effects Model adjusted for Setting")

##male pct
ma3_malepct_mix <- 
  rma(mi = PANSS_GeneralPsychopt_mean_forensic,
      sdi = PANSS_GeneralPsychopt_SD_forensic,
      ni = N_forensic, mods = as.numeric(`%_male_forensic`),
      data = dati,
      measure = "MN", method = "REML")

ma3_malepct_mix
ranktest(ma3_malepct_mix)
regtest(ma3_malepct_mix)

par(mfrow=c(1,1))
forest(ma3_malepct_mix,
       slab = paste(dati$Author, dati$Year, sep = ", "),
       main = "Mixed effects model",
       cex = .75,
       refline = F)
text(-5,17,c("Study"), cex = .75)
text(70,17,c("Mean [95%CI]"), cex = .75)

par(mfrow=c(1,2))
funnel(ma3, main="Random-Effects Model")
funnel(ma3_malepct_mix, main="Mixed-Effects Model adjusted for % of male")

##age
ma3_age_mix <- 
  rma(mi = PANSS_GeneralPsychopt_mean_forensic,
      sdi = PANSS_GeneralPsychopt_SD_forensic,
      ni = N_forensic, mods = as.numeric(Age_mean_forensic),
      data = dati,
      measure = "MN", method = "REML")

ma3_age_mix
ranktest(ma3_age_mix)
regtest(ma3_age_mix)

par(mfrow=c(1,1))
forest(ma3_age_mix,
       slab = paste(dati$Author, dati$Year, sep = ", "),
       main = "Mixed effects model",
       cex = .75,
       refline = F)
text(-5,17,c("Study"), cex = .75)
text(70,17,c("Mean [95%CI]"), cex = .75)

par(mfrow=c(1,2))
funnel(ma3, main="Random-Effects Model")
funnel(ma3_age_mix, main="Mixed-Effects Model adjusted for mean age")

##quality
dati <- 
  dati %>% 
  mutate(qualityL = ifelse(`QUALITY_high-low` == "Low", 1, 0))

ma3_quality_mix <- 
  rma(mi = PANSS_GeneralPsychopt_mean_forensic,
      sdi = PANSS_GeneralPsychopt_SD_forensic,
      ni = N_forensic, mods = qualityL,
      data = dati,
      measure = "MN", method = "REML")

ma3_quality_mix
ranktest(ma3_quality_mix)
regtest(ma3_quality_mix)

par(mfrow=c(1,1))
forest(ma3_quality_mix,
       slab = paste(dati$Author, dati$Year, sep = ", "),
       main = "Mixed effects model",
       cex = .75,
       refline = F)
text(-5,17,c("Study"), cex = .75)
text(70,17,c("Mean [95%CI]"), cex = .75)

par(mfrow=c(1,2))
funnel(ma3, main="Random-Effects Model")
funnel(ma3_quality_mix, main="Mixed-Effects Model adjusted for mean age")

###Total PANSS 
##SSD
ma4_ssd_mix <- 
  rma(mi = PANSS_Total_mean_forensic,
      sdi = PANSS_Total_SD_forensic,
      ni = N_forensic, mods = as.numeric(`SSD_ %`),
      data = dati,
      measure = "MN", method = "REML")

ma4_ssd_mix
ranktest(ma4_ssd_mix)
regtest(ma4_ssd_mix)

par(mfrow=c(1,1))
forest(ma4_ssd_mix,
       slab = paste(dati$Author, dati$Year, sep = ", "),
       main = "Mixed effects model",
       cex = .75,
       refline = F)
text(-10,19,c("Study"), cex = .75)
text(140,19,c("Mean [95%CI]"), cex = .75)

par(mfrow=c(1,2))
funnel(ma4, main="Random-Effects Model")
funnel(ma4_ssd_mix, main="Mixed-Effects Model adjusted for SSD%")

##setting
dati <- 
  dati %>% 
  mutate(Setting0 = ifelse(Setting != "I", 1, 0))

ma4_setting_mix <- 
  rma(mi = PANSS_Total_mean_forensic,
      sdi = PANSS_Total_SD_forensic,
      ni = N_forensic, mods = Setting0,
      data = dati,
      measure = "MN", method = "REML")

ma4_setting_mix
ranktest(ma4_setting_mix)
regtest(ma4_setting_mix)

par(mfrow=c(1,1))
forest(ma4_setting_mix,
       slab = paste(dati$Author, dati$Year, sep = ", "),
       main = "Mixed effects model",
       cex = .75,
       refline = F)
text(-10,19,c("Study"), cex = .75)
text(140,19,c("Mean [95%CI]"), cex = .75)

par(mfrow=c(1,2))
funnel(ma4, main="Random-Effects Model")
funnel(ma4_setting_mix, main="Mixed-Effects Model adjusted for Setting")

##male pct
ma4_malepct_mix <- 
  rma(mi = PANSS_Total_mean_forensic,
      sdi = PANSS_Total_SD_forensic,
      ni = N_forensic, mods = as.numeric(`%_male_forensic`),
      data = dati,
      measure = "MN", method = "REML")

ma4_malepct_mix
ranktest(ma4_malepct_mix)
regtest(ma4_malepct_mix)

par(mfrow=c(1,1))
forest(ma4_malepct_mix,
       slab = paste(dati$Author, dati$Year, sep = ", "),
       main = "Mixed effects model",
       cex = .75,
       refline = F)
text(-10,18,c("Study"), cex = .75)
text(140,18,c("Mean [95%CI]"), cex = .75)

par(mfrow=c(1,2))
funnel(ma4, main="Random-Effects Model")
funnel(ma4_malepct_mix, main="Mixed-Effects Model adjusted for % of male")

##age
ma4_age_mix <- 
  rma(mi = PANSS_Total_mean_forensic,
      sdi = PANSS_Total_SD_forensic,
      ni = N_forensic, mods = as.numeric(Age_mean_forensic),
      data = dati,
      measure = "MN", method = "REML")

ma4_age_mix
ranktest(ma4_age_mix)
regtest(ma4_age_mix)

par(mfrow=c(1,1))
forest(ma4_age_mix,
       slab = paste(dati$Author, dati$Year, sep = ", "),
       main = "Mixed effects model",
       cex = .75,
       refline = F)
text(-10,19,c("Study"), cex = .75)
text(140,19,c("Mean [95%CI]"), cex = .75)

par(mfrow=c(1,2))
funnel(ma4, main="Random-Effects Model")
funnel(ma4_age_mix, main="Mixed-Effects Model adjusted for mean age")

##quality
dati <- 
  dati %>% 
  mutate(qualityL = ifelse(`QUALITY_high-low` == "Low", 1, 0))

ma4_quality_mix <- 
  rma(mi = PANSS_Total_mean_forensic,
      sdi = PANSS_Total_SD_forensic,
      ni = N_forensic, mods = qualityL,
      data = dati,
      measure = "MN", method = "REML")

ma4_quality_mix
ranktest(ma4_quality_mix)
regtest(ma4_quality_mix)

par(mfrow=c(1,1))
forest(ma4_quality_mix,
       slab = paste(dati$Author, dati$Year, sep = ", "),
       main = "Mixed effects model",
       cex = .75,
       refline = F)
text(-10,19,c("Study"), cex = .75)
text(140,19,c("Mean [95%CI]"), cex = .75)

par(mfrow=c(1,2))
funnel(ma4, main="Random-Effects Model")
funnel(ma4_quality_mix, main="Mixed-Effects Model adjusted for mean age")


# Plots -------------------------------------------------------------------


#Figure 1: saved in A4 6x8.48
par(mfrow=c(2,2))
forest(ma1, 
       slab = paste(dati$Author, dati$Year, sep = ", "),
       mlab = "Random effects model",
       cex = .5,
       xlab = "Raw Mean",
       main = "PANSS Positive Scale",
       refline = NA)
segments(14.67, -1, 14.67, 19, lty = 2, col = 2, lwd = 2)
text(-7, 20, c("Study"), cex = .6)
text(36, 20, c("Mean [95%CI]"), cex = .6)
forest(ma2, 
       slab = paste(dati$Author, dati$Year, sep = ", "),
       mlab = "Random effects model",
       cex = .5,
       xlab = "Raw Mean",
       main = "PANSS Negative Scale",
       refline = NA)
segments(16.76, -1, 16.76, 18, lty = 2, col = 2, lwd = 2)
text(-6, 19, c("Study"), cex = .6)
text(35, 19, c("Mean [95%CI]"), cex = .6)
forest(ma3,
       slab = paste(dati$Author, dati$Year, sep = ", "),
       mlab = "Random effects model",
       cex = .5,
       xlab = "Raw Mean",
       main = "PANSS General Scale",
       refline = NA)
segments(31.345, -1, 31.345, 16, lty = 2, col = 2, lwd = 2)
text(-6, 17, c("Study"), cex = .6)
text(66, 17, c("Mean [95%CI]"), cex = .6)
forest(ma4,
       slab = paste(dati$Author, dati$Year, sep = ", "),
       mlab = "Random effects model",
       cex = .5,
       xlab = "Raw Mean",
       main = "PANSS Total Score",
       refline = NA)
segments(65.26, -1, 65.26, 18, lty = 2, col = 2, lwd = 2)
text(-13, 19, c("Study"), cex = .6)
text(135, 19, c("Mean [95%CI]"), cex = .6)

#Figure 2: saved in A4 6x8.48
par(mfrow=c(2,2))
forest(ma1_nf,
       slab = paste(dati$Author, dati$Year, sep = ", "),
       mlab = "Random effects model",
       cex = .5,
       xlab = "Mean difference",
       main = "PANSS Positive Scale:\nforensic - non forensic")
segments(2.45, -1, 2.45, 8, lty = 2, col = 2, lwd = 2)
text(-17, 9, c("Study"), cex = .6)
text(22, 9, c("Mean [95%CI]"), cex = .6)
forest(ma2_nf,
       slab = paste(dati$Author, dati$Year, sep = ", "),
       mlab = "Random effects model",
       cex = .5,
       xlab = "Mean difference",
       main = "PANSS Negative Scale:\nforensic - non forensic")
segments(1.81, -1, 1.81, 8, lty = 2, col = 2, lwd = 2)
text(-13, 9, c("Study"), cex = .6)
text(13, 9, c("Mean [95%CI]"), cex = .6)
forest(ma3_nf,
       slab = paste(dati$Author, dati$Year, sep = ", "),
       mlab = "Random effects model",
       cex = .5,
       xlab = "Mean difference",
       main = "PANSS General Scale:\nforensic - non forensic")
segments(3.32, -1, 3.32, 8, lty = 2, col = 2, lwd = 2)
text(-21, 9, c("Study"), cex = .6)
text(25, 9, c("Mean [95%CI]"), cex = .6)
forest(ma4_nf,
       slab = paste(dati$Author, dati$Year, sep = ", "),
       mlab = "Random effects model",
       cex = .5,
       xlab = "Mean difference",
       main = "PANSS Total Score:\nforensic - non forensic")
segments(7.65, -1, 7.65, 4, lty = 2, col = 2, lwd = 2)
text(-45, 5, c("Study"), cex = .6)
text(50, 5, c("Mean [95%CI]"), cex = .6)