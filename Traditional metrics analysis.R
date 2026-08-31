# read the data
library(readxl)
library(tidyverse)
library(ggplot2)
library(gridExtra)
dat_file <- "Data\\CGM_Insulin.xlsx"
cgm_dat1 <- read_excel(dat_file, sheet = 1)
cgm_dat2 <- read_excel(dat_file, sheet = 2)
cgm_dat3 <- read_excel(dat_file, sheet = 3)

#we are unable to share the real data we used for this analysis
#However, we provide the formatof the input file here:
#SUBJID2: unique subject identifier. In the data set that we ran this analysis on, there were 12 unique subjects
#LBDTC: date and time of CGM reading in the following format, yyyy-mm-ddThh:mm:ss. Timezone was UTC
#LBDY: study day in reference to treatment initiation. date of interest - reference date + 1 if date of interest is after reference date, reference date - #date of interest if date of interest is before reference date. We suggest using baseline date as the reference date if no treatment is used in the study
#CGM_mg_dL.r: CGM reading in mg/dL

cgm_dat <- rbind(cgm_dat1, cgm_dat2, cgm_dat3)
n_reading_1d <- 288


cgm_df <- cgm_dat %>%
  mutate(LBDTC = as.POSIXct(LBDTC, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC"),
         week = sign(LBDY)*(((sign(LBDY)*LBDY - 1) %/% 7) + 1),
         month = sign(LBDY)*(((sign(LBDY)*LBDY - 1) %/% 30) + 1)) %>%
  arrange(SUBJID2, LBDTC) %>%
  filter(week > 0)
colnames(cgm_df)=c("id","time","day","glucose","week","month")

# proportion of missing data every week
cgm_missing_week <- cgm_df %>% group_by(id, week) %>%
  summarise(Missing = 1-n()/(n_reading_1d*7), .groups = "drop")

ggplot(aes(x=week, y=Missing), data=filter(cgm_missing_week,week>0)) +
  geom_line(aes(color=id))+
  labs(color = "Subject", x = "Week since therapy",
       y = "Proportion of missing data")
ggsave("Figures\\missing.png", width = 9, height = 6)

# proportion of missing data every month
cgm_missing_month <- cgm_df %>% group_by(id, month) %>%
  summarise(Missing = 1-n()/(n_reading_1d*30), .groups = "drop")

ggplot(aes(x=month, y=Missing), data=cgm_missing_month) +
  geom_line(aes(color=id))


# weekly metrics
# TIR, TBR, mean glucose, SD, CV
metrics_weekly <- cgm_df %>%
  group_by(id, week) %>%
  summarise(
    n_obs = n(),
    n_in_range = sum(between(glucose, 70, 180)),
    tir_pct = 100 * n_in_range / n_obs,
    n_below_range = sum(glucose<54),
    tbr_pct = 100 * n_below_range / n_obs,
    glu_mean = mean(glucose),
    glu_SD = sd(glucose),
    glu_CV = glu_SD/glu_mean,
    .groups = "drop"
  ) %>%
  ungroup() %>%
  mutate(id=factor(id, levels = paste0("A", seq(1,12))))

p1<-ggplot(aes(x=week,y=tir_pct), data=metrics_weekly)+
  geom_line(aes(color=id, linetype = Insulin_free))+
  labs(color = "Subject", x = "Week since therapy",
       y = "Time in range")+
  guides(color = guide_legend(ncol = 3))

p2<-ggplot(aes(x=week,y=tbr_pct), data=metrics_weekly)+
  geom_line(aes(color=id, linetype = Insulin_free))+
  labs(color = "Subject", x = "Week since therapy",
       y = "Time below range")+
  guides(color = guide_legend(ncol = 3))

p3 <- ggplot(aes(x=week,y=glu_mean), data=metrics_weekly)+
  geom_line(aes(color=id, linetype = Insulin_free))+
  labs(color = "Subject", x = "Week since therapy",
       y = "Glucose mean")+
  guides(color = guide_legend(ncol = 3))

p4 <- ggplot(aes(x=week,y=glu_CV), data=metrics_weekly)+
  geom_line(aes(color=id, linetype = Insulin_free))+
  labs(color = "Subject", x = "Week since therapy",
       y = "Glucose CV")+
  guides(color = guide_legend(ncol = 3))

my_grid <- grid.arrange(p1,p2,p3,p4, nrow=2)
ggsave("Figures\\metrics_weekly.png",my_grid, width = 12, height = 6)



# monthly metrics
# TIR, TBR, mean glucose, SD, CV
metrics_monthly <- cgm_df %>%
  filter(between(month, 1,12)) %>%
  group_by(id, month) %>%
  summarise(
    n_obs = n(),
    n_in_range = sum(between(glucose, 70, 180)),
    tir_pct = 100 * n_in_range / n_obs,
    n_below_range = sum(glucose<54),
    tbr_pct = 100 * n_below_range / n_obs,
    glu_mean = mean(glucose),
    glu_SD = sd(glucose),
    glu_CV = glu_SD/glu_mean,
    .groups = "drop"
  )


p5<-ggplot(aes(x=month,y=tir_pct), data=metrics_monthly)+
  geom_line(aes(color=id))+
  labs(color = "Subject", x = "Month since therapy",
       y = "Time in range")+
  guides(color = guide_legend(ncol = 3))

p6<-ggplot(aes(x=month,y=tbr_pct), data=metrics_monthly)+
  geom_line(aes(color=id))+
  labs(color = "Subject", x = "Month since therapy",
       y = "Time below range")+
  guides(color = guide_legend(ncol = 3))

p7<-ggplot(aes(x=month,y=glu_mean), data=metrics_monthly)+
  geom_line(aes(color=id))+
  labs(color = "Subject", x = "Month since therapy",
       y = "Glucose mean")+
  guides(color = guide_legend(ncol = 3))

p8<-ggplot(aes(x=month,y=glu_CV), data=metrics_monthly)+
  geom_line(aes(color=id))+
  labs(color = "Subject", x = "Month since therapy",
       y = "Glucose CV")+
  guides(color = guide_legend(ncol = 3))

my_grid <- grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8, nrow=4)
ggsave("Figures\\metrics.png",my_grid, width = 11, height = 11)