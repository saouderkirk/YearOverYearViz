library(tidyverse)
library(anytime)
library(lubridate)

options(scipen=10000)

df <- read.csv("2018jan.csv")

df <- df %>%
  select(Week.beginning,Global.New.cash.sales..WordPress.com) %>%
  transmute(week = anytime(Week.beginning),
            cash_sales = Global.New.cash.sales..WordPress.com,
            year = year(week),
            weeknum = lubridate::week(week)) %>%
  filter(row_number() > 1, row_number() < dim(df)[1] - 1) %>%
  mutate(year = as.factor(year))

ggplot(df, aes(x=weeknum, y=cash_sales, color = year)) + 
  theme_minimal() + 
  geom_point(alpha=I(.5)) +
  geom_smooth(se = FALSE, span = .4, alpha=I(.75), data=subset(df, year != "2018")) + 
  geom_line(alpha = I(.75), data=subset(df, span = .4, year == "2018")) +
  guides(colour = guide_legend(reverse=T)) + 
  geom_vline(xintercept=14, color = "darkgrey", linetype = "dashed") + 
  geom_vline(xintercept=27, color = "darkgrey", linetype = "dashed") + 
  geom_vline(xintercept=40, color = "darkgrey", linetype = "dashed") + 
  scale_color_manual(values=c("#b2b2b2","#b2b2b2","#a6a6a6", "#999999","#8c8c8c", "#78dcfa", "red")) + 
  ylab("Weekly Revenue") + 
  xlab("Week Number") + 
  ggtitle("Overall Revenue by Week, 2012-2018")

