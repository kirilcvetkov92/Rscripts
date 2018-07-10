library(tidyverse)
library(DescTools)
library(ggpubr)

tnf <- readRDS("./tnf.rds")


tnf_long <- tnf %>% 
  gather(vis0:vis4, key = "visit", value = "value") %>% 
  group_by(visit)


gr_tnf_vis2 <- tnf_long %>% 
  filter(!is.na(visit) & !is.na(value)) %>% 
  ggboxplot(x = "visit", y = "value",
            palette = "aaas", 
            title = "TNF Levels by Visit - Thalidomide Group",
            xlab = "Visit",
            ylab = "TNF level",
            ggtheme = theme_gray())
suppressMessages(gr_tnf_vis2)


# substitute
tnf2 <- tnf_long
# case no. of next case
tnf2$value[tnf2$id == "25" & tnf2$visit == "vis4"] <- 
  tnf2$value[tnf$id == "7" & tnf2$visit == "vis4"]