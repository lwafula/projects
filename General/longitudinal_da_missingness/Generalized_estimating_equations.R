
# https://data.library.virginia.edu/getting-started-with-generalized-estimating-equations/

URL <- "http://static.lib.virginia.edu/statlab/materials/data/depression.csv"
dat <- read.csv(URL, stringsAsFactors = TRUE)
dat$id <- factor(dat$id)
dat$drug <- relevel(dat$drug, ref = "standard")
head(dat, n = 3)

length(unique(dat$id))

library(magrittr)
with(dat, tapply(depression, list(diagnose, drug, time), mean)) %>% 
  ftable() %>% 
  round(2)


# GEE
# 

# install.packages("gee")
library(gee) # version 4.13-25
dep_gee <- gee(depression ~ diagnose + drug*time,
               data = dat, 
               id = id, 
               family = binomial,
               corstr = "independence")
summary(dep_gee)

# GEE w/ exchangeable correlation

dep_gee2 <- gee(depression ~ diagnose + drug*time,
                data = dat, 
                id = id, 
                family = binomial,
                corstr = "exchangeable")
summary(dep_gee2)

# AR-1 correlation structure

dep_gee3 <- gee(depression ~ diagnose + drug*time,
                data = dat, 
                id = id, 
                family = binomial,
                corstr = "AR-M", Mv = 1)

dep_gee3$working.correlation

## compare with mixed effect model

library(lme4) # version 1.1-31
dep_glmer <- glmer(depression ~ diagnose + drug*time + (1|id), 
                   data = dat, family = binomial)
summary(dep_glmer, corr = FALSE)

library(ggeffects) # version 1.2.0
plot(ggemmeans(dep_glmer, terms = c("time", "drug"), 
               condition = c(diagnose = "severe"))) + 
  ggplot2::ggtitle("GLMER Effect plot")


library(emmeans) # version 1.8.4-1
emm_out <- emmeans(dep_gee2, specs = c("time", "drug"), 
                   at = list(diagnose = "severe"), 
                   cov.keep = "time", 
                   regrid = "response") %>% 
  as.data.frame()

library(ggplot2) # version 3.4.1
ggplot(emm_out) +
  aes(x = time, y = prob, color = drug, fill = drug) +
  geom_line() +
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL, 
                  color = NULL), alpha = 0.15) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1", guide = NULL) +
  scale_y_continuous(labels = scales::percent) +
  theme_ggeffects() +
  labs(title = "GEE Effect plot", y = "depression")
