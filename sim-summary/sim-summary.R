# Summary of sim
# Arseniy Khvorov
# Created 2019/12/09
# Last edit 2019/12/09

library(tidyverse)

# Directories to be used later
sim_dir <- "sim"
summ_dir <- "sim-summary"

# Script ======================================================================

sims <- read_csv(file.path(sim_dir, "res-10000sims.csv"))

summ <- sims %>%
  group_by(term, pop, exposure_prob, n_per_hhold) %>%
  summarise(
    est_mean = mean(estimate),
    se_mean = mean(std_error),
    est_sd = sd(estimate),
    nsim = length(unique(seed))
  )

write_csv(summ, file.path(summ_dir, "summ-10000sims.csv"))
