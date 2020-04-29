# Summary of sim

library(tidyverse)

# Directories to be used later
sim_dir <- here::here("sim")
summ_dir <- here::here("sim-summary")

# Script ======================================================================

sims <- read_csv(file.path(sim_dir, "sim.csv"), col_types = cols())

summ <- sims %>%
  group_by(term, pop, exposure_prob, n_per_hhold) %>%
  summarise(
    est_mean = mean(estimate),
    se_mean = mean(std_error),
    est_sd = sd(estimate),
    nsim = length(unique(seed))
  )

write_csv(summ, file.path(summ_dir, "sim-summary.csv"))
