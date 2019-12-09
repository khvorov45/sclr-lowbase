# Simulations
# Arseniy Khvorov
# Created 2019/12/09
# Last edit 2019/12/09

library(tidyverse)
library(extraDistr)
library(sclr)
library(broom)
library(furrr)

plan(multiprocess)

# Directories to be used later
sim_dir <- "sim"

# Settings ====================================================================

nsim <- 1e4
exp_probs <- seq(0.1, 1, 0.1)

# Functions ===================================================================

sim_dat <- function(exposure_prob,
                    seed = sample.int(.Machine$integer.max, 1)) {
  set.seed(seed)
  nsam <- 1e4
  pop <- tibble(
    logTitre = rnorm(nsam, 2, 2),
    inf_prob = 0.5 / (1 + exp(-5 + 1.5 * logTitre)),
    exposed = rbern(nsam, exposure_prob),
    status = rbern(nsam, inf_prob * exposed)
  )
  attr(pop, "seed") <- seed
  attr(pop, "exposure_prob") <- exposure_prob
  pop
}

fit_sclr_one <- function(dat, pop_name) {
  sclr(
    status ~ logTitre, dat, seed = attr(dat, "seed"),
    algorithm = "newton-raphson"
  ) %>%
    tidy() %>%
    mutate(
      pop = pop_name,
      exposure_prob = attr(dat, "exposure_prob"),
      seed = attr(dat, "seed")
    )
}

fit_sclr <- function(dat) {
  bind_rows(
    fit_sclr_one(dat, "general"),
    fit_sclr_one(filter(dat, exposed == 1), "exposed")
  )
}

sim_fit_one <- function(exposure_prob,
                        seed = sample.int(.Machine$integer.max, 1)) {
  fit_sclr(sim_dat(exposure_prob, seed))
}

sim_fit_many <- function(exposure_prob, nsim) {
  future_map_dfr(1:nsim, ~ sim_fit_one(exposure_prob))
}

vary_ep <- function(ep_range, nsim) {
  map_dfr(ep_range, sim_fit_many, nsim)
}

save_res <- function(sims) {
  write_csv(sims, file.path(sim_dir, paste0("res-", nsim, "sims.csv")))
}

# Script ======================================================================

sims <- vary_ep(exp_probs, nsim)
save_res(sims)
