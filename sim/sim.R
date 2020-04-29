# Simulations

library(tidyverse)
library(extraDistr)
library(sclr)
library(broom)
library(furrr)

plan(multiprocess)

# Directories to be used later
sim_dir <- here::here("sim")

# Settings ====================================================================

nsim <- 1e4
exp_probs <- seq(0.1, 1, 0.1)

# Functions ===================================================================

sim_dat <- function(exposure_prob, n_per_hhold = 1,
                    seed = sample.int(.Machine$integer.max, 1)) {
  set.seed(seed)
  nsam <- 1e4
  n_hhold <- nsam / n_per_hhold
  pop <- tibble(
    hhold = seq(1:n_hhold),
    exposed = rbern(n_hhold, exposure_prob)
  ) %>%
    slice(rep(1:n(), each = n_per_hhold)) %>%
    mutate(
      logTitre = rnorm(n(), 2, 2),
      inf_prob = 0.5 / (1 + exp(-5 + 1.5 * logTitre)),
      status = rbern(n(), inf_prob * exposed)
    )
  exp_hhold_meas <- pop %>%
    group_by(hhold) %>%
    filter(sum(status == 1) > 0) %>%
    pull(hhold)
  pop <- pop %>%
    mutate(exposed_meas = hhold %in% exp_hhold_meas)
  attr(pop, "seed") <- seed
  attr(pop, "exposure_prob") <- exposure_prob
  attr(pop, "n_per_hhold") <- n_per_hhold
  pop
}

fit_sclr_one <- function(dat, pop_name) {
  sclr(
    status ~ logTitre, dat,
    seed = attr(dat, "seed"),
    algorithm = "newton-raphson"
  ) %>%
    tidy() %>%
    mutate(
      pop = pop_name,
      exposure_prob = attr(dat, "exposure_prob"),
      n_per_hhold = attr(dat, "n_per_hhold"),
      seed = attr(dat, "seed")
    )
}

fit_sclr <- function(dat) {
  always <- bind_rows(
    fit_sclr_one(dat, "general"),
    fit_sclr_one(filter(dat, exposed == 1), "exposed")
  )
  if (attr(dat, "n_per_hhold") > 1) {
    sometimes <- fit_sclr_one(filter(dat, exposed_meas), "exposed_meas")
    always <- bind_rows(always, sometimes)
  }
  always
}

sim_fit_one <- function(exposure_prob, n_per_hhold = 1,
                        seed = sample.int(.Machine$integer.max, 1)) {
  fit_sclr(sim_dat(exposure_prob, n_per_hhold, seed))
}

sim_fit_many <- function(exposure_prob, n_per_hhold = 1, nsim = 2) {
  future_map_dfr(1:nsim, ~ sim_fit_one(exposure_prob, n_per_hhold))
}

vary_ep <- function(ep_range, n_per_hhold = 1, nsim = 2) {
  map_dfr(ep_range, sim_fit_many, n_per_hhold, nsim)
}

save_res <- function(sims) {
  write_csv(sims, file.path(sim_dir, "sim.csv"))
}

add_res <- function(sims) {
  write_csv(sims, file.path(sim_dir, "sim.csv"), append = TRUE)
}

# Script ======================================================================

# One per household
sims <- vary_ep(exp_probs, 1, nsim)
save_res(sims)

# Multiple per household
sims_mult <- vary_ep(exp_probs, 4, nsim)
add_res(sims_mult)
