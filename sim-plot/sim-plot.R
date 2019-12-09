# Graphs of sim-summary
# Arseniy Khvorov
# Created 2019/12/09
# Last edit 2019/12/09

library(tidyverse)
library(ggdark) # devtools::install_github("khvorov45/ggdark")
library(latex2exp)

# Directories to be used later
summ_dir <- "sim-summary"
plot_dir <- "sim-plot"

# Functions ===================================================================

my_guide_legend <- function(name) {
  guide_legend(title = name, title.position = "left", direction = "vertical")
}

par_labeller <- function(breaks) {
  breaks <- recode(
    breaks, "beta_0" = "$\\beta_0$", "beta_logTitre" = "$\\beta_T$",
    "theta" = "$\\theta$"
  )
  TeX(breaks)
}

# Script ======================================================================

summ <- read_csv(file.path(summ_dir, "summ-10000sims.csv"))

summ %>%
  pivot_longer(c(est_mean, se_mean), "measure") %>%
  mutate(pop = recode(pop, "exposed" = "Exposed", "general" = "General")) %>%
  ggplot(aes(exposure_prob, value, col = term, lty = pop, shape = pop)) +
  dark_theme_bw(verbose = FALSE) +
  theme(
    legend.position = "bottom",
    strip.background = element_blank(),
    panel.spacing = unit(0, "null"),
    strip.placement = "outside",
    axis.title.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.box.spacing = unit(0, "null"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  facet_wrap(
    ~measure, scales = "free_y", nrow = 1, strip.position = "left",
    labeller = as_labeller(c(
      "est_mean" = "Estimate mean", "se_mean" = "Mean SE"
    ))
  ) +
  xlab("Proportion exposed") +
  scale_shape_manual(values = c(17, 18)) +
  scale_x_continuous(
    breaks = seq(0, 1, 0.1), labels = scales::percent_format(1)
  ) +
  scale_color_discrete(labels = par_labeller) +
  guides(
    shape = my_guide_legend("Population"),
    linetype = my_guide_legend("Population"),
    color = my_guide_legend("Term")
  ) +
  geom_line() +
  geom_point()

ggsave_dark(
  file.path(plot_dir, "plot-10000sims.pdf"), width = 15, height = 8,
  units = "cm"
)
