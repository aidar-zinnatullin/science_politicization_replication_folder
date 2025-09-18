# R code to replicate Figure 2 in the appendix: Distribution of words in the science politicized articles (non-biomedical disciplines).

pkgs <- c("forcats","tidyverse", "here", "scales") 
invisible(lapply(pkgs, require, character.only = TRUE))
options(scipen = 999)
load(here("data", "social_science_polit.RData"))

# Per-media medians for reference lines
meds <- soc_science_polit %>%
  group_by(Media) %>%
  summarise(med = median(n_words), .groups = "drop")

ggplot(soc_science_polit, aes(x = n_words)) +
  geom_histogram(bins = 30, fill = "red") +            
  geom_vline(data = meds, aes(xintercept = med), linetype = "dashed") +
  facet_wrap(~ Media, scales = "free_y") +
  labs(title = "Distribution of words by media outlet",
    x = "Word count",
    y = "Articles") +
  scale_x_continuous(labels = comma) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold"))

ggsave(here("figures", "fig_2_app.jpeg"), width = 9, height = 6, units = "in", dpi = 500)



