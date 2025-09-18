# R code to replicate Figure 1 in the appendix: Distribution of words in the science politicized articles (biomedical disciplines)

pkgs <- c("forcats","tidyverse", "here", "scales") 
invisible(lapply(pkgs, require, character.only = TRUE))
options(scipen = 999)
load( here("data", "natural_science_polit.RData")) 
natural_science_polit %>% glimpse()


# Per-media medians for reference lines
meds <- natural_science_polit %>% group_by(Media) %>% summarise(med = median(n_words), .groups = "drop")

ggplot(natural_science_polit, aes(x = n_words)) +
  geom_histogram(bins = 30, fill = "green") +  
  geom_vline(data = meds, aes(xintercept = med), linetype = "dashed") +
  facet_wrap(~ Media, scales = "free_y") +
  labs(title = "Distribution of words by media outlet", x = "Word count", y = "Articles") +
  scale_x_continuous(labels = comma) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold"))

ggsave( here( "figures", "fig_1_app.jpeg"), width = 9, height = 6, units = "in", dpi = 500)
