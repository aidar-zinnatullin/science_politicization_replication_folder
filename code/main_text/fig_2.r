
# R script to replicate Figure 2: Science politicization in German COVID-19 coverage
# (biomedical, n = 1,675; non-biomedical, n = 771). Red: centered 5-day moving average. 
# Blue: LOESS trend. Dots: daily observations.

pkgs <- c("zoo","tidyverse", "here", "patchwork") 
invisible(lapply(pkgs, require, character.only = TRUE))

load( here("data", "natural_science_polit.RData")) 
natural_science_polit %>% glimpse()

load(here( "data", "social_science_polit.RData"))
soc_science_polit %>% glimpse()

# remove the messages that are in both datasets
natural_science_polit <- natural_science_polit %>% filter(!id_article %in% soc_science_polit$id_article)


daily_counts <- natural_science_polit %>% mutate(pub_date = as.Date(Date)) %>%     
  group_by(pub_date) %>% summarize(count = n())                  


# daily article counts
daily_counts <- daily_counts %>% arrange(pub_date) %>%
  mutate(ma_5day = rollmean(count, k = 5, fill = NA, align = "center"))

natural <- ggplot(daily_counts, aes(x = pub_date)) +
  geom_point(aes(y = count), color = "azure4", size = 1) +
  # 5-day moving average as a line
  geom_line(aes(y = ma_5day), color = "red", linewidth = 1.5, na.rm = TRUE) +
  geom_smooth(aes(y = count), method = "loess", span = 0.3, se = TRUE, color = "blue", lwd = 1.2) +
  geom_vline(xintercept = as.Date("2020-11-01"), linetype = "dotted", linewidth = 0.8, color = "brown") +
  annotate("text", label = "Mild lockdown", x = as.Date("2020-11-01"), y = 11, angle = 90, vjust = -0.4, hjust = 1, color = "brown") +
  geom_vline(xintercept = as.Date("2020-12-01"), linetype = "dotted", linewidth = 0.8, color = "orange") +
  annotate("text", label = "Strict lockdown", x = as.Date("2020-12-01"), y = 15, angle = 90, vjust = -0.4, hjust = 1, color = "orange") +
  geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dotted", linewidth = 0.8, color = "violetred") +
  annotate("text", label = "First lockdown", x = as.Date("2020-03-01"), y = 10, angle = 90, vjust = -0.4, hjust = 1, color = "violetred") +
  geom_vline(xintercept = as.Date("2020-04-01"), linetype = "dotted", linewidth = 0.8, color = "slateblue4") +
  annotate("text", label = "Heinsberg's study", x = as.Date("2020-04-01"), y = 16, angle = 90, vjust = -0.4, hjust = 1, color = "slateblue4") +
  geom_vline(xintercept = as.Date("2021-05-01"), linetype = "dotted", linewidth = 0.8, color = "darkgreen") +
  annotate("text", label = "Easing restrictions and vaccination", x = as.Date("2021-05-01"), y = 17, angle = 90, vjust = -0.4, hjust = 1,
           color = "darkgreen") +
  geom_vline(xintercept = as.Date("2021-12-01"), linetype = "dotted", linewidth = 1.1, color = "blue") +
  annotate("text", label = "Omicron-related restrictions", x = as.Date("2021-12-01"), y = 17, angle = 90, vjust = -0.4, hjust = 1,
           color = "blue") +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m", guide = guide_axis(angle = 45)) +
  labs(title = "Science politicization coverage of COVID-19 (biomedical disciplines)", x = "Date", y = "Number of articles") +
  theme_classic()


# social science ----------------------------------------------------------


soc_science_polit <- soc_science_polit %>% filter(!id_article %in% natural_science_polit$id_article)


daily_counts_soc <- soc_science_polit %>% mutate(pub_date = as.Date(Date)) %>% group_by(pub_date) %>% summarize(count = n())                  


# daily article counts
daily_counts_soc <- daily_counts_soc %>% arrange(pub_date) %>% mutate(ma_5day = rollmean(count, k = 5, fill = NA, align = "center"))

social_implications <- ggplot(daily_counts_soc, aes(x = pub_date)) +
  geom_point(aes(y = count), color = "azure4", size = 1) +
  # 5-day moving average as a line
  geom_line(aes(y = ma_5day), color = "red",  linewidth = 1.5,  na.rm = TRUE) +
  geom_smooth(aes(y = count), method = "loess", span = 0.3, se = TRUE, color = "blue", lwd = 1.2) +
  geom_vline(xintercept = as.Date("2020-11-01"), linetype = "dotted", linewidth = 0.8,color = "brown") +
  annotate("text", label = "Mild lockdown", x = as.Date("2020-11-01"), y = 11, angle = 90, vjust = -0.4, hjust = 1, color = "brown") +
  geom_vline(xintercept = as.Date("2020-12-01"), linetype = "dotted", linewidth = 0.8, color = "orange") +
  annotate("text", label = "Strict lockdown", x = as.Date("2020-12-01"), y = 15, angle = 90, vjust = -0.4, hjust = 1, color = "orange") +
  geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dotted", linewidth = 0.8, color = "violetred") +
  annotate("text", label = "First lockdown", x = as.Date("2020-03-01"), y = 10, angle = 90, vjust = -0.4, hjust = 1, color = "violetred") +
  geom_vline(xintercept = as.Date("2020-04-01"), linetype = "dotted", linewidth = 0.8, color = "slateblue4") +
  annotate("text", label = "Heinsberg's study", x = as.Date("2020-04-01"), y = 16, angle = 90, vjust = -0.4, hjust = 1, color = "slateblue4") +
  geom_vline(xintercept = as.Date("2021-05-01"), linetype = "dotted", linewidth = 0.8, color = "darkgreen") +
  annotate("text", label = "Easing restrictions and vaccination", x = as.Date("2021-05-01"), y = 17, angle = 90, vjust = -0.4, hjust = 1, 
           color = "darkgreen") +
  geom_vline(xintercept = as.Date("2021-12-01"), linetype = "dotted", linewidth = 1.1, color = "blue") +
  annotate("text", label = "Omicron-related restrictions", x = as.Date("2021-12-01"), y = 17, 
           angle = 90, vjust = -0.4, hjust = 1, color = "blue") +
  scale_x_date(date_breaks = "1 month", date_labels = "%Y-%m", guide = guide_axis(angle = 45)) +
  labs(title = "Science politicization coverage of COVID-19 (non-biomedical disciplines)", x = "Date", y = "Number of articles") +
  theme_classic()

combined_plot <- (natural + social_implications) + plot_layout(nrow = 2) + plot_annotation(tag_levels = "a")

ggsave(here( "figures", "figure_2.jpeg"), combined_plot, width = 10, height = 8, dpi = 300)



