#  R script to replicate Table 22 in Appendix E

pkgs <- c("data.table","tidyverse", "here", "performance", "lme4", "lmerTest",  "gt", "htmltools") 
invisible(lapply(pkgs, require, character.only = TRUE))
options(scipen=999)

load( here( "data", "smaller_corpus", "corpus_covid_media_reduced.RData"))
load( here( "data", "natural_science_polit.RData")) 
load(here( "data", "social_science_polit.RData"))
load(here( "data", "covid_pandemic_data.RData"))

# remove the messages that are in both datasets

natural_science_polit <- natural_science_polit %>% filter(!id_article %in% soc_science_polit$id_article)

n_articles_per_media <- smaller_corpus %>% 
  group_by(Media) %>% 
  summarise(n_articles = n()) %>% 
  arrange(desc(n_articles)) %>% 
  ungroup() 


per_media_politicization <- natural_science_polit %>% 
  group_by(Media) %>% 
  summarise(n_articles = n()) %>% 
  arrange(desc(n_articles)) %>% 
  ungroup() 

n_articles_per_media <- n_articles_per_media %>% 
  left_join(per_media_politicization, by = "Media") %>% 
  rename(n_articles_corpus = n_articles.x,
         n_articles_partisanship = n_articles.y) 
n_articles_per_media$share <- n_articles_per_media$n_articles_partisanship / n_articles_per_media$n_articles_corpus
n_articles_per_media <- n_articles_per_media[-11,]

# descending order
n_articles_per_media <- n_articles_per_media %>% 
  arrange(desc(share)) %>% 
  mutate(Media = factor(Media, levels = Media)) # to keep the order in the plot

n_articles_per_media$share <- n_articles_per_media$share * 100
names(n_articles_per_media) <- c("Media", "Overall", "Politicized Materials", "Percentage")


table(smaller_corpus$Media)
smaller_corpus$ideology <- NA
smaller_corpus$ideology[smaller_corpus$Media == "Tagesschau"] <- "Centrist"
smaller_corpus$ideology[smaller_corpus$Media == "ZDF Heute"] <- "Centrist"
smaller_corpus$ideology[smaller_corpus$Media == "T-Online"] <- "Centrist"
smaller_corpus$ideology[smaller_corpus$Media == "RTL"] <- "Centrist"
smaller_corpus$ideology[smaller_corpus$Media == "NTV"] <- "Centrist"

smaller_corpus$ideology[smaller_corpus$Media == "Die Zeit"] <- "Center-Left"
smaller_corpus$ideology[smaller_corpus$Media == "Der Spiegel"] <- "Center-Left"
smaller_corpus$ideology[smaller_corpus$Media == "SZ"] <- "Center-Left"

smaller_corpus$ideology[smaller_corpus$Media == "Die Welt"] <- "Center-Right"
smaller_corpus$ideology[smaller_corpus$Media == "FAZ"] <- "Center-Right"
smaller_corpus$ideology[smaller_corpus$Media == "Bild"] <- "Center-Right"


# politicized
table(natural_science_polit$Media)
natural_science_polit$ideology <- NA
natural_science_polit$ideology[natural_science_polit$Media == "Tagesschau"] <- "Centrist"
natural_science_polit$ideology[natural_science_polit$Media == "ZDF Heute"] <- "Centrist"
natural_science_polit$ideology[natural_science_polit$Media == "T-Online"] <- "Centrist"
natural_science_polit$ideology[natural_science_polit$Media == "RTL"] <- "Centrist"
natural_science_polit$ideology[natural_science_polit$Media == "NTV"] <- "Centrist"

natural_science_polit$ideology[natural_science_polit$Media == "Die Zeit"] <- "Center-Left"
natural_science_polit$ideology[natural_science_polit$Media == "Der Spiegel"] <- "Center-Left"
natural_science_polit$ideology[natural_science_polit$Media == "SZ"] <- "Center-Left"

natural_science_polit$ideology[natural_science_polit$Media == "Die Welt"] <- "Center-Right"
natural_science_polit$ideology[natural_science_polit$Media == "FAZ"] <- "Center-Right"
natural_science_polit$ideology[natural_science_polit$Media == "Bild"] <- "Center-Right"

smaller_corpus$day <- as.Date(smaller_corpus$Date)
natural_science_polit$day <- as.Date(natural_science_polit$Date)


# now I can summarize the corpus by media and day
smaller_corpus_daily <-  smaller_corpus %>% 
  group_by(Media, day) %>% 
  summarise(n_articles = n()) %>% 
  ungroup() 

# now I need to do the same thing in order to get the number of politicized articles
natural_science_polit %>% glimpse()
# now I can summarize the corpus by media and day
natural_science_polit_daily <-  natural_science_polit %>% 
  group_by(Media, day) %>% 
  summarise(n_articles = n()) %>% 
  ungroup()

# next step is to merge the two dataframes by the largest one, because the corpus is larger than the politicized articles
# and if I have NA in the politicized corpus, I will replace it with 0
names(smaller_corpus_daily)
names(natural_science_polit_daily)
corpus_combined <- smaller_corpus_daily %>% 
  left_join(natural_science_polit_daily, by = c("Media", "day")) %>% 
  rename(n_articles_politicized = n_articles.y,
         n_articles_corpus = n_articles.x) 

# need to add info about the ideology:

corpus_combined$ideology <- NA
corpus_combined$ideology[corpus_combined$Media == "Tagesschau"] <- "Centrist"
corpus_combined$ideology[corpus_combined$Media == "ZDF Heute"] <- "Centrist"
corpus_combined$ideology[corpus_combined$Media == "T-Online"] <- "Centrist"
corpus_combined$ideology[corpus_combined$Media == "RTL"] <- "Centrist"
corpus_combined$ideology[corpus_combined$Media == "NTV"] <- "Centrist"
corpus_combined$ideology[corpus_combined$Media == "Die Zeit"] <- "Center-Left"
corpus_combined$ideology[corpus_combined$Media == "Der Spiegel"] <- "Center-Left"
corpus_combined$ideology[corpus_combined$Media == "SZ"] <- "Center-Left"
corpus_combined$ideology[corpus_combined$Media == "Die Welt"] <- "Center-Right"
corpus_combined$ideology[corpus_combined$Media == "FAZ"] <- "Center-Right"
corpus_combined$ideology[corpus_combined$Media == "Bild"] <- "Center-Right"


# now I need to replace the NA in the politicized corpus with 0
corpus_combined$n_articles_politicized[is.na(corpus_combined$n_articles_politicized)] <- 0

# create shares
corpus_combined$share <- corpus_combined$n_articles_politicized / corpus_combined$n_articles_corpus

corpus_combined_wide <- corpus_combined %>% 
  select(Media, day, share) %>% 
  pivot_wider(names_from = Media, values_from = share) 
corpus_combined_wide$`ZDF Heute` <- NULL

corpus_combined_wide[is.na(corpus_combined_wide)] <- 0

data_long <- corpus_combined_wide %>% pivot_longer(cols = -day, names_to = "media", values_to = "share")

# need to add ideology
data_long$ideology <- NA
data_long$ideology[data_long$media == "Tagesschau"] <- "Centrist"
data_long$ideology[data_long$media == "T-Online"] <- "Centrist"
data_long$ideology[data_long$media == "RTL"] <- "Centrist"
data_long$ideology[data_long$media == "NTV"] <- "Centrist"
data_long$ideology[data_long$media == "Die Zeit"] <- "Center-Left"
data_long$ideology[data_long$media == "Der Spiegel"] <- "Center-Left"
data_long$ideology[data_long$media == "SZ"] <- "Center-Left"
data_long$ideology[data_long$media == "Die Welt"] <- "Center-Right"
data_long$ideology[data_long$media == "FAZ"] <- "Center-Right"
data_long$ideology[data_long$media == "Bild"] <- "Center-Right"

data_long$media <- factor(data_long$media,  levels = c("Tagesschau", "T-Online", "NTV", "RTL", "Bild", "Der Spiegel", "FAZ", "Die Welt", "Die Zeit", "SZ"))
data_long$ideology <- factor(data_long$ideology, levels = c("Centrist", "Center-Left", "Center-Right"))

epsilon <- 0.001

data_long <- data_long %>% mutate(share_adj = ifelse(share == 0, epsilon,
                                                     ifelse(share == 1, 1 - epsilon, share)))
data_long <- data_long %>%
  mutate(share_logit = log(share_adj / (1 - share_adj)))

# adding day (as numeric or factor) to account for trends over time:
data_long$time <- as.numeric(data_long$day)  # if you wish to treat it as a continuous variable

n_distinct(data_long$day) == n_distinct(data_long$time) # TRUE


# merge the two datasets
data_long <- data_long %>% 
  left_join(cases_to_merge, by = "day")
table(is.na(data_long$lagged_cases))
nas <- data_long %>% filter(is.na(lagged_cases))

# need to fill with 0s all NAs
data_long$lagged_cases[is.na(data_long$lagged_cases)] <- 0
data_long$lagged_death[is.na(data_long$lagged_death)] <- 0

data_long$log_lag_cases <-log(data_long$lagged_cases +1)
data_long$log_lag_death <-log(data_long$lagged_death +1)

lme_model_2_cases <- lmer(share_logit ~ ideology + log_lag_cases + log_lag_death + (1|media) + (1|day), data = data_long)
summary(lme_model_2_cases)  

# table 4

coef_df <- as.data.frame(coef(summary(lme_model_2_cases))) %>% rownames_to_column("term")

label_map <- c("(Intercept)" = "(Intercept)",
               "ideologyCenter-Left" = "Center-Left",
               "ideologyCenter-Right" = "Center-Right",
               "log_lag_cases" = "Lagged cases (log)",
               "log_lag_death"  = "Lagged deaths (log)")

order_levels <- unname(label_map[c("(Intercept)","ideologyCenter-Left",
                                   "ideologyCenter-Right","log_lag_cases",
                                   "log_lag_death")])

fmt_p <- function(p) ifelse(p < 0.001, "0.000", sprintf("%.3f", p))

tab22_df <- coef_df %>% 
  filter(term %in% names(label_map)) %>% 
  mutate(Variable = unname(label_map[term])) %>% 
  select(Variable, Estimate, `Std. Error`, `Pr(>|t|)`) %>% 
  mutate(Estimate   = round(Estimate, 2), 
         `Std. Error` = round(`Std. Error`, 2),
         `p-value`  = fmt_p(`Pr(>|t|)`)) %>% 
  select(Variable, Estimate, `Std. Error`, `p-value`) %>% 
  arrange(factor(Variable, levels = order_levels))

tab22 <- tab22_df %>% 
  gt() %>% 
  tab_header(title = md("Table 22. Linear mixed-effects model results for the proportion of science-politicized (biomedical disciplines) COVID-19 content by media outlet ideological leaning")) %>% 
  cols_align(align = "center", columns = -Variable)

gtsave(tab22,  here( "tables",  "tab_22_app.html"))

