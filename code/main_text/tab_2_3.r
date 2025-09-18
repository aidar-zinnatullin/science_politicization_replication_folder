# R script to test H1: Between-outlet variance in the daily politicized biomedical coverage is smaller than day variance.


pkgs <- c("data.table","tidyverse", "here", "performance", "lme4", "lmerTest",  "gt", "htmltools") 

invisible(lapply(pkgs, require, character.only = TRUE))

load(here("data", "corpus_covid_media.RData"))
load( here("data", "natural_science_polit.RData")) 
load(here("data", "social_science_polit.RData"))
load(here("data", "covid_pandemic_data.RData"))

# remove the messages that are in both datasets

natural_science_polit <- natural_science_polit %>% filter(!id_article %in% soc_science_polit$id_article)

# need to identify starting and end points
relevant_dates_corpus <- corpus_covid_media %>% filter(as.Date(Date) >= "2020-01-01" & as.Date(Date) <= "2021-12-31")

# first, Date must be a day only, without the exact time
relevant_dates_corpus$day <- as.Date(relevant_dates_corpus$Date)
# n_distinct(relevant_dates_corpus$day) # 722 days

# now I can summarize the corpus by media and day
corpus_covid_media_daily <-  relevant_dates_corpus %>% group_by(Media, day) %>% summarise(n_articles = n()) %>% 
  ungroup() 

natural_science_polit$day <- as.Date(natural_science_polit$Date)
# now I can summarize the corpus by media and day
natural_science_polit_daily <-  natural_science_polit %>% group_by(Media, day) %>% 
  summarise(n_articles = n()) %>% ungroup()

# next step is to merge the two dataframes by the largest one, because the corpus is larger than the politicized articles
# and if I have NA in the politicized corpus, I will replace it with 0
corpus_combined <- corpus_covid_media_daily %>% left_join(natural_science_polit_daily, by = c("Media", "day")) %>% 
  rename(n_articles_politicized = n_articles.y, n_articles_corpus = n_articles.x) 

corpus_combined$n_articles_politicized[is.na(corpus_combined$n_articles_politicized)] <- 0

# create shares
corpus_combined$share <- corpus_combined$n_articles_politicized / corpus_combined$n_articles_corpus

# all of them are different when it comes to the number of days

corpus_combined_wide <- corpus_combined %>% select(Media, day, share) %>% pivot_wider(names_from = Media, values_from = share) 
corpus_combined_wide$`ZDF Heute` <- NULL
corpus_combined_wide[is.na(corpus_combined_wide)] <- 0


# statistical testing here ------------------------------------------------

data_long <- corpus_combined_wide %>% pivot_longer(cols = -day, names_to = "media", values_to = "share")
data_long$media <- factor(data_long$media,  levels = c("Tagesschau", "T-Online", "NTV", "RTL", "Bild", "Der Spiegel", "FAZ", "Die Welt", "Die Zeit", "SZ"))

epsilon <- 0.001

data_long <- data_long %>%
  mutate(share_adj = ifelse(share == 0, epsilon,
                            ifelse(share == 1, 1 - epsilon, share)))
data_long <- data_long %>%
  mutate(share_logit = log(share_adj / (1 - share_adj)))


# merge the two datasets
data_long <- data_long %>% 
  left_join(cases_to_merge, by = "day")
table(is.na(data_long$lagged_cases))
nas <- data_long %>% filter(is.na(lagged_cases))

# need to fill with 0s all NAs
data_long$lagged_cases[is.na(data_long$lagged_cases)] <- 0
data_long$lagged_death[is.na(data_long$lagged_death)] <- 0

data_long$log_lag_cases <-  log(data_long$lagged_cases +1 )
data_long$log_lag_death <-  log(data_long$lagged_death +1 )

options(scipen = 999) # to avoid scientific notation in the output


# variance-components model (no media fixed effects) 
m_h1 <- lmer(share_logit ~ log_lag_cases + log_lag_death + (1 | media) + (1 | day),
             data = data_long, REML = TRUE)
summary(m_h1)

# day-only model to assess the added value of the media random effect
m_dayonly <- lmer(share_logit ~ log_lag_cases + log_lag_death + (1 | day), data = data_long, REML = TRUE)


# table 2
vc <- as.data.frame(VarCorr(m_h1)) %>%
  select(grp, vcov, sdcor)

var_media <- vc$vcov[vc$grp == "media"]
var_day   <- vc$vcov[vc$grp == "day"]
var_resid <- vc$vcov[vc$grp == "Residual"]
tot_var   <- var_media + var_day + var_resid

icc_media <- var_media / tot_var
icc_day   <- var_day   / tot_var

tab2_df <- tibble(Component = c("Outlet (media)", "Day", "Residual"),
  Variance  = c(var_media, var_day, var_resid),
  `Std. Dev.` = c(vc$sdcor[vc$grp == "media"],
                  vc$sdcor[vc$grp == "day"],
                  vc$sdcor[vc$grp == "Residual"]),
  ICC = c(icc_media, icc_day, NA_real_))  %>% 
  mutate(Variance = round(Variance, 3),
         `Std. Dev.`= round(`Std. Dev.`, 3),
         ICC = round(ICC, 3))

tab2 <- tab2_df %>% gt() %>% 
  tab_header(title = md("Table 2: Variance components and intraclass correlations (ICCs) for H1 model; linear mixed model fitted by REML with random intercepts for outlet and day.")) %>% 
  sub_missing(columns = "ICC", missing_text = "—") %>% 
  cols_align(align = "center", columns = -Component)

gtsave(tab2, here("tables", "table_2.html"))

# table 3

m_dayonly_ml <- update(m_dayonly, REML = FALSE)
m_h1_ml <- update(m_h1, REML = FALSE)

cmp <- anova(m_dayonly_ml, m_h1_ml)  

tab3_df <- tibble(Model = c("Day only", "Day + media"),
  AIC = c(AIC(m_dayonly_ml), AIC(m_h1_ml)),
  BIC = c(BIC(m_dayonly_ml), BIC(m_h1_ml)),
  logLik  = c(as.numeric(logLik(m_dayonly_ml)), as.numeric(logLik(m_h1_ml))),
  `-2*log(L)`= -2 * c(as.numeric(logLik(m_dayonly_ml)), as.numeric(logLik(m_h1_ml))),
  `Chi-sq` = c(NA_real_, cmp$Chisq[2]),
  `p-value` = c(NA_real_, cmp$`Pr(>Chisq)`[2])) %>% 
  mutate(AIC = round(AIC), 
         BIC = round(BIC), 
         logLik  = round(logLik),
         `-2*log(L)`= round(`-2*log(L)`),
         `Chi-sq` = round(`Chi-sq`, 2),
         `p-value` = ifelse(is.na(`p-value`), NA_character_, 
                            ifelse(`p-value` < 0.001, "<0.001",
                                   sprintf("%.3f", `p-value`))))

tab3 <- tab3_df %>% gt() %>% 
  tab_header(title = md("Table 3: Likelihood-ratio test comparing random-intercept specifications for H1 (models refit with maximum likelihood)")) %>% 
  sub_missing(columns = c("Chi-sq", "p-value"), missing_text = "—") %>% cols_align(align = "center", columns = -Model)

gtsave(tab3, here("tables", "table_3.html"))




