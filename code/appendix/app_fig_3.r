# R script to replicate Figure 3 in Appendix D: Distribution of words in the science politicized articles (non-biomedical disciplines)

pkgs <- c("data.table","tidyverse", "here", "performance", "proxy", "dtw") 
invisible(lapply(pkgs, require, character.only = TRUE))

load(here("data", "corpus_covid_media.RData"))
load( here("data", "natural_science_polit.RData")) 
load(here("data", "social_science_polit.RData"))

# remove the messages that are in both datasets

soc_science_polit <- soc_science_polit %>% filter(!id_article %in% natural_science_polit$id_article)

# need to identify starting and end points
relevant_dates_corpus <- corpus_covid_media %>% filter(as.Date(Date) >= "2020-01-01" & as.Date(Date) <= "2021-12-31")

# first, Date must be a day only, without the exact time
relevant_dates_corpus$day <- as.Date(relevant_dates_corpus$Date)
# n_distinct(relevant_dates_corpus$day) # 722 days

# now I can summarize the corpus by media and day
corpus_covid_media_daily <-  relevant_dates_corpus %>% group_by(Media, day) %>% summarise(n_articles = n()) %>% 
  ungroup() 

soc_science_polit$day <- as.Date(soc_science_polit$Date)
# now I can summarize the corpus by media and day
soc_science_polit_daily <-  soc_science_polit %>% group_by(Media, day) %>% 
  summarise(n_articles = n()) %>% ungroup()

# next step is to merge the two dataframes by the largest one, because the corpus is larger than the politicized articles
# and if I have NA in the politicized corpus, I will replace it with 0
corpus_combined <- corpus_covid_media_daily %>% left_join(soc_science_polit_daily, by = c("Media", "day")) %>% 
  rename(n_articles_politicized = n_articles.y, n_articles_corpus = n_articles.x) 

corpus_combined$n_articles_politicized[is.na(corpus_combined$n_articles_politicized)] <- 0

# create shares
corpus_combined$share <- corpus_combined$n_articles_politicized / corpus_combined$n_articles_corpus
table(is.na(corpus_combined$share)) 
table(corpus_combined$Media) 


corpus_combined_wide <- corpus_combined %>% select(Media, day, share) %>% pivot_wider(names_from = Media, values_from = share) 
corpus_combined_wide$`ZDF Heute` <- NULL
corpus_combined_wide[is.na(corpus_combined_wide)] <- 0


proxy_test <- corpus_combined_wide %>% select(-day) %>% as.matrix()
dtw_distances <- proxy::dist(proxy_test, method = "dtw",  by_rows = FALSE)
hc <- hclust(dtw_distances, method = "average")

# dendrogram to visualize clustering of the media outlets
jpeg("figures/fig_3_app.jpeg", width = 9, height = 6, units = 'in', res = 500)
plot(hc, main = "Hierarchical clustering of the science politicization \nreporting trends about COVID-19 (non-biomedical disciplines)",
     xlab = "Media outlets", sub = "", ylab = "DTW Distance")
dev.off()





