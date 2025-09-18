# R script to replicate Figure 4 in Appendix D: Closeness of the media outlets by the share of politicized COVID-19 content in the 
# all COVID-related daily materials, text corpus with articles that contain covid-related keywords at least three times (biomedical disciplines)


pkgs <- c("data.table","tidyverse", "here", "performance", "proxy", "dtw") 
invisible(lapply(pkgs, require, character.only = TRUE))

load( here("data", "smaller_corpus", "corpus_covid_media_reduced.RData"))
load( here("data", "natural_science_polit.RData")) 
load(here("data", "social_science_polit.RData"))

# removing the overlaps
natural_science_polit <- natural_science_polit %>%
  filter(!id_article %in% soc_science_polit$id_article)

# need to identify starting and end points
relevant_dates_corpus <- smaller_corpus %>% filter(as.Date(Date) >= "2020-01-01" & as.Date(Date) <= "2021-12-31")

# first, Date must be a day only, without the exact time
relevant_dates_corpus$day <- as.Date(relevant_dates_corpus$Date)
n_distinct(relevant_dates_corpus$day) # 722 days


# now I can summarize the corpus by media and day
corpus_covid_media_daily <-  relevant_dates_corpus %>% 
  group_by(Media, day) %>% 
  summarise(n_articles = n()) %>% 
  ungroup() 

# Date must be a day only, without the exact time
natural_science_polit$day <- as.Date(natural_science_polit$Date)
# now I can summarize the corpus by media and day
natural_science_polit_daily <-  natural_science_polit %>% 
  group_by(Media, day) %>% 
  summarise(n_articles = n()) %>% 
  ungroup()

# next step is to merge the two dataframes by the largest one, because the corpus is larger than the politicized articles
# and if I have NA in the politicized corpus, I will replace it with 0
names(corpus_covid_media_daily)
names(natural_science_polit_daily)
corpus_combined <- corpus_covid_media_daily %>% 
  left_join(natural_science_polit_daily, by = c("Media", "day")) %>% 
  rename(n_articles_politicized = n_articles.y,
         n_articles_corpus = n_articles.x) 

table(is.na(corpus_combined$n_articles_politicized)) # 3645 of the corpus is NA, which is a lot!
# now I need to replace the NA in the politicized corpus with 0
corpus_combined$n_articles_politicized[is.na(corpus_combined$n_articles_politicized)] <- 0

# create shares
corpus_combined$share <- corpus_combined$n_articles_politicized / corpus_combined$n_articles_corpus
table(is.na(corpus_combined$share)) 
table(corpus_combined$Media) 


# all of them are different when it comes to the number of days

corpus_combined_wide <- corpus_combined %>% 
  select(Media, day, share) %>% 
  pivot_wider(names_from = Media, values_from = share) 

corpus_combined_wide$`ZDF Heute` <- NULL

corpus_combined_wide[is.na(corpus_combined_wide)] <- 0


proxy_test <- corpus_combined_wide %>% 
  select(-day) %>% 
  as.matrix()

proxy_test %>% glimpse()

dtw_distances <- proxy::dist(proxy_test, method = "dtw",  by_rows = FALSE)

hc <- hclust(dtw_distances, method = "average")

# dendrogram to visualize clustering of the newspapers
jpeg("figures/fig_4_app.jpeg", width = 9, height = 6, units = 'in', res = 500)

plot(hc, main = "Hierarchical clustering of the politicized science \nreporting trends about COVID (biomedical disciplines)",
     xlab = "Media outlets", sub = "", ylab = "DTW Distance")
dev.off()

