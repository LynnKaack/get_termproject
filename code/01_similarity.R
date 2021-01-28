# 01: Cosine Similarity

library(readtext) #reading text files
library(tidyverse) #data wragnling
library(quanteda) #text analysis
library(patchwork) #glue plots together
library(ggrepel) # making labelling easier

load("data/preprocessed.Rdata")

#containing at least once "renewable energy"
#preprocessed <- preprocessed %>% 
 # filter(re_tot > 0)

# Setup -------------------------------------------------------------------

pastelred <- "#ff6666"

theme_set(theme_minimal())
theme_update(text = element_text(family = "CMU Sans Serif Medium"),
             panel.grid.minor = element_blank(),
             plot.title = element_text(face = "bold", size = 15),
             panel.grid.major.x = element_blank(),
             axis.text = element_text(size = 20),
             axis.title = element_text(size = 22))

# Create corpus -----------------------------------------------------------

corpus <- corpus(preprocessed)
#summary(corpus)

# Texts Metadata ----------------------------------------------------------

texts_meta <- preprocessed %>% 
  mutate(id_meta = paste0("text", row_number())) %>% 
select(-text)

# Creating dfm and weigthing ---------------------------------------------------------------

dfm <- dfm(corpus, 
           remove = stopwords("english"),
           stem = TRUE,
           remove_punct = TRUE,
           remove_symbols = TRUE,
           remove_numbers = TRUE)

freqs <- as_tibble(textstat_frequency(dfm))

docfreq <- freqs %>% 
  arrange(desc(docfreq))

# Cosine Similarity (with tfidf weighting) -------------------------------------------------------

weight_tdfidf <- dfm_tfidf(dfm)

cos_sim_tfidf <- textstat_simil(weight_tdfidf, margin = "documents", method = "cosine")
cos_sim_tfidf <- as.matrix(cos_sim_tfidf)

cos_sim_tfidf_df <- as.data.frame(as.table(cos_sim_tfidf)) %>% 
  arrange(Var1) %>% 
  mutate(across(where(is.factor), as.character)) %>% 
  rename("cosine_sim" = Freq) %>% 
  dplyr::filter(cosine_sim < 0.99) %>% 
  group_by(Var1) %>% 
  slice(which.max(cosine_sim))

cos_sim_tfidf_df %>% 
    arrange(desc(cosine_sim))

max(cos_sim_tfidf_df$cosine_sim)

# Adding meta data
merge_tfidf <- cos_sim_tfidf_df %>% 
  left_join(texts_meta, by = c("Var1" = "id_meta"))

# Cosine similarity over the years
ggplot(merge_tfidf, aes(x = year, y = cosine_sim)) +
  geom_smooth(color = "black", alpha = 0.5) +
  geom_point(size = 3, alpha = 0.7, aes(color = as.factor(afterparis))) +
  labs(#title = "Cosine similarity over the years",
       #subtitle = "TF-IDF weighting applied",
       x = "",
       y = "Maximum cosine similarity (MaxSim)\n") +
  ylim(0,1) +
  scale_color_manual(values = c("black", pastelred)) +
  theme(legend.position = "none", 
        panel.grid.major.y = element_line(linetype = "dashed")) 

ggsave("figures/cossim_evolution_tfidf.png", dpi = 300)

similarity_df_tfidf <- merge_tfidf %>% 
  rename(document = Var1, paired_document = Var2) %>% 
  select(-c(id))

sd(similarity_df_tfidf$cosine_sim)

n_distinct(similarity_df_tfidf$document)
n_distinct(similarity_df_tfidf$paired_document)

similarity_df_tfidf %>% 
  group_by(paired_document) %>% 
  count(sort = T)

save(similarity_df_tfidf, file = "data/similarity_tfidf.RData")

# Cosine Similarity (with 99% weighting) ----------------------------------

weight_99 <- dfm_trim(dfm, max_termfreq = 0.995, termfreq_type = "quantile", min_docfreq = 0.1, docfreq_type = "prop")

textstat_frequency(weight_99)

cos_sim_99 <- textstat_simil(weight_99, margin = "documents", method = "cosine")
cos_sim_99 <- as.matrix(cos_sim_99)

cos_sim_99_df <- as.data.frame(as.table(cos_sim_99)) %>% 
  arrange(Var1) %>% 
  rename("cosine_sim" = Freq) %>% 
  filter(cosine_sim < 1) %>% 
  group_by(Var1) %>% 
  slice(which.max(cosine_sim))

# Adding meta data
merge_99 <- cos_sim_99_df %>% 
  left_join(texts_meta, by = c("Var1" = "id_meta"))

similarity_df <- merge_99 %>% 
  rename(document = Var1, paired_document = Var2) %>% 
  select(-c(id, doc_name))

save(similarity_df, file = "data/similarity.RData")


# Cosine similarity over the years
ggplot(similarity_df, aes(x = year, y = cosine_sim, color = as.factor(afterparis))) +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_manual(values = c("#CEB992", "#73937E")) +
  scale_y_continuous(labels = c(0, 0.25, 0.5, 0.75, 1), breaks = c(0, 0.25, 0.5, 0.75, 1), limits = c(0,1)) +
  labs(#title = "Cosine similarity over the years",
    #subtitle = "99% weighting scheme applied",
    x = "",
    y = "Max. Cosine Similarity\n") +
  #geom_label_repel(data = subset(merge_99, Var1 == "text140"), aes(label = "Information exchange\n on energy"), size = 3, 
  #                 position = position_stack(vjust = 0.5), box.padding = 0.0, point.padding = 0.0) +
  theme(legend.position = "none",
        panel.grid.major.y = element_line(linetype = "dashed"))

ggsave("figures/evoluton_cosinesim.png", dpi = 400)

