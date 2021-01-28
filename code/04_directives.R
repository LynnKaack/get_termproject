# 04: Comparing the 2009 and the 2018 renewable energy directive

library(readtext) #reading text files
library(tidyverse) #data wragnling
library(quanteda) #text analysis
library(stringr) # handling text 
library(SnowballC) # word stemming

pastelred <- "#ff6666"

theme_set(theme_minimal())
theme_update(text = element_text(family = "CMU Sans Serif Medium"),
             panel.grid.minor = element_blank(),
             plot.title = element_text(face = "bold", size = 15),
             panel.grid.major = element_blank(),
             axis.text = element_text(size = 20),
             strip.text = element_text(size = 20),
             axis.title = element_text(size = 22))


# loading all texts
dir_2009 <- readtext("data/EU_processed/EU_32009L0028/*.txt")
dir_2018 <- readtext("data/EU_processed/EU_32018L2001/*.txt")

directives_singlearticles <- bind_rows(dir_2009, dir_2018) %>% 
  mutate(front = str_detect(doc_id, "front"),
         whereas = str_detect(doc_id, pattern = "whereas"),
         directive = ifelse(str_detect(doc_id, "EU_32009L0028"), 2009, 2018),
         text = str_replace_all(text, "[\r\n]" , " ")) %>% 
  filter(front == FALSE, whereas == FALSE)
  
directives <- directives_singlearticles %>% 
  group_by(directive) %>% 
  summarise(text = paste(text, collapse = " "))

# creating corpus and dfm
corpus <- corpus(directives)
summary(corpus)

dfm <- dfm(corpus, 
           remove = stopwords("english"),
           remove_punct = TRUE,
           stem = TRUE,
           remove_symbols = TRUE,
           remove_numbers = TRUE,
           groups = "directive")

dfm

# Frequencies

frequencies <- textstat_frequency(dfm, groups = c(2009, 2018))


# Keyness plot ------------------------------------------------------------

tstat_key <- textstat_keyness(dfm)
textplot_keyness(tstat_key, n = 6)


# Simple frequencies ------------------------------------------------------

freq_dir <- as_tibble(textstat_frequency(dfm, groups = "directive"))

freq_dir %>% 
  group_by(group) %>% 
  filter(!feature %in% c("shall", "articl", "use")) %>% 
  top_n(30, wt = frequency) %>% 
  ggplot(aes(x = fct_reorder(feature, frequency), y = frequency)) +
  geom_col() +
  facet_wrap(~ group, scales = "free_y") +
  coord_flip() +
  theme(panel.grid.major.x = element_line())



# most common trigrams ----------------------------------------------------

library(tidytext)

# bigrams

bigrams_filtered <- directives %>%
  group_by(directive) %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  mutate(word1 = wordStem(word1),
         word2 = wordStem(word2)) %>% 
  filter(!word1 %in% stop_words$word,
         !word1 %in% c(1:100),
         !word2 %in% stop_words$word,
         !word2 %in% c(1:100)) %>% 
  count(word1, word2, sort = TRUE)

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ") 

bigrams_united %>% 
  top_n(10) %>% 
  ggplot(aes(y = reorder_within(bigram, n, directive), x = n)) +
  geom_vline(xintercept = 100, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = 200, linetype = "dashed", color = "grey") +
  geom_bar(stat = "identity", width = 0.8) +
  scale_y_reordered()  +
  facet_wrap(~ as.factor(directive), scales = "free_y") +
  labs(#title = "Most common bigrams",
       x = "\nOccurrences",
       y = "")

ggsave("figures/topbigrams.png", dpi = 400)

trigrams_filtered <- directives %>%
  group_by(directive) %>% 
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  mutate(word1 = wordStem(word1),
         word2 = wordStem(word2),
         word3 = wordStem(word3)) %>% 
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word,
         !word1 %in% c(1:100),
         !word2 %in% c(1:100),
         !word3 %in% c(1:100)) %>% 
  count(word1, word2, word3, sort = TRUE)

trigrams_united <- trigrams_filtered %>%
  mutate(word1 = ifelse(word1 == "emissions", "emission", word1),
         word2 = ifelse(word2 == "emissions", "emission", word2),
         word3 = ifelse(word3 == "emissions", "emission", word3)) %>% 
  unite(trigram, word1, word2, word3, sep = " ")

trigrams_united %>% 
  top_n(8) %>% 
  ungroup() %>% 
  ggplot(aes(y = reorder_within(trigram,n, directive),
             x = n)) +
  geom_vline(xintercept = 50, linetype = "dashed", color = "grey") +
  geom_vline(xintercept = 100, linetype = "dashed", color = "grey") +
  geom_col() +
  scale_y_reordered() +
  xlim(0, 100) +
  facet_wrap(~ directive, scales = "free") +
  labs(y = "",
       x = "\nOccurrences") 

ggsave("figures/toptrigrams.png", dpi = 400)

#fourgram

fourgras_filtered <- directives %>%
  group_by(directive) %>% 
  unnest_tokens(fourgram, text, token = "ngrams", n = 4) %>%
  separate(fourgram, c("word1", "word2", "word3", "word4"), sep = " ") %>%
  mutate(word1 = wordStem(word1),
         word2 = wordStem(word2),
         word3 = wordStem(word3),
         word4 = wordStem(word4)) %>% 
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word,
         !word4 %in% stop_words$word) %>%
  count(word1, word2, word3, word4, sort = TRUE)

fourgrams_united <- fourgras_filtered %>%
  unite(fourgram, word1, word2, word3, word4, sep = " ")

fourgrams_united %>% 
  top_n(4) %>% 
  ggplot(aes(x = fct_reorder(fourgram, n), y = n)) +
  geom_col(width = 0.8) +
  ylim(0,80) +
  coord_flip() +
  facet_wrap(~ as.factor(directive), scales = "free")