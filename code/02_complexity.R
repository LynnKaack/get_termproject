# Measuring policy complexity

# general setup ----

library(readtext) #reading text files
library(tidyverse) #data wragnling
library(quanteda) #text analysis
library(patchwork) #glue plots together
#library(ggforce)

pastelred <- "#ff6666"

theme_set(theme_minimal())
theme_update(text = element_text(family = "CMU Sans Serif Medium"),
             panel.grid.minor = element_blank(),
             plot.title = element_text(face = "bold", size = 15),
             panel.grid.major.x = element_blank(),
             panel.grid.major.y = element_line(linetype = "dashed"),
             axis.text = element_text(size = 20),
             axis.title = element_text(size = 22))

load("data/preprocessed.Rdata")

# Plot: Number of policies

glimpse(preprocessed)

preprocessed %>% 
  group_by(year) %>% 
  count() %>% 
  ggplot(aes(x = year, y = n)) +
  geom_col(width = 0.8) +
  labs(y = "Number of policies\n",
       x = "") 

ggsave("figures/npolicies.png", dpi = 400)

# Structural and relational policy complexity --------------------------------------------

# plot: more articles, more subitems?

entropy_df %>% 
  filter(articles_tot < 99, items_tot + subitems_tot < 1000) %>% 
  ggplot(aes(x = articles_tot, y = items_tot + subitems_tot)) +
  geom_smooth(method = "lm", color = pastelred, fill = pastelred) +
  geom_point() +
  labs(title = "More articles, more (sub)items?",
       subtitle = "Yes, the more article a policy contains, the more sub(items) there are",
       x = "\nNumber of articles in a policy",
       y = "Number of (sub)items\n")

# plot: more articles, more references?

entropy_df %>% 
  filter(articles_tot < 99) %>% 
  ggplot(aes(x = articles_tot, y = article_ref_tot)) +
  geom_point() +
  geom_smooth(fill = pastelred, color = pastelred) +
  labs(x = "\nNumber of a articles",
       y = "Number of references\n")

# plot: change in structural complexity (outliers excluded)

preprocessed %>% 
  filter(items_tot + subitems_tot < 1000) %>% 
  ggplot(aes(x = year, y = items_tot + subitems_tot)) +
  geom_smooth(method = "lm", color = pastelred, fill = pastelred) +
  geom_point() +
  labs(#title = "Structural policy complexity (outliers excluded)",
       #subtitle = "Number of items and subitems in a policy",
       y = "Number of items and subitems\n",
       x = "") 

ggsave("figures/evolution_structcomplex.png", dpi = 500)

# plot: change in relational complexity (outliers excluded)

preprocessed %>% 
  filter(article_ref_tot < 1000) %>% 
  ggplot(aes(x = year, y = article_ref_tot)) +
  geom_smooth(method = "lm", color = pastelred, fill = pastelred) +
  geom_point() +
  labs(#title = "Relational policy complexity (outliers excluded)",
       #subtitle = "Mention of another act as a whole or of individual parts as well as mentions of\nother articles of the proposal itself",
       y = "Number of articles referred to\n",
       x = "") +
  scale_y_continuous(breaks = c(0, 50, 250, 500, 750))

ggsave("figures/evolution_relcomplex.png", dpi = 500)

# calculating entropy -----------------------------------------------------

#the the the
-(3/3*log2(3/3)+3/3*log2(3/3)+3/3*log2(3/3))

#the cat the
-(2/3*log2(2/3)+2/3*log2(2/3)+1/3*log2(1/3))

#the cat is
-(1/3*log2(1/3)+1/3*log2(1/3)+1/3*log2(1/3))

#the cat is tiny
-(1/4*log2(1/4)+1/4*log2(1/4)+1/4*log2(1/4)+1/4*log2(1/4))

#create corpus
corpus <- corpus(preprocessed)
summary(corpus)

#prepare meta data
texts_meta <- preprocessed %>% 
  mutate(id_meta = paste0("text", row_number())) %>% 
  select(-text)

#dfm 
dfm <- dfm(corpus, 
           remove = stopwords("english"),
           stem = TRUE,
           remove_punct = TRUE,
           remove_symbols = TRUE,
           remove_numbers = TRUE)

#calculating entropy
entropy <- textstat_entropy(dfm)

#constructing dataset
entropy_df <- entropy %>% 
  left_join(texts_meta, by = c("document" = "id_meta"))

entropy_slim <- entropy_df %>% 
  as_tibble() %>% 
  select(document, entropy)

save(entropy, file = "data/entropy.RData")

# plotting entropy -----------------------------------------------------

# plot: higher entropy over the years?

entropy_df %>% 
  ggplot(aes(x = year, y = entropy)) +
  geom_smooth(method = "lm", color = pastelred, fill = pastelred) +
  geom_point() +
  ylim(0, 10) +
  labs(#title = "Higher linguistic complexity over the years?",
       y = "Policy's entropy\n",
       x = "")

ggsave("figures/evolution_entropy.png", dpi = 500)

# plot: higher structural complexity, higher entropy?

struc_entropy <- entropy_df %>% 
  filter(items_tot + subitems_tot < 500) %>% 
  ggplot(aes(x = items_tot + subitems_tot, y = entropy)) +
  geom_point() +
  geom_smooth(color = pastelred, fill = pastelred) +
  scale_y_continuous(breaks = seq(0,10,2), labels = seq(0,10,2), limits = c(0, 11)) +
  labs(#title = "Higher structural complexity, higher entropy?",
       #subtitle = "Yes, the more article a policy contains, the more sub(items) there are",
       x = "\nNumber of (sub)paragraphs in a policy",
       y = "Entropy\n")

ggsave("figures/structcomplex-entropy.png", dpi = 500)

# plot: more articles, higher entropy? (irrelevant for the time being)

entropy_df %>% 
  filter(articles_tot < 99) %>% 
  ggplot(aes(x = articles_tot, y = entropy)) +
  geom_point() +
  geom_smooth(color = pastelred, fill = pastelred, alpha = 0.5) +
  labs(title = "Are more articles associated with more uncertainty? (outliers excluded)",
       y = "Policy's entropy\n",
       x = "\nNumber of articles in the policy") +
  scale_y_continuous(breaks = seq(0,10,2), labels = seq(0,10,2), limits = c(0, 11))

# plot: higher relational complexity, higher entropy? Less than 500 article references

relat_entropy <- entropy_df %>% 
  filter(article_ref_tot < 500) %>% 
  ggplot(aes(x = article_ref_tot, y = entropy)) +
  geom_smooth(color = "#ff6666", fill = "#ff6666") +
  geom_point(alpha = 0.8) +
  labs(#title = "Higher relational complexity, higher entropy?",
       y = "",
       x = "\nArticles referred to in policy\n") +
  scale_y_continuous(breaks = seq(0,10,2), labels = seq(0,10,2), limits = c(0, 11))

ggsave("figures/relcomplex500-entropy.png", dpi = 500)

# plot: higher relational complexity, higher entropy? Less than 100 article references

entropy_df %>% 
  filter(article_ref_tot < 100) %>% 
  ggplot(aes(x = article_ref_tot, y = entropy)) +
  geom_smooth(color = "#ff6666", fill = "#ff6666") +
  geom_point(alpha = 0.8) +
  labs(#title = "Higher relational complexity, higher entropy?",
       y = "Entropy\n",
       x = "\nArticles referred to in policy\n") +
  scale_y_continuous(breaks = seq(0,10,2), labels = seq(0,10,2), limits = c(0, 11))

ggsave("figures/relcomplex100-entropy.png", dpi = 500)

# combinded plot

struc_entropy + relat_entropy

ggsave("figures/entropy_combined.png", dpi = 400)

# predicting number of references -----------------------------------------

reg_art_df <- entropy_df %>% 
  filter(articles_tot < 99,
         article_ref_tot < 500)

summary(lm(article_ref_tot ~ articles_tot, data = reg_art_df))

reg_art_df %>% 
  summarise(Mean = mean(article_ref_tot),
            Median = median(article_ref_tot))

quantile(reg_art_df$article_ref_tot, probs = seq(0, 1, 0.1))
quantile(reg_art_df$articles_tot, probs = seq(0, 1, 0.1))
