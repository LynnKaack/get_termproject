# 00: Preprocessing

library(readtext) #reading text files
library(tidyverse) #data wragnling
library(quanteda) #text analysis

# loading all texts
eu_texts <- readtext("data/EU_processed/*/*.txt")

# preprocessing
texts <- eu_texts %>% 
  mutate_all(.funs = tolower) %>% 
  mutate(doc_name = str_sub(doc_id, 4, 13),
         whereas = str_detect(doc_id, pattern = "whereas"),
         front = str_detect(doc_id, pattern = "front"),
         generaldates = str_extract(text, pattern = "\\s[0-9]{1,2}\\s[A-z]{3,10}\\s[0-9]{4}"),
         date = ifelse(front == TRUE & !is.na(generaldates), generaldates, NA),
         year = as.numeric(str_sub(date, start= -4)),
         items = str_count(text, pattern = "\\s[0-9]\\.\\s"),
         subitems = str_count(text, pattern = "\\([a-z]\\)"),
         article_ref = str_count(text, pattern = "article")-1,
         energy_count = str_count(text, pattern = "energy"),
         renewable_count = str_count(text, pattern = "renewable"),
         re_count = str_count(text, pattern = "renewable\\senerg"),
         article_num = 1) %>% 
  group_by(doc_name) %>% 
  fill(c(date, year), .direction = "updown") %>% 
  filter(whereas == FALSE, front == FALSE) %>% 
  group_by(doc_name, date, year) %>% 
  summarise(text = paste(text, collapse = " "),
            articles_tot = sum(article_num),
            article_ref_tot = sum(article_ref),
            items_tot = sum(items),
            subitems_tot = sum(subitems),
            energy_tot = sum(energy_count),
            renewable_tot = sum(renewable_count),
            re_tot = sum(re_count)) %>% 
  filter(energy_tot > 0 | renewable_tot > 0)

# irrelevant documents (checked manually)

irrelevant_docs <- c("32017d0864", "32009l0003", "32012d0243", "32014r1141",
                     "32014l0089", "32019l0883", "32013l0035", "32011r1169",
                     "32008l0056", "32017r0352", "32019r2152", "32019r0452")

preprocessed <- texts %>% 
  filter(!doc_name %in% irrelevant_docs) %>% 
  group_by(doc_name) %>% 
  mutate(id = paste0("id-",cur_group_id())) %>% 
  relocate(id) %>%  
  ungroup() %>% 
  mutate(afterparis = ifelse(year > 2015, 1, 0))


save(preprocessed, file = "data/preprocessed.Rdata")


# Fronts only -------------------------------------------------------------

# preprocessing
fronts <- eu_texts %>% 
  mutate_all(.funs = tolower) %>% 
  mutate(doc_name = str_sub(doc_id, 4, 13),
         whereas = str_detect(doc_id, pattern = "whereas"),
         front = str_detect(doc_id, pattern = "front"),
         generaldates = str_extract(text, pattern = "\\s[0-9]{1,2}\\s[A-z]{3,10}\\s[0-9]{4}"),
         date = ifelse(front == TRUE & !is.na(generaldates), generaldates, NA),
         year = as.numeric(str_sub(date, start= -4)),
         items = str_count(text, pattern = "\\s[0-9]\\.\\s"),
         subitems = str_count(text, pattern = "\\([a-z]\\)"),
         article_ref = str_count(text, pattern = "article")-1,
         energy_count = str_count(text, pattern = "energy"),
         renewable_count = str_count(text, pattern = "renewable"),
         re_count = str_count(text, pattern = "renewable\\senerg"),
         article_num = 1) %>% 
  group_by(doc_name) %>% 
  fill(c(date, year), .direction = "updown") %>% 
  filter(front == TRUE)
