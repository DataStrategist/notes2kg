### Devise a POS tag parser

library(tidyverse)
library(udpipe)

## read data in
text <- read_lines("data/john.txt")

## read in the model. To see a comparison of english models, https://universaldependencies.org/treebanks/en-comparison.html
model <- udpipe_download_model(language = "english-ewt")

## load model, parse text into individual sentances, and create the POS df
loaded_model <- udpipe_load_model(model)

text <- text %>% 
  str_split("\\.|\\?|\\!|\\;") %>% unlist %>% 
  discard(~. == "")

df <- text %>% udpipe::udpipe(x = ., object = loaded_model) %>% 
  select(doc_id, sentence, token_id, lemma, upos, xpos, head = head_token_id)

# ## first version: just keep NN and VBN
# df %>% filter(xpos == "NN" | xpos == "VBN" | xpos == "VBZ") %>% View
# 
# ## This leaves out a lot of key info... it seems we need to keep JJ (adjectives) and NNP, and stick those to their nouns
# nouns <- df %>% filter(xpos == "NN" | xpos == "JJ" | xpos == "NNP" | xpos == "NNS") %>% 
#   select(doc_id, id = token_id, lemma, xpos, head = head_token_id) %>% 
#   mutate(id = as.numeric(id), head = as.numeric(head))
# 
# nouns %>% unite(id, doc_id, head) %>% select(id, adj = lemma) %>% unique %>% 
#   left_join(nouns %>% unite(id, doc_id, id) %>% select(id, main = lemma, xpos) %>% unique,
#             by = "id") %>% filter(!is.na(main))

## for each sentence, find the root, and everything that references it
df <- df %>% group_split(doc_id)

df %>% map(~mutate(., root = filter(head == 0) %>% pull(token_id)))



# NER ---------------------------------------------------------------------

library("spacyr")
# spacy_install()
spacy_initialize(condaenv = "myenv", 
                 model = "en_core_web_sm")

library(entity)
library(tidyverse)

## read data in

text <- read_lines("data/john.txt")
text <- text %>% 
  str_split("\\.|\\?|\\!|\\;") %>% unlist %>% 
  discard(~. == "")
Sys.setenv(JAVA_HOME="C://Program Files//Java//jdk-14.0.2")
per <- person_entity(text)
loc <- location_entity(text)%>% as.list()
tibble(per = per, loc = loc) %>% unnest(per)
