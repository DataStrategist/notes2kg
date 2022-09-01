### Knowledge map

library(tidyverse)
library(visNetwork)

## read data in
df_nodes <- readxl::read_excel("data/knowledge graph.xlsx", sheet = "nodes") %>% 
  mutate(label = node %>% stringr::str_wrap(width = 10), shape = "box") %>% rename(title = extra)

df_edges <- readxl::read_excel("data/knowledge graph.xlsx", sheet = "edges") %>% 
  mutate(label = what, group = what, arrows = "to",
         color = case_when(
           status == "present" ~ "black",
           status == "in progress" ~ "black",
           status == "to do" ~ "lightgreen",
           status == "should be" ~ "red"),
         dashes = case_when(
           status == "present" ~ FALSE,
           status == "in progress" ~ TRUE,
           status == "to do" ~ TRUE,
           status == "should be" ~ TRUE))

## add in node sizes, and bold the names
df_nodes <- df_nodes %>% 
  left_join(c(df_edges$entity1, df_edges$entity2) %>% table %>% 
              as_tibble %>% rename(node = 1, value = n)) %>% 
  mutate(node = paste0("<b>", node, "</b>"))

##plot
visNetwork(nodes = df_nodes, edges = df_edges) %>% 
  visEdges(font = list(color = "lightgrey")) %>% 
  visOptions(nodesIdSelection = TRUE)
