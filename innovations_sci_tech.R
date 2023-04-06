# short script
library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(purrr)
library(plotly)
library(DT)
library(ggtext)
library(shinycssloaders) 

setwd("C:/Users/nidal/OneDrive/Documents/Dash")

source("data_fcts.R")
model_file = "data/model_40.RDS"
out_file = "data/out_40.RDS"
SETTINGS <- list(
  model_file = "data/model_40.RDS",
  out_file = "data/out_40.RDS",
  # selection of ideas/concepts, if FALSE only bi-/tri-/n-grams
  all_terms = TRUE,
  # keywords from papers included if occurence in dataset greater
  keyword_occ = 20,
  # probability threshold for assigning a term to a topic, 
  #   all terms below kicked out from analysis 
  beta_min_term = 0.5,
  # innovation definitions
  #   leadspan: time to judge on innovation, 
  #   leadsize: number of applications within leadspan
  arch_inno_leadspan = 5, # in the next x year
  arch_inno_leadsize = 5, # used at least x times
  comp_inno_leadspan = 5,
  comp_inno_leadsize = 5,
  #recomb_inno_leadspan = 5,
  recomb_inno_leadsize = 10,
  # reference period to accumulate state-of-the-art, starts with first obs. year
  ref_period = 1995
)



# laod data
df <- load_data()




# one observation is a (document, term) combination
# a document is a paper or patent, a term is a token or an n-gram (n=2,3)
# n is the number of times the term appears in the document title or abstract
# year the patent is applied for or paper is published,
# country is where patent is applied 
# type - patent or paper
# authors/keywords/conference - only for papers
# topic - is the topic with which the term is most likely associated - taking into account science and technology layer
# beta is the probability of the term being associated with that topic.

# NOTE: We call a `term' also a `component' (or a concept or an idea).
#       We call a `type` also patent/paper or layer.

comp_hist <- get_comp_hist(df, SETTINGS)
# one observation is (type,term,year) - yielding some information on a term in a layer (patent/paper) in a year
# the topic is strictly associated with the topic (as in df above)
# n - number of uses in that (type,year)
# n_cum - cummulated number of uses in that (type,year)
# comp_inno - whether it is an innovation (1) or not (0) given the definition in the settings
# n_any - number of uses in that (year) but any layer
# n_cum_any - number of cummulated uses up to that (year) but any layer
# first_year_mentioned - when the term is first mentioned in any layer
data_for_chart <- comp_hist %>%
  filter(comp_inno == 1) %>% 
  select(year, first_year_mentioned, type,term)

df4 <- data_for_chart %>% 
  group_by(first_year_mentioned, type) %>%
  summarize(count = n()) 

df5 <- data_for_chart %>% 
  group_by(year, type) %>% 
  summarize(count1 = n()) 

g_combined = combine(gg1,gg2)

g1 <- graph.ring(10)
g2 <- graph.ring(8)

# Combine the two graphs into a single graph
g_combined <- combine(g1, g2)

# Add edges between nodes in the two networks
g_combined <- add_edges(g_combined, c(1,11), c(2,12), c(3,13))

# Plot the combined graph with different colors for nodes in each network
plot(g_combined, 
     vertex.color=ifelse(V(g_combined)$name < 11, "green", "red"),
     vertex.size=20,
     edge.width=2)
