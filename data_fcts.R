#*******************************
#* data transformation functions
#* 
#* MM, 17.10.2022
#*******************************

#model <- readRDS("C:/Users/nidal/OneDrive/Documents/Dashboard/Dashboard/data/out_40.RDS")

load_data <- function(model_file="model_40.RDS",out_file="out_40.RDS"){
  
  data_folder <- "C:/Users/nidal/OneDrive/Documents/Dashboard/Dashboard/data/"
  
  # load data
  f <- paste0(data_folder,model_file)
  model <- readRDS(f)
  f <- paste0(data_folder,out_file)
  out <- readRDS(f)
  #C:\Users\nidal\OneDrive\Documents\Dashboard\Dashboard\data\CountriesStatesCities
  # filter location names from keywords
  countries <- read.csv(paste0(data_folder,"CountriesStatesCities/countries.csv"))
  states <- read.csv(paste0(data_folder,"CountriesStatesCities/states.csv"))
  cities <- read.csv(paste0(data_folder,"CountriesStatesCities/cities.csv")) %>%
    filter(!tolower(name) %in% c("basalt","boron","caldera","cool","radon","reservoir","volcano","well",
                                 "bath","clay","deposit","experiment","hazard","log","price",
                                 "prospect","radon","research","spring","thermal","train"))
  
  stem_dict <- read.csv(paste0(data_folder,"stem_dict.csv"))
  
  keywords <- read.csv(paste0(data_folder,"keywords.csv")) %>% 
    left_join(stem_dict,by=c("keyword"="word_stem")) %>%
    filter(n > SETTINGS$keyword_occ &
             (is.na(word) | (!word %in% tolower(countries$name) &
                               !word %in% tolower(states$name) &
                               !word %in% tolower(cities$name)))
             ) %>%
    pull(keyword)
  
  
  # remove supplementary datasets
  rm(countries, states, cities)
  
  # get data ready
  topic_term <- tidytext::tidy(model, matrix = "beta")
  
  # term topic attribution for both layers (where max)
  stop_terms <- c("invent_relat","present_invent","independ_claim","present_invent_relat",
                  "year_ago","data_obtain","data_show","western_part")
  
  term_topic_attribution <- topic_term %>%
    filter(str_detect(term, "_") | term %in% keywords  | SETTINGS$all_terms) %>%
    filter(!term %in% stop_terms) %>%
    group_by(term) %>% mutate(beta = beta / sum(beta)) %>%
    filter(beta >= SETTINGS$beta_min_term) %>% slice_max(beta) %>%
    select(-y.level) %>%
    filter(!duplicated(term))
  
  
  document_term <- stm:::doc.to.ijv(out$documents) %>%
    tidytext:::tidy.simple_triplet_matrix(row_names = out$meta$id, col_names = out$vocab) %>%
    rename(document = row, term = column, n = value)
  
  df <- document_term %>%
    left_join(out$meta, by = c("document" = "id")) %>%
    inner_join(term_topic_attribution, by=c("term")) # filters out non-matching terms
  
  return(df)
}


get_comp_hist <- function(df, SETTINGS){
  
  comp_hist <- df %>% distinct(document, type, year, topic, term) %>%
    count(type, year, topic, term) %>%
    group_by(type, topic, term) %>%
    complete(year = 1975:2018, fill = list(n = 0)) %>%
    mutate(n_cum = with_order(year, cumsum, n),
           comp_inno = +(n_cum > 0 & lag(n_cum, order_by = year) == 0 & 
                           lead(n_cum, SETTINGS$comp_inno_leadspan, order_by = year) >=
                           SETTINGS$comp_inno_leadsize & 
                           year > SETTINGS$ref_period)) %>%
    ungroup() %>%
    group_by(term,year) %>% 
    mutate(n_any = sum(n),
           n_cum_any = sum(n_cum)) %>%
    left_join(df %>% distinct(term, year) %>% arrange(year) %>% distinct(term,.keep_all=TRUE) %>%
                rename(first_year_mentioned = year),
              by="term"
    ) %>%
    ungroup()
  
  return(comp_hist)
}


get_recomb_hist <- function(df, comp_hist=NULL, SETTINGS){
  
  if(is.null(comp_hist)){
    comp_hist <- get_comp_hist(df,SETTINGS)
  }
  
  # get basic statistics on terms
  comps <- comp_hist %>%
    select(type,term,year,topic,n,n_cum) %>%
    group_by(type,year) %>%
    mutate(p_term = n / sum(n)) %>%
    ungroup()
  
  
  recomb_hist <- df %>% 
    # get all potential (type, term.x, term.y, year) combinations
    select(document, term) %>%
    left_join(df %>% select(document, type, year, term), 
              by = c("document")) %>%
    filter(term.x != term.y) %>%
    count(type, year, term.x, term.y) %>%
    complete(type, nesting(term.x,term.y), year = 1975:2018, fill = list(n = 0)) %>%
    # join indiv. term stats    
    left_join(comps %>% 
                select(type,term,year,topic,n_cum,p_term) %>%
                rename(term.x=term,topic.x=topic,n_cum.x=n_cum,p_term.x=p_term),
              by=c("type","term.x","year")) %>%
    left_join(comps %>% 
                select(type,term,year,topic,n_cum,p_term) %>%
                rename(term.y=term,topic.y=topic,n_cum.y=n_cum,p_term.y=p_term),
              by=c("type","term.y","year")) %>%
    # get (term.x,term.y) (excess) probabilities
    group_by(type,year) %>%
    mutate(p_term.xy = n / sum(n),
           z_dep = p_term.xy / (p_term.x*p_term.y)) %>%
    # identify pot innovation as first observation 
    ungroup() %>%
    group_by(type,term.x,term.y) %>%
    mutate(n_cum = with_order(year, cumsum, n),
           n_cum_max = max(n_cum),
           pot_inno.xy = +(n_cum > 0 & lag(n_cum, order_by = year) == 0 &
                                  year > SETTINGS$ref_period &
                                  n_cum.x>1 & n_cum.y>1),
           avg_p_term.xy = ifelse(p_term.xy>0,p_term.xy,NA),
           avg_p_term.xy = mean(avg_p_term.xy,na.rm=TRUE),
           avg_z_dep = ifelse(!is.nan(z_dep),z_dep,NA),
           avg_z_dep = mean(avg_z_dep,na.rm=TRUE))
  
  return(recomb_hist)
}


get_arch_hist <- function(df,SETTINGS){
  
  arch_hist <- df %>% select(document, topic) %>%
    left_join(df %>% select(document, type, year, topic), 
              by = c("document")) %>%
    distinct(document, type, year, topic.x, topic.y) %>%
    count(type, year, topic.x, topic.y) %>%
    complete(type, year = 1975:2018, topic.x = 1:40, topic.y = 1:40, fill = list(n = 0)) %>%
    #filter(topic.x != topic.y) %>%
    group_by(type, topic.x, topic.y) %>%
    mutate(n_cum = with_order(year, cumsum, n),
           arch_inno = +(n_cum > 0 & lag(n_cum, order_by = year) == 0 & 
                           year > SETTINGS$ref_period &
                           max(n_cum) > SETTINGS$arch_inno_leadsize )) %>%
    # +(n_cum > 0 & lag(n_cum, order_by = year) == 0 & 
    #               lead(n_cum, SETTINGS$arch_inno_leadspan, order_by = year) >= 
    #               SETTINGS$arch_inno_leadsize & 
    #               year > SETTINGS$ref_period)) %>%
    ungroup() %>%
    group_by(topic.x,topic.y,year) %>% 
    mutate(n_any = sum(n),
           n_cum_any = sum(n_cum)) %>%
    ungroup() %>%
    left_join(y= (eval(.) %>% 
                    filter(n > 0) %>%
                    select(topic.x,topic.y,year) %>% 
                    group_by(topic.x,topic.y) %>%
                    summarise(first_year_mentioned = first(year, order_by=year),.groups="drop")),
              by=c("topic.x","topic.y"))
  
  return(arch_hist)
}

