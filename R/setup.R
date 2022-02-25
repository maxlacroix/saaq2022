data_saaq <- readxl::read_excel("data/SAAQCASAV2NBKCOMM.xlsx")
doc_themes <- readxl::read_excel("data/doc_themes.xlsx")
doc_names <- readxl::read_excel("data/doc_names.xlsx")
doc_filtres <- readxl::read_excel("data/doc_filtres.xlsx")


data_saaq[data_saaq == 99] <- NA
data_saaq <- data_saaq %>% 
  mutate(GLOB = 1) %>% 
  mutate(VP = case_when(
    
    is.na(VP) ~ 9001,
    TRUE ~ VP
  ))

themes_uniques <- unique(doc_themes$Theme)

for(j in 1:nrow(doc_filtres)){
  
  
  data_filtre <- data_saaq %>% 
    dplyr::filter(get(doc_filtres$Var[j]) == doc_filtres$Value[j]) %>% 
    mutate(UA = get(doc_filtres$UA[j]))
  
  
  source("R/fiches_generator.R", encoding = "UTF-8")
  
  
  
}