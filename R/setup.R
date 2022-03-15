data_saaq <- readxl::read_excel("data/SAAQCASAV2NBKCOMM_complete.xlsx")
doc_themes <- readxl::read_excel("data/doc_themes.xlsx")
doc_names <- readxl::read_excel("data/doc_names.xlsx")
doc_filtres <- readxl::read_excel("data/doc_filtres.xlsx")


data_saaq[data_saaq == 99] <- NA
data_saaq <- data_saaq %>% 
  mutate(GLOB = 1) %>% 
  mutate(VP = case_when(
    
    is.na(VP) ~ 1001,
    TRUE ~ VP
  ))

themes_uniques <- unique(doc_themes$Theme)

source("R/tr_calculator.R", encoding = "UTF-8")

for(j in 1:nrow(doc_filtres)){
  
  
  data_filtre <- data_saaq %>% 
    dplyr::filter(get(doc_filtres$Var[j]) == doc_filtres$Value[j]) %>% 
    mutate(UA = get(doc_filtres$UA[j]))
  
  if(nrow(data_filtre) > 5){
  source("R/fiches_generator.R", encoding = "UTF-8")
  }
  
  
}
