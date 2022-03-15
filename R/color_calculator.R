# Calcul par Unité --------------------------------------------------------


data_specific_color <- data_filtre %>% 
  select(UA, all_of(vars_to_keep)) %>% 
  tidyr::pivot_longer(all_of(vars_to_keep),
                      names_to = "question",
                      values_to = "value") %>% 
  
  left_join(doc_names %>% dplyr::filter(Var == doc_filtres$UA[j]), by = c("UA" = "Value")) %>% 
  
  na.omit() %>% 
  group_by(Label, question) %>% 
  summarise(moy = mean(value), .groups = "drop", 
            n  = n()) %>%
  dplyr::filter(n > 5) %>% 
  select(-n) %>% 
  mutate(moy = case_when(
    
    moy < 5 ~ "#CE4760",
    moy < 6.5 ~ "#F4A261",
    moy < 8 ~ "#90E39A",
    TRUE ~ "#638475")) %>% 
  tidyr::pivot_wider(names_from = Label, values_from = moy)



# Calcul global -----------------------------------------------------------



data_global_global <- data_filtre %>% 
  mutate(GLOBAL = "Global") %>% 
  select(GLOBAL, all_of(vars_to_keep)) %>% 
  tidyr::pivot_longer(all_of(vars_to_keep),
                      names_to = "question",
                      values_to = "value") %>% 
  na.omit() %>% 
  group_by(GLOBAL, question) %>% 
  summarise(moy = mean(value), .groups = "drop",
            n = n()) %>% 
  dplyr::filter(n > 5) %>% 
  select(-n) %>% 
  mutate(moy = case_when(
    
    moy < 5 ~ "#CE4760",
    moy < 6.5 ~ "#F4A261",
    moy < 8 ~ "#90E39A",
    TRUE ~ "#638475")) %>% 
  tidyr::pivot_wider(names_from = GLOBAL, values_from = moy) 

#Fix pour les cas où on est incapable d'avoir des moyennes (n trop bas)

if(nrow(data_specific_color) == 0 & nrow(data_global_global) == 0){
  
  
  colnames_fake <- data_filtre %>% 
    count(UA) %>% 
    filter(n > 5) %>% 
    left_join(doc_names %>% dplyr::filter(Var == doc_filtres$UA[j]), by = c("UA" = "Value")) %>% 
    pull("Label")
  
  data_specific_color <- tibble(question = "Q9999")
  
  data_specific_color[[colnames_fake]] <- NA

  
  data_global_global <- tibble(question = "Q9999",
                        Global = NA)
  
  
}


data_colors  <- questions_to_keep %>%
  select(-starts_with("Ordre")) %>% 
  left_join(data_specific_color, by = c("Var" = "question")) %>% 
  left_join(data_global_global, by = c("Var" = "question")) %>% 
  ungroup() %>% 
  select(-Theme, -Var) %>% 
  select(` ` = Label,
         everything())


data_colors[is.na(data_colors)] <- "#000000"
