# Calcul par Unité --------------------------------------------------------


data_specific_color_at <- data_filtre %>% 
  select(UA, all_of(doc_themes$Var)) %>% 
  tidyr::pivot_longer(all_of(doc_themes$Var),
                      names_to = "question",
                      values_to = "value") %>% 
  left_join(doc_themes %>% select(-Label), by = c("question" = "Var")) %>% 
  left_join(doc_names, by = c("UA" = "Value")) %>% 
  
  na.omit() %>% 
  group_by(Label, Theme, Ordre) %>% 
  summarise(moy = mean(value), .groups = "drop") %>% 
  mutate(moy = case_when(
    
    moy < 5 ~ "#CE4760",
    moy < 6.5 ~ "#F4A261",
    moy < 8 ~ "#90E39A",
    TRUE ~ "#638475")) %>% 
  tidyr::pivot_wider(names_from = Label, values_from = moy) %>% 
  arrange(Ordre) %>% 
  select(-Ordre)


# Calcul global -----------------------------------------------------------



data_colors_global_at <- data_filtre %>% 
  mutate(GLOBAL = "Global") %>% 
  select(GLOBAL, all_of(doc_themes$Var)) %>% 
  tidyr::pivot_longer(all_of(doc_themes$Var),
                      names_to = "question",
                      values_to = "value") %>% 
  left_join(doc_themes %>% select(-Label), by = c("question" = "Var")) %>% 
  
  na.omit() %>% 
  group_by(GLOBAL, Theme, Ordre) %>% 
  summarise(moy = mean(value), .groups = "drop") %>% 
  mutate(moy = case_when(
    
    moy < 5 ~ "#CE4760",
    moy < 6.5 ~ "#F4A261",
    moy < 8 ~ "#90E39A",
    TRUE ~ "#638475")) %>% 
  tidyr::pivot_wider(names_from = GLOBAL, values_from = moy) %>% 
  arrange(Ordre) %>% 
  select(-Ordre)


data_colors_at  <- data_specific_color_at %>% 
  left_join(data_colors_global_at, by = "Theme") %>% 
  ungroup()


