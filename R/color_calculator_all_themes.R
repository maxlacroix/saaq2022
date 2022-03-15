# Calcul par Unité --------------------------------------------------------


data_specific_color_at <- data_filtre %>% 
  select(UA, all_of(doc_themes$Var), CLE) %>% 
  tidyr::pivot_longer(all_of(doc_themes$Var),
                      names_to = "question",
                      values_to = "value") %>% 
  left_join(doc_themes %>% select(-Label), by = c("question" = "Var")) %>% 
  left_join(doc_names %>%  filter(Var == doc_filtres$UA[j]), by = c("UA" = "Value")) %>% 
  group_by(Label, Theme, Ordre, question) %>% 
  filter(!is.na(value)) %>% 
  mutate(n = n_distinct(CLE)) %>% 
  dplyr::filter(n >5) %>% 
  
  group_by(Label, Theme, Ordre) %>% 
  summarise(moy = mean(value, na.rm = TRUE),
            n = n_distinct(CLE),
            .groups = "drop") %>% 
  filter(n > 5) %>% 
  select(-n) %>% 
  mutate(moy = case_when(
    
    moy < 5 ~ "#CE4760",
    moy < 6.5 ~ "#F4A261",
    moy < 8 ~ "#90E39A",
    TRUE ~ "#638475")) %>% 
  filter(!is.na(Label)) %>% 
  tidyr::pivot_wider(names_from = Label, values_from = moy) %>% 
  arrange(Ordre) %>% 
  select(-Ordre)


# Calcul global -----------------------------------------------------------



data_colors_global_at <- data_filtre %>% 
  mutate(GLOBAL = "Global") %>% 
  select(GLOBAL, all_of(doc_themes$Var), CLE) %>% 
  tidyr::pivot_longer(all_of(doc_themes$Var),
                      names_to = "question",
                      values_to = "value") %>% 
  left_join(doc_themes %>% select(-Label), by = c("question" = "Var")) %>% 
  
  group_by(GLOBAL, Theme, Ordre, question) %>% 
  filter(!is.na(value)) %>% 
  mutate(n = n_distinct(CLE)) %>% 
  dplyr::filter(n >5) %>% 
  
  group_by(GLOBAL, Theme, Ordre) %>% 
  summarise(moy = mean(value, na.rm = TRUE), .groups = "drop") %>% 
  mutate(moy = case_when(
    
    moy < 5 ~ "#CE4760",
    moy < 6.5 ~ "#F4A261",
    moy < 8 ~ "#90E39A",
    TRUE ~ "#638475")) %>% 
  tidyr::pivot_wider(names_from = GLOBAL, values_from = moy) %>% 
  arrange(Ordre) %>% 
  select(-Ordre)


data_colors_at  <- doc_themes %>% distinct(Theme) %>% 
  left_join(data_specific_color_at, by = "Theme") %>% 
  left_join(data_colors_global_at, by = "Theme") %>% 
  ungroup()

data_colors_at[is.na(data_colors_at)] <- "#000000"


