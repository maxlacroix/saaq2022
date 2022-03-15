data_all_themes_specific <- data_filtre %>% 
  select(UA, all_of(doc_themes$Var), CLE) %>% 
  tidyr::pivot_longer(all_of(doc_themes$Var),
                      names_to = "question",
                      values_to = "value") %>% 
  left_join(doc_themes %>% select(-Label), by = c("question" = "Var")) %>% 
  left_join(doc_names %>% dplyr::filter(Var == doc_filtres$UA[j]), by = c("UA" = "Value")) %>% 
  group_by(Label, Theme, Ordre, question) %>% 
  filter(!is.na(value)) %>% 
  mutate(n = n_distinct(CLE)) %>% 
  dplyr::filter(n >5) %>% 
group_by(Label, Theme, Ordre) %>% 
  summarise(moy = round(mean(value, na.rm = TRUE)*10,1),
            n = n_distinct(CLE, Var),
            .groups = "drop") %>% 
  mutate(moy = paste0(moy, " %")) %>%
  filter(n > 5) %>% 
  select(-n) %>% 
  filter(!is.na(Label)) %>% 
  mutate(moy = stringr::str_replace(moy, "\\.", ",")) %>% 
  tidyr::pivot_wider(names_from = Label, values_from = moy) %>% 
  arrange(Ordre) %>% 
  select(-Ordre)


data_all_themes_global <- data_filtre %>% 
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
  summarise(moy = round(mean(value, na.rm = TRUE)*10,1),
            .groups = "drop") %>% 
  mutate(moy = paste0(moy, " %")) %>% 
  mutate(moy = stringr::str_replace(moy, "\\.", ",")) %>% 
  tidyr::pivot_wider(names_from = GLOBAL, values_from = moy) %>% 
  arrange(Ordre) %>% 
  select(-Ordre)


ma_table <- doc_themes %>% distinct(Theme) %>% 
  left_join(data_all_themes_specific, by = "Theme") %>% 
  left_join(data_all_themes_global, by = "Theme") %>% 
  ungroup() %>% 
  select(` ` = Theme,
         everything())

if(ncol(ma_table) == 2){optimal_width = 6.5}    
if(ncol(ma_table) > 2){optimal_width <- 13/ (ncol(ma_table) - 1)}

ma_table[is.na(ma_table)] <- "NA"


# Ajouter les couleurs ----------------------------------------------------

source("R/color_calculator_all_themes.R")

# Créer la flex -----------------------------------------------------------



flex_at <- flextable(ma_table) %>% 
  align(align = "left", part = "header", i = 1) %>%
  align(align = "right", part = "body", j = 1)  %>% 
  align(align = "center", part = "body", j = 2:ncol(ma_table)) %>% 
  align(align = "center", part = "header", i = 1) %>% 
  fontsize(part = "all", size = 8) %>% 
  height(part = "body", height = 0.3 / 2.54) %>% 
  width(j = 1, 6/2.54) %>% 
  width(j = 2:ncol(ma_table), optimal_width / 2.54) %>% 
  bold(j = ncol(ma_table),bold = TRUE, part = "all") %>% 
  color(j = 2:ncol(ma_table), color = data_colors_at[,-1] %>% as.matrix())


# Créer le texte puis la page ---------------------------------------------

page_title_at <- "Résultat par thème"

title_officer_at <- page_title_at %>% 
  ftext(., style_titre) %>% 
  fpar(fp_p = fp_par(text.align = "center", padding = 1))


my_doc <- my_doc %>%
  body_add_fpar(title_officer_at) %>% 
  body_add_par(value = " ", style = "Normal") %>% 
  body_add_flextable(value = flex_at) %>% 
  body_add_par(value = " ", style = "Normal") %>% 
  body_add_flextable(value = flex_legend, align = "center") 


