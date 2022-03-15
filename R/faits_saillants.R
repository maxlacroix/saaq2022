

# Calcul des top/bottom 10 ------------------------------------------------


tous_enonces <- data_filtre %>% 
  select(UA, all_of(doc_themes$Var)) %>% 
  tidyr::pivot_longer(all_of(doc_themes$Var),
                      names_to = "question",
                      values_to = "value") %>% 
  left_join(doc_themes, by = c("question" = "Var")) %>% 
  group_by(Label) %>% 
  summarise(moy = round(mean(value, na.rm = TRUE)*10,1),
            .groups = "drop") 


top_10 <- tous_enonces %>% 
  top_n(10, moy) %>% 
  mutate(colors = case_when(
    
    moy < 50 ~ "#CE4760",
    moy < 65 ~ "#F4A261",
    moy < 80 ~ "#90E39A",
    TRUE ~ "#638475")) %>% 
  arrange(desc(moy)) %>% 
  mutate(moy = paste0(moy, " %")) %>% 
  mutate(moy = stringr::str_replace(moy, "\\.", ",")) %>% 
  rename("Cote" = "moy")


bottom_10 <- tous_enonces %>% 
  top_n(10, -moy) %>% 
  mutate(colors = case_when(
    
    moy < 50 ~ "#CE4760",
    moy < 65 ~ "#F4A261",
    moy < 80 ~ "#90E39A",
    TRUE ~ "#638475")) %>% 
  arrange(moy) %>% 
  mutate(moy = paste0(moy, " %")) %>% 
  mutate(moy = stringr::str_replace(moy, "\\.", ",")) %>% 
  rename("Cote" = "moy")



# Créer le flex -----------------------------------------------------------


flex_top10 <- top_10 %>% 
  select(-colors) %>% 
  rename(" " = "Label") %>% 
  flextable(.) %>% 
  align(align = "left", part = "header", i = 1) %>%
  align(align = "right", part = "body", j = 1)  %>% 
  align(align = "center", part = "body", j = 2) %>% 
  align(align = "center", part = "header", i = 1) %>% 
  fontsize(part = "all", size = 8) %>% 
  height(part = "body", height = 0.3 / 2.54) %>% 
  width(j = 1, 12/2.54) %>% 
  width(j = 2, 3 / 2.54) %>% 
  bold(j = 2,bold = TRUE, part = "all") %>% 
  color(j = 2, color = top_10[,-c(1,2)] %>% as.matrix())



flex_bottom10 <- bottom_10 %>% 
  select(-colors) %>% 
  rename(" " = "Label") %>% 
  flextable(.) %>% 
  align(align = "left", part = "header", i = 1) %>%
  align(align = "right", part = "body", j = 1)  %>% 
  align(align = "center", part = "body", j = 2) %>% 
  align(align = "center", part = "header", i = 1) %>% 
  fontsize(part = "all", size = 8) %>% 
  height(part = "body", height = 0.3 / 2.54) %>% 
  width(j = 1, 12/2.54) %>% 
  width(j = 2, 3 / 2.54) %>% 
  bold(j = 2,bold = TRUE, part = "all") %>% 
  color(j = 2, color = bottom_10[,-c(1,2)] %>% as.matrix())




# Créer l'écriture des Faits Saillants ------------------------------------


style_titre <- fp_text(color = "black", font.size = 12, font.family = "Arial", bold = TRUE)
style_st <- fp_text(color = "black", font.size = 9, font.family = "Arial", bold = TRUE)

title_top_page <- "Faits Saillants" %>% 
  ftext(., style_titre) %>% 
  fpar(fp_p = fp_par(text.align = "center", padding = 1))


title_top_10<- "10 énoncés avec la cote d'appréciation la plus élevée" %>% 
  ftext(., style_st) %>% 
  fpar(fp_p = fp_par(text.align = "center", padding = 1))

title_bottom_10<- "10 énoncés avec la cote d'appréciation la plus basse" %>% 
  ftext(., style_st) %>% 
  fpar(fp_p = fp_par(text.align = "center", padding = 1))


# Générer la page du document word ----------------------------------------


my_doc <- my_doc %>%
  body_add_fpar(title_top_page) %>% 
  body_add_par(value = " ", style = "Normal") %>%
  body_add_fpar(title_top_10) %>% 
  body_add_flextable(value = flex_top10, align = "center") %>% 
  body_add_par(value = " ", style = "Normal") %>% 
  body_add_fpar(title_bottom_10) %>% 
  body_add_flextable(value = flex_bottom10, align = "center") %>%
  body_add_break()



