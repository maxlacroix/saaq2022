

# Créer la table avec les taux --------------------------------------------



if( doc_filtres$filtre_name[j] == "SAAQ"){
  
  tr_saaq <- data_saaq %>% 
    group_by(CR) %>% 
    summarise(nech = mean(nech),
              n = n(),
              .groups = "drop") %>% 
    summarise(nech = sum(nech),
              n = sum(n),
              tr = n / nech) %>% 
    pluck("tr")
  
  tr_UA <- data_filtre %>% 
    group_by(UA, CR) %>% 
    summarise(nech = mean(nech),
              n = n(),
              .groups = "drop") %>% 
    group_by(UA) %>% 
    summarise(nech = sum(nech),
              n = sum(n),
              tr = n / nech,
              .groups = "drop") %>% 
    pluck("tr")
  
  
  summary_taux <- bind_rows(
    data_saaq %>% 
    summarise(`Nombre de répondants` = n(),
              `Nombre de commentaires` = sum(!is.na(VCOM)),
              pct = `Nombre de commentaires` / n()) %>% 
    mutate(tr = tr_saaq,
           pct = paste0(round(pct*100,1), " %"),
           pct = stringr::str_replace(pct, "\\.", ","),
           tr = paste0(round(tr*100,1), " %"),
           tr = stringr::str_replace(tr, "\\.", ",")) %>% 
    mutate(groupe = "SAAQ") %>% 
      select(groupe,
             `Nombre de répondants`,
             `Taux de réponse` = tr,
             `Nombre de commentaires`,
             `Pourcentage de commentaires` = pct),
    
    data_filtre %>% 
      group_by(UA) %>% 
      summarise(`Nombre de répondants` = n(),
                `Nombre de commentaires` = sum(!is.na(VCOM)),
                pct = `Nombre de commentaires` / n()) %>% 
      mutate(tr = tr_UA,
             pct = paste0(round(pct*100,1), " %"),
             pct = stringr::str_replace(pct, "\\.", ","),
             
             tr = paste0(round(tr*100,1), " %"),
             tr = stringr::str_replace(tr, "\\.", ",")) %>% 
      mutate(groupe = UA) %>% 
      left_join(doc_names %>% dplyr::filter(Var == doc_filtres$UA[j]), by = c("groupe" = "Value")) %>% 
      mutate(groupe = Label) %>% 
      select(groupe,
             `Nombre de répondants`,
             `Taux de réponse` = tr,
             `Nombre de commentaires`,
             `Pourcentage de commentaires` = pct) %>% 
      filter(!is.na(groupe))
    ) 
}



if(doc_filtres$filtre_name[j] != "SAAQ"){
  
  
  
  tr_saaq <- data_saaq %>% 
    group_by(CR) %>% 
    summarise(nech = mean(nech),
              n = n(),
              .groups = "drop") %>% 
    summarise(nech = sum(nech),
              n = sum(n),
              tr = n / nech) %>% 
    pluck("tr")
  
  tr_filtre <- data_filtre %>% 
    group_by(CR) %>% 
    summarise(nech = mean(nech),
              n = n(),
              .groups = "drop") %>% 
    summarise(nech = sum(nech),
              n = sum(n),
              tr = n / nech) %>% 
    pluck("tr")
  
  
  tr_UA <- data_filtre %>% 
    group_by(UA, CR) %>% 
    summarise(nech = mean(nech),
              n = n(),
              .groups = "drop") %>% 
    group_by(UA) %>% 
    summarise(nech = sum(nech),
              n = sum(n),
              tr = n / nech,
              .groups = "drop") %>% 
    pluck("tr")
  
  
  
  
  
  
  
  
  
  summary_taux <-  bind_rows(
data_saaq %>% 
  summarise(`Nombre de répondants` = n(),
            tr = runif(1),
            `Nombre de commentaires` = sum(!is.na(VCOM)),
            pct = `Nombre de commentaires` / n()) %>% 
  mutate(tr = tr_saaq,
         pct = paste0(round(pct*100,1), " %"),
         pct = stringr::str_replace(pct, "\\.", ","),
         
         tr = paste0(round(tr*100,1), " %"),
         tr = stringr::str_replace(tr, "\\.", ",")) %>% 
  mutate(groupe = "SAAQ") %>% 
  select(groupe,
         `Nombre de répondants`,
         `Taux de réponse` = tr,
         `Nombre de commentaires`,
         `Pourcentage de commentaires` = pct),



data_filtre %>% 
  summarise(`Nombre de répondants` = n(),
            `Nombre de commentaires` = sum(!is.na(VCOM)),
            pct = `Nombre de commentaires` / n()) %>% 
  mutate(tr = tr_filtre,
         pct = paste0(round(pct*100,1), " %"),
         pct = stringr::str_replace(pct, "\\.", ","),
         
         tr = paste0(round(tr*100,1), " %"),
         tr = stringr::str_replace(tr, "\\.", ",")) %>% 
  mutate(groupe = doc_filtres$Value[j]) %>% 
  left_join(doc_names %>% filter(Var == doc_filtres$Var[j]), by = c("groupe" = "Value")) %>% 
  mutate(groupe = Label) %>% 
  select(groupe,
         `Nombre de répondants`,
         `Taux de réponse` = tr,
         `Nombre de commentaires`,
         `Pourcentage de commentaires` = pct),


data_filtre %>% 
  group_by(UA) %>% 
  summarise(`Nombre de répondants` = n(),
            `Nombre de commentaires` = sum(!is.na(VCOM)),
            pct = `Nombre de commentaires` / n()) %>% 
  mutate(tr = tr_UA,
         pct = paste0(round(pct*100,1), " %"),
         pct = stringr::str_replace(pct, "\\.", ","),
         
         tr = paste0(round(tr*100,1), " %"),
         tr = stringr::str_replace(tr, "\\.", ",")) %>% 
  mutate(groupe = UA) %>% 
  left_join(doc_names %>% dplyr::filter(Var == doc_filtres$UA[j]), by = c("groupe" = "Value")) %>% 
  mutate(groupe = Label) %>% 
  select(groupe,
         `Nombre de répondants`,
         `Taux de réponse` = tr,
         `Nombre de commentaires`,
         `Pourcentage de commentaires` = pct) %>% 
  filter(!is.na(groupe))

)
}


# Créer le flextable ------------------------------------------------------


flex_taux <- summary_taux %>% 
  rename(" " = "groupe") %>% 
  flextable(.) %>% 
  align(align = "left", part = "header", i = 1) %>%
  align(align = "right", part = "body", j = 1)  %>% 
  align(align = "center", part = "body", j = 2:ncol(summary_taux)) %>% 
  align(align = "center", part = "header", i = 1) %>% 
  fontsize(part = "all", size = 8) %>% 
  height(part = "body", height = 0.3 / 2.54) %>% 
  width(j = 1, 6/2.54) %>% 
  width(j = 2:ncol(summary_taux), 3 / 2.54) %>% 
  bold(i = ~ ` ` == stringr::str_remove(doc_filtres$filtre_name[j],
                                        " \\(par CR\\)"),
       bold = TRUE,
       part = "body") 



# Créer l'écriture du titre -----------------------------------------------


style_titre <- fp_text(color = "black", font.size = 12, font.family = "Arial", bold = TRUE)
style_st <- fp_text(color = "black", font.size = 9, font.family = "Arial", bold = TRUE)

title_titre_rapport <- paste0("Rapport pour ",doc_filtres$filtre_name[j]) %>% 
  ftext(., style_titre) %>% 
  fpar(fp_p = fp_par(text.align = "center", padding = 1))

title_tc <- "Taux de réponse et de commentaire" %>% 
  ftext(., style_st) %>% 
  fpar(fp_p = fp_par(text.align = "center", padding = 1))

# Générer la page du document word ----------------------------------------


my_doc <- my_doc %>%
  body_add_fpar(title_titre_rapport) %>% 
  body_add_fpar(title_tc) %>%
  body_add_par(value = " ", style = "Normal") %>%
  body_add_flextable(value = flex_taux, align = "center") %>% 
  body_add_break()

