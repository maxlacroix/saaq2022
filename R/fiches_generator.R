my_doc <- read_docx("ref/mod.docx")
for(i in 1:length(themes_uniques)){


# Filtre des questions ----------------------------------------------------

  
  
  questions_to_keep <- doc_themes %>% 
    dplyr::filter(Theme == themes_uniques[i]) 
  
  
  vars_to_keep <- questions_to_keep %>% 
    pluck("Var")

  

# Calcul par Unité --------------------------------------------------------

  
  
  data_specific <- data_filtre %>% 
    select(UA, all_of(vars_to_keep)) %>% 
    tidyr::pivot_longer(all_of(vars_to_keep),
                        names_to = "question",
                        values_to = "value") %>% 
    
    left_join(doc_names, by = c("UA" = "Value")) %>% 
    
    na.omit() %>% 
    group_by(Label, question) %>% 
    summarise(moy = round(mean(value)*10,1),
              n = n(), .groups = "keep") %>% 
    filter(n > 5) %>% 
    mutate(moy = paste0(moy, " %\nn : ", n)) %>% 
    mutate(moy = stringr::str_replace(moy, "\\.", ",")) %>% 
    select(-n) %>% 
    tidyr::pivot_wider(names_from = Label, values_from = moy) 


# Calcul global -----------------------------------------------------------

  
  
  data_global <- data_filtre %>% 
    mutate(GLOBAL = "Global") %>% 
    select(GLOBAL, all_of(vars_to_keep)) %>% 
    tidyr::pivot_longer(all_of(vars_to_keep),
                        names_to = "question",
                        values_to = "value") %>% 
    na.omit() %>% 
    group_by(GLOBAL, question) %>% 
    summarise(moy = round(mean(value)*10,1),
              n = n(),
              .groups = "keep") %>% 
    filter(n > 5) %>% 
    mutate(moy = paste0(moy, " %\nn : ", n)) %>% 
    mutate(moy = stringr::str_replace(moy, "\\.", ",")) %>% 
    select(-n) %>% 
    tidyr::pivot_wider(names_from = GLOBAL, values_from = moy) 
 


# Joindre et créer la table -----------------------------------------------

  source("R/color_calculator.R")
  
  
  
  ma_table <- questions_to_keep %>% select(-starts_with("Ordre")) %>% 
    left_join(data_specific, by = c("Var" = "question")) %>% 
    left_join(data_global, by = c("Var" = "question")) %>%
    ungroup() %>% 
    select(-Theme, -Var) %>% 
    select(` ` = Label,
           everything())
  
  ma_table[is.na(ma_table)] <- "NA"
  
  
  filtre_name <- paste0(doc_filtres$filtre_name[j], " - ", "Global")
  
  
  ma_table <- ma_table %>% 
    rename_at(.vars = c("Global"), ~ filtre_name)
  
    
      
  optimal_width <- 13/ (ncol(ma_table) - 1)
  
  
  flex <- flextable(ma_table) %>% 
    align(align = "left", part = "header", i = 1) %>%
    align(align = "right", part = "body", j = 1)  %>% 
    align(align = "center", part = "body", j = 2:ncol(ma_table)) %>% 
    align(align = "center", part = "header", i = 1) %>% 
    fontsize(part = "all", size = 8) %>% 
    height(part = "body", height = 0.3 / 2.54) %>% 
    width(j = 1, 6/2.54) %>% 
    width(j = 2:ncol(ma_table), optimal_width / 2.54) %>% 
    bold(j = ncol(ma_table),bold = TRUE, part = "all") %>% 
    color(j = 2:ncol(ma_table), color = data_colors[,-1] %>% as.matrix())
 
  
  

# Flex pour légende -------------------------------------------------------



  leg <-data.frame(x1 = "0 à 49,9 %",
                   x2 = "50 à 64,9 %",
                   x3 = "65 à 79,9 %",
                   x4 = "80 % et plus",
               stringsAsFactors = FALSE)
  
  
  flex_legend <- flextable(leg) %>% 
      color(~ x1 == "0 à 49,9 %", ~x1, color  = "#CE4760") %>% 
      color(~ x2 == "50 à 64,9 %", ~x2, color  = "#F4A261") %>% 
      color(~ x3 == "65 à 79,9 %", ~x3, color  = "#90E39A") %>% 
      color(~ x4 == "80 % et plus", ~x4, color  = "#638475") %>% 
      delete_part("header") %>% 
      border_outer() %>% 
    fontsize(part = "all", size = 8) %>% 
    align(align = "center", part = "body") %>% 
    width(j = 1:4, 1)
      
    
    

# Créer l'écriture du thème -----------------------------------------------


  page_title <- paste0("Thème ", i, " : ", themes_uniques[i])
  style_titre <- fp_text(color = "black", font.size = 12, font.family = "Arial", bold = TRUE)
  
  
  title_officer <- page_title %>% 
    ftext(., style_titre) %>% 
    fpar(fp_p = fp_par(text.align = "center", padding = 1))
    
  
  

# Générer la page du document word ----------------------------------------

  
  my_doc <- my_doc %>%
    body_add_fpar(title_officer) %>% 
    body_add_par(value = " ", style = "Normal") %>% 
    body_add_flextable(value = flex) %>% 
    body_add_par(value = " ", style = "Normal") %>% 
    body_add_flextable(value = flex_legend, align = "center") %>%
    body_add_break()
    

}


# Section pour ajouter tous les thèmes ------------------------------------

source("R/all_themes.R", encoding = "UTF-8")


# Enregistrer le document -------------------------------------------------



print(my_doc, target = paste0("out/",
                              doc_filtres$Var[j],
                              "/",
                              doc_filtres$filtre_name[j], ".docx"))

message("Fiche pour ", doc_filtres$Var[j], " = ", doc_filtres$filtre_name[j], " complete")
