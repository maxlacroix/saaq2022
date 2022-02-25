%>% 
  bg(bg = "#8EC9A9", part = "body", j = 5) %>% 
  bg(bg = "#C0C0C0", part = "header", j = 2:5, i = 1) %>% 
  bg(bg = "#C0C0C0", part = "header", j = 1:5, i = 2) 
bg(bg = "#000000", part = "body", i = 36) %>%    
  font(part = "all", fontname = "calibri") %>% 
  border_remove() %>% 
  hline(part="body", i = 36, border = fp_border(color="black") ) %>% 
  hline(part="header",i = 2 ,border = fp_border(color="black") ) %>% 
  bold( i = c(1,4,15,19,26,36),bold = TRUE, part = "body") %>% 
  #  color(i = ~ `n*` == 2086, color  = "red") %>% 
  color(i = ~ `n*` < 30, color  = "red") %>% 
  add_footer_row(values = c(down_footer), colwidths = c(5)) %>% 
  fontsize(part = "footer", size = 7) %>% 
  
  color(i = 36, color = "white") %>%   
  fontsize(i = 36, size = 10) %>% 
  bold(i = 36, bold = TRUE) %>% 
  # display(col_key =  "Connait très bien la solution \n(% de 9 ou 10) \nInclus dans le calcul de l'indicateur",
  #       pattern = "{{text}} \n(% de 9 ou 10) \nInclus dans le calcul de l'indicateur",
  #       formatters = list(text ~ paste0("Connait très bien la solution")),
  #       fprops = list(text = fp_text(bold = TRUE, font.family = "calibri", font.size = 8)),
  #       part = "header") %>% 
  # display(col_key =  "Recommande toujours la solution \n(% de 9 ou 10) \nInclus dans le calcul de l'indicateur",
  #         pattern = "{{text}} \n(% de 9 ou 10) \nInclus dans le calcul de l'indicateur",
  #         formatters = list(text ~ paste0("Recommande toujours la solution")),
  #         fprops = list(text = fp_text(bold = TRUE, font.family = "calibri", font.size = 8)),
  #         part = "header") %>% 
  # display(col_key =  "Indicateur \n(% qui connaissent très bien et recommandent toujours)",
#         pattern = "{{text}} \n(% qui connaissent très bien et recommandent toujours)",
#         formatters = list(text ~ paste0("Indicateur")),
#         fprops = list(text = fp_text(bold = TRUE, font.family = "calibri", font.size = 8)),
#         part = "header")  %>% 
color(i = ~ `Connait très bien la solution \n(% de 9 ou 10) \nInclus dans le calcul de l'indicateur` == "-" &
        !(` ` %in% c("Solutions Épargne",                          
                     "Solutions Financement",                      
                     "Solutions Gestion des finances au quotidien",
                     "Solutions Protection",                  
                     "Solutions Transversal",
                     "INDICATEUR GLOBAL")), 
      color  = "white",
      j = c(3,4)) %>% 
  color(i = ~ `Connait très bien la solution \n(% de 9 ou 10) \nInclus dans le calcul de l'indicateur` == "-" &
          !(` ` %in% c("Solutions Épargne",                          
                       "Solutions Financement",                      
                       "Solutions Gestion des finances au quotidien",
                       "Solutions Protection",                  
                       "Solutions Transversal",
                       "INDICATEUR GLOBAL")), 
        color  = "#8EC9A9",
        j = c(5)) %>% 
  color(i = ~ `Connait très bien la solution \n(% de 9 ou 10) \nInclus dans le calcul de l'indicateur` == "-" &
          (` ` %in% c("Solutions Épargne",                          
                      "Solutions Financement",                      
                      "Solutions Gestion des finances au quotidien",
                      "Solutions Protection",                  
                      "Solutions Transversal")), 
        color  = "#C0C0C0",
        j = c(3,4,5)) %>% 
  
  color(i = ~ `Connait très bien la solution \n(% de 9 ou 10) \nInclus dans le calcul de l'indicateur` == "-" &
          (` ` %in% c("INDICATEUR GLOBAL")), 
        color  = "#000000",
        j = c(3,4)) %>% 
  color(i = ~ `Indicateur \n(% qui connaissent très bien et recommandent toujours)` == "-" &
          (` ` %in% c("INDICATEUR GLOBAL")), 
        color  = "#000000",
        j = c(5)) 