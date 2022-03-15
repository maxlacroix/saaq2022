ech_global <- readxl::read_excel("ref/Structure_SAAQ_VF (003).xlsx", 
                   sheet = "ListeEmployésSondage",
                   n_max = 3696)


nech_par_CR <- ech_global %>% 
  select(CR = `No CR`) %>% 
  count(CR, name = "nech")

nrep_par_CR <- data_saaq %>% 
  count(CR, name = "nrep")



data_saaq <- data_saaq %>% 
  left_join(nech_par_CR, by = "CR")

rm(nrep_par_CR,
   nech_par_CR,
   ech_global)
