data_saaq <- readxl::read_excel("data/SAAQCASAV2NBKCOMM.xlsx")

data_saaq %>% 
  distinct(VP, DIG) %>% 
  na.omit() %>% 
  count(VP)

data_saaq %>% 
  distinct(DIG, DIR) %>% 
  na.omit() %>% 
  count(DIG)

