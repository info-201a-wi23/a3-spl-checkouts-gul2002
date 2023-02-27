library("dplyr")
library("ggplot2")
library("stringr")

fivecheckouts <- read.csv("2013-2023-5-Checkouts-SPL.csv",stringsAsFactors = FALSE)

mystery_authors <- fivecheckouts %>% filter(str_detect(Subjects, "(i?)Mystery"))

mystery_authors1 <- mystery_authors %>% group_by(Creator) %>% 
  summarize(Checkouts = sum(Checkouts)) 
mystery_Type <- mystery_authors %>% group_by(MaterialType) %>% 
  summarize(Checkouts = sum(Checkouts))

mystery_type <- mystery_Type %>% filter(Checkouts >= 5000)
  

highest_mystery_Character <- fivecheckouts %>% 
  filter(str_detect(Subjects,"(i?)Sherlock"))

highest_mystery_character <- fivecheckouts %>% 
  filter(str_detect(Subjects,"(i?)Poirot"))

highest_mystery_Characters <- fivecheckouts %>% 
  filter(str_detect(Subjects,"(i?)Sherlock|(i?)Poirot"))

summary_info <- list()
 
summary_info$worst_material_type <- mystery_type %>% filter(Checkouts == min(Checkouts)) %>% 
  pull(MaterialType)

summary_info$best_material_type <- mystery_type %>% filter(Checkouts == max(Checkouts)) %>% 
  pull(MaterialType)

summary_info$Checkouts_Sherlock <- sum(highest_mystery_Character$Checkouts)

summary_info$Checkouts_Hercule <- sum(highest_mystery_character$Checkouts)

summary_info$best_mystery_author <- mystery_authors1 %>% filter(Checkouts == max(Checkouts)) %>% 
  pull(Creator)



