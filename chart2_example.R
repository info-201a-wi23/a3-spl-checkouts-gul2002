library(ggplot2)
library(dplyr)
library(tidyr)

fivecheckouts <- read.csv("2013-2023-5-Checkouts-SPL.csv",stringsAsFactors = FALSE)

mystery_authors <- fivecheckouts %>% filter(str_detect(Subjects, "(i?)Mystery"))

highest_mystery_Character <- fivecheckouts %>% 
  filter(str_detect(Subjects,"(i?)Sherlock"))

highest_mystery_character <- fivecheckouts %>% 
  filter(str_detect(Subjects,"(i?)Poirot"))

highest_mystery_Characters <- fivecheckouts %>% 
  filter(str_detect(Subjects,"(i?)Sherlock|(i?)Poirot"))


Sherlock <- highest_mystery_Character %>% group_by(CheckoutYear) %>% summarize(Checkouts = sum(Checkouts)) %>% mutate(Character = "Sherlock")
Hercule <- highest_mystery_character %>% group_by(CheckoutYear) %>% summarize(Checkouts = sum(Checkouts)) %>% mutate(Character = "Hercule")

comparison <- left_join(Sherlock, Hercule, by = "CheckoutYear")


ggplot(comparison, aes(x=CheckoutYear)) + 
  geom_bar(aes(y=Checkouts.x, fill="Sherlock"), stat="identity",width = 0.2, position = "jitter") + 
  geom_bar(aes(y=Checkouts.y, fill="Hercule"), stat="identity",width = 0.2, position = "stack") + 
  scale_fill_manual(values=c("#008080", "#FF5733"), name="Character")+
  labs(title = "Hercule Vs Sherlock",x = "Time", y = "Checkouts")+
  scale_x_continuous(breaks = seq(2012,2023,1))+
  theme(plot.title = element_text(hjust = 0.5))
 
