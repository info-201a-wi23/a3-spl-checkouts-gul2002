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

Hercule_authors <- highest_mystery_character %>% group_by(Creator) %>% 
  summarize(Checkouts = sum(Checkouts))
Sherlock_authors <- highest_mystery_Character %>% group_by(Creator) %>% 
  summarize(Checkouts = sum(Checkouts))


Creators_by_years <- mystery_authors %>% group_by(Creator,CheckoutYear) %>%  
  summarize(Checkouts = sum(Checkouts))

top5_creators <- Creators_by_years %>% 
  filter(Creator %in% c("Agatha Christie","Louise Penny","Janet Evanovich","Lee Child","Michael Connelly")) %>% 
  filter(CheckoutYear != 2023)


ggplot(top5_creators, aes(x= CheckoutYear, y=Creator, fill=Checkouts)) +
  geom_tile()+
  scale_fill_gradient(low="white", high="blue") +
  labs(title="Total Checkouts by Creator and Year",
       x="Year",
       y="Creator") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
  scale_x_continuous(breaks = seq(2012,2023,1))


















