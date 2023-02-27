library(ggplot2)
library(dplyr)
library(tidyr)

fivecheckouts <- read.csv("2013-2023-5-Checkouts-SPL.csv",stringsAsFactors = FALSE)

mystery_authors <- fivecheckouts %>% filter(str_detect(Subjects, "(i?)Mystery"))

mystery_author <- mystery_authors %>% filter(str_detect(MaterialType,"BOOK|EBOOK|AUDIOBOOK|SOUNDDISC|VIDEODISC"))

mystery <- mystery_author %>% group_by(CheckoutYear,MaterialType) %>% summarize(Checkouts = sum(Checkouts))

mystery1 <- mystery %>% filter(MaterialType != "ER, SOUNDDISC") %>% filter(CheckoutYear != 2023)
library(ggplot2)
ggplot(mystery1 ,mapping = aes(x= CheckoutYear, y = Checkouts, group = MaterialType)) +
  geom_line(aes(color = MaterialType))+
  geom_point(aes(color = MaterialType))+
  labs(title = "Different mediums over time",x = "Time", y = "Checkouts")+
  scale_x_continuous(breaks = seq(2013,2022,1))+
  theme(plot.title = element_text(hjust = 0.5))

