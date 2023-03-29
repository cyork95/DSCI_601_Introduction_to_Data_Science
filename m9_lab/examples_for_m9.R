## R Pre Processing Notes


install.packages("ggthemes")
install.packages("ggrepel")
install.packages("dslabs")

library(tidyverse)
library(readr)
library(ggthemes)
library(ggrepel)
library(dslabs)

interviews <- read_csv("SAFI_clean.csv")
View(interviews)

# select columns

select(interviews, village, no_members, months_lack_food)

select(interviews, village:respondent_wall_type)

#to choose rows based on a criteria use filter() function

filter(interviews, village == "Chirodzo")

filter(interviews, village == "Chirodzo",
       rooms > 1,
       no_meals > 2
)

#logical operator instead of commas

filter(interviews, village == "Chirodzo" &
  rooms > 1 &
  no_meals > 2
)

filter(interviews, village == "Chirodzo" | village == "Ruaca")

#pipe operator can be used to chain statements

interviews_pipes <- interviews %>%
  filter(village == "Chirodzo") %>%
  select

#mutate

interviews <- interviews %>%
  mutate(people_per_room = no_membrs / rooms)

#group_by() is often used with summarize() 

unique(interviews$village)

interviews <- interviews %>%
  group_by(village) %>%
  summarise(mean_no_membrs = mean(no_membrs))

#arrange()

interviews <- interviews %>%
  filter(!is.na(memb_assoc)) %>%
  group_by(village, memb_assoc) %>%
  summarise(mean_no_membrs = mean(no_membrs),
            min_membrs = min(no_membrs)) %>%
  arrange(min_membrs)

# long and wide function

interiews_wide <- interviews %>%
  mutate(wall_type_logical = TRUE) %>%
  pivot_wider(names_from = respondent_wall_type,
              values_from = wall_type_logical,
              values_fill = list(wall_type_logical = FALSE))
interviews_long <- interiews_wide %>%
  pivot_longer(cols = burntbricks:sunbricks,
               names_to = "respondent_wall_type",
               values_to = "wall_type_logical")

#save'

save.image("interview_data.RData")
load("interview_data.RData")

write_csv(interviews_long, file = "interviews_long.csv")

#ggplot

names(interviews)

interviews %>%
  filter(!is.na(memb_assoc)) %>%
  ggplot(aes(interview_date, no_membrs, label = years_liv)) +
  geom_point(aes(color = village), size = 3) +
  geom_text_repel() +
  xlab("Interview Date") +
  ylab("Number of Members") +
  ggtitle("The Interview Data") +
  theme_economist()

png(filename = "test.png", width = 600, height = 500)