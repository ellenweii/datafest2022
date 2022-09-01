library(dplyr)
#data is only on ONE player
#clustering based on features

#time series (revamped as clustering) on one of the 12 stories
df <- sample_n(data, size=20)
table(df$session)

session1 <- data %>% 
  filter(session == "Session 1") %>% 
  select(!(wave))

table(data$date)
table(data$session)
table(data$wave)

features <- c(row_id, session, skill_level_know, skill_level_priority, skill_level_people, 
              skill_level_refusal, skill_level_me)

sessions <- data %>% 
  filter(!is.na(skill_level_know)) %>% 
  select(row_id, session, skill_level_know, skill_level_priority, skill_level_people, 
         skill_level_refusal, skill_level_me)

table(is.na(session1p$skill_level_people)) # a lot of NAs, range from 0-29 when should be 0-9





