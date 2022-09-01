#clustering_all
library(ggplot2)



sessions <- data %>% 
  filter(!is.na(skill_level_know)) %>% 
  select(row_id, session, skill_level_know, skill_level_priority, skill_level_people, 
         skill_level_refusal, skill_level_me
         )

session1 <- data %>% 
  filter(session == "Session 1") %>% 
  select(row_id, event_id)

#pre n filtering: 763, after: 572
sessions_event <- data %>% 
  group_by(session, event_id) %>% 
  tally() %>% 
  filter(n >= 12)

  
ggplot(sessions_event, aes(fill=sessions_event$session, y = sessions_event$n, 
                           x = sessions_event$event_id)) +
  geom_bar(position = "stack", stat = "identity")

