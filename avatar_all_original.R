avatar_all <- as.data.frame(matrix(nrow = 1, ncol=64))
colnames(avatar_all) <- colnames(avatar_choices)

for (player in player_ids){
  #subset for a single player
  avatar <- full_data %>% 
    filter(player_id == player, event_id == 304) %>% 
    select(row_id, event_time_dbl, data_headers, data_values)
  
  #choose 6th value in choice vector
  choice_6 <- c()
  for (i in 1:nrow(avatar)){
    sixth <- unlist(strsplit(x = avatar[[i,4]], split = ','))[6]
    choice_6 <- c(choice_6, sixth)
  }
  
  avatar$new_choice_value <- choice_6
  
  #code in choice id
  choice_id <- c()
  for (i in 1:nrow(avatar)){
    chunk <- unlist(strsplit(x = avatar[[i,4]], split = ','))[1:3]
    chunk <- paste(chunk, collapse="")
    choice_id <- c(choice_id, chunk)
  }
  
  avatar$new_choice_id <- choice_id
  
  #pivot into 1 row
  avatar_choices <- avatar %>% 
    select(new_choice_id, new_choice_value) %>% 
    group_by(new_choice_id)  %>% do(tail(., n=1)) %>% 
    pivot_wider(names_from = new_choice_id, values_from = new_choice_value)
  
  avatar_choices <- cbind(player, avatar_choices)
  cat("number of columns:", ncol(avatar_choices), "\n")
  
  #if (ncol(avatar_choices) == 64){
  #  avatar_all <- rbind(avatar_all, avatar_choices)
  #}
  
  #View(avatar_all)
  avatar_all <- smartbind(avatar_all, avatar_choices)
  
}

avatar_all <- avatar_all[-1,]


write.csv(avatar_all,"avatar_all.csv", row.names = FALSE)
