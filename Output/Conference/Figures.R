load("Output/_catalogues_tmp_topic_model_tibble.RDATA")

theme_set(theme_light())

d <- tb_topic_model

names(d)
gamma <- d$Gamma[[1]]
beta  <- d$Beta[[1]]

# Figure 1: Beta. ####

plot_beta <- function(topic = 1, sub = NULL){
  
  top <-  str_c("Topic ", topic)
  
  beta %>%
    filter(topic == top) %>% 
    top_n(10, beta) %>%
    
    ggplot() +
    geom_col(aes(x = reorder(term, beta), y = beta)) +
    
    coord_flip() +
    
    labs(title = str_c("Main Terms of ", top),
         subtitle = sub,
         x = "Terms",
         y = "Beta"
    )
  
}

# topic 1: "Topic 1 corresponds to engineering",
plot_beta(1, sub = "Topic 1 corresponds to engineering")
# topic 9: computer science, topic 
# Topic 20: psychology
# topic 25: history
# topic 26: literature 



# Gamma ####

# SCI3003 optimization dominated by topic 1

plot_gamma <- function(course = "COR1002", sub = NULL){
  
  gamma %>%
    filter(document == course) %>% 
    top_n(10, gamma) %>%
    
    ggplot() +
    geom_col(aes(x = reorder(topic, gamma), y = gamma)) +
    
    coord_flip() +
    
    labs(title    = str_c("Main Topics of Course ", course),
         subtitle = sub,
         x = "Topics",
         y = "Gamma"
         )
  
}

plot_gamma("HUM1014")
plot_beta(43)
plot_beta(26)

