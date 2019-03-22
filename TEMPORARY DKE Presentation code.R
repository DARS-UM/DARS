#Graph Results

load("Output/LDA_ntopics.RDATA")

graph_results_package <- function(data) {
  data %>%
    select(-Griffiths2004) %>%
    gather(key = "Metric", value = "Value", CaoJuan2009, Arun2010, Deveaud2014) %>%
    mutate(MinMax = ifelse(Metric=="Deveaud2014", "maximize", "minimize")) %>%
    group_by(Metric) %>%
    mutate(Value = Value/sum(Value))%>%
    
    ggplot(aes(x = topics, y = Value))+
    geom_line(aes(colour = Metric))+
    geom_point(aes(colour = Metric))+
    facet_wrap(~MinMax, nrow = 2, scales = "free")+
    theme_minimal()
}


graph_results_package(result_manual)
ggsave("Graphs_presentation/Find_manuals.jpg")
graph_results_package(result_overview)
ggsave("Graphs_presentation/Find_overview.jpg")


#------------------------------------------
load("Output/topic_model.RDATA")

old_topic_model <- as_tibble(topic_model) %>%
  gather(key = "Origin", value = "Model", TM_overview, TM_manual) %>%
  mutate(Origin = str_remove(Origin,"TM_")) %>%
  mutate(Origin = str_replace(Origin,"manual", "manuals")) %>%
  mutate( `Topic ID` = paste(Origin, n_topic, sep = "_"),
          
          #Document Term Matrix
          DTM        = pmap(.l = list(Origin), .f = extract_dtm),
          
          #Gamma distribution
          Gamma = pmap(.l = list(Model), .f = get_distributions, distrib_str = "gamma"),
          
          #Beta distribution
          Beta = pmap(.l = list(Model),  .f = get_distributions, distrib_str = "beta"),
          
          #Perplexity
          Perplexity = pmap_dbl(.l = list(Model, DTM), .f = my_perplexity),
          
          #Loglikelihood
          LogLikelihood = pmap_dbl(.l = list(Model), .f = logLik), #this removes the degrees of freedom data
          
          
          #
          ##Inspect
          
          #top words per topic
          top_terms_topic = map(Beta, .f = get_top_terms_topic),
          
          #top topics per course
          top_topic_course = map(Gamma, .f = get_top_topics_course),
          
          #Select keywords
          kw = map(Beta, .f = get_kw)
  )

model_to_graph <- old_topic_model %>%
  select(n_topic, LogLikelihood, Perplexity, Origin) %>%
  gather(key = "Metric", value = "Value", LogLikelihood, Perplexity) 

#Al lin one
model_to_graph %>%
  ggplot(aes(x= n_topic, y = Value, colour = Origin))+
  geom_line()+
  geom_point()+
  facet_wrap(Metric~Origin, scales = "free", nrow = 2)+
  labs(title ="Perplexity and Log Likelihood of Models",
       x = "Number of Topics",
       y = NULL) +
  theme(plot.title = element_text(size = 20),
        axis.title = element_text(size = 20)
  )
ggsave("Graphs_presentation/per_log_ALL.jpg")

#Per origin
graph_per_origin <-  function(origin){
  model_to_graph %>%
    
    filter(Origin == origin) %>%
    ggplot(aes(x= n_topic, y = Value, colour = Metric ))+
    geom_line()+
    geom_point()+
    facet_grid(Metric~Origin, scales = "free") +
    labs(title = paste("Perplexity and Log likelihood of Models on ", origin, sep= ""),
         x = "Number of Topics",
         y = NULL) +
    theme(plot.title = element_text(size = 20),
          axis.title = element_text(size = 20),
          legend.position = "none"
          )
}

graph_per_origin("manuals")
ggsave("Graphs_presentation/Per_log_manuals.jpg")
graph_per_origin("overview")
ggsave("Graphs_presentation/Per_log_overview.jpg")

#Individual
graph_each <-  function(origin, metric){
  model_to_graph %>%
    
    filter(Origin == origin, Metric == metric) %>%
    ggplot(aes(x= n_topic, y = Value))+
    geom_line()+
    geom_point()+
    ggtitle(paste(metric, " of Models on ", origin, sep= ""))
}

graph_each("overview", "Perplexity")
ggsave("Graphs_presentation/Per_overview.jpg")
  