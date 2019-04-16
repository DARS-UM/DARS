#Pass file to App
load("./Output/_catalogues_tmp_topic_model_tibble.RDATA")
save(tb_topic_model, file = "./App/Recommender System/tmp_catalogues_topic_model.RDATA")

#Load necessary files to the environment
load("./App/Recommender System/data_topic_models.RDATA") 
load("./App/Recommender System/rules_clean.RDATA")

#Inspect what number of topics to use:
ggplot(tb_topic_model, aes(n_topic, Perplexity))+
  geom_point()
ggplot(tb_topic_model, aes(n_topic, LogLikelihood))+
  geom_point()

#select Model to pass to App
app_model <- tb_topic_model %>%
  filter(n_topic == 55) %>%
  select(Gamma, Beta, kw)

#pass model to app
save(app_model, file = "./App/Recommender System/app_model.RDATA" )

app_model