load("Output/data_pillar_1.RDATA")
library(tidyverse)

# Set up
height <- 4; width <- height * 1.6
theme_set(theme_light())

# Student transcript
student_ID <- "6087587"

transcript <- d_transcript_augmented %>%
  
  mutate(Cluster = factor(Cluster)) %>%
  
  filter(`Student ID` == student_ID)


# Aod & Assessment

my_barplot_AoD <- function(transcript, var){
  
  transcript %>%
    
    filter(! is.na(!!ensym(var))) %>%
    
    pull(!!ensym(var)) %>% 
    
    str_c(collapse = ", ") %>%
    
    str_split(", ") %>% .[[1]] %>%
    
    tibble(AoD = .) %>%
    
    ggplot(aes(reorder(AoD, AoD, function(x) length(x)))) +
    geom_bar() +
    coord_flip() +
    labs(x = NULL)
  
}

my_barplot_AoD(transcript, "AoD Covered")
ggsave("AoD.jpeg", path = "Output/Plots", width = width, height = height)

my_barplot_AoD(transcript, "Assessments Covered")
ggsave("Assessment.jpeg", path = "Output/Plots", width = width, height = height)


## Profile: cluster & concentration

my_barplot_concentration <- function(transcript, var){
  
  transcript  %>%
    
    filter(! is.na(!!ensym(var)), ! is.na(Level)) %>%
    
    ggplot(aes(!!ensym(var), fill = Level)) +
    
    geom_bar(col = "black") +
    
    facet_grid(Year_numerical ~ .) + 
    
    scale_x_discrete(drop = FALSE) + 
    scale_fill_discrete(drop = FALSE) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
    labs(x = NULL)

}

my_barplot_concentration(transcript, "Concentration")
ggsave("concentration.jpeg", path = "Output/Plots", width = width, height = height)

my_barplot_concentration(transcript, "Cluster")
ggsave("Cluster.jpeg", path = "Output/Plots", width = width, height = height)

