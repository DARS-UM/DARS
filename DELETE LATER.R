#saved in workspace
load("Output/LDA_ntopics.RDATA")

save(result_manual, result_overview , result_description,
     file = "Output/LDA_ntopics_Find.RDATA")

#recovery
load("Output/LDA_ntopics_Find.RDATA")
save(result_manual, result_overview , result_description,
     file = "Output/LDA_ntopics.RDATA")