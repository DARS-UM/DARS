data <- read.csv("/Users/sofia/Downloads/poliblogs2008.csv")
processed <- textProcessor(data$documents, metadata = data)

out <- prepDocuments(processed$documents, processed$vocab, processed$meta)
docs <- out$documents
vocab <-  out$vocab
meta <- out$meta

poliblogPrevFit <- stm(documents = out$documents, vocab = out$vocab, K = 20, prevalence = ~rating + s(day), max.em.its = 75, data = out$meta, init.type = "Spectral")


