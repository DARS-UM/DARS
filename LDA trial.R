
#results <- tibble(nstart = numeric(0),
 #                 burnin = numeric(0),
  #                iter   = numeric(0),
   #               thin   = numeric(0),
    #              k      = numeric(0),
     #             time   = numeric(0),
      #            LL     = numeric(0))

#
# Setup
nstart <- 10
burnin <- 1000
iter   <- 1000
thin   <- 1
k      <- 10

# starting time
t_bef <- proc.time()

#
# run algorithm
LDA_model <- LDA(
  
  x       = d_description_cast,
  k       = k,
  method  = "Gibbs",
  
  control = list(
    nstart  = nstart,
    seed    = c(1 : nstart),
    
    burnin  = burnin,
    iter    = iter,
    thin    = thin
    )
  
)

# finish time
t_aft <- proc.time()

# running time
t_run <- t_aft - t_bef

# Save

results <- results %>%
  add_row(
    nstart = nstart,
    burnin = burnin,
    iter   = iter,
    thin   = thin,
    k      = k,
    time   = t_run[3],
    LL     = LDA_model@loglikelihood
  )


print(results)
print(t_run[3])