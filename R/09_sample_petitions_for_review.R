## Sample 50 petitions for test review
## "Thu Feb 29 19:58:07 2024"

source(here::here("R", "utilities.R"))

years <- 2002:2012
file.names <- paste0("data/tidy/pub_petition_content_",years,".csv")
all.petition.df <- as_tibble(map_dfr(file.names, read.csv))
N <- nrow(all.petition.df) ## 13747

set.seed(4321)
idx <- sample(1:N,50)
sample.petitions <- all.petition.df[idx,]

saveRDS(file="data/sample/sample_petitions.rds",sample.petitions)
