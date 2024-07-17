## Use DFM to predict the response delay
## "Wed Jul 17 17:03:32 2024"

source(here::here("R", "utilities.R"))

## load dfm
petition_dfm <- readRDS("data/DFM.rds")

## load meta data
years <- 2002:2023
file.names <- paste0("your_dropbox/data/tidy/pub_petition_content_", years, ".csv") ## this should be the path to the data in dropbox folder
meta_df <- as_tibble(map_dfr(file.names, read_csv))

## generate y for dfm (days to response)
meta_df <- meta_df %>%
    mutate(date_petitioned = as.Date(date_petitioned),
           date_answered = as.Date(date_answered)) %>%
    mutate(respond_delay = date_answered - date_petitioned, ## days to response
           implement_petition_delay = date_implemented - date_petitioned, ## days from petition to implementation
           implement_respond_delay = date_implemented - date_answered) ## days from response to implmentation 

quantile(meta_df$respond_delay,na.rm=TRUE)
## Time differences in days
##   0%  25%  50%  75% 100% 
##    1   17   29   39 4725
## some petitions get response after more than 10 years...!

quantile(meta_df$implement_petition_delay,na.rm=TRUE)
## Time differences in days
##   0%  25%  50%  75% 100% 
##    1   17   27   33 3911

quantile(meta_df$implement_respond_delay,na.rm=TRUE)
## Time differences in days
##   0%  25%  50%  75% 100% 
##    0    0    0    0  230 
## most responses accompany almost immediate implementation
## probably the reason why responses are so slow?

## merge y with dfm
df <- as.matrix(petition_dfm)
df <- as_tibble(df)
df$doc_name <- quanteda::docnames(petition_dfm)

meta_df <- meta_df %>%
    select(title,respond_delay,implement_petition_delay,implement_respond_delay,area,date_petitioned,date_answered,date_implemented)
df <- df %>%
    left_join(meta_df, by=c("doc_name"="title"))

## run ML algorithms
## 1. LASSO regression

