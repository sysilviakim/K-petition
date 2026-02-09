source(here::here("R", "utilities.R"))

# Load data ====================================================================
## Output from manually selected petition subset
pet <- read_excel("data/screenshots/main/공개제안_전체.xlsx")

## remove petitions that all three reviewers didn't evaluate
pet <- pet %>%
  filter(!is.na(SK) | !is.na(BK) | !is.na(KY))

## final camb by majority vote (GenAI, SK, BK, KY)
maj_vote <- function(x) {
  out <- sort(table(x), decreasing = TRUE)
  if (all(out == 1)) { ## all votes diff
    out <- "no majority"
  } else if (any(out > 1)) { ## choose majority
    out <- names(out)[1]
  }
  return(out)
}

pet <- pet %>%
  rowwise() %>%
  mutate(comb_fin = maj_vote(c(combo, SK, BK, KY))) %>%
  ungroup()

## 7 cases that didn't meet majority consensus
## manual filling
pet$comb_fin[c(2, 3, 5, 10, 28, 29, 61)] <-
  c("1000", "1000", "0011", "1010", "1000", "1011", "1000")

## montecarlo simulation
## 65 choose 2 = 2080 total pairs of petitions
## how many pairs should we assign to respondents to cover all grounds?
## N: the total number of respondents
## K: the total number of petitions
## P: the total number of pairs assigned to each respondents
## N*P = the total number of pairs randomly sampled from the pool (K)
K <- 2080
N <- 600
pet_id <- 1:65

get_random_pairs <- function(pet_id = 1:65, P) {
  pair_sample <- replicate(P, sample(pet_id, 2))
  pair_sample <- apply(pair_sample, 2, sort)
  pair_sample <- as_tibble(t(pair_sample))
  pair_sample <- pair_sample %>%
    distinct()

  if (nrow(pair_sample) < P) {
    pair_sample <- get_random_pairs(pet_id, P)
  } else {
    return(pair_sample)
  }
}

## average times a petition is assigned to a respondent
## N*P*2/65
toy <- t(replicate(N, get_random_pairs(pet_id, P = 3)))
toy <- cbind(unlist(toy[, 1]), unlist(toy[, 2]))
table(c(toy[, 1], toy[, 2]))

## approximately covered grounds
pet_pair_df <- read_excel("data/screenshots/main/공개제안_pairs.xlsx")
## expected count of within-topic pairs for each topic in the survey
## (random sample)
N * P * table(pet_pair_df$category) / K
## expected count of within-topic pairs for each topic in the survey
## (within-topic sample)
N * P * table(pet_pair_df$category) / sum(table(pet_pair_df$category))


# Generate Random Pairs of Petitions + Petition for Scale Question =============
pet_df <-
  read_excel("data/screenshots/main/final/공개제안_tidy_url_appended.xlsx")
N <- 1200

set.seed(1234)
rand_pairs <- t(replicate(N, sample(1:65, 2)))
rand_pairs <- t(apply(rand_pairs, 1, sort))

random_assign_df <- tibble(
  "Respondent_id" = 1:1200,
  "Petition_pair_A" = rand_pairs[, 1],
  "Petition_pair_B" = rand_pairs[, 2]
)
random_assign_df <- random_assign_df %>%
  rowwise() %>%
  mutate(
    Petition_scale = sample((1:65)[-c(Petition_pair_A, Petition_pair_B)], 1)
  )

writexl::write_xlsx(
  random_assign_df,
  "data/screenshots/main/final/공개제안_random_assignments.xlsx"
)
