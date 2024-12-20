## fits the Bayesian pairwise IRT for the survey data
library(MCMCpack)
library(tidyverse)

df <- readxl::read_excel("data/pilot/raw_data.xlsx",sheet="Raw Data")

pwc_df1 <- df %>%
    select(No,Info1_A,Info1_B,Q11_1) %>%
    rename(Item1=Info1_A,Item2=Info1_B,Choice=Q11_1)
pwc_df2 <- df %>%
    select(No,Info2_A,Info2_B,Q11_2) %>%
    rename(Item1=Info2_A,Item2=Info2_B,Choice=Q11_2)
pwc_df3 <- df %>%
    select(No,Info3_A,Info3_B,Q11_3) %>%
    rename(Item1=Info3_A,Item2=Info3_B,Choice=Q11_3)
pwc_df4 <- df %>%
    select(No,Info4_A,Info4_B,Q11_4) %>%
    rename(Item1=Info4_A,Item2=Info4_B,Choice=Q11_4)
pwc_df5 <- df %>%
    select(No,Info5_A,Info5_B,Q11_5) %>%
    rename(Item1=Info5_A,Item2=Info5_B,Choice=Q11_5)
pwc_df6 <- df %>%
    select(No,Info6_A,Info6_B,Q11_6) %>%
    rename(Item1=Info6_A,Item2=Info6_B,Choice=Q11_6)
pwc_df7 <- df %>%
    select(No,Info7_A,Info7_B,Q11_7) %>%
    rename(Item1=Info7_A,Item2=Info7_B,Choice=Q11_7)
pwc_df8 <- df %>% 
    select(No,Info8_A,Info8_B,Q11_8) %>%
    rename(Item1=Info8_A,Item2=Info8_B,Choice=Q11_8)
pwc_df9 <- df %>%
    select(No,Info9_A,Info9_B,Q11_9) %>%
    rename(Item1=Info9_A,Item2=Info9_B,Choice=Q11_9)
pwc_df10 <- df %>%
    select(No,Info10_A,Info10_B,Q11_10) %>%
    rename(Item1=Info10_A,Item2=Info10_B,Choice=Q11_10)

pwc_df <- rbind(pwc_df1,
                pwc_df2,
                pwc_df3,
                pwc_df4,
                pwc_df5,
                pwc_df6,
                pwc_df7,
                pwc_df8,
                pwc_df9,
                pwc_df10)
## sanity check
sanity <- pwc_df %>% group_by(No) %>% summarise(k = mean(Choice))
sanity %>% filter(k == 1 | k == 2) ## no brainer: respondent 41 chose 1 for all 10 pairs

## remove no brainer
pwc_df <- pwc_df %>%
    filter(No != 41)

pwc_df <- pwc_df %>%
    rowwise() %>%
    mutate(Item1 = paste0("item.",Item1),
           Item2 = paste0("item.",Item2),
           Choice = c(Item1,Item2)[Choice]) %>%
    ungroup()
pwc_df <- as.data.frame(pwc_df)

## item 1: emotional, concrete, good grammar
## item 2: dry, abstract, bad grammar
## item 8: dry, concrete, good grammar

## theta constraints
## set 1st dimension to be about concrete + grammar
## set 2nd dimension to be about emotions vs dry
set.seed(1234)
post_out <- MCMCpaircompare2d(pwc.data=pwc_df,
                              theta.constraints=list(item.1=list(1,2),
                                                     item.1=list(2,2),
                                                     item.2=list(1,-2),
                                                     item.2=list(2,-2),
                                                     item.8=list(1,"+"),
                                                     item.8=list(2,"-")),
                              burnin=500,mcmc=20000,thin=5,verbose=1000,
                              store.theta=TRUE,store.gamma=TRUE,tune=0.5)

theta1.draws <- post_out[, grep("theta1", colnames(post_out))]
theta2.draws <- post_out[, grep("theta2", colnames(post_out))]
gamma.draws <- post_out[, grep("gamma", colnames(post_out))]
theta1.post.med <- apply(theta1.draws, 2, median)
theta2.post.med <- apply(theta2.draws, 2, median)
gamma.post.med <- apply(gamma.draws, 2, median)
theta1.post.025 <- apply(theta1.draws, 2, quantile, prob=0.025)
theta1.post.975 <- apply(theta1.draws, 2, quantile, prob=0.975)
theta2.post.025 <- apply(theta2.draws, 2, quantile, prob=0.025)
theta2.post.975 <- apply(theta2.draws, 2, quantile, prob=0.975)
gamma.post.025 <- apply(gamma.draws, 2, quantile, prob=0.025)
gamma.post.975 <- apply(gamma.draws, 2, quantile, prob=0.975)

## visualize theta posteriors (item parameters)
labs <- gsub("theta1.","",names(theta1.post.med))
pdf("output/theta_post_median.pdf",width=10,height=10)
plot(theta1.post.med, theta2.post.med,type="n",
     xlim=c(-2.5, 2.5), ylim=c(-2.5, 2.5),
     xlab="Theta 1D", ylab="Theta 2D")
text(x=theta1.post.med, y=theta2.post.med, label=labs)
dev.off()


## visualize theta with gamma (item parameters overlayed with respondent vectors)
plot(theta1.post.med, theta2.post.med,type="n",
     xlim=c(-2.5, 2.5), ylim=c(-2.5, 2.5),
     xlab="Theta 1D", ylab="Theta 2D")
text(x=theta1.post.med, y=theta2.post.med, label=labs)

for (i in 1:length(gamma.post.med)){
    arrows(x0=0, y0=0,
           x1=cos(gamma.post.med[i]),
           y1=sin(gamma.post.med[i]),
           col=rgb(1,0,0,0.2), len=0.05, lwd=0.5)
}

## visualize with gamma posteriors (individual parameters)




## Dirichlet process
postDP_out <- MCMCpaircompare2dDP(pwc.data=pwc_df,
                                  theta.constraints=list(item.1=list(1,2),
                                                         item.1=list(2,2),
                                                         item.2=list(1,-2),
                                                         item.2=list(2,-2),
                                                         item.8=list(1,"+"),
                                                         item.8=list(2,"-")),
                                  burnin=500,mcmc=20000,thin=5,verbose=1000,
                                  store.theta=TRUE,store.gamma=TRUE,tune=0.5)

theta1.draws <- postDP_out[, grep("theta1", colnames(postDP_out))]
theta2.draws <- postDP_out[, grep("theta2", colnames(postDP_out))]
gamma.draws <- postDP_out[, grep("gamma", colnames(postDP_out))]
theta1.postDP.med <- apply(theta1.draws, 2, median)
theta2.postDP.med <- apply(theta2.draws, 2, median)
gamma.postDP.med <- apply(gamma.draws, 2, median)
theta1.postDP.025 <- apply(theta1.draws, 2, quantile, prob=0.025)
theta1.postDP.975 <- apply(theta1.draws, 2, quantile, prob=0.975)
theta2.postDP.025 <- apply(theta2.draws, 2, quantile, prob=0.025)
theta2.postDP.975 <- apply(theta2.draws, 2, quantile, prob=0.975)
gamma.postDP.025 <- apply(gamma.draws, 2, quantile, prob=0.025)
gamma.postDP.975 <- apply(gamma.draws, 2, quantile, prob=0.975)

pdf("output/theta_gamma_postDP_median.pdf",width=10,height=10)
plot(theta1.postDP.med, theta2.postDP.med,type="n",
     xlim=c(-2.5, 2.5), ylim=c(-2.5, 2.5),
     xlab="Theta 1D", ylab="Theta 2D")
text(x=theta1.postDP.med, y=theta2.postDP.med, label=labs)

for (i in 1:length(gamma.postDP.med)){
    arrows(x0=0, y0=0,
           x1=cos(gamma.postDP.med[i]),
           y1=sin(gamma.postDP.med[i]),
           col=rgb(1,0,0,0.2), len=0.05, lwd=0.5)
}
dev.off()

## check clusters
table(postDP_out[,131]) ## how many distinct respondent parameter values?
##    2    3    4    5    6    7    8    9 
## 1165 1507  892  323   82   24    6    1
## respondents mostly fall under 3 distinct clusters

get_mode <- function(x) {
  x %>%
    table() %>%
    which.max() %>%
    names() %>%
    as.numeric()
}

## two clusters in pilot study data
## summarize demographics of clusters
c1 <- str_extract(names(gamma.postDP.med[gamma.postDP.med < 1]),"\\d+")
c1 <- as.numeric(c1)
c2 <- str_extract(names(gamma.postDP.med[gamma.postDP.med > 1]),"\\d+")
c2 <- as.numeric(c2)

df <- df %>%
    mutate(cluster = 1)
df$cluster[df$No %in% c2] <- 2

df %>%
    group_by(cluster) %>%
    summarise(sex = mean(SQ1),
              age = mean(SQ2),
              edu = mean(SQ3),
              married = mean(SQ5),
              kid = mean(SQ6),
              income = mean(SQ7),
              life = mean(Q1),
              ideal = mean(Q2),
              party = get_mode(Q3))

##  cluster   sex   age   edu married   kid income  life ideal party
##    <dbl> <dbl> <dbl> <dbl>   <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl>
##1       1  1.47  43.8   3.8    1.97  1.97   5.17  5.13   4.2     1
##2       2  1.55  44.2   4.1    1.8   1.85   6.7   6.15   3.8     1
## clusters are very similar in demographics
## both clusters identify with liberal party
## cluster 2 has higher income level, life satisfaction

## cluster 1 more towards 2nd dimension of theta
## cluster 2 more towards 1st dimension of theta
