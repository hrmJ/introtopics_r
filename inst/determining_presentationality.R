# This script demonstrates the process for determining 

library(dplyr) library(pbapply)

adverbials  %>% 
    group_by(lang) %>% 
    count(headverb)  %>% 
    arrange(desc(n))  %>% 
    filter(n>=5) -> 
    headverbs

headverbs$is_presentational <- pbapply(headverbs, 1, CheckSample_df, cols_to_show=c("headverb","n"))
