## Fully coded data 
cleaned_ind <- read.csv("adapt_gen_subset.csv")

cleaned_ind <- cleaned_ind[,-c(1, 4,6,7,9)]

## seeing how many NAs

NAs <- cleaned_ind %>% group_by(ISO) %>% summarise(sum(is.na(coder_1)))

# Checking class 
class(cleaned_ind$coder_1)
cleaned_ind$coder_1 <- cleaned_ind$coder_1 %>% as.numeric()

# NAs <- cleaned_ind %>% group_by(ISO) %>% summarise(sum(is.na(coder_1)))

# Switching to Long 

adapt_ind <- cleaned_ind %>% pivot_wider(id_cols = ISO,
                                         names_from = Indicator.name,
                                          values_from = coder_1)
adapt_comb <- aggregate(coder_1 ~ Country + Indicator.name, data = cleaned_ind,
                        FUN=sum, drop = FALSE)
adapt_wide <- adapt_comb >% pivot_wider(id_cols = Country,
                                        names_from = Indicator.name,
                                        values_from = coder_1)
