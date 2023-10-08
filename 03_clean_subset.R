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

# same --- result 
adapt_comb <- aggregate(coder_1 ~ Country + Indicator.name, data = cleaned_ind,
                        FUN=sum, drop = FALSE)
adapt_wide <- adapt_comb %>% pivot_wider(id_cols = Country,
                                        names_from = Indicator.name,
                                        values_from = coder_1)

###  creating list of subsectors to match  
categories_match <- c("Financial Needs", "Loss and Damage", "Vulnerability")

sub_cat <- subset(cleaned_ind, 
                  subset = cleaned_ind$Overview.category == categories_match)
subsectors <- sub_cat[,c(3,7)]

list_of_sectors <- subsectors[!duplicated(subsectors),]

finan_needs_match <- list_of_sectors[list_of_sectors$Overview.category == "Financial Needs", 2]

loss_damage_match <- list_of_sectors[list_of_sectors$Overview.category == "Loss and Damage", 2]

vulnerability_match <- list_of_sectors[list_of_sectors$Overview.category == "Vulnerability", 2]


# marking each category based on subsector 

adapt_comb$financial_need_ind <- ifelse(grepl(paste(finan_needs_match, 
                                              collapse = "|"), adapt_comb$Indicator.name,
                                              ignore.case = TRUE), 1, 0)
adapt_comb$loss_damage_ind <- ifelse(grepl(paste(loss_damage_match, 
                                                 collapse = "|"), adapt_comb$Indicator.name,
                                           ignore.case = TRUE), 1, 0)
adapt_comb$vul_ind <- ifelse(grepl(paste(vulnerability_match, 
                                         collapse = "|"), adapt_comb$Indicator.name,
                                   ignore.case = TRUE), 1, 0)
## Creating index -- first subsets

finan_subset <- subset(adapt_comb, subset = adapt_comb$financial_need_ind == 1)

loss_subset <- subset(adapt_comb, subset = adapt_comb$loss_damage_ind == 1)

vul_subset <- subset(adapt_comb, subset = adapt_comb$vul_ind == 1)

## turning to wide 

finan_wide <- finan_subset %>% pivot_wider(id_cols = Country,
                                         names_from = Indicator.name,
                                         values_from = coder_1)
finan_wide <- finan_wide %>% mutate(Finan_Needs = select(., 
"Conditional financial needs": "Unconditional financial needs") %>% 
                                      rowSums(na.rm = TRUE))
finan_wide$
