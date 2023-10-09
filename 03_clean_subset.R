## Fully coded data 
cleaned_ind <- read.csv("adapt_gen_subset.csv")
cleaned_ind_new <- read.csv("adapt_gen_subset_new.csv")
cleaned_ind_new <- cleaned_ind_new[,-c(1, 4,6,7,10)]

## seeing how many NAs by country 

NAs <- cleaned_ind_new %>% group_by(Indicator.name) %>% summarise(sum(is.na(Value)))

# Checking class 
class(cleaned_ind_new$Value)
cleaned_ind$Value <- cleaned_ind$Value %>% as.numeric()

# NAs <- cleaned_ind %>% group_by(ISO) %>% summarise(sum(is.na(coder_1)))

# Switching to Wide 

adapt_ind <- cleaned_ind_new %>% pivot_wider(id_cols = ISO,
                                         names_from = Indicator.name,
                                          values_from = Value)

# same --- result 
adapt_comb <- aggregate(Value ~ Country + Indicator.name, data = cleaned_ind_new,
                        FUN=sum, drop = FALSE)
adapt_wide <- adapt_comb %>% pivot_wider(id_cols = Country,
                                        names_from = Indicator.name,
                                        values_from = Value)
## most 

###  creating list of subsectors to match  
categories_match <- c("Financial Needs", "Loss and Damage", "Vulnerability")

sub_cat <- cleaned_ind_new[grepl(paste(categories_match, 
                      collapse = "|"), adapt_comb$Indicator.name,
                ignore.case = TRUE), c(5,11)]

list_of_sectors <- sub_cat[!duplicated(sub_cat),]

finan_needs_match <- list_of_sectors[list_of_sectors$Overview.category == "Financial Needs",2]

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


##### Financial --- 
## turning to wide 

finan_wide <- finan_subset %>% pivot_wider(id_cols = Country,
                                         names_from = Indicator.name,
                                         values_from = Value)
# Excluding Unconditional and Conditional due to differences in coding 
finan_wide_cleaned <- finan_wide[,-c(2,5)]

## Sums 
finan_wide_sums <- finan_wide_cleaned %>% mutate(Finan_Needs = select(., 
"Financial needs for implementation":"Other non-financial support needs") %>% 
                                      rowSums(na.rm = TRUE))
finan_wide_sums$Finan_Needs <- ifelse(is.na(finan_wide_sums$`Financial needs for implementation`) &
                                   is.na(finan_wide_sums$`Other non-financial support needs`), 
                                 NA, finan_wide_sums$Finan_Needs)


###### Costs - - 

loss_wide <- loss_subset %>% pivot_wider(id_cols = Country,
                                           names_from = Indicator.name,
                                           values_from = Value)

## Sums 
loss_wide_sums <- loss_wide_cleaned%>% mutate(Loss_Counts = select(., 
                        "Definition of loss and damage":"Use of climate change scenario") %>% 
                                                   rowSums(na.rm = TRUE))
loss_wide_sums$Loss_Counts <- ifelse(is.na(loss_wide_sums$`Definition of loss and damage`) &
                                       is.na(loss_wide_sums$`Economic loss and damage`) & 
                                       is.na(loss_wide_sums$`Future economic loss and damage figures`) &
                                       is.na(loss_wide_sums$`Human mobility`) & 
                                       is.na(loss_wide_sums$`Loss and damage mentioned`) & 
                                       is.na(loss_wide_sums$`Non-economic loss and damage`) & 
                                       is.na(loss_wide_sums$`Risk management approaches`) &
                                       is.na(loss_wide_sums$`Slow-onset events`) &
                                       is.na(loss_wide_sums$`Use of climate change scenario`), 
                                      NA, loss_wide_sums$Loss_Counts)

###### Vulnerability 
vul_wide <- vul_subset %>% pivot_wider(id_cols = Country,
                                         names_from = Indicator.name,
                                         values_from = Value)


## Sums 
vul_wide_sums <- vul_wide %>% mutate(Vul_Counts = select(., 
                 "Identification of children as a vulnerable group":"Identification of young people as a vulnerable group") %>% 
                                                rowSums(na.rm = TRUE))
vul_wide_sums$Vul_Counts <- ifelse(is.na(vul_wide_sums$`Identification of young people as a vulnerable group`) &
                                       is.na(vul_wide_sums$`Identification of children as a vulnerable group`), 
                                     NA, vul_wide_sums$Vul_Counts)
############ Combining 

full_data <- inner_join(finan_wide_sums, loss_wide_sums, by = "Country")

full_data <- inner_join(full_data, vul_wide_sums, by = "Country")

# writing csv 

write.csv(full_data, "full_data.csv")
