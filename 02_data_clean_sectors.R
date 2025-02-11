adapt_gen <- read.csv("adapt_general_clean.csv")

categories <- unique(adapt_gen$Overview.category)

categories_match <- c("Financial Needs", "Loss and Damage", "Vulnerability")

### Subsetting 

adapt_gen_subset <- subset(adapt_gen, 
                           subset = grepl(paste(categories_match, collapse = "|"), adapt_gen$Overview.category,
                                                                         ignore.case = TRUE) == TRUE)

## quotes -- 206 left 
adapt_sub_quotes <- adapt_gen_subset[!adapt_gen_subset$Value == 0,]
adapt_sub_quotes <- adapt_sub_quotes[!adapt_sub_quotes$Value == 1,]
adapt_sub_quotes <- adapt_sub_quotes[!is.na(adapt_sub_quotes$Value),]

##### CSV File

write.csv(adapt_gen_subset, "adapt_gen_subset_new.csv")


### duplicated only -- to fill in NAs 

adapt_removed <- adapt_general[duplicated(adapt_general),]


