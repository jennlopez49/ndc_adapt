## importing data from Climate Watch 
adapt_general <- read.csv("ndc_content-2/ndc_content.csv")
adapt_targets <- read.csv("ndc_content-3/ndc_content.csv")
adapt_vision <- read.csv("ndc_content-4/ndc_content.csv")


######### General Adaptation Data ----------------------------------------------
adapt_gen <- adapt_general
# changing NAs to actual NAs 
nas <- c("N/A", "Not Applicable", "Not related", "Not Related", "no new",
         "no document submitted")
adapt_gen$Value <- ifelse(grepl(paste(nas, collapse = "|"), adapt_gen$Value, 
                                ignore.case = TRUE),
                                NA, adapt_gen$Value)

# string matches that will be changed to 0 
matches <- c("not included", "not mentioned", "does not",
             "no future losses indicated", "no losses indicated", "not Specified")
adapt_gen$Value <- ifelse(grepl(paste(matches, collapse = "|"), adapt_gen$Value,
                                ignore.case = TRUE), 0, 
                            adapt_gen$Value)

# string matches that will be changed to 1 
id <- c("identified", "identifies",  "included", "includes", "mentioned", "yes",
        "revised")
adapt_gen$Value <- ifelse(grepl(paste(id, collapse = "|"), adapt_gen$Value,
                                ignore.case = TRUE), 1, 
                          adapt_gen$Value)


## some indicators have "no" but to avoid replacing data that could be a "1" 

nos <- adapt_gen[adapt_gen$Value == "No",]
nos <- nos %>% drop_na(ISO)

## comparing to grepl -- pulls out from quotes 

# nos_grep <- adapt_gen[grepl("No", adapt_gen$Value),]

adapt_gen$Value <- ifelse(adapt_gen$Value == "No", "0", adapt_gen$Value)

write.csv(adapt_gen, "adapt_general_clean.csv")

### TO SWITCH TO WIDE FORM SO EACH COLUMN HAS ONLY INDICATOR FOR EACH COUNTRY
## switch AFTER finishing hand-coding the last of the string variables


# # grouping by indicator name

# adapt_gen_nodup <- adapt_gen %>% group_by(Country, Indicator.name) %>% 
#   summarise_all(list(~toString(unique(na.omit(.)))))

adapt_gen$duplicate <- duplicated(adapt_gen$Indicator.ID)
adapt_gen$dup_num <- ifelse(adapt_gen$duplicate == TRUE, 1, 0)

#########  Adaptation Target Data ----------------------------------------------

## Replacing "Not Available" with NAs

adapt_tar <- adapt_targets

adapt_tar$Value <- ifelse(adapt_tar$Value == "Not Available", NA, 
                          adapt_tar$Value)
adapt_tar <- adapt_tar %>% drop_na(Value)


write.csv("adapt_tar", "adapt_targets.csv")

## pulling out financial targets 

financial_targets <- adapt_tar[grepl("USD", adapt_tar$Value),]

# cleaning - getting rid of "spent" costs 
financial_targets <- financial_targets[!grepl("spent", financial_targets$Value),]

### list of countries 

countries_finan <- unique(financial_targets$Country)

write.csv(countries_finan, "countries_financial_targets.csv")


# duplicates

adapt_tar$duplicate <- duplicated(adapt_tar$Indicator.ID)
adapt_tar$dup_num <- ifelse(adapt_tar$duplicate == TRUE, 1, 0)

# collapsing -- no duplicates  --- after converting everything to numerical

# adapt_tar_nodup <- adapt_tar %>% group_by(Country, Indicator.ID) %>%
#   summarise_all(list(~toString(unique(na.omit(.)))))

#########  Adaptation Vision Data ----------------------------------------------

adapt_vis <- adapt_vision

# reusing the string matches from earlier

adapt_vis$Value <- ifelse(grepl(paste(nas, collapse = "|"), adapt_vis$Value, 
                                ignore.case = TRUE),
                          NA, adapt_vis$Value)
adapt_vis$Value <- ifelse(grepl(paste(matches, collapse = "|"), adapt_vis$Value,
                                ignore.case = TRUE), 0, 
                          adapt_vis$Value)
adapt_vis$Value <- ifelse(grepl(paste(id, collapse = "|"), adapt_vis$Value,
                                ignore.case = TRUE), 1, 
                          adapt_vis$Value)
# duplicates 

adapt_vis$duplicate <- duplicated(adapt_vis$Indicator.ID)
adapt_vis$dup_num <- ifelse(adapt_vis$duplicate == TRUE, 1, 0)

