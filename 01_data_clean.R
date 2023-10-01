## importing data from Climate Watch 
adapt_general <- read.csv("ndc_content-2/ndc_content.csv")
adapt_targets <- read.csv("ndc_content-3/ndc_content.csv")
adapt_vision <- read.csv("ndc_content-4/ndc_content.csv")

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

# grouping by indicator name
adapt_gen_nodup <- adapt_gen %>% group_by(Country, Indicator.name) %>% 
  summarise_all(list(~toString(unique(na.omit(.)))))






