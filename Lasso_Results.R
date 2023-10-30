### loading previous vars ---------
ren_data <- read.csv("~/Dropbox (jl0049a)/2022_joint_Research_Todd_Jennifer_Karl/political_constraints_ambition/renewable-longitudinal-data_0828.csv")
ren_wide <- ren_data %>% subset(subset = ren_data$year == 2021)

# emissions -- stops in 2018 
# em_data <- read.csv("~/Dropbox (jl0049a)/2022_joint_Research_Todd_Jennifer_Karl/political_constraints_ambition/updated_mitigation.csv")
# em_short <- ren_data[, c(2, 197,225, 227, 255)]

### ndc data
full_data <- read.csv("full_data.csv")

## submissions years
sub_years <- readxl::read_excel("years_submissions.xlsx")
sub_years$submission_year <- ifelse(sub_years$submission_year == "NA", NA,
                                    sub_years$submission_year)
### merged 
merged <- merge(ren_wide, full_data, by = "country.code")

merged_full <- merge(merged, sub_years, by = "country.code")

### FILTERING TO ONLY 2021 

merged_2021 <- merged_full %>% filter(submission_year == "2021")

### Calculating/Creating New vars --------
# ### C02 & GHG Change 
# merged_full$co2_1990 <- as.numeric(merged_full$co2_1990)
# 
# merged_full$change_co2 <- merged_full$co2_2018 - merged_full$co2_1990
# merged_full$change_ghg <- merged_full$ghg2018 - merged_full$ghg1990

### Adding Binary Vars 
# financial
merged_2021$finan_needs_imp <- ifelse(merged_2021$Financial.needs.for.implementation > 0, 1, 0)
merged_2021$Other_needs <- ifelse(merged_2021$Other.non.financial.support.needs > 0, 1, 0)
# vulnerability & costs 
merged_2021$costs <- ifelse(merged_2021$Current.economic.loss.and.damage.figures > 0, 1, 0)
merged_2021$future_costs <- ifelse(merged_2021$Future.economic.loss.and.damage.figures > 0, 1, 0)

merged_2021$loss_damage <- ifelse(merged_2021$Loss.and.damage.mentioned > 0, 1, 0)
merged_2021$non_econ_loss <- ifelse(merged_2021$Non.economic.loss.and.damage > 0, 1, 0)

# Migration
merged_2021$migration <- ifelse(merged_2021$Human.mobility > 0, 1, 0)

### Checking for NAs -- looks like Taiwan, Venezula, Yemen, South Sudan
countries <- merged_2021 %>% group_by(country.name) %>% 
  summarise(across(everything(), ~ sum(is.na(.))))

### Checking by Category --- fuel_pct, prev.leader, reelect, turnover_a, vulnerable group vars 
merged_2021 %>% 
  summarise(across(everything(), ~ sum(is.na(.))))

### saving 

write.csv(merged_full, "merged_full.csv")

write.csv(merged_2021, "merged_2021.csv")
### Variable Selection -----
reg_data_countries <- merged_2021 %>% filter(!(country.name %in% c("Taiwan", 
                                                                   "Venezuela", "Yemen",
                                                                   "South Sudan")))

reg_data <- reg_data_countries %>% select(!c(fuel.pct, prev.leader, reelect, 
                                             turnover,
                                             turnover90,
                                             turnover00,
                                             years.in.office,
                                             parliamentary,
                                             finite.term,
                                             term.limit,
                                             explicit.subsidy,
                                             implicit.subsidy,
                                             green_party,
                                             turnover_a,
                                             Identification.of.children.as.a.vulnerable.group,
                                             Identification.of.young.people.as.a.vulnerable.group,
                                             Country.x,
                                             Country.y,
                                             Other.non.financial.support.needs,
                                             Financial.needs.for.implementation,
                                             renewable.generation,
                                             renewable.consumption,
                                             Finan_Needs,
                                             country.code,
                                             country.name))
reg_data_no_nas <- na.omit(reg_data)
reg_full <- lm(finan_needs_imp ~ ., 
               data = reg_data_no_nas)
reg_null <- lm(finan_needs_imp ~ 1, data = reg_data_no_nas)

step_out <- step(reg_null, 
                 scope = list(lower = reg_null, upper = reg_full),
                 method = "forward")
summary(step_out)

### Correlations for Financial Needs 
cor_data <- reg_data[,c(80:117)]
cor_data_sub <- cor_data[, c(12:38)]
cor_data_new <- cor_data_sub %>% na.omit()
correlations <- cor(cor_data_new)
write.csv(correlations, "correlations_climatewatch.csv")
### Lasso 


reg <- lm(finan_needs_imp ~ ., data = reg_data_no_nas) 
x <- model.matrix(reg)
dim(x)
y <- reg_data_no_nas$finan_needs_imp

set.seed(123)
lr_cv <- cv.glmnet(x, y)

plot(lr_cv)

coef(lr_cv)


### Non-financial 

reg_data_nonfin <- reg_data_countries %>% select(!c(fuel.pct, prev.leader, reelect, 
                                                    turnover,
                                                    turnover90,
                                                    turnover00,
                                                    years.in.office,
                                                    parliamentary,
                                                    finite.term,
                                                    term.limit,
                                                    explicit.subsidy,
                                                    implicit.subsidy,
                                                    green_party,
                                             turnover_a,
                                             Identification.of.children.as.a.vulnerable.group,
                                             Identification.of.young.people.as.a.vulnerable.group,
                                             Country.x,
                                             Country.y,
                                             Financial.needs.for.implementation,
                                             Finan_Needs,
                                             finan_needs_imp,
                                             Other.non.financial.support.needs,
                                             country.code,
                                             country.name))
reg_non_cl <- na.omit(reg_data_nonfin)
reg_full_non <- lm(Other_needs ~ ., 
               data = reg_non_cl)
reg_null_non <- lm(Other_needs ~ 1, data = reg_non_cl)

step_out_non <- step(reg_null_non, 
                 scope = list(lower = reg_null_non, upper = reg_full_non),
                 method = "forward")
summary(step_out_non)



reg_other <- lm(Other_needs ~ ., data = reg_non_cl) 
x <- model.matrix(reg_other)
dim(x)
y <- reg_non_cl$Other_needs

set.seed(123)
lr_cv_o <- cv.glmnet(x, y)

plot(lr_cv_o)

coef(lr_cv_o)
