### loading previous vars ---------
rep_data <- read.csv("~/Dropbox (jl0049a)/2022_joint_Research_Todd_Jennifer_Karl/political_constraints_ambition/updated_mitigation.csv")
rep_data_cl <- rep_data[,-c(43:196)]
rep_short <- rep_data_cl[, -c(3:4, 44:70, 74:100)]
full_data <- read.csv("full_data.csv")
### merged 
merged <- merge(rep_short, full_data, by = "country.code")

### Calculating/Creating New vars --------
### C02 & GHG Change 
merged$co2_1990 <- as.numeric(merged$co2_1990)

merged$change_co2 <- merged$co2_2018 - merged$co2_1990
merged$change_ghg <- merged$ghg2019 - merged$ghg1990

### Adding Binary Vars 
# financial
merged$finan_needs_imp <- ifelse(merged$Financial.needs.for.implementation > 0, 1, 0)
merged$Other_needs <- ifelse(merged$Other.non.financial.support.needs > 0, 1, 0)
# vulnerability & costs 
merged$costs <- ifelse(merged$Current.economic.loss.and.damage.figures > 0, 1, 0)
merged$future_costs <- ifelse(merged$Future.economic.loss.and.damage.figures > 0, 1, 0)

merged$loss_damage <- ifelse(merged$Loss.and.damage.mentioned > 0, 1, 0)
merged$non_econ_loss <- ifelse(merged$Non.economic.loss.and.damage > 0, 1, 0)

# Migration
merged$migration <- ifelse(merged$Human.mobility > 0, 1, 0)


## Getting rid of certain vars 
merged_clean <- merged[, -c(8:13)]
merged_clean <- subset(merged_clean, select = -c(X2nd_party, X3rd_party,
                                  top_party, Country.y, ambition_dv))
### Variable Selection -----
reg_data <- merged_clean[, -c(1:2)]
reg_data <- subset(reg_data, select = -c(coal_elec_2015, oil_elec_2015,
                                         renew_2015, hydro_2015, green_party_votes.y,
                                         green_party_seats.y,
                                         eff_num_parties_13,eff_num_parties_14,
                                         eff_num_parties_15, eff_num_parties_16,
                                         eff_num_parties_17, eff_num_parties_18,
                                         eco.party.man, env.party.man, 
                                         green_party_seats.x, const_hosterml,
                                         WJP_Rule_of_Law_19, green_party_votes.x,
                                         wjp_rule_18,govt_spend_18, 
                                         govt_spend_19, green_party_dummy.x
                                         ))
reg_full <- lm(Financial.needs.for.implementation ~ .  -Other.non.financial.support.needs
                - Finan_Needs 
               - finan_needs_imp - Other_needs - party_pos_env
               , 
               data = reg_data)
reg_null <- lm(Financial.needs.for.implementation ~ 1, data = merged_clean)

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
