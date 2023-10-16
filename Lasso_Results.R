### loading previous vars 
rep_data <- read.csv("~/Dropbox (jl0049a)/2022_joint_Research_Todd_Jennifer_Karl/political_constraints_ambition/updated_mitigation.csv")
rep_data_cl <- rep_data[,-c(43:196)]
rep_short <- rep_data_cl[, -c(3:4, 44:70, 74:100)]
full_data <- read.csv("full_data.csv")
### merged 
merged <- merge(rep_short, full_data, by = "country.code")


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