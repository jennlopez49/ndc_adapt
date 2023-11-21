#### Financial Model for 2021
full_2021_sub$finan_needs_bin <- ifelse(full_2021_sub$Financial.needs.for.implementation > 0, 1, 0)
ren_wide <- read.csv("~/Dropbox (jl0049a)/2022_joint_Research_Todd_Jennifer_Karl/political_constraints_ambition/renewable-longitudinal-data_0828.csv")
ren_data <- ren_wide %>% subset(subset = ren_wide$year == 2021)

full_2021_complete <- merge(full_2021_sub, ren_data, by = "country.code")

financial_mod <- glm(finan_needs_bin ~ Economic.loss.and.damage + 
                       Non.economic.loss.and.damage + income_class + 
                       cor2021 + pol2021 + acc2021 + pop, 
                     data = full_2021_complete,
                     family = "binomial")
summary(financial_mod)


financial_mod_pub <- glm(finan_needs_bin ~ Economic.loss.and.damage + 
                       Non.economic.loss.and.damage + income_class + 
                       pub2021 + pol2021 + acc2021 + pop, data = full_2021_complete,
                     family = "binomial")
summary(financial_mod_pub)

financial_mod_g20 <- glm(finan_needs_bin ~ Economic.loss.and.damage + 
                           Non.economic.loss.and.damage + g20_class + 
                           pub2021 + pol2021 + acc2021 + pop, data = full_2021_complete,
                         family = "binomial")
summary(financial_mod_g20)

financial_cor_g20 <- glm(finan_needs_bin ~ Economic.loss.and.damage + 
                           Non.economic.loss.and.damage + g20_class + 
                           cor2021 + pol2021 + acc2021 + pop, data = full_2021_complete,
                         family = "binomial")
summary(financial_cor_g20)

stargazer(financial_mod, financial_mod_pub, financial_cor_g20, financial_mod_g20, type = "text",
         dep.var.labels = "Financial Needs Mentioned",
         covariate.labels = c("Economic Loss and Damage",
         "Non-Economic Loss and Damage",
         "Income Class of Country", "G20",
         "Corruption", "Public Service Provision", "Political Instability/Violence",
         "Accountability", "Population", "Constant"),
         out = "financial_models.html"
          )

### Loss and Damage 
full_2021_complete$loss_damage <- ifelse(full_2021_complete$Loss.and.damage.mentioned > 0, 1, 0)
loss_mod <- glm(loss_damage ~ income_class + oil.rent + polyarchya + 
                       cor2021 + pol2021 + acc2021 + pop, 
                     data = full_2021_complete,
                     family = "binomial")



loss_mod_pub <- glm(loss_damage ~ income_class + oil.rent + polyarchya + 
                           pub2021 + pol2021 + acc2021 + pop, data = full_2021_complete,
                         family = "binomial")


loss_mod_g20 <- glm(loss_damage ~ g20_class + oil.rent + polyarchya + 
                           pub2021 + pol2021 + acc2021 + pop, data = full_2021_complete,
                         family = "binomial")


loss_cor_g20 <- glm(loss_damage ~  g20_class + oil.rent + polyarchya + 
                           cor2021 + pol2021 + acc2021 + pop, data = full_2021_complete,
                         family = "binomial")


stargazer(loss_mod, loss_mod_pub,loss_mod_g20, loss_cor_g20, type = "text",
          dep.var.labels = "Loss and Damage Mentioned",
          covariate.labels = c("Income Class of Country", "G20",
                               "Oil Rents", "Polyarchy (Democracy)",
                               "Corruption", "Public Service Provision", 
                               "Political Instability/Violence",
                               "Accountability", "Population", "Constant"),
          out = "loss_models.html"
)


### Non-economic Loss and Damage

full_2021_complete$non_econ_loss_damage <- ifelse(full_2021_complete$Non.economic.loss.and.damage > 0, 1, 0)
nonloss_mod <- glm(non_econ_loss_damage ~ income_class + oil.rent + polyarchya + 
                  cor2021 + pol2021 + acc2021 + pop, 
                data = full_2021_complete,
                family = "binomial")



nonloss_mod_pub <- glm(non_econ_loss_damage ~ income_class + oil.rent + polyarchya + 
                      pub2021 + pol2021 + acc2021 + pop, data = full_2021_complete,
                    family = "binomial")


nonloss_mod_g20 <- glm(non_econ_loss_damage ~ g20_class + oil.rent + polyarchya + 
                      pub2021 + pol2021 + acc2021 + pop, data = full_2021_complete,
                    family = "binomial")


nonloss_cor_g20 <- glm(non_econ_loss_damage ~  g20_class + oil.rent + polyarchya + 
                      cor2021 + pol2021 + acc2021 + pop, data = full_2021_complete,
                    family = "binomial")


stargazer(nonloss_mod, nonloss_mod_pub,nonloss_mod_g20, nonloss_cor_g20, type = "text",
          dep.var.labels = "Non-Economic Loss and Damage Mentioned",
          covariate.labels = c("Income Class of Country", "G20",
                               "Oil Rents", "Polyarchy (Democracy)",
                               "Corruption", "Public Service Provision",
                               "Political Instability/Violence",
                               "Accountability", "Population", "Constant"),
          out = "non_econloss_models.html"
)

