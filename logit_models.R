### LOGIT MODELS 

merged_full <- read.csv("merged_full.csv")

reg_data_countries <- merged_full %>% filter(!(country.name %in% c("Taiwan", 
                                                                   "Venezuela", "Yemen",
                                                                   "South Sudan")))

reg_data <- reg_data_countries %>% select(!c(fuel.pct, prev.leader, reelect, 
                                             turnover_a,
                                             Identification.of.children.as.a.vulnerable.group,
                                             Identification.of.young.people.as.a.vulnerable.group,
                                             Country.x,
                                             Country.y,
                                             Other.non.financial.support.needs,
                                             Financial.needs.for.implementation,
                                             Finan_Needs,
                                             country.code,
                                             country.name))
### DVs

non_e_dvs <- c("Other_needs", "non_econ_loss")
e_dvs <- c("finan_needs_imp","loss_damage")

## IVs 

non_e_ivs <- list()
non_e_ivs[[1]] <- c("eu","parliamentary", "polyarchym", "Slow.onset.events", 
              "oil.rent", "gdp.p.capita", "pop")
non_e_ivs[[2]] <- c("eu",  "parliamentary","polyarchym", "renewable.capacity", "Slow.onset.events",
              "explicit.subsidy", "oil.rent", "gdp.p.capita", "pop")
non_e_ivs[[3]] <- c("eu", "parliamentary","polyarchym", "renewable.capacity", "Slow.onset.events",
              "Human.mobility", "oil.rent", "gdp.p.capita", "pop")
non_e_ivs[[4]] <- c("eu","parliamentary","polyarchym", "renewable.capacity", "Slow.onset.events",
              "Economic.loss.and.damage", "oil.rent", "gdp.p.capita", "pop")
non_e_ivs[[5]] <- c("eu", "parliamentary","polyarchym", "renewable.capacity", "Slow.onset.events",
             "migration", "oil.rent", "gdp.p.capita", "pop")

e_ivs <- list()

e_ivs[[1]] <- c("eu","parliamentary", "green_party", "non_econ_loss", "govt.spend", 
                    "oil.rent", "gdp.p.capita", "pop")
e_ivs[[2]] <- c("eu", "parliamentary","green_party", "non_econ_loss", "govt.spend",
                    "explicit.subsidy", "oil.rent", "gdp.p.capita", "pop")
e_ivs[[3]] <- c("eu", "parliamentary","green_party", "non_econ_loss", "govt.spend",
                    "migration", "oil.rent", "gdp.p.capita", "pop")
e_ivs[[4]] <- c("eu","parliamentary","green_party", "non_econ_loss", "govt.spend",
                    "Human.mobility", "oil.rent", "gdp.p.capita", "pop")
e_ivs[[5]] <- c("eu", "parliamentary","green_party", "non_econ_loss", "govt.spend",
                    "turnover", "oil.rent", "gdp.p.capita", "pop")

####### Financial Models

fin_mods1 <- list()

for (Y in e_dvs) {
  mod1 <- list()
  for (i in 1:length(e_ivs)) {
    form <- as.formula(paste(Y, " ~ ", paste(e_ivs[[i]], collapse = " + ")))
    mod1[[i]] <- glm(form, data=reg_data, family = "binomial") 
    
    
  }
  fin_mods1[[Y]] <- mod1
}


# Tables 
stargazer(fin_mods1$finan_needs_imp, type = "text")
stargazer(fin_mods1$loss_damage, type = "text")


####### Non-Financial Models

non_fin_mods1 <- list()

for (Y in non_e_dvs) {
  mod1 <- list()
  for (i in 1:length(non_e_ivs)) {
    form <- as.formula(paste(Y, " ~ ", paste(non_e_ivs[[i]], collapse = " + ")))
    mod1[[i]] <- glm(form, data=reg_data, family = "binomial") 
    
    
  }
  non_fin_mods1[[Y]] <- mod1
}


# Tables 
stargazer(non_fin_mods1$Other_needs, type = "text")
stargazer(non_fin_mods1$non_econ_loss, type = "text")
