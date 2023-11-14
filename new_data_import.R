cleaned_full <- read.csv("cleaned_full.csv")

### importing new data 

corruption <- read.csv("corruption.csv")
accountability <- read.csv("accountability.csv")
public_services <- read.csv("public_service_effect.csv")
pol_stability <- read.csv("political_stabiility.csv")

#### 

names(corruption) <- c("indicator", "code", "country.name", "country.code",
                       "2000", "2013", "2014", "2015", "2016", "2017",
                       "2018", "2019", "2020", "cor2021", "2022")
names(accountability) <- c("indicator", "code", "country.name", "country.code",
                       "2000", "2013", "2014", "2015", "2016", "2017",
                       "2018", "2019", "2020", "acc2021", "2022")
names(public_services) <- c("indicator", "code", "country.name", "country.code",
                           "2000", "2013", "2014", "2015", "2016", "2017",
                           "2018", "2019", "2020", "pub2021", "2022")
names(pol_stability) <- c("indicator", "code", "country.name", "country.code",
                           "2000", "2013", "2014", "2015", "2016", "2017",
                           "2018", "2019", "2020", "pol2021", "2022")


#### adding data by year subsets 

cleaned_2021 <- subset(cleaned_full, subset = cleaned_full$year == 2021)
c_2021 <- corruption[,c(4, 14)]
a_2021 <- accountability[,c(4, 14)]
pub_2021 <- public_services[,c(4, 14)]
pol_2021 <- pol_stability[,c(4, 14)]


full_2021 <- merge(cleaned_2021, c_2021, by = "country.code")

full_2021_a <- merge(full_2021, a_2021, by = "country.code")

full_2021_pub <- merge(full_2021_a, pub_2021, by = "country.code")

full_2021_sub <- merge(full_2021_pub, pol_2021, by = "country.code")

full_2021_sub <- full_2021_sub %>% mutate(
  cor2021 = as.numeric(cor2021),
  acc2021 = as.numeric(acc2021),
  pub2021 = as.numeric(pub2021),
  pol2021 = as.numeric(pol2021)
)

####### visualizing 

full_2021_sub %>% ggplot(aes(cor2021, Country.x)) + geom_()
ggpairs(full_2021_sub[c(41:44)])



