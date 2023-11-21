## Visualizations of all indicators 

# By criteria -- financial 
ggpairs(full_data[,c(2:4)])


# By criteria -- costs 
ggpairs(full_data[,c(5:14)])


# By criteria - vulnerability 
ggpairs(full_data[,c(15:17)])

# histograms --- financial 

fin_hist <- full_data %>% ggplot(aes(`Financial needs for implementation`, fill = "blue")) + 
  geom_histogram(bins = 5) + stat_bin(binwidth = 1, 
                                                     geom = "text", 
                                                     color = "white", 
                              aes(label = ..count..),
                              position = position_stack(vjust = 0.5))

fin_hist + theme(legend.position = "none") + labs(x = "Mentions for Financial Needs",
                                                  y = "Number of NDCs")

nonfin_hist <- full_data %>% ggplot(aes(`Other non-financial support needs`, fill = "green")) + 
  geom_histogram(bins = 5) + stat_bin(binwidth = 1, 
                                      geom = "text", 
                                      color = "white", 
                                      aes(label = ..count..),
                                      position = position_stack(vjust = 0.5))

nonfin_hist + theme(legend.position = "none") + labs(x = "Mentions for Non-Financial Needs",
                                                  y = "Number of NDCs")

index_finan_hist <- full_data %>% ggplot(aes(Finan_Needs, fill = "green")) + 
  geom_histogram(bins = 9) + stat_bin(binwidth = 1, 
                                      geom = "text", 
                                      color = "black", 
                                      aes(label = ..count..)
                                      # ,
                                      # position = position_stack(vjust = 0.5)
                                      )
index_finan_hist + theme(legend.position = "none") + labs(x = "Mentions for Any Need",
                                                     y = "Number of NDCs")

# histograms --- costs 


loss_hist <- full_data %>% ggplot(aes(`Current economic loss and damage figures`, fill = "black")) + 
  geom_histogram(bins = 5) + stat_bin(binwidth = 1, 
                                      geom = "text", 
                                      color = "white", 
                                      aes(label = ..count..),
                                      position = position_stack(vjust = 0.5))

loss_hist + theme(legend.position = "none") + labs(x = "Mentions of Current Economic Loss and Damage Figures",
                                                     y = "Number of NDCs")

nonecon_hist <- full_data %>% ggplot(aes(`Future economic loss and damage figures`, fill = "black")) + 
  geom_histogram(bins = 5) + stat_bin(binwidth = 1, 
                                      geom = "text", 
                                      color = "white", 
                                      aes(label = ..count..),
                                      position = position_stack(vjust = 0.5))

nonecon_hist + theme(legend.position = "none") + labs(x = "Mentions of Future Economic Loss and Damage Figures",
                                                   y = "Number of NDCs")

loss_men_hist <- full_data %>% ggplot(aes(`Loss and damage mentioned`, fill = "black")) + 
  geom_histogram(bins = 5) + stat_bin(binwidth = 1, 
                                      geom = "text", 
                                      color = "white", 
                                      aes(label = ..count..),
                                      position = position_stack(vjust = 0.5))
loss_men_hist + theme(legend.position = "none") + labs(x = "Mentions of Loss and Damage",
                                                   y = "Number of NDCs")

nonloss_men_hist <- full_data %>% ggplot(aes(`Non-economic loss and damage`, fill = "black")) + 
  geom_histogram(bins = 5) + stat_bin(binwidth = 1, 
                                      geom = "text", 
                                      color = "white", 
                                      aes(label = ..count..),
                                      position = position_stack(vjust = 0.5))
nonloss_men_hist + theme(legend.position = "none") + labs(x = "Mentions of Non-Economic Loss and Damage",
                                                       y = "Number of NDCs")
index_loss <- full_data %>% ggplot(aes(Loss_Counts, fill = "black")) + 
  geom_histogram(bins = 29) + stat_bin(binwidth = 1, 
                                      geom = "text", 
                                      color = "black", 
                                      aes(label = ..count..)
                                      ,
                                      position = position_nudge(y = -1)
                                      )
index_loss + theme(legend.position = "none") + labs(x = "Mentions of Any Kind of Loss",
                                                          y = "Number of NDCs")

### Vulnerability 

vul_children <- full_data %>% ggplot(aes(`Identification.of.children.as.a.vulnerable.group`, fill = "black")) + 
  geom_histogram(bins = 29) + stat_bin(binwidth = 1, 
                                       geom = "text", 
                                       color = "black", 
                                       aes(label = ..count..)
                                       ,
                                       position = position_nudge(y = -1)
  )
vul_children + theme(legend.position = "none") + labs(x = "Mentions Children as Vulnerable Group",
                                                    y = "Number of NDCs")

vul_young<- full_data %>% ggplot(aes(`Identification.of.young.people.as.a.vulnerable.group`, fill = "black")) + 
  geom_histogram(bins = 29) + stat_bin(binwidth = 1, 
                                       geom = "text", 
                                       color = "black", 
                                       aes(label = ..count..)
                                       ,
                                       position = position_nudge(y = -1)
  )
vul_young + theme(legend.position = "none") + labs(x = "Mentions Young People as Vulnerable Group",
                                                      y = "Number of NDCs")

index_vul <- full_data %>% ggplot(aes(Vul_Counts, fill = "black")) + 
  geom_histogram(bins = 29) + stat_bin(binwidth = 1, 
                                       geom = "text", 
                                       color = "black", 
                                       aes(label = ..count..)
                                       ,
                                       position = position_nudge(y = -1)
  )

index_vul + theme(legend.position = "none") + labs(x = "Mentions Vulnerable Group",
                                                   y = "Number of NDCs")



#### non-Econ Loss vs Econ Loss for 2021 

color <- c(rep("red", 1.0), rep("black", 0.0))

p1 <- full_2021_complete %>% ggplot(aes(loss_damage,
                                        fill = factor(loss_damage))) + 
  geom_histogram(bins = 3, show.legend = F) + 
  labs(x = "Economic Loss and Damage Mentioned", y = "Count")

p2 <- full_2021_complete %>% ggplot(aes(non_econ_loss_damage,
                                        fill = factor(non_econ_loss_damage))) + 
  geom_histogram(bins = 3) + 
  labs(x = "Non-economic Loss and Damage Mentioned", y = NULL)  + 
  scale_fill_discrete(name = "Mention Binary Indicator")

p1 + p2


## Financial mentions by income 

p.1 <- full_2021_complete %>% ggplot(aes(finan_needs_bin,
                                  fill = factor(income_details))) + 
  geom_histogram(bins = 3) + labs(x = NULL, y = "Count")  + 
  scale_fill_discrete(name = "Income Classification")

p.2 <- full_2021_complete %>% ggplot(aes(finan_needs_bin,
                                  fill = factor(region_name))) + 
  geom_histogram(bins = 3) + labs(x = NULL, y = NULL) + 
  scale_fill_discrete(name = "Region")


both <- p.1 + p.2 

wrap_elements(panel = both) +
  labs(tag = "Financial Needs Mentioned") +
  theme(
    plot.tag = element_text(size = rel(1)),
    plot.tag.position = "bottom"
  )


### Full dataset for comparison 
cleaned_full$financial_bin <- ifelse(cleaned_full$Financial.needs.for.implementation > 0, 1, 0)
p.1_full <- cleaned_full %>% ggplot(aes(financial_bin,
                                         fill = factor(income_details))) + 
  geom_histogram(bins = 3) + labs(x = NULL, y = "Count") + 
  scale_fill_discrete(name = "Income Classification")

p.2_full <- cleaned_full %>% ggplot(aes(financial_bin,
                                         fill = factor(region_name))) + 
  geom_histogram(bins = 3) + labs(x = NULL, y = NULL) + 
  scale_fill_discrete(name = "Region")


both_full <- p.1_full + p.2_full

wrap_elements(panel = both_full) +
  labs(tag = "Financial Needs Mentioned, Full Dataset") +
  theme(
    plot.tag = element_text(size = rel(1)),
    plot.tag.position = "bottom"
  )

adapt <- read.csv("adapt_general_clean.csv") 
adapt <- adapt[,-c(6,7)]

