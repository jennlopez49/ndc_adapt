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