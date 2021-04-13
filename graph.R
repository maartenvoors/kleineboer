# Land Deals Figure Voedselbundel
# Author: Mandy Malan, Maarten Voors (WUR)

# Data are from https://landmatrix.org/
# Script to create a graph with all concluded land deals in African countries over time.
# The land matrix data is a bit strenuous to work with as there is no variable for the year in which a contract was concluded 
# but instead the negotiation status is given as a series of strings per deal: 
# e.g.: 2008##Intended (Under negotiation)|2009-01-09##Concluded (Contract signed)
# I wrote a script to split this string up into 3 phases. Then for each phase, I seperately split up the string into year and activity (numbered according to phase).
# Sometimes a contract is concluded in the first phase, sometimes in the third. 
# I make one contract and year variable with the type of contract that was reached and in which year it was reached by going through the phases. If a contract
# was reached in the first phase, I use this year. If another contract was reached in a later phase, I ignore this. I therefore only record the first time
# a contract was concluded. I do this for oral and signed agreements.

# Load packages
library(tidyr)
library(dplyr)
library(ggplot2)

# Load data
setwd("[SET YOUR DIRECTORY]")
deals = read.csv2("deals.csv")

# Clean data
deals_con = deals %>%
  filter(Current.negotiation.status == 'Concluded (Contract signed)' | Current.negotiation.status == 'Concluded (Oral Agreement)') %>%
  subset(select = c('Deal.ID', 'Current.size.under.contract', 'Current.size.in.operation..production.', 'Current.negotiation.status', 'Current.implementation.status', 'Negotiation.status', 'Implementation.status'))

x= deals_con %>% separate(Negotiation.status, into = c("phase1", "phase2", 'phase3'), sep = "\\|", remove=F) #for 4 observations, there are more than 3 phases but all of them concern a change of ownership
x2 = x %>% separate(phase1, into = c("year1.1", "activity1"), sep = "\\##", remove = F)
x3 = x2 %>% separate(year1.1, into = c("year1", "activity1.1"), sep = "\\#current#", remove = T)
x3$activity1 = ifelse(is.na(x3$activity1), x3$activity1.1, x3$activity1)
x3$activity1.1 = NULL
x3$year1 = sub("-\\S*", "", x3$year1) #remove month and day if known

x4 = x3 %>% separate(phase2, into = c("year2.1", "activity2"), sep = "\\##", remove = F)
x5 = x4 %>% separate(year2.1, into = c("year2", "activity2.1"), sep = "(\\#current)", remove = T)
x5$activity2 = ifelse(is.na(x5$activity2), x5$activity2.1, x5$activity2)
x5$activity2.1 = NULL
x5$year2 = sub("-\\S*", "", x5$year2) #remove month and day if known

x6 = x5 %>% separate(phase3, into = c("year3.1", "activity3"), sep = "\\##", remove = F)
dfPlot = x6 %>% separate(year3.1, into = c("year3", "activity3.1"), sep = "(\\#current)", remove = T)
dfPlot$activity3 = ifelse(is.na(dfPlot$activity3), dfPlot$activity3.1, dfPlot$activity3)
dfPlot$activity3.1 = NULL
dfPlot$year3 = sub("-\\S*", "", dfPlot$year2) #remove month and day if known

dfPlot$contract = ifelse(dfPlot$activity1 == 'Concluded (Contract signed)' | dfPlot$activity1 == 'Concluded (Oral Agreement)', dfPlot$activity1, NA)
dfPlot$year = ifelse(!is.na(dfPlot$contract), dfPlot$year1, NA)
dfPlot$year = ifelse(is.na(dfPlot$contract) & 
                       ((grepl('Concluded (Contract signed)', dfPlot$activity2, fixed = T) | grepl('Concluded (Oral Agreement)', dfPlot$activity2, fixed = T))), 
                     dfPlot$year2, dfPlot$year)
dfPlot$contract = ifelse(is.na(dfPlot$contract) & 
                           ((grepl('Concluded (Contract signed)', dfPlot$activity2, fixed = T) | grepl('Concluded (Oral Agreement)', dfPlot$activity2, fixed = T))), 
                         dfPlot$activity2, dfPlot$contract)
dfPlot$year = ifelse(is.na(dfPlot$contract) & 
                       ((grepl('Concluded (Contract signed)', dfPlot$activity3, fixed = T) | grepl('Concluded (Oral Agreement)', dfPlot$activity3, fixed = T))), 
                     dfPlot$year3, dfPlot$year)
dfPlot$contract = ifelse(is.na(dfPlot$contract) & 
                           ((grepl('Concluded (Contract signed)', dfPlot$activity3, fixed = T) | grepl('Concluded (Oral Agreement)', dfPlot$activity3, fixed = T))), 
                         dfPlot$activity3, dfPlot$contract)
dfPlot$year = as.numeric(dfPlot$year)

dfPlot = dfPlot %>%
  dplyr::select(Deal.ID, year, Current.implementation.status, Current.negotiation.status, Current.size.under.contract, Current.size.in.operation..production.)

# Plot
p = ggplot(data = dfPlot, aes(x = as.numeric(year)))+
  geom_histogram(binwidth = 1, color = 'grey1')+
  xlim(1975,2020) +
  labs(title = 'Aantal gesloten landdeals in Afrikaanse landen van 1975 tot heden', y = 'Aantal deals', x='Jaar')+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
ggsave(p, filename = 'deals_from1975.png', width = 10, height = 8)

# Some additional stats
table(dfPlot$Current.implementation.status) #frequency table implementation status

dfActive = dfPlot %>%
  filter(!is.na(dfPlot$year))
prop.table(table(dfActive$Current.implementation.status)) #proportion table for implementation status

dfActive$land_percentage = dfActive$Current.size.in.operation..production./dfActive$Current.size.under.contract
dfActive$land_percentage = ifelse(is.nan(dfActive$land_percentage), NA, dfActive$land_percentage) # percentage of contracted land in operation

# reveal graph
p
