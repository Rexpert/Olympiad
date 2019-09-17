# setwd("~/R/Machine Learning/Olympiad")
# rm(list = ls())

# Objective:
# 1. Which country acquire most reward in this competition
# 2. Which country send most representative to this competition
# 3. Which question is the hardest?

library(readxl)
library(dplyr)
library(tidyr)
library(forcats)
library(ggplot2)

result <- read_excel("./data/1541082607.xlsx", sheet = 1, skip = 1, n_max = 218)
resultInd <- read_excel("./data/1541082607.xlsx", sheet = 1, skip = 221, n_max = 8)
participant <- read_excel("./data/1541082607.xlsx", sheet = 2)

result <- result[,-1]
colnames(participant) <- c("#", "Country", "University", colnames(participant)[c(-1:-3)])
participant$University <- replace(participant$University, which(participant$University == "Peking Univiversity"), "Peking University")


participant <- participant %>%
  sapply(function(x) {
    gsub("_+", NA, x)
  }) %>% 
  data.frame(stringsAsFactors = F) %>%
  mutate(student_count = sapply(1:nrow(participant), function(x) {
    sum(!is.na((.)[x,6:9]))
  })) %>%
  mutate(lecturer_count = sapply(1:nrow(participant), function(x) {
    sum(!is.na((.)[x,4:5]))
  })) %>%
  .[, c(2,3,10,11)]

# Absent participants
newData <- data.frame(c("Tajikistan", "Uzbekistan", "Uzbekistan"),
                      c("Tajik National University", "University of World Economy and Diplomacy", "Denov Branch of Termez State University"),
                      c(4,2,1),
                      c(0,0,0))

colnames(newData) <- colnames(participant)
participant <- rbind(participant, newData)
participant <- participant[order(participant$Country),]

result <- left_join(result, participant[, c("University","Country")], by = "University")

# 1. Which country acquire most reward in this competition----------------------

medal <- result %>%
  group_by(Country) %>%
  summarise(Gold = sum(Price == "Gold Medal"),
            Silver = sum(Price == "Silver Medal"),
            Bronze = sum(Price == "Bronze Medal"),
            Honour = sum(Price == "Honourable Mention"),
            Cert = sum(Price == "Certificate"))

medal <- medal %>%
  mutate(Total = Gold + Silver + Bronze + Honour + Cert) %>%
  mutate(Gold = Gold/Total*100,
         Silver = Silver/Total*100,
         Bronze = Bronze/Total*100,
         Honour = Honour/Total*100,
         Cert = Cert/Total*100,
         Total = NULL)

medal <- medal[order(medal$Gold, medal$Silver, medal$Bronze, medal$Honour, 
                     medal$Cert, decreasing = T),]
medal$Country <- factor(medal$Country, levels = medal$Country)
tidymedal <- gather(medal, "award", "percentage", -1, factor_key = T)
tidymedal$Country <- fct_rev(tidymedal$Country)
tidymedal$award <- fct_rev(tidymedal$award)

medalColour <- c("#000000", "#FFFFFF", "#A77044", "#A7A7AD", "#D6AF36")
ggplot(tidymedal, aes(Country, percentage, fill = award)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  ggtitle("Awards received by each Country in Mathematical Olympiad") +
  theme(plot.title = element_text(size = 20, hjust = 0.5)) +
  scale_fill_manual(values = medalColour) +
  guides(fill = guide_legend(title = "Award", reverse = T)) +
  coord_flip()

# 2. Which country send most representative to this competition-----------------

participant %>%
  group_by(Country) %>%
  summarise(students = sum(student_count)) %>%
  ggplot(aes(x = reorder(Country, students), y = students, fill = Country)) +
  geom_col() +
  ggtitle("Number of representatives for each Country") +
  theme(legend.position = "none", 
        plot.title = element_text(size = 20, hjust = 0.5)) +
  labs(x = "Country", y = "Number of participants") +
  coord_flip()


# 3. Which question is the hardest?---------------------------------------------

result <- result[3:7] %>%
  rbind(resultInd[4:8])

total <- result %>%
  colSums() %>%
  as.data.frame()

colnames(total) <- "totalMark"

total %>%
  mutate(totalMark = totalMark/(8 * nrow(result))) %>%
  ggplot(aes(x = row.names(total), y = totalMark, fill = row.names(total))) +
  geom_col() +
  ggtitle("Probability for a full-mark answer") +
  labs(x = "Question", y = "Probability") +
  ylim(0, 1) +
  theme(plot.title = element_text(size = 20, hjust = 0.5), 
        legend.position = "none")
