
library(tidyverse)
library(tidyr)
library(foreign)
library(psych)
library(ggplot2)
library(ggrepel)
library(Hmisc)
library(reshape2)
library(metan)
library(ggthemes)
library(haven)

#change directory
#setwd("")

#read file
df1 <- read_dta("Goudarzi&Napier_2025_full.dta")
View(df1)

#change dataset to wide 

df_att_poverty <- subset(df1, select=c('country','respondent_serial','ind4', 'struct4', 'fate3','gov_size', 'labor_reg'))

df_att_poverty_long <- melt(df_att_poverty, id=c("country","respondent_serial","gov_size", "labor_reg"))

df_att_poverty_long[order(df_att_poverty_long$country, df_att_poverty_long$respondent_serial),]

colnames(df_att_poverty_long)[colnames(df_att_poverty_long) == "variable"] ="att_type"

df_att_poverty_filtered <- df_att_poverty_long %>%
  filter(!is.na(value))

# calculate summary statistics

summary_data <- df_att_poverty_filtered %>%
  group_by(country, att_type) %>%
  summarise(
    mean_value = mean(value),
    ci_lower = mean_value - qt(0.975, n()) * sd(value) / sqrt(n()),
    ci_upper = mean_value + qt(0.975, n()) * sd(value) / sqrt(n()),
    .groups = 'drop'  # Ensure that grouping attributes are dropped
  )

# order countries by average of gov_size and labor_reg (neolib_av variable in the country file)

order <- c( "Sweden", "Brazil", "Japan", "South Korea", "Turkey", "UK", "Indonesia", "Hungary", "Russia", "US", "India","Egypt", "Saudi Arabia")

# mapping between numeric country codes and country names

country_mapping <- c( "Brazil", "Egypt", "Hungary", "India", "Indonesia", "Japan", "Russia", 
                      "Saudi Arabia", "South Korea", "Sweden", "Turkey", "UK", "US")

summary_data$country <- country_mapping[summary_data$country]

summary_data$country <- factor(summary_data$country, levels = order)

# check for missing values in 'mean_value', 'ci_lower', and 'ci_upper'
sum(is.na(summary_data$mean_value))
sum(is.na(summary_data$ci_lower))
sum(is.na(summary_data$ci_upper))

# convert country column to factor
summary_data$country <- as.factor(summary_data$country)

# create labels
labels <- c("Structural", "Individualist", "Fatalist")

summary_data$att_type <- factor(summary_data$att_type, levels = c("struct4", "ind4", "fate3"))
y_limits <- c(1, 5)

# plot figure 1

figure1 <- ggplot(summary_data, aes(x = country, y = mean_value, color = att_type, group = interaction(country, att_type))) +
  geom_point(position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.1, position = position_dodge(width = 0.3)) +
  labs(x = "", y = "Mean Value", color = "Attribution Type") +
  scale_color_manual(values = c("red", "blue", "#4CAF50"), labels = labels) +  
  scale_y_continuous(limits = y_limits) +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.key.size = unit(0.8, "lines"),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10),
        panel.grid.major.x = element_blank() 
  )

figure1
ggsave("figure1_lineplot.png", plot = figure1, width = 10, height = 7, units = "in", dpi = 300)


labels1 <- c("Structural", "Individualist")

# Filter the data for att_type values 

filtered_data <- subset(summary_data, att_type %in% c("ind4", "struct4"))

# Then create the plot using the filtered data
figure1_filtered <- ggplot(filtered_data, aes(x = country, y = mean_value, color = att_type, group = interaction(country, att_type))) +
  geom_point(position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.1, position = position_dodge(width = 0.3)) +
  labs(x = "", y = "Mean Value", color = "Attribution Type") +
  scale_color_manual(values = c("red", "blue"), labels = labels1) +  # adjust if needed
  scale_y_continuous(limits = y_limits) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        legend.key.size = unit(0.8, "lines"),
        legend.text = element_text(size = 9),
        legend.title = element_text(size = 10),
        panel.grid.major.x = element_blank()
  )

figure1_filtered


