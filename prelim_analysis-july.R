library(tidyverse)
library(readr)
library(here) # file organization
library(ggeffects) # model predictions
library(lme4)
library(lmerTest)
library(lubridate)
library(ggplot2)
library(dplyr)



burn_data2 <- read_csv("bb_visual_surveys-burn2.csv")

#cleaning burn data

burn_data2$num_holes[60] <- "30-40"
burn_data2 <- burn_data2[-c(147,14, 15), ]
burn_data2$num_holes[is.na(burn_data2$num_holes)] <- 0
burn_data2$tree_cond[22] <- "excellent"
burn_data2$tree_cond[burn_data2$tree_cond == "ex"] <- "excellent"
burn_data2$tree_cond[77] <- "good"
burn_data2$tree_cond[107] <- "good"
burn_data2$date[1] <- "11/20"
colnames(burn_data2)[colnames(burn_data2) == "...1"] <- "date"
burn_data2$tree_sp[c(22, 114) ] <- "quag"
burn_data2$tree_sp[26] <- "qudo"

#checking for duplicates 
colnames(burn_data2)

# Check for duplicates
any(duplicated(colnames(burn_data2)))

colnames(burn_data2) <- make.unique(colnames(burn_data2))

# Convert tree_cond to numeric
burn_data2 <- burn_data2 %>%
  mutate(tree_cond_num = case_when(
    tree_cond == "excellent" ~ 4,
    tree_cond == "good" ~ 3,
    tree_cond == "fair" ~ 2,
    tree_cond == "poor" ~ 1
  ))


#converting ranges to numeric midpoints

library(tidyverse)

burn_data2 <- burn_data2 %>%
  mutate(num_holes = case_when(
    str_detect(num_holes, "-") ~ {
      # Split ranges and calculate midpoints
      str_split(num_holes, "-", simplify = TRUE) %>%
        apply(1, function(x) mean(as.numeric(x), na.rm = TRUE))
    },
    str_detect(num_holes, "\\+") ~ as.numeric(str_remove(num_holes, "\\+")),  # for "50+" take lower bound
    TRUE ~ as.numeric(num_holes) # fallback: convert directly
  ))


# Convert date for burn
burn_data2$date <- as.Date(paste0("2025/", burn_data2$date), format = "%Y/%m/%d")
burn_data2$date[1:20] <- as.Date(format(burn_data2$date[1:20], "2024-%m-%d"))


# Fit the linear mixed model
model <- lmer(tree_cond_num ~ num_holes + date + (1|tree_id), data=burn_data2)

# View summary of model
summary(model)

# View residuals
plot(resid(model))

# ANOVA table for fixed effects
anova(model)

#ggplot of linear model

ggplot(burn_data2, aes(x = num_holes, y = tree_cond_num)) +
  geom_point(alpha = 0.6, color = "forestgreen") +  # Scatterplot of observations
  geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "lightblue") +  # Linear fit + CI
  labs(
    title = "Effect of Bore Holes on Tree Condition Over Time - Burned",
    subtitle = "Each panel shows a survey date",
    x = "Number of Bore Holes",
    y = "Tree Condition (numeric)"
  ) +
  facet_wrap(~ date, ncol = 3, labeller = labeller(date = function(x) format(as.Date(x), "%B %Y"))) +  # Create one panel per date
  theme_minimal(base_size = 14) + 
  theme(panel.grid = element_blank(),
        axis.text = element_text(family = "mono", size = 10),
        plot.title = element_text(hjust = 1.1, family = "mono", size = 10),
        plot.subtitle = element_text(hjust = -.16, family = "mono", size = 10),
        axis.title = element_text(family = "mono"))



# one plot 
burn_data2$date <- as.factor(burn_data2$date)

ggplot(burn_data2, aes(x = num_holes, y = tree_cond_num, color = date)) +
  geom_point(alpha = 0.7, size = 2) +  # scatter points
  geom_smooth(method = "lm", se = FALSE, size = 1) +  # linear fits by date
  scale_color_viridis_d(option = "plasma") +  # prettier color scale
  labs(
    title = "Effect of Bore Holes on Tree Condition Over Time",
    x = "Number of Bore Holes",
    y = "Tree Condition (numeric)",
    color = "Survey Date"
  ) +
  theme_minimal(base_size = 14)



###########################################

#laoding in data

control_data2 <- read_csv("bb_visual_surveys-control2.csv")


#cleaning control data

control_data2 <- control_data2[-c(189,429, 390, 427), ]
control_data2$num_holes[415] <- "10-20"
control_data2$num_holes[283] <- "50"
control_data2$tree_cond[c(415, 282, 283)] <- "good"
control_data2$num_holes[is.na(control_data2$num_holes)] <- 0
control_data2$tree_cond[control_data2$tree_cond == "ex"] <- "excellent"
control_data2$tree_cond[control_data2$tree_cond == "excelent"] <- "excellent"
control_data2$tree_cond[control_data2$tree_cond == "exc3ll3n5"] <- "excellent"
control_data2$tree_cond[control_data2$tree_cond == "exc"] <- "excellent"
control_data2$tree_cond[control_data2$tree_cond == "goood"] <- "good"
control_data2$tree_sp[c(228, 415)] <- "qudo"
control_data2$tree_sp[441] <- "quag"


# Convert tree_cond to numeric
control_data_clean <- control_data2 %>%
  mutate(tree_cond_num = case_when(
    tree_cond == "excellent" ~ 4,
    tree_cond == "good" ~ 3,
    tree_cond == "fair" ~ 2,
    tree_cond == "poor" ~ 1
  ))


# control plot


# Convert date for control
# Add 2025 to the start of each date
control_data_clean$date <- as.Date(paste0("2025/", control_data_clean$date), format = "%Y/%m/%d")
control_data_clean$date[c(1:28, 145:179)] <- as.Date(format(control_data_clean$date[c(1:28, 145:179)], "2024-%m-%d"))

#combine 5/20 and 5/29

control_data_clean$grouped_date <- control_data_clean$date
control_data_clean$grouped_date[control_data_clean$date %in% c("2025-05-20", "2025-05-29")] <- "2025-05-25"
control_data_clean$grouped_date[c(1:28, 145:179)] <- as.Date(format(control_data_clean$grouped_date[c(1:28, 145:179)], "2024-%m-%d"))

#convert ranges to midpoints 
control_data_clean <- control_data_clean %>%
  mutate(num_holes = case_when(
    str_detect(num_holes, "-") ~ {
      # Split ranges and calculate midpoints
      str_split(num_holes, "-", simplify = TRUE) %>%
        apply(1, function(x) mean(as.numeric(x), na.rm = TRUE))
    },
    str_detect(num_holes, "\\+") ~ as.numeric(str_remove(num_holes, "\\+")),  # for "50+" take lower bound
    TRUE ~ as.numeric(num_holes) # fallback: convert directly
  ))


# Fit the linear mixed model
model2 <- lmer(tree_cond_num ~ num_holes + grouped_date + (1|tree_id), data=control_data_clean)

# View summary of model
summary(model2)

# View residuals
plot(resid(model2))

# ANOVA table for fixed effects
anova(model2)


ggplot(control_data_clean, aes(x = num_holes, y = tree_cond_num)) +
  geom_point(alpha = 0.6, color = "forestgreen") +  # Scatterplot of observations
  geom_smooth(method = "lm", se = TRUE, color = "blue", fill = "lightblue") +  # Linear fit + CI
  scale_y_continuous(limits = c(1, 4), breaks = 1:4) +
  scale_x_continuous(breaks = seq(0, 40, by = 20))+
  labs(
    title = "Effect of Bore Holes on Tree Condition Over Time - Unburned",
    subtitle = "Each panel shows a survey date",
    x = "Number of Bore Holes",
    y = "Tree Condition (numeric)"
  ) +
  facet_wrap(~ grouped_date, ncol = 3, labeller = labeller(grouped_date = function(x) format(as.Date(x), "%B %Y"))) +  # Create one panel per date
  theme_minimal(base_size = 14) + 
  theme(panel.grid = element_blank(),
        axis.text = element_text(family = "mono", size = 10),
        plot.title = element_text(hjust = .9, family = "mono", size = 10),
        plot.subtitle = element_text(hjust = -.16, family = "mono", size = 8),
        axis.title = element_text(family = "mono"))


# between species - controlled

#more cleaning

control_data_clean_noNA <- control_data_clean %>%
  drop_na(num_holes, tree_cond_num, tree_sp) %>%
  droplevels()

control_data_clean_noNA$tree_sp[c(220, 432)] <- "quag"
control_data_clean_noNA$tree_sp[c(390)] <- "qudo"

#linear model

model3 <- lmer(tree_cond_num ~ num_holes * tree_sp + (1|tree_id), data = control_data_clean_noNA)


#summary 

summary(model3)

#plot 

plot(resid(model3))

#anova table for mixed effects

anova(model3)

#ggplot 

ggplot(control_data_clean_noNA, aes(x = num_holes, y = tree_cond_num, color = tree_sp)) +
  geom_jitter(alpha = 0.3, na.rm = TRUE) + 
  geom_smooth(method = "lm", se = FALSE, size = 1, na.rm = TRUE) +
  scale_x_continuous(breaks = seq(0, 40, by = 20))+
  labs(
    title = "Effect of Bore Holes on Tree Condition - Unburned",
    x = "# of Boreholes",
    y = "Tree Condition"
  ) +
  facet_wrap(~ tree_sp, drop = TRUE, labeller = as_labeller(c(
    quag = "Coast Live Oak(Quercus agrifolia)",
    qudo = "Blue Oak(Quercus douglasii)"
  ))) +
  scale_color_manual(values = c("quag" = "orchid3",
                                "qudo" = "tomato3")) + 
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(),
        plot.title = element_text(family = "mono", size = 10),
        axis.title = element_text( family = "mono", size = 10),
        plot.text = element_text( family = "mono", size = 10),
        legend.title = element_text(family = "mono", size = 10),
        legend.text = element_text(family = "mono", size = 8),
        legend.position = "none",
  )
#comparing tree species in Burn

#linear model

model4 <- lmer(tree_cond_num ~ num_holes * tree_sp + (1|tree_id), data = burn_data2)


#summary 

summary(model4)

#plot 

plot(resid(model4))

#anova table for mixed effects

anova(model4)


#ggplot

ggplot(burn_data2, aes(x = num_holes, y = tree_cond_num, color = tree_sp)) +
  geom_jitter(
    alpha = 0.3
  ) + 
  geom_smooth(method = "lm", se = FALSE, size = 1) +
  labs(
    title = "Effect of Bore Holes on Tree Condition - Burned",
    x = "# of Boreholes",
    y = "Tree Condition"
  ) +
  facet_wrap(~ tree_sp, labeller = as_labeller(c(
    quag = "Coast Live Oak(Quercus agrifolia)",
    qudo = "Blue Oak(Quercus douglasii)"
  ))) +
  scale_color_manual(values = c("quag" = "orchid3",
                                "qudo" = "tomato3")) + 
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(),
        plot.title = element_text(family = "mono", size = 10),
        axis.title = element_text( family = "mono", size = 10),
        plot.text = element_text( family = "mono", size = 10),
        legend.title = element_text(family = "mono", size = 10),
        legend.text = element_text(family = "mono", size = 8),
        legend.position = "none",
  )

########################################################################

# Add burn status to each dataset
control_data_clean_noNA$burn_status <- "unburned"
burn_data2$burn_status <- "burned"

# remove columns
burn_data_clean <- burn_data2 %>% 
  select(-date.1)

control_data_clean_2 <- control_data_clean_noNA %>% 
  select(-grouped_date)

#dik
control_data_clean_noNA$date <- as.Date(control_data_clean_noNA$date)
burn_data_clean$date <- as.Date(burn_data_clean$date)

# Combine the datasets
combined_data <- bind_rows(control_data_clean_2, burn_data_clean)

#combined plot

ggplot(combined_data, aes(x = num_holes, y = tree_cond_num, color = burn_status)) +
  geom_jitter(alpha = 0.4) + 
  geom_smooth(method = "lm", se = FALSE, size = 1) +
  labs(
    title = "Effect of Bore Holes on Tree Condition Between Species",
    x = "# of Boreholes",
    y = "Tree Condition",
    color = "Burn Status" ) +
  facet_wrap(~ tree_sp, labeller = as_labeller(c(
    quag = "Coast Live Oak(Quercus agrifolia)",
    qudo = "Blue Oak(Quercus douglasii)"
  ))) +
  scale_color_manual(values = c("unburned" = "darkseagreen3",
                                "burned" = "lightslateblue")) + 
  scale_y_continuous(breaks = 1:4,
                     labels = c("Poor", "Fair", "Good", "Excellent")
  ) +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = ".5", family = "wqy-microhei", size = 10),
        axis.title = element_text( family = "wqy-microhei", size = 10),
        axis.text.y = element_text(angle = 45),
        legend.title = element_text(family = "wqy-microhei", size = 8),
        legend.text = element_text(family = "wqy-microhei", size = 7),
        legend.spacing.x = unit(0, "cm"),               # ✅ remove horizontal spacing
        legend.position = "bottom",
        legend.box.background = element_rect(color = "black", size = .5),
        legend.margin = margin(0,5,0,5),
        legend.box.margin = margin(0,0,0,0),
  )

#fontd

# Install if needed
install.packages("showtext")

# Load it
library(showtext)

# Automatically use showtext for new plots
showtext_auto()

sysfonts::font_families()

###################################################################################


# gpplot over time with grouped data

#making collum for grouped month

combined_data$grouped_date <- combined_data$date
combined_data$grouped_date[combined_data$date %in% c("2025-05-20", "2025-05-29")] <- "2025-05-25"
combined_data$grouped_date[combined_data$date %in% c("2024-11-20", "2024-12-09")] <- "2024-12-01"


#new plot

ggplot(combined_data, aes(x = num_holes, y = tree_cond_num, color = burn_status)) +
  geom_point(alpha = 0.5, color = "palegreen2") +  # Scatterplot of observations
  geom_smooth(method = "lm", se = FALSE,) +  # Linear fit + CI
  scale_y_continuous(limits = c(1, 4), breaks = 1:4) +
  scale_x_continuous(breaks = seq(0, 40, by = 20))+
  labs(
    title = "Effect of Bore Holes on Tree Condition Over Time",
    x = "Number of Boreholes",
    y = "Tree Condition",
    color = "Burn Status"
  ) +
  facet_wrap(~ grouped_date, ncol = 3, labeller = labeller(grouped_date = function(x) format(as.Date(x), "%B %Y"))) +  # Create one panel per date
  scale_color_manual(values = c("unburned" = "darkseagreen4",
                                "burned" = "lightslateblue")) + 
  scale_y_continuous(breaks = 1:4,
                     labels = c("Poor", "Fair", "Good", "Excellent")) +
  theme_minimal(base_size = 10) + 
  theme(panel.grid = element_blank(),
        panel.border = element_rect(color = "darkslategrey", fill = NA, size = .2),
        axis.text = element_text(family = "wqy-microhei", size = 10),
        plot.title = element_text(hjust = .4, family = "wqy-microhei", size = 12),
        axis.title = element_text(family = "wqy-microhei"),
        axis.text.y = element_text(angle = 45),
        legend.position = "bottom",
        legend.title = element_text(family = "wqy-microhei", size = 8),
        legend.text = element_text(family = "wqy-microhei", size = 7),
        legend.spacing.x = unit(0, "cm"),               # ✅ remove horizontal spacing
        legend.box.background = element_rect(color = "darkslategrey", size = .2),
        legend.margin = margin(0,5,0,5),
        legend.box.margin = margin(0,0,0,0))

######################

#mean boreholes 
library(janitor)

cleaned_data <- combined_data %>%
  clean_names()

# Summarize mean num_holes by burned/unburned (type) and species (tree_sp)
mean_holes_summary <- cleaned_data %>%
  group_by(burn_status, tree_sp) %>%
  summarise(mean_num_holes = mean(as.numeric(num_holes), na.rm = TRUE)) %>%
  ungroup()

print(mean_holes_summary)

# Clean column names first (if not done already)
cleaned_data <- combined_data %>% 
  clean_names()

# Create separate datasets for each species
quag_clean <- cleaned_data %>%
  filter(tree_sp == "quag")

qudo_clean <- cleaned_data %>%
  filter(tree_sp == "qudo")

# Welch's t-test for Coast Live Oak (quag)
ttest_quag <- t.test(as.numeric(num_holes) ~ burn_status,
                     data = quag_clean,
                     var.equal = FALSE)
print(ttest_quag)

# Welch's t-test for Blue Oak (qudo)
ttest_qudo <- t.test(as.numeric(num_holes) ~ burn_status,
                     data = qudo_clean,
                     var.equal = FALSE)
print(ttest_qudo)


#Boxplot

ggplot(data = combined_data %>% clean_names(), 
       aes(x = burn_status, y = num_holes, color = burn_status)) + 
  geom_point(position = position_jitter(width = 0.15, seed = 3), alpha = 0.15) + 
  stat_summary(geom = "pointrange", fun.data = mean_se, position = position_dodge(width = 0.15)) + 
  stat_summary(geom = "errorbar", fun.data = mean_se, width = 0.2, position = position_dodge(width = 0.15)) + 
  labs(
    x = 'Burn Status',
    y = 'Mean Number of Boreholes ± SE',
    title = "Effects of Burn Status on Boreholes Between Tree Species"
  ) + 
  facet_wrap(~ tree_sp, nrow = 1, labeller = as_labeller(c(
    quag = "Coast Live Oak (Quercus agrifolia)",
    qudo = "Blue Oak (Quercus douglasii)"
  ))) + 
  theme_bw() + 
  theme_classic() + 
  scale_color_manual(values = c("unburned" = "darkgreen", "burned" = "lightslateblue")) + 
  theme(
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold", family = "wqy-microhei"),
    axis.title = element_text(family = "wqy-microhei"),
    axis.text = element_text(family = "wqy-microhei"),
    strip.text = element_text(family = "wqy-microhei"),
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  )
