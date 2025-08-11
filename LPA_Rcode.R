
# -------------------------------
# 1) Load packages
# -------------------------------
## Load packages
library(dplyr)
library(readr)
library(haven)
library("lavaanPlot")
library(moments)
library(semPlot)
library(lavaan)
library(psy)
library(semPlot)
library(psych)
library(ggplot2)
library(knitr)
library(tidyLPA)
library(caret)
library(mclust)  
library(missForest)
library(tikzDevice)
library(emmeans)

# -------------------------------
# 2) Data wrangling
#    - Load raw data, filter for completeness, coerce to numeric, clean names
# -------------------------------
## Data wrangling
### Load data
dat <- read_xls("love_may.xls")

dat[4:186] <- lapply(dat[4:186], function(x) as.numeric(gsub("[^0-9]", "", x))) ## Recode the cell as numeric number

# Normalize variable names (remove hyphens)
names(dat) <- gsub("-", "", names(dat))

# -------------------------------
# 3) Sternberg's Love Theory (current)
# -------------------------------
### Generate dataset - Sternberg's love theory (current relationship)

love_tra <- dat[, grepl("^a", names(dat))]
love_tra <- love_tra[,1:30]

set.seed(123)
love_tra_imputed <- missForest(as.data.frame(love_tra[,1:30]))$ximp
love_tra_imputed <- as.data.frame(love_tra_imputed)

love_tra$id <- dat$id

# -------------------------------
# 4) ATTEL (current)
#    - Select b1~b6 item sets, impute, compute subscale sums, keep ID
# -------------------------------
### Generate dataset - ATTEL (current relationship)
love_new <- dat[,grep(c("^b1(10|[1-9])$"), names(dat), value = TRUE)]
love_new <- cbind(love_new,dat[,grep(c("^b2(10|[1-9])$"), names(dat), value = TRUE)])
love_new <- cbind(love_new,dat[,grep(c("^b3(10|[1-9])$"), names(dat), value = TRUE)])
love_new <- cbind(love_new,dat[,grep(c("^b4(10|[1-9])$"), names(dat), value = TRUE)])
love_new <- cbind(love_new,dat[,grep(c("^b5(10|[1-9])$"), names(dat), value = TRUE)])
love_new <- cbind(love_new,dat[,grep(c("^b6(10|[1-9])$"), names(dat), value = TRUE)])

love_new_imputed <- missForest(as.data.frame(love_new[,1:60]))$ximp ## missing data imputation

love_new_merge <- data.frame(
  b1_sum = rowSums(love_new_imputed[, grep("^b1(10|[1-9])$", 
                                           names(love_new_imputed), 
                                           value = TRUE)], na.rm = TRUE),
  b2_sum = rowSums(love_new_imputed[, grep("^b2(10|[1-9])$", 
                                           names(love_new_imputed), 
                                           value = TRUE)], na.rm = TRUE),
  b3_sum = rowSums(love_new_imputed[, grep("^b3(10|[1-9])$", 
                                           names(love_new_imputed), 
                                           value = TRUE)], na.rm = TRUE),
  b4_sum = rowSums(love_new_imputed[, grep("^b4(10|[1-9])$", 
                                           names(love_new_imputed), 
                                           value = TRUE)], na.rm = TRUE),
  b5_sum = rowSums(love_new_imputed[, grep("^b5(10|[1-9])$", 
                                           names(love_new_imputed), 
                                           value = TRUE)], na.rm = TRUE),
  b6_sum = rowSums(love_new_imputed[, grep("^b6(10|[1-9])$", 
                                           names(love_new_imputed), 
                                           value = TRUE)], na.rm = TRUE)
)

love_new_merge$id <- dat$id

# -------------------------------
# 5) ATTEL (ideal)
#    - Select d1~d6 item sets, impute, compute subscale sums, keep ID
# -------------------------------
### Generate dataset - ATTEL (ideal relationship)
love_new_2 <- dat[,grep(c("^d1(10|[1-9])$"), names(dat), value = TRUE)]
love_new_2 <- cbind(love_new_2,dat[,grep(c("^d2(10|[1-9])$"), names(dat), value = TRUE)])
love_new_2 <- cbind(love_new_2,dat[,grep(c("^d3(10|[1-9])$"), names(dat), value = TRUE)])
love_new_2 <- cbind(love_new_2,dat[,grep(c("^d4(10|[1-9])$"), names(dat), value = TRUE)])
love_new_2 <- cbind(love_new_2,dat[,grep(c("^d5(10|[1-9])$"), names(dat), value = TRUE)])
love_new_2 <- cbind(love_new_2,dat[,grep(c("^d6(10|[1-9])$"), names(dat), value = TRUE)])
love_new_2$id <- dat$id
love_new_2_imputed <- missForest(as.data.frame(love_new_2[,1:60]))$ximp ## missing data imputation

love_new_ideal <- data.frame(
  d1_sum = rowSums(love_new_2_imputed[, grep("^d1(10|[1-9])$", 
                                             names(love_new_2_imputed), 
                                             value = TRUE)], na.rm = TRUE),
  d2_sum = rowSums(love_new_2_imputed[, grep("^d2(10|[1-9])$", 
                                             names(love_new_2_imputed), 
                                             value = TRUE)], na.rm = TRUE),
  d3_sum = rowSums(love_new_2_imputed[, grep("^d3(10|[1-9])$", 
                                             names(love_new_2_imputed), 
                                             value = TRUE)], na.rm = TRUE),
  d4_sum = rowSums(love_new_2_imputed[, grep("^d4(10|[1-9])$", 
                                             names(love_new_2_imputed), 
                                             value = TRUE)], na.rm = TRUE),
  d5_sum = rowSums(love_new_2_imputed[, grep("^d5(10|[1-9])$", 
                                             names(love_new_2_imputed), 
                                             value = TRUE)], na.rm = TRUE),
  d6_sum = rowSums(love_new_2_imputed[, grep("^d6(10|[1-9])$", 
                                             names(love_new_2_imputed), 
                                             value = TRUE)], na.rm = TRUE)
)

love_new_ideal$id <- dat$id

# -------------------------------
# 6) LPA - Sternberg (current)
#    - Fit 1:6 profiles, select 4-class, label for plotting
# -------------------------------
## LPA - Sternberg's theory (current relationship)
love_tra_merge <- data.frame(
  a1_sum = rowSums(dat[, grep("^a1(10|[1-9])$", names(dat), 
                              value = TRUE)], na.rm = TRUE),
  a2_sum = rowSums(dat[, grep("^a2(10|[1-9])$", names(dat), 
                              value = TRUE)], na.rm = TRUE),
  a3_sum = rowSums(dat[, grep("^a3(10|[1-9])$", names(dat), 
                              value = TRUE)], na.rm = TRUE))

profiles_all_1 <- love_tra_merge |>
  estimate_profiles(n_profiles = 1:6)
profiles_all_1

lpa1 <- love_tra_merge |>
  estimate_profiles(n_profiles = 4, models = 1) ## profile number 4 turned out to be best fitting number for latent group

love_tra_merge$class<-lpa1$model_1_class_4$model$classification

# -------------------------------
# 7) Summaries & plot (Sternberg)
#    - Class-wise means, factor labels, TikZ plot
# -------------------------------
group_means <- love_tra_merge |>
  group_by(class)|>
  summarise(across(everything(), mean, na.rm = TRUE))

print(group_means)
library(tidyverse)

group_means_long <- group_means %>%
  pivot_longer(
    cols = -class,
    names_to = "item",
    values_to = "mean_score"
  )

group_means_long$item <- factor(group_means_long$item,
                                levels = c("a1_sum", "a2_sum", "a3_sum"),
                                labels = c("Intimacy", "Commitment","Passion"))

group_means_long$class <- factor(group_means_long$class,
                                 levels = c(2, 4, 3, 1),
                                 labels = c("Very High", "Upper Moderate", "Lower Moderate", "Very Low"))

### generate plot
tikz("love_profiles_plot_tra.tex", width = 6, height = 4, standAlone = TRUE)

ggplot(group_means_long, aes(x = item, y = mean_score, color = class, group = class)) +
  geom_line() +
  geom_point() +
  labs(x = "Subdimension", y = "Sum Score", color = "Profile") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()

# -------------------------------
# 8) Satisfaction models (Sternberg class)
#    - Linear models and emmeans by class
# -------------------------------
dat$class_tra <- as.factor(love_tra_merge$class)

## predicting satisfaction
#relationship
model_rel <- lm(Q276 ~ class_tra, data = dat)
emmeans(model_rel, ~ class_tra) |> summary() 
#partner
model_rel <- lm(Q277 ~ class_tra, data = dat)
emmeans(model_rel, ~ class_tra) |> summary()

dat$class_tra_merge <- as.factor(love_tra_merge$class)

lm(relationsatisfaction ~ class_tra_merge, data = dat) |>
  summary()
lm(partnersatisfaction ~ class_tra_merge, data = dat) |>
  summary()

group_means_long$item <- factor(group_means_long$item,
                                levels = c("a1_sum", "a2_sum", "a3_sum"),
                                labels = c("Intimacy", "Commitment","Passion"))

dat$class_tra_merge <- factor(love_tra_merge$class,
                              levels = c(2, 4, 3, 1),  
                              labels = c("Very High", "Upper Moderate", "Lower Moderate", "Very Low"))

library(emmeans)
#relationship
model_rel <- lm(Q276 ~ class_tra_merge, data = dat)
emmeans(model_rel, ~ class_tra_merge) |> summary() 
#partner
model_rel <- lm(Q277 ~ class_tra_merge, data = dat)
emmeans(model_rel, ~ class_tra_merge) |> summary()

# -------------------------------
# 9) LPA - ATTEL (current)
#    - Fit 2:5 profiles, choose 4-class, tables, plots, models
# -------------------------------
## LPA-ATTEL (current relationship)
set.seed(123)

profiles_all_2 <- love_new_merge |>
  estimate_profiles(n_profiles = 2:5)
profiles_all_2

lpa2 <- love_new_merge[,-c(7,8)] |>
  estimate_profiles(n_profiles = 4, models = 1)
love_new_merge$class<-lpa2$model_1_class_4$model$classification

group_means <- love_new_merge |>
  group_by(class)|>
  summarise(across(everything(), mean, na.rm = TRUE))|>
  round(2)

ft <- flextable(group_means[,1:7])

doc <- read_docx()
doc <- body_add_par(doc, "group", style = "heading 1")
doc <- body_add_flextable(doc, ft)

print(doc, target = "lpa_new.docx")


group_means_long <- group_means[,-8] %>%
  pivot_longer(
    cols = -class,
    names_to = "item",
    values_to = "mean_score"
  )

group_means_long$item <- factor(group_means_long$item,
                                levels = c("b1_sum","b2_sum","b3_sum","b4_sum","b5_sum","b6_sum"),
                                labels = c("Dependence","Independence","Constraint","Freedom","Control", "Lack of control"))

group_means_long$class <- factor(group_means_long$class,
                                 levels = c(3, 2, 1, 4),  
                                 labels = c("Unsettled", "Stable", "Autonomous", "Traditional"))

tikz("love_profiles_plot.tex", width = 6, height = 4, standAlone = TRUE)

ggplot(group_means_long, aes(x = item, y = mean_score, color = class, group = class)) +
  geom_line() +
  geom_point() +
  labs(x = "Subdimension", y = "Sum Score", color = "Profile") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()

ggplot(group_means_long, aes(x = item, y = mean_score, color = factor(class), 
                             group = class)) +
  geom_line() +
  geom_point() +
  labs(x = "Item", y = "Sum Score", color = "Class") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

love_new_merge$class<- factor(love_new_merge$class,
                              levels = c(3, 2, 1, 4),  
                              labels = c("Unsettled", "Stable", "Autonomous", "Traditional"))

dat$class_current <- as.factor(love_new_merge$class)

#relationship
model_rel <- lm(Q276 ~ class_current, data = dat)
emmeans(model_rel, ~ class_current) |> summary() 
#partner
model_rel <- lm(Q277 ~ class_current, data = dat)
emmeans(model_rel, ~ class_current) |> summary()

# -------------------------------
# 10) LPA - ATTEL (difference score)
#     - Compute (current - ideal), fit 3-class, label, plot, models
# -------------------------------
## LPA-ATTEL (difference score)
love_sub <- love_new_merge[,1:6]-love_new_ideal[,1:6]
love_sub$id <- dat$id

profiles_all_4 <- love_sub|>
  estimate_profiles(n_profiles = 2:6)
profiles_all_4

love_sub$id <- dat$id
lpa4 <- love_sub[,-7] |>
  estimate_profiles(n_profiles = 3, models = 1)

love_sub$class<-lpa4$model_1_class_3$model$classification

group_means <- love_sub |>
  group_by(class)|>
  summarise(across(everything(), mean, na.rm = TRUE))

print(group_means)
library(tidyverse)

group_means_long <- group_means[,-8] %>%
  pivot_longer(
    cols = -class,
    names_to = "item",
    values_to = "mean_score"
  )

group_means_long$item <- factor(group_means_long$item,
                                levels = c("b1_sum","b2_sum","b3_sum","b4_sum","b5_sum","b6_sum"),
                                labels = c("Dependence","Independence","Constraint","Freedom","Control", "Lack of control"))

group_means_long$class <- factor(group_means_long$class,
                                 levels = c(1, 3, 2), 
                                 labels = c("Similar", "Intermediate difference", "Hugh difference"))

tikz("love_profiles_plot_diff.tex", width = 6, height = 4, standAlone = TRUE)

ggplot(group_means_long, aes(x = item, y = -(mean_score), color = class, group = class)) +
  geom_line() +
  geom_point() +
  labs(x = "Subdimension", y = "Sum Score", color = "Profile") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

dev.off()

love_new_merge$class<- factor(love_sub$class,
                              levels = c(1,3,2),  
                              labels = c("Similar", "Intermediate difference", "Hugh difference"))

dat$class_sub <- as.factor(love_new_merge$class)

#relationship
model_rel <- lm(Q276 ~ class_sub, data = dat)
emmeans(model_rel, ~ class_sub) |> summary() 
#partner
model_rel_2 <- lm(Q277 ~ class_sub, data = dat)
emmeans(model_rel_2, ~ class_sub) |> summary() 
