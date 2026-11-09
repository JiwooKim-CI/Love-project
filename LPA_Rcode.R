# First run the data preparation and imputation in CFA_Rcode.R.

# -------------------------------
# 1) Load packages
# -------------------------------
## Load packages
library(dplyr)
library(tidyr)
library(officer)
library(flextable)


library(ggplot2)

library(tidyLPA)


library(tikzDevice)
library(emmeans)

# -------------------------------
# 2) Data wrangling-use the same data from CFA imputation
# -------------------------------

# Keep participants in the same order. Retain ID, but do not use it to form profiles.
if (!file.exists("analysis_data_imputed.rds"))
  stop("First run the missForest imputation in script 1.")
dat <- readRDS("analysis_data_imputed.rds")
if (!"id" %in% names(dat)) stop("The data do not contain an id column.")
a_scores <- paste0("a", 1:3, "_sum")
b_scores <- paste0("b", 1:6, "_sum")
d_scores <- paste0("d", 1:6, "_sum")
set.seed(123)
# Original labels: Sternberg=Very High/Upper Moderate/Lower Moderate/Very Low;
# current=Unsettled/Stable/Autonomous/Traditional; difference=Similar/Intermediate difference/Huge difference.
# Use Class 1, Class 2, etc., because profile numbers can change when models are fitted again.
# The original labels are listed above. Check profile means before reusing these labels.

# -------------------------------
# 3) Sternberg's Love Theory (current)
# -------------------------------
### Generate dataset - Sternberg's love theory (current relationship)

love_tra <- dat[, grep("^a[1-3](10|[1-9])$", names(dat), value = TRUE)]

love_tra$id <- dat$id

# -------------------------------
# 4) ATTEL (current)
#    - Select b1~b6 item sets, impute, compute subscale sums, keep ID
# -------------------------------
### Generate dataset - ATTEL (current relationship)
love_new_imputed <- dat[,grep(c("^b1(10|[1-9])$"), names(dat), value = TRUE)]
love_new_imputed <- cbind(love_new_imputed,dat[,grep(c("^b2(10|[1-9])$"), names(dat), value = TRUE)])
love_new_imputed <- cbind(love_new_imputed,dat[,grep(c("^b3(10|[1-9])$"), names(dat), value = TRUE)])
love_new_imputed <- cbind(love_new_imputed,dat[,grep(c("^b4(10|[1-9])$"), names(dat), value = TRUE)])
love_new_imputed <- cbind(love_new_imputed,dat[,grep(c("^b5(10|[1-9])$"), names(dat), value = TRUE)])
love_new_imputed <- cbind(love_new_imputed,dat[,grep(c("^b6(10|[1-9])$"), names(dat), value = TRUE)])

love_new_merge <- data.frame(
  b1_sum = rowSums(love_new_imputed[, grep("^b1(10|[1-9])$", 
                                           names(love_new_imputed), 
                                           value = TRUE)], na.rm = FALSE),
  b2_sum = rowSums(love_new_imputed[, grep("^b2(10|[1-9])$", 
                                           names(love_new_imputed), 
                                           value = TRUE)], na.rm = FALSE),
  b3_sum = rowSums(love_new_imputed[, grep("^b3(10|[1-9])$", 
                                           names(love_new_imputed), 
                                           value = TRUE)], na.rm = FALSE),
  b4_sum = rowSums(love_new_imputed[, grep("^b4(10|[1-9])$", 
                                           names(love_new_imputed), 
                                           value = TRUE)], na.rm = FALSE),
  b5_sum = rowSums(love_new_imputed[, grep("^b5(10|[1-9])$", 
                                           names(love_new_imputed), 
                                           value = TRUE)], na.rm = FALSE),
  b6_sum = rowSums(love_new_imputed[, grep("^b6(10|[1-9])$", 
                                           names(love_new_imputed), 
                                           value = TRUE)], na.rm = FALSE)
)

love_new_merge$id <- dat$id

# -------------------------------
# 5) ATTEL (ideal)
#    - Select d1~d6 item sets, impute, compute subscale sums, keep ID
# -------------------------------
### Generate dataset - ATTEL (ideal relationship)
love_new_2_imputed <- dat[,grep(c("^d1(10|[1-9])$"), names(dat), value = TRUE)]
love_new_2_imputed <- cbind(love_new_2_imputed,dat[,grep(c("^d2(10|[1-9])$"), names(dat), value = TRUE)])
love_new_2_imputed <- cbind(love_new_2_imputed,dat[,grep(c("^d3(10|[1-9])$"), names(dat), value = TRUE)])
love_new_2_imputed <- cbind(love_new_2_imputed,dat[,grep(c("^d4(10|[1-9])$"), names(dat), value = TRUE)])
love_new_2_imputed <- cbind(love_new_2_imputed,dat[,grep(c("^d5(10|[1-9])$"), names(dat), value = TRUE)])
love_new_2_imputed <- cbind(love_new_2_imputed,dat[,grep(c("^d6(10|[1-9])$"), names(dat), value = TRUE)])
love_new_2_imputed$id <- dat$id

love_new_ideal <- data.frame(
  d1_sum = rowSums(love_new_2_imputed[, grep("^d1(10|[1-9])$", 
                                             names(love_new_2_imputed), 
                                             value = TRUE)], na.rm = FALSE),
  d2_sum = rowSums(love_new_2_imputed[, grep("^d2(10|[1-9])$", 
                                             names(love_new_2_imputed), 
                                             value = TRUE)], na.rm = FALSE),
  d3_sum = rowSums(love_new_2_imputed[, grep("^d3(10|[1-9])$", 
                                             names(love_new_2_imputed), 
                                             value = TRUE)], na.rm = FALSE),
  d4_sum = rowSums(love_new_2_imputed[, grep("^d4(10|[1-9])$", 
                                             names(love_new_2_imputed), 
                                             value = TRUE)], na.rm = FALSE),
  d5_sum = rowSums(love_new_2_imputed[, grep("^d5(10|[1-9])$", 
                                             names(love_new_2_imputed), 
                                             value = TRUE)], na.rm = FALSE),
  d6_sum = rowSums(love_new_2_imputed[, grep("^d6(10|[1-9])$", 
                                             names(love_new_2_imputed), 
                                             value = TRUE)], na.rm = FALSE)
)

love_new_ideal$id <- dat$id

# -------------------------------
# 6) LPA - Sternberg (current)
#    - Fit 1:6 profiles, select 4-class, label for plotting
# -------------------------------
## LPA - Sternberg's theory (current relationship)
love_tra_merge <- data.frame(
  a1_sum = rowSums(dat[, grep("^a1(10|[1-9])$", names(dat), 
                              value = TRUE)], na.rm = FALSE),
  a2_sum = rowSums(dat[, grep("^a2(10|[1-9])$", names(dat), 
                              value = TRUE)], na.rm = FALSE),
  a3_sum = rowSums(dat[, grep("^a3(10|[1-9])$", names(dat), 
                              value = TRUE)], na.rm = FALSE))

if (anyNA(love_tra_merge)) stop("Some a subscale scores are still missing.")
profiles_all_1 <- love_tra_merge[, a_scores, drop = FALSE] |>
  estimate_profiles(n_profiles = 1:6, models = 1)
profiles_all_1

lpa1 <- profiles_all_1[[4]] # Retain four profiles; review model fit before treating this as the preferred solution.

love_tra_merge$class<-tidyLPA::get_data(lpa1)$Class

# -------------------------------
# 7) Summaries & plot (Sternberg)
#    - Class-wise means, factor labels, TikZ plot
# -------------------------------
group_means <- love_tra_merge |>
  group_by(class)|>
  summarise(across(matches("^[ab][1-6]_sum$"), ~ mean(.x)), .groups = "drop")

print(group_means)

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
                                 levels = 1:4,
                                 labels = paste("Class", 1:4))

### generate plot
tikz("love_profiles_plot_tra.tex", width = 6, height = 4, standAlone = TRUE)

p_lpa <- ggplot(group_means_long, aes(x = item, y = mean_score, color = class, group = class)) +
  geom_line() +
  geom_point() +
  labs(x = "Subdimension", y = "Sum Score", color = "Profile") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_lpa)
dev.off()

# -------------------------------
# 8) Satisfaction models (Sternberg class)
# -------------------------------
dat$class_tra <- as.factor(love_tra_merge$class)

## predicting satisfaction
#relationship
model_relationship_tra <- lm(Q276 ~ class_tra, data = dat)
emmeans(model_relationship_tra, ~ class_tra) |> summary() 
#partner
model_partner_tra <- lm(Q277 ~ class_tra, data = dat)
emmeans(model_partner_tra, ~ class_tra) |> summary()

dat$class_tra_merge <- as.factor(love_tra_merge$class)

lm(Q276 ~ class_tra_merge, data = dat) |>
  summary()
lm(Q277 ~ class_tra_merge, data = dat) |>
  summary()

# Item labels have already been assigned.

dat$class_tra_merge <- factor(love_tra_merge$class,
                              levels = 1:4,  
                              labels = paste("Class", 1:4))


#relationship
model_relationship_tra <- lm(Q276 ~ class_tra_merge, data = dat)
emmeans(model_relationship_tra, ~ class_tra_merge) |> summary() 
#partner
model_partner_tra <- lm(Q277 ~ class_tra_merge, data = dat)
emmeans(model_partner_tra, ~ class_tra_merge) |> summary()

# -------------------------------
# 9) LPA - ATTEL (current)
#    - Fit 2:5 profiles, choose 4-class, tables, plots, models
# -------------------------------
## LPA-ATTEL (current relationship)
set.seed(123)

if (anyNA(love_new_merge[, b_scores])) stop("Some b subscale scores are still missing.")
profiles_all_2 <- love_new_merge[, b_scores, drop = FALSE] |>
  estimate_profiles(n_profiles = 2:5, models = 1)
profiles_all_2

lpa2 <- profiles_all_2[[3]] # Select the four-profile solution from the two- to five-profile models.
love_new_merge$class<-tidyLPA::get_data(lpa2)$Class

group_means <- love_new_merge |>
  group_by(class)|>
  summarise(across(matches("^[ab][1-6]_sum$"), ~ mean(.x)), .groups = "drop")|>
  round(2)

ft <- flextable(group_means)

doc <- read_docx()
doc <- body_add_par(doc, "group", style = "heading 1")
doc <- body_add_flextable(doc, ft)

print(doc, target = "lpa_new.docx")


group_means_long <- group_means %>%
  pivot_longer(
    cols = -class,
    names_to = "item",
    values_to = "mean_score"
  )

group_means_long$item <- factor(group_means_long$item,
                                levels = c("b1_sum","b2_sum","b3_sum","b4_sum","b5_sum","b6_sum"),
                                labels = c("Dependence","Independence","Constraint","Freedom","Lack of Control", "Control"))

group_means_long$class <- factor(group_means_long$class,
                                 levels = 1:4,  
                                 labels = paste("Class", 1:4))

tikz("love_profiles_plot.tex", width = 6, height = 4, standAlone = TRUE)

p_lpa <- ggplot(group_means_long, aes(x = item, y = mean_score, color = class, group = class)) +
  geom_line() +
  geom_point() +
  labs(x = "Subdimension", y = "Sum Score", color = "Profile") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_lpa)
dev.off()

p_lpa <- ggplot(group_means_long, aes(x = item, y = mean_score, color = factor(class), 
                             group = class)) +
  geom_line() +
  geom_point() +
  labs(x = "Item", y = "Sum Score", color = "Class") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

love_new_merge$class_label <- factor(love_new_merge$class,
                              levels = 1:4,  
                              labels = paste("Class", 1:4))

dat$class_current <- love_new_merge$class_label

#relationship
model_relationship_current <- lm(Q276 ~ class_current, data = dat)
emmeans(model_relationship_current, ~ class_current) |> summary() 
#partner
model_partner_current <- lm(Q277 ~ class_current, data = dat)
emmeans(model_partner_current, ~ class_current) |> summary()

# -------------------------------
# 10) LPA - ATTEL (difference score)
#     - Compute (current - ideal), fit 3-class, label, plot, models
# -------------------------------
## LPA-ATTEL (difference score)
love_sub <- love_new_merge[, b_scores] - love_new_ideal[, d_scores]
love_sub$id <- dat$id

# Do not impute d items. Exclude participants with missing difference scores from this LPA only.
diff_rows <- which(complete.cases(love_sub[, b_scores]))
message("Participants excluded from the difference-score LPA: ", nrow(dat) - length(diff_rows))
if (length(diff_rows) < 7L) stop("Too few participants have complete difference scores for this LPA.")
love_sub <- love_sub[diff_rows, , drop = FALSE]
profiles_all_4 <- love_sub[, b_scores, drop = FALSE] |>
  estimate_profiles(n_profiles = 2:6, models = 1)
profiles_all_4

lpa4 <- profiles_all_4[[2]] # Select the three-profile solution from the two- to six-profile models.

love_sub$class<-tidyLPA::get_data(lpa4)$Class

group_means <- love_sub |>
  group_by(class)|>
  summarise(across(matches("^[ab][1-6]_sum$"), ~ mean(.x)), .groups = "drop")

print(group_means)

group_means_long <- group_means %>%
  pivot_longer(
    cols = -class,
    names_to = "item",
    values_to = "mean_score"
  )

group_means_long$item <- factor(group_means_long$item,
                                levels = c("b1_sum","b2_sum","b3_sum","b4_sum","b5_sum","b6_sum"),
                                labels = c("Dependence","Independence","Constraint","Freedom","Lack of Control", "Control"))

group_means_long$class <- factor(group_means_long$class,
                                 levels = 1:3, 
                                 labels = paste("Class", 1:3))

tikz("love_profiles_plot_diff.tex", width = 6, height = 4, standAlone = TRUE)

p_lpa <- ggplot(group_means_long, aes(x = item, y = -(mean_score), color = class, group = class)) +
  geom_line() +
  geom_point() +
  labs(x = "Subdimension", y = "Ideal - Current (sum score)", color = "Profile") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_lpa)
dev.off()

love_sub$class_label <- factor(love_sub$class,
                              levels = 1:3,  
                              labels = paste("Class", 1:3))

dat$class_sub <- factor(rep(NA_character_, nrow(dat)), levels = levels(love_sub$class_label))
dat$class_sub[diff_rows] <- love_sub$class_label

#relationship
model_relationship_diff <- lm(Q276 ~ class_sub, data = dat)
emmeans(model_relationship_diff, ~ class_sub) |> summary() 
#partner
model_partner_diff <- lm(Q277 ~ class_sub, data = dat)
emmeans(model_partner_diff, ~ class_sub) |> summary() 

# Save the data, profile assignments, and fitted models for later use.
saveRDS(list(data = dat, traditional = love_tra_merge, current = love_new_merge,
             difference = love_sub, fits_traditional = profiles_all_1,
             fits_current = profiles_all_2, fits_difference = profiles_all_4),
        "LPA_results.rds")
