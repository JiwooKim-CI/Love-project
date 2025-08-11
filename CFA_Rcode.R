# ------------------------------------------------------------
# Variable Naming Convention for Subscale Data
# ------------------------------------------------------------
# dep       : "dependence" subscale items
# indep     : "independence" subscale items
# depindep  : combined "dependence" + "independence" items
# cons      : "constraints" subscale items
# free      : "freedom" subscale items
# confree   : combined "constraints" + "freedom" items
# cont      : "control" subscale items
# no_cont   : "lack of control" subscale items
# contnocont: combined "control" + "lack of control" items
# ------------------------------------------------------------
# -------------------------------
# 1) Load packages
# -------------------------------
library(dplyr)
library(tidyr)
library(tidyverse)
library(readxl)
library(apaTables)
library(officer)
library(missForest)
library(readr)
library(haven)
library("lavaanPlot")
library(moments)
library(lavaan)
library(psy)
library(semPlot)
library(psych)
library(ggplot2)
library(knitr)
library(tidyLPA)
library(caret)
library(mclust)  



# -------------------------------
# 2) Load & clean data
# -------------------------------
dat <- read_xls("love_may.xls")
dat <- dat_orginal |>
  filter(Progress>=70)
dat[4:186] <- lapply(dat[4:186], function(x) as.numeric(gsub("[^0-9]", "", x)))

names(dat) <- gsub("-", "", names(dat))


# -------------------------------
# 3) Imputation setup & execution
#    - Select variables; factorize chars; missForest imputation; merge back
# -------------------------------
###Information used for imputation
Q_vars <-c("Q267", "Q265", "Q268", "Q275") ## demographic information

abc_vars <- grep("^[abc][0-9]+$", names(dat), value = TRUE) ## questionnaires (Sternberg's love theory and ATTELS questions)


target_vars <- c(Q_vars, abc_vars)

df_impute <- dat[, target_vars]
char_vars <- sapply(df_impute, is.character)
df_impute[, char_vars] <- lapply(df_impute[, char_vars], factor)

dat_imputed <- missForest(as.data.frame(df_impute))$ximp
### merged imputed data with existing data
dat[, intersect(names(dat), names(dat_imputed))] <- 
  dat_imputed[, intersect(names(dat), names(dat_imputed))]


# -------------------------------
# 4) Reliability (ATTELS(current))
#    - Cronbach alpha by subscale and combined sets
# -------------------------------
### dependence
dep <- dat[,grep("^b1(10|[1-9])$", names(dat), value = TRUE)]
reli<-psych::alpha(x=dep,check.keys=FALSE)
reli$alpha.drop
reli$total

### independence
indep <- dat[,grep("^b2(10|[1-9])$", names(dat), value = TRUE)]
reli<-psych::alpha(x=indep,check.keys=FALSE)
reli$alpha.drop
reli$total

### dependence + independence
depindep <- cbind(dep,indep)
reli <- psych::alpha(x=depindep,check.keys=FALSE)
reli$total


### constrains
cons <- dat[,grep("^b3(10|[1-9])$", names(dat), value = TRUE)]
reli<-psych::alpha(x=cons)
reli$alpha.drop
reli$total

### freedom
free <- dat[,grep("^b4(10|[1-9])$", names(dat), value = TRUE)]
reli<-psych::alpha(x=free,check.keys=TRUE)
reli$alpha.drop
reli$total

### freedom + constrains
confree <- cbind(cons,free)
reli<-psych::alpha(x=confree,check.keys=FALSE)
reli$alpha.drop
reli$total


### control
cont <- dat[,grep("^b6(10|[1-9])$", names(dat), value = TRUE)]
reli<-psych::alpha(x=cont)
reli$alpha.drop
reli$total

### lack of control
no_cont <- dat[,grep("^b5(10|[1-9])$", names(dat), value = TRUE)]
reli<-psych::alpha(x=no_cont)
reli$alpha.drop
reli$total

### control + lack of control 
contnocont <- cbind(cont,no_cont)
reli<-psych::alpha(x=contnocont,check.keys=FALSE)
reli$alpha.drop
reli$total


total <- cbind(dep, indep, cons, free, cont,no_cont)
reli<-psych::alpha(x=total,check.keys=FALSE)
reli$alpha.drop
reli$total


# -------------------------------
# 5) Composite scores (current relationship)
#    - Row sums for subscales and total; pairwise correlations
# -------------------------------
var <- dat[, grepl("^b", names(dat))]
current <- list()
current$confree <- rowSums(confree)
current$contnocont <- rowSums(contnocont)
current$depindep <- rowSums(depindep)
current$cons <- rowSums(cons)
current$free <- rowSums(free)
current$dep <- rowSums(dep)
current$indep <- rowSums(indep)
current$cont <- rowSums(cont)
current$no_cont <- rowSums(no_cont)
current$total <- rowSums(var)

current <- as.data.frame(current)

var_combinations <- combn(names(current), 2, simplify = FALSE)

cor_results <- sapply(var_combinations, function(pair) {
  result <- cor.test(current[[pair[1]]], current[[pair[2]]])
  c(correlation = result$estimate, p_value = result$p.value)
})


cor_results_df <- as.data.frame(t(cor_results))
names(cor_results_df) <- c("Correlation", "P-value")
cor_results_df$Variable1 <- sapply(var_combinations, `[`, 1)
cor_results_df$Variable2 <- sapply(var_combinations, `[`, 2)

cor_results_df |> kable(digits = 3)

cor_test<-cor(dat[, grepl("^b", names(dat))], use = "pairwise.complete.obs")
apa.cor.table(dat[, grepl("^b", names(dat))], filename = "Correl.doc")
correlation<-as.data.frame(cor_test)


# -------------------------------
# 6) Descriptive stats & APA correlation table
# -------------------------------
df_items <- dat_imputed[, grepl("^b", names(dat_imputed))]
desc_stats <- describe(df_items)[, c("n", "mean", "sd", "median", "min", "max", "skew", "kurtosis")] %>%
  round(2) %>%
  tibble::rownames_to_column(var = "Item")

doc <- read_docx()


desc_table <- flextable(desc_stats)
desc_table <- autofit(desc_table)
library(flextable)
doc <- body_add_par(doc, "Table 1. Summary statistics for the 37 AIAS items.", style = "heading 1")
doc <- body_add_flextable(doc, desc_table)

print(doc, target = "AIAS_Descriptive_Table.docx")

# -------------------------------
# 7) Reliability (ATTELS(ideal))
# -------------------------------
### dependence
dep <- dat[,grep("^d1(10|[1-9])$", names(dat), value = TRUE)]
reli<-psych::alpha(x=dep,check.keys=FALSE)
reli$alpha.drop
reli$total

### independence
indep <- dat[,grep("^d2(10|[1-9])$", names(dat), value = TRUE)]
reli<-psych::alpha(x=indep,check.keys=FALSE)
reli$alpha.drop
reli$total

### dependence + independence
depindep <- cbind(dep,indep)
reli <- psych::alpha(x=depindep,check.keys=FALSE)
reli$alpha.drop
reli$total


### constrains
cons <- dat[,grep("^d3(10|[1-9])$", names(dat), value = TRUE)]
reli<-psych::alpha(x=cons)
reli$alpha.drop
reli$total

### freedom
free <- dat[,grep("^d4(10|[1-9])$", names(dat), value = TRUE)]
reli<-psych::alpha(x=free,check.keys=TRUE)
reli$alpha.drop
reli$total

### freedom + constrains
confree <- cbind(cons,free)
reli<-psych::alpha(x=confree,check.keys=FALSE)
reli$alpha.drop
reli$total


### control
cont <- dat[,grep("^d6(10|[1-9])$", names(dat), value = TRUE)]
reli<-psych::alpha(x=cont)
reli$alpha.drop
reli$total

### lack of control
no_cont <- dat[,grep("^d5(10|[1-9])$", names(dat), value = TRUE)]
reli<-psych::alpha(x=no_cont)
reli$alpha.drop
reli$total

### control + lack of control 
contnocont <- cbind(cont,no_cont)
reli<-psych::alpha(x=contnocont,check.keys=FALSE)
reli$alpha.drop
reli$total


total <- cbind(dep, indep, cons, free, cont,no_cont)
reli<-psych::alpha(x=total,check.keys=FALSE)
reli$alpha.drop
reli$total




var <- dat[, grepl("^d", names(dat))]
ideal <- list()
ideal$confree <- rowSums(confree)
ideal$contnocont <- rowSums(contnocont)
ideal$depindep <- rowSums(depindep)
ideal$cons <- rowSums(cons)
ideal$free <- rowSums(free)
ideal$dep <- rowSums(dep)
ideal$indep <- rowSums(indep)
ideal$cont <- rowSums(cont)
ideal$no_cont <- rowSums(no_cont)

ideal$total <- rowSums(var)

ideal <- as.data.frame(ideal)
var_combinations <- combn(names(ideal), 2, simplify = FALSE)

cor_results <- sapply(var_combinations, function(pair) {
  result <- cor.test(ideal[[pair[1]]], ideal[[pair[2]]])
  c(correlation = result$estimate, p_value = result$p.value)
})


cor_results_df <- as.data.frame(t(cor_results))
names(cor_results_df) <- c("Correlation", "P-value")
cor_results_df$Variable1 <- sapply(var_combinations, `[`, 1)
cor_results_df$Variable2 <- sapply(var_combinations, `[`, 2)

cor_results_df |> kable(digits = 3)

cor_test<-cor(dat[, grepl("^b", names(dat))], use = "pairwise.complete.obs")
correlation<-as.data.frame(cor_test)

# -------------------------------
# 8) Reliability (Sternberg's theory (current))
# -------------------------------
### intimacy

int <- dat[,grep("^a1(10|[1-9])$", names(dat), value = TRUE)]
reli<-psych::alpha(x=int,check.keys=FALSE)
reli$alpha.drop
reli$total

### commitment
com <- dat[,grep("^a2(10|[1-9])$", names(dat), value = TRUE)]
reli<-psych::alpha(x=com)
reli$alpha.drop
reli$total

### passion
pas <- dat[,grep("^a3(10|[1-9])$", names(dat), value = TRUE)]
reli<-psych::alpha(x=pas)
reli$alpha.drop
reli$total


total <- cbind(int, com, pas)
reli<-psych::alpha(x=total,check.keys=FALSE)
reli$alpha.drop
reli$total


var <- dat[, grepl("^a", names(dat))]
trad <- list()
trad$int <- rowSums(int)
trad$com <- rowSums(com)
trad$pas <- rowSums(pas)
trad$total <- rowSums(var)
trad <- as.data.frame(trad)
var_combinations <- combn(names(trad), 2, simplify = FALSE)


cor_results <- sapply(var_combinations, function(pair) {
  result <- cor.test(trad[[pair[1]]], trad[[pair[2]]])
  c(correlation = result$estimate, p_value = result$p.value)
})


cor_results_df <- as.data.frame(t(cor_results))
names(cor_results_df) <- c("Correlation", "P-value")
cor_results_df$Variable1 <- sapply(var_combinations, `[`, 1)
cor_results_df$Variable2 <- sapply(var_combinations, `[`, 2)

cor_results_df |> kable(digits = 3)

cor_test<-cor(dat[, grepl("^b", names(dat))], use = "pairwise.complete.obs")
correlation<-as.data.frame(cor_test)


# -------------------------------
# 9) CFA models
# -------------------------------

## Model 1

CFA_1 <- '
dep =~ b11 + b12 + b13 + b14 + b15 + b16 + b17 + b18 + b19 + b110
ind =~ b21 + b22 + b23 + b24 + b25 + b26 + b27 + b28 + b29 + b210
cons =~ b31 + b32 + b33 + b34 + b35 + b36 + b37 + b38 + b39 + b310
free =~ b41 + b42 + b43 + b44 + b45 + b46 + b47 + b48 + b49 + b410
cont =~ b51 + b52 + b53 + b54 + b55 + b56 + b57 + b58 + b59 + b510
lcont =~ b61 + b62 + b63 + b64 + b65 + b66 + b67 + b68 + b69 + b610
dep ~~ ind
dep ~~ cons
dep ~~ free
dep ~~ cont
dep ~~ lcont
ind ~~ cons
ind ~~ free
ind ~~ cont
ind ~~ lcont
cons ~~ free
cons ~~ cont
cons ~~ lcont
free ~~ cont
free ~~ lcont
cont ~~ lcont

'

cfa.out_basic <- cfa(CFA_1, dat[, grepl("^b", names(dat))], bounds="pos.var", 
                        orthogonal=TRUE, 
                        bootstrap = 500,
                        check.gradient = FALSE, se="bootstrap", std.lv = FALSE)

basic_result<-summary(cfa.out_basic, fit.measures=T, standardized=T)
basic_result
basic_result$pe[,c("lhs","op","rhs","est","std.all")] |> kable(digits = 3) 
tab <- basic_result$pe[, c("lhs", "op", "rhs","est", "std.all","pvalue")] |> 
  mutate(across(where(is.numeric), ~round(., 3)))



## Model 2

CFA_2 <- '
dep =~ b11 + b12 + b13 + b14 + b15 + b16 + b17 + b18 + b19 + b110
ind =~ b21 + b22 + b23 + b24 + b25 + b26 + b27 + b28 + b29 + b210
cons =~ b31 + b32 + b33 + b34 + b35 + b36 + b37 + b38 + b39 + b310
free =~ b41 + b42 + b43 + b44 + b45 + b46 + b47 + b48 + b49 + b410
cont =~ b51 + b52 + b53 + b54 + b55 + b56 + b57 + b58 + b59 + b510
lcont =~ b61 + b62 + b63 + b64 + b65 + b66 + b67 + b68 + b69 + b610
love =~ b11 + b12 + b13 + b14 + b15 + b16 + b17 + b18 + b19 + b110 +
b21 + b22 + b23 + b24 + b25 + b26 + b27 + b28 + b29 + b210 +
b31 + b32 + b33 + b34 + b35 + b36 + b37 + b38 + b39 + b310 + 
b41 + b42 + b43 + b44 + b45 + b46 + b47 + b48 + b49 + b410 +
b51 + b52 + b53 + b54 + b55 + b56 + b57 + b58 + b59 + b510 +
b61 + b62 + b63 + b64 + b65 + b66 + b67 + b68 + b69 + b610
int =~ dep + ind
com =~ cons + free
pas =~ cont + lcont
int ~~ com
com ~~ pas
int ~~ pas
'

cfa.out_bifactor <- cfa(CFA_2, dat[, grepl("^b", names(dat))], bounds="pos.var", 
                 orthogonal=TRUE, 
                 bootstrap = 500,
                 check.gradient = FALSE, se="bootstrap", std.lv = FALSE)

bifactor_result<-summary(cfa.out_bifactor, fit.measures=T, standardized=T)
bifactor_result
bifactor_result$pe[,c("lhs","op","rhs","est","std.all")] |> kable(digits = 3) 
tab <- bifactor_result$pe[, c("lhs", "op", "rhs","est", "std.all","pvalue")] |> 
  mutate(across(where(is.numeric), ~round(., 3)))


ft <- flextable(tab)


doc <- read_docx()
doc <- body_add_par(doc, "Bifactor Model: Standardized Estimates", style = "heading 1")
doc <- body_add_flextable(doc, ft)


print(doc, target = "bifactor_std_estimates.docx")
semPaths(cfa.out_bifactor, layout = "tree2")


## Model 3
CFA_3 <- '
dep =~ b11 + b12 + b13 + b14 + b15 + b16 + b17 + b18 + b19 + b110
ind =~ b21 + b22 + b23 + b24 + b25 + b26 + b27 + b28 + b29 + b210
cons =~ b31 + b32 + b33 + b34 + b35 + b36 + b37 + b38 + b39 + b310
free =~ b41 + b42 + b43 + b44 + b45 + b46 + b47 + b48 + b49 + b410
cont =~ b51 + b52 + b53 + b54 + b55 + b56 + b57 + b58 + b59 + b510
lcont =~ b61 + b62 + b63 + b64 + b65 + b66 + b67 + b68 + b69 + b610
int =~ dep + ind
com =~ cons + free
pas =~ cont + lcont
love =~ int + com + pas
'


cfa.out_bifactor2 <- cfa(CFA_3, dat[, grepl("^b", names(dat))], bounds="pos.var", 
                 orthogonal=TRUE, 
                 bootstrap = 500,
                 check.gradient = FALSE, se="bootstrap", std.lv = FALSE)
bifactor2_result<-summary(cfa.out_bifactor2, fit.measures=T, standardized=T)
bifactor2_result
semPaths(cfa.out_bifactor2, whatLabels = "none")

## Model 4
CFA_4 <- '
int =~ b11 + b12 + b13 + b14 + b15 + b16 + b17 + b18 + b19 + b110
+ b21 + b22 + b23 + b24 + b25 + b26 + b27 + b28 + b29 + b210
com =~ b31 + b32 + b33 + b34 + b35 + b36 + b37 + b38 + b39 + b310
+ b41 + b42 + b43 + b44 + b45 + b46 + b47 + b48 + b49 + b410
pas =~ b51 + b52 + b53 + b54 + b55 + b56 + b57 + b58 + b59 + b510
+ b61 + b62 + b63 + b64 + b65 + b66 + b67 + b68 + b69 + b610
love =~ b11 + b12 + b13 + b14 + b15 + b16 + b17 + b18 + b19 + b110
+ b21 + b22 + b23 + b24 + b25 + b26 + b27 + b28 + b29 + b210
+b31 + b32 + b33 + b34 + b35 + b36 + b37 + b38 + b39 + b310
+ b41 + b42 + b43 + b44 + b45 + b46 + b47 + b48 + b49 + b410
+ b51 + b52 + b53 + b54 + b55 + b56 + b57 + b58 + b59 + b510
+ b61 + b62 + b63 + b64 + b65 + b66 + b67 + b68 + b69 + b610
int ~~ com
com ~~ pas
int ~~ pas
'

cfa.out_h <- cfa(CFA_4, dat[, grepl("^b", names(dat))], bounds="pos.var", 
                 orthogonal=TRUE, 
                 bootstrap = 500, std.lv = FALSE,
                 check.gradient = FALSE, se="bootstrap")
higher_result<-summary(cfa.out_h, fit.measures=T, standardized=T)
higher_result
semPaths(cfa.out_h, whatLabels = "none")



