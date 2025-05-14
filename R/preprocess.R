if(!"pacman %in% installed.packages()) install.packages("pacman")
pacman::p_load(tidyverse, rstatix)
set.seed(42)

data_raw <- read_csv("data/proc/data_ewba6.csv") 

colnames(data_raw)
str(data_raw)

data_proc <- select(data_raw, c(CASE, matches("EI\\d{2}"), matches("K\\d{3}_\\d{2}")))
colnames(data_proc) <- c("id_participant", "consent", "gender", "age", paste0("fomo_", seq(1, 35)))

anyNA(data_proc) # FALSE -> but we have NAs in the data (-9 and -1 see codebook)

# Sosci-Survey stores NAs as -9 and -1 
any(data_proc == -9)
any(data_proc == -1)

data_proc[data_proc == -9] <- NA
data_proc[data_proc == -1] <- NA

data_proc$gender <- factor(data_proc$gender, levels = 1:3, labels = c("m", "w", "d"))
data_proc$consent <- factor(data_proc$consent, levels = 1:2, labels = c("yes", "no"))
data_proc$fomo <- rowSums(data_proc[, paste0("fomo_", 1:35)], na.rm = TRUE)
lp <- 0 + 0.5 * scale(data_proc$fomo)
data_proc$erwartung <- factor(rbinom(nrow(data_proc), size = 2, prob = plogis(lp)), levels = 0:2, labels = c("niedrig", "mittel", "hoch"))



# descriptives


freq_table(data_proc, gender)
freq_table(data_proc, gender, erwartung)

get_summary_stats(select(data_proc, c(age, fomo)))

data_proc |>
   select(age, fomo, erwartung) |>
   group_by(erwartung) |>
   get_summary_stats(type = "mean_sd") 





# anova
aov_test <- anova_test(data_proc, fomo ~ erwartung)
tukey_hsd(data_proc, fomo ~ erwartung)
ggplot(data_proc, aes(x = factor(erwartung), y = fomo)) +
   geom_boxplot()


# correlation

with(data_proc, cor(fomo, age, use = "pairwise.complete.obs"))
ggplot(data_proc, aes(age, sum_score)) +
   geom_point() +
   geom_smooth(method = "lm", se = TRUE) +
   theme_light()
cor_test(data_proc, sum_score,  age, method = "pearson", use = "pairwise.complete.obs")



# t-test

data_ttest <- subset(data_proc, subset = (gender == "m" | gender == "w"))
data_ttest$gender <- droplevels(data_ttest$gender)


t_test(data_ttest, fomo ~ gender)
ggplot(data_ttest, aes(gender, fomo)) +
   geom_boxplot()



