if(!"pacman" %in% installed.packages()) install.packages("pacman")
pacman::p_load(tidyverse,  readxl, mice)
set.seed(123)


# Rohdaten einlesen
data_raw <- read_csv("data/raw/data_ewba6_raw_v0.csv")
data_b <- select(data_raw, c(CASE, matches("EI\\d{2}"), matches("K\\d{3}_\\d{2}")))

# IDs für Personen mit Abbruch nach EI05
case_ids_dropout <- c(42, 99)

# Alle Item-Spalten, z. B. K001_01 bis K999_99 (je nach Datenstruktur)
item_vars <- names(data_raw)[grepl("K\\d{3}_\\d{2}", names(data_raw))]

# Setze alle Item-Antworten auf -9 (Abbruch) bei diesen Personen
data_raw[which(data_raw$CASE %in% case_ids_dropout), item_vars] <- -9

# Setze Metadaten dieser Personen passend zu "Abbrecher"
data_raw_na <- data_raw |>
  mutate(
    FINISHED = if_else(CASE %in% case_ids_dropout, 0, FINISHED),
    Q_VIEWER = if_else(CASE %in% case_ids_dropout, 0, Q_VIEWER),
    LASTPAGE = if_else(CASE %in% case_ids_dropout, 2, LASTPAGE),
    MISSING = if_else(CASE %in% case_ids_dropout, 100, MISSING),
    MISSREL = if_else(CASE %in% case_ids_dropout, 100, MISSREL)
  )


# Schreibe den fertigen Datensatz
write_csv(data_raw_na, "data/raw/data_ewba6_raw_v1.csv")


#### FOMO erstellen ----
data_proc <- select(data_raw_na, c(CASE, matches("EI\\d{2}"), matches("K\\d{3}_\\d{2}")))
head(data_proc)
colnames(data_proc) <- c("case", "consent", "gender", "age", paste0("fomo", seq(1, 35)))
head(data_proc)
anyNA(data_proc) # FALSE -> but we have NAs in the data (-9 and -1 see codebook)

# Sosci-Survey stores NAs as -9 and -1 
any(data_proc == -9)
any(data_proc == -1)

data_proc[data_proc == -9] <- NA
data_proc[data_proc == -1] <- NA

anyNA(data_proc)
sum(is.na(data_proc))

data_proc$gender <- factor(data_proc$gender, levels = 1:3, labels = c("m", "w", "d"))
data_proc$consent <- factor(data_proc$consent, levels = 1:2, labels = c("yes", "no"))
data_proc$fomo <- rowSums(data_proc[, paste0("fomo", 1:35)], na.rm = TRUE)
lp <- 0 + 0.6 * scale(data_proc$fomo)
availability_expectation <- factor(rbinom(nrow(data_proc), size = 2, prob = plogis(lp)), levels = 0:2, labels = c("niedrig", "mittel", "hoch"))

data_raw_all <- cbind(data_raw_na, VE01 = availability_expectation)


write_csv(data_raw_all, "data/raw/data_ewba6_raw_v2.csv") 
