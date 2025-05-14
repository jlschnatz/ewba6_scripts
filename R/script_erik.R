#### Vorbereitungen in R ----
## Packages
library(readr)
library(dplyr)
library(skimr)
library(psych)
library(naniar)
library(sjPlot)
library(janitor)
library(tidyr)

## Datensatz herunterladen
Datensatz.raw <- read.csv("Datensatz.csv")

#### Datenmanagement ----
## Daten angucken
head(Datensatz.raw)
skim(Datensatz.raw)

## Sosci-Charakteristika rauswerfen
Daten.brauchbar <- Datensatz.raw[ , 8:44]
head(Daten.brauchbar)
skim(Daten.brauchbar)
Daten.brauchbar <- Daten.brauchbar[ , - Spalte des Beispielsitems] # Spalte mit Beispielitem ausschlie?en

## Daten umbenennen 
names(Daten.brauchbar) <- c("Geschlecht", "Alter", "I1", "I2", "I3", "I4", "I5", "I6", "I7", "I8", "I9", "I10",
                            "I11","I12","I13","I14","I15","I16","I17","I18","I19","I20","I21","I22","I23",
                            "I24","I25","I26","I27","I28","I29","I30","I31","I32","I33","I34","I35")
skim(Daten.brauchbar)


## Aufmerksamkeitsfragen betrachten und Personen, die falsch geantwortet haben, ausschlie?en
Daten.brauchbar_filtered <- Daten.brauchbar[Daten.brauchbar$Beispielitem != abgefragte Kategorie, ]

## Fehlende Werte finden und ausschlie?en
anyNA(Daten.brauchbar) # Gibt es NAs?

Daten.brauchbar[Daten.brauchbar == -1] <- NA # falsche Werte als NAs
Daten.brauchbar[Daten.brauchbar == -9] <- NA # falsche Werte als NAs

anyNA(Daten.brauchbar) # Gibt es jetzt noch NAs?

naniar::gg_miss_case(Daten.brauchbar, show_pct = TRUE, order_cases = FALSE) # Welche Werte fehlen? 
Daten.brauchbar <- na.omit(Daten.brauchbar) # Personen mit fehlenden Werten ausschlie?en => Im Bericht nennen, wie viele es sind!
anyNA(Daten.brauchbar)

## Datensatz nur mit Items f?r die Itemanalyse
data_item <- subset(Daten.brauchbar, select = I1:I35) # Items ausw?hlen

#### Deskriptivstatistik: ----
## alle Items
descr_data_item <- psych::describe(data_item) # Speichert Datenframe mit deskriptiven Infos im Environment
sjPlot::tab_df( # M?glichkeit der Erstellung eines Worddokuments:  #file = table_descr_item.doc um Speichern als .doc Datei
  x = descr_data_item, 
  show.rownames = TRUE)

## Alter
descr_alter <- describe(Daten.brauchbar$Alter) # Verteilung der demografischen Variable Alter (=kontinuierlich)
sjPlot::tab_df(descr_alter)

## Geschlecht
descr_geschlecht <- janitor::tabyl(Daten.brauchbar$Geschlecht) # Verteilung der demografischen Variable Alter (= kategorial)
tab_df(descr_geschlecht) # => Problem: 1,2,3 ist keine g?ngige Beschreibung f?r Geschlechter

# Labels hinzuf?gen:
Daten.brauchbar$Geschlecht_factor <- factor(Daten.brauchbar$Geschlecht, # Ausgangsvariable
                                            levels = c(1:3),               # Faktorstufen
                                            labels = c("maennlich", "weiblich", "divers"))  # Label f?r Faktorstufen, ACHTUNG: gleiche Reihenfolge wie Levels

descr_geschlecht <- janitor::tabyl(Daten.brauchbar$Geschlecht_factor) # Verteilung der demografischen Variable Alter (= kategorial)
tab_df(descr_geschlecht) # => Problem: 1,2,3 ist keine g?ngige Beschreibung f?r Geschlechter       

## Alter nach Geschlecht
descr_age_by_sex <- describeBy(x = Daten.brauchbar$Alter, group = Daten.brauchbar$Geschlecht_factor)

tab_dfs(
  x = descr_age_by_sex,
  titles = c("M?nnlich","Weiblich", "Divers")
)

## Mehrere Tabellen in einem Dokument
tab_dfs(
  x = list(descr_alter, descr_geschlecht),
  titles = c("Deskriptivstatistik Alter", "Deskriptivstatistik Geschlecht")
)

#### Invertierte Items umkodieren ----
# mein hier genutzter Datensatz hat leider keine, deswegen zeige ich euch nur die Codes :)

## 1.) Der leichtere Weg mit Fallstrick:
data_item$I1 <- -1 * (data_item$I1 - 6) # Rekodierung
# VORSICHT: Bei mehrfachem Ausf?hren codiert man immer wieder hin und her!
# man kann nat?rlich auch ein "neues recodiertes Item" erstellen und dann das alte l?schen
data_item$I1_r <- -1 * (data_item$I1 - 6) # Rekodierung

data_item <- data_item[, !names(data_item) %in% "I1"] # ?berfl?ssige Zeile l?schen; Variante 1

library(dplyr)
data_item <- data_item %>% select(-c(data_item$I1, data_item$I2)) # ?berfl?ssige Zeile l?schen; Variante 2

## 2.) Die sch?ne, aber etwas kompliziertere Methode:

library(sjmisc)
# Voraussetzung: ihr habt die Items einheitlich benannt!

inverse_items <- paste0("I", c(1,2,3))

data_item_rec <- data_item %>%
  mutate(across(
    .cols = all_of(inverse_items), 
    .fns = ~sjmisc::rec(.x, rec = "rev"),
    .names = "{.col}_r" # dadurch erkennt ihr, welche Items rekodiert wurden
  )) %>%
  select(-all_of(inverse_items)) 

colnames(data_item_rec) # es wurden die nicht rekodierten durch die rekodierten Items ersetzt

## Ab hier f?r beide Varianten:
# So kommt ihr wieder zur urspr?nglichen Reihenfolge:
col_order <- sort(colnames(data_item_rec)) 
data_item_rec <- select(data_item_rec, all_of(col_order))
colnames(data_item_rec)

#### Ergänzung zu Itemanalyse ----

# Im Skript habt ihr die schöne Variante der Kick-Outs mit Filtern kennengelernt
step1_kick_diff <- extract_itemtable(data_item_rec) %>%
  filter(item_difficulty < .2 | item_difficulty > .8) %>% # `|` ist der ODER Operator in R 
  dplyr::pull(id_item) # macht das selbe wie `$id_item`(indiziert in den dataframe)

# Alternativ geht es aber auch so:
sjt.itemanalysis(data_item)
Umwelt.step1 <- subset(Umwelt.item, select = -c(20,30,32)) # raus muss: 1, 5, 23,26

# hierbei sind die Nummern jeweils die Items, die das Kriterium nicht erf?llen!
# TIPP: Macht unbedingt f?r jeden Schritt einen neuen Dataframe, das macht es nachvollziehbar und das Fehlerfinden leichter!
# Das gleiche kann man dann f?r jeden Schritt (auch bei Ladungen in der EFA) wiederholen :)

#### Itemanalyse nach Skript ----
## 1. Itemanalyse:
item_analysis_1 <- tab_itemscale(
  df = data_item,
  factor.groups.titles = "Erste Itemanalyse")

item_analysis_1

## Vorbereitung:
str(object = item_analysis_1, vec.len = 1, nchar.max = 30)

extract_itemtable <- function(.data) {
  tab <- sjPlot::tab_itemscale(.data)$df.list[[1]] 
  # clean_names ändert die Spaltennamen in lowercase und tauscht Leerzeichen mit _
  out <- janitor::clean_names(cbind(id_item = rownames(tab), tab))
  return(out)
}

head(extract_itemtable(data_item))

## Schritt 1: Itemschwierigkeit
step1_kick_diff <- extract_itemtable(data_item) %>%
  filter(item_difficulty < .2 | item_difficulty > .8) %>% # `|` ist der ODER Operator in R 
  dplyr::pull(id_item) # macht das selbe wie `$id_item`(indiziert in den dataframe)

print(step1_kick_diff)

data_item_s1 <- dplyr::select(data_item, -all_of(step1_kick_diff))

item_analysis_2 <- tab_itemscale(
  df = data_item_s1,
  factor.groups.titles = "Zweite Itemanalyse"
)

item_analysis_2 # Itemanalye nach dem ersten Schritt

## Schritt 2: Itemtrennschärfe
step2_kick_disc <- extract_itemtable(data_item_s1) %>%
  dplyr::filter(item_discrimination < .3) %>%
  dplyr::pull(id_item)

print(step2_kick_disc)

data_item_s2 <- dplyr::select(data_item_s1, -all_of(step2_kick_disc))

item_analysis_final <- tab_itemscale(
  df = data_item_s2,
  factor.groups.titles = "Dritte Itemanalyse")

item_analysis_final # Itemanalye nach dem ersten Schritt

data_item_final <- data_item_s2

## Reliabilität vor EFA:
omega_items <- psych::omega(data_item_final, plot = FALSE)
omega_items$omega.tot
# McDonalds Omega bei mehrfaktoriellen Konstrukt besser geeignet als Cronbach's Alpha

## Itemvarianzen:
diag(var(data_item_final))

#### EFA ----
## hier aufgrund der Einfachheit nur das konservativste Kriterien betrachtet: Parallelanalyse
psych::fa.parallel(
  x = data_item_final, 
  fm = "pa", # Principal Axis Factoring Extraktion
  fa = "fa", # Factor Analysis (fa = "pc" für Hauptkomponentenanalyse)
  n.iter = 1000, # Anzahl der Simulationen
  quant = .95, # Vergleichsintervall
  main = "Parallelanalyse mittels Base R und psych-Package",
  ylabel = "Eigenwert",
  error.bars = FALSE # TRUE für Error-Bars
)

## Faktorenladungen betrachten:
fit_fa <- tab_fa(
  data = data_item_final,
  nmbr.fctr = 3, # Faktorenanzahl
  rotation = "oblimin", # Rotationsverfahren
  fctr.load.tlrn = 0, 
  method = "pa", # Alternative Methoden aus Lernbar, "ml" für Maximum-Likelihood 
  title = "Faktorenanalyse",
  #file = "fit_fa.doc" # Ergebnisse können wieder als .doc gespeichert werden
)

fit_fa

## Filterung auf Doppelladungen:
res_psych <- psych::fa(
  r = data_item_final, 
  nfactors = 3, # 3 Faktoren
  fm = "pa" # Principal Axis Factoring
) 

df_fa <- as.data.frame.matrix(loadings(res_psych))
df_fa$id_item <- rownames(df_fa) # Neue Variable mit den Itemnamen

step_kick3_efa <- df_fa %>%
  filter(
    # 1. Kriterium: Ladungen auf mindestens zwei Faktoren < 0.3
    (PA1 > .3 & PA2 > .3) |
    (PA1 > .3 & PA3 > .3) |
    (PA2 > .3 & PA3 > .3)
    | # ODER
      # 2. Kriterium: Absolute Differenz zwischen allen Faktoren < 0.1
      (abs(PA1 - PA2) < .1 & abs(PA1 - PA3) < .1 & abs(PA2 - PA3) < .1)
  ) %>%
  pull(id_item)

data_item_final_s3_efa  <- select(data_item_final, -all_of(step_kick3_efa))

## Finale Faktorenanalyse:
tab_fa(
  data = data_item_final_s3_efa, 
  nmbr.fctr = 3,
  rotation = "oblimin", 
  fctr.load.tlrn = 0, 
  method = "pa",
  title = "Finale Faktorenanalyse"
)

## Deskriptive Analyse der Faktorenstruktur:
fa_index <- tab_fa(
  data = data_item_final_s3_efa, 
  nmbr.fctr = 3, 
  rotation = "oblimin", 
  fctr.load.tlrn = 0, 
  method = "pa"
)$factor.index

print(fa_index)

tab_itemscale(
  df = data_item_final_s3_efa, 
  factor.groups = fa_index, 
  factor.groups.titles = c("Faktor 1", "Faktor 2", "Faktor 3")) # hier auf die inhaltliche Benennung achten!

f1_names <- names(fa_index[fa_index == 1])
f2_names <- names(fa_index[fa_index == 2])
f3_names <- names(fa_index[fa_index == 3])

## Reliabilität:
get_reliability <- function(.data, .var_names) {
  fit_omega <- omega(.data[, .var_names], plot = FALSE)
  out <- list(
    omega = fit_omega$omega.tot, 
    alpha = fit_omega$alpha
  )
  # Angabe, welche Objekte ausgegeben werden sollen
  return(out)
}

get_reliability(data_item_final_s3_efa, f1_names)
get_reliability(data_item_final_s3_efa, f2_names)
get_reliability(data_item_final_s3_efa, f3_names)

#### Testwertanalyse: ----
data_item_final_scores <- data_item_final_s3_efa
data_item_final_scores$f1 <- rowSums(data_item_final_scores[, f1_names])
data_item_final_scores$f2 <- rowSums(data_item_final_scores[, f2_names])
data_item_final_scores$f3 <- rowSums(data_item_final_scores[, f3_names])
data_item_final_scores$fullscore <- rowSums(data_item_final_scores[,  setdiff(names(data_item_final_scores), c("f1", "f2", "f3"))])

colnames(data_item_final_scores)

describe(data_item_final_scores$fullscore)
tab_df(describe(data_item_final_scores$fullscore)) 

describe(data_item_final_scores$f1)
describe(data_item_final_scores$f2)
describe(data_item_final_scores$f3)

install.packages("ggh4x")
library(ggh4x)
library(ggplot2)
library(jcolors)

ggplot(data_item_final_scores, aes(x = fullscore))+
  geom_histogram(binwidth = 2, fill = "lightgrey")+
  stat_theodensity(mapping = aes(y = after_stat(count)),
                   distri = "norm", color = "royalblue4", size = 0.5) +
  scale_x_continuous(name = "FOMO-Score", limits = c(0,135), breaks = seq(0,135, 5), expand = c(0,0)) +
  scale_y_continuous(name = "Häufigkeit", limits = c(0, 15), breaks = seq(0,15,5), expand = c(0,0)) +
  theme_sjplot()
  

## Testwertplot aller Faktoren und des Gesamtscores:
strip_labels <- as_labeller(c(f1 = "Factor 1", f2 = "Factor 2", f3 = "Factor 2", fullscore = "Gesamtscore"))

data_item_final_scores %>% 
  # Variablen auswählen, die benötigt werden
  select(f1, f2, f3,fullscore) %>% 
  # Vom Wide ins Long-Format
  pivot_longer(
    cols = everything(),
    names_to = "factor",
    values_to = "score"
  ) %>% 
  # ggplot Base Layer
  ggplot(aes(x = score, fill = factor)) + 
  # facet_wrap erstellt kleineres Plotting Fenster anhand einer Variable
  facet_wrap(
    facets  = vars(factor), # Variable, welche gefacetted werden soll
    labeller = strip_labels, # Facet Labels
    scales = "free"  # freies Koordinatensystem
  )  +
  # Histogramm
  geom_histogram(
    bins = 15,
    alpha = 0.6,
    show.legend = FALSE,
    aes(fill = factor)
  ) +
  # Theoretische Normalverteilung
  stat_theodensity(aes(y = after_stat(count), color = factor)) +
  # Achsenmanipulation in den einzelnen Facets
  facetted_pos_scales(
    x = list(
      factor == "f1" ~ scale_x_continuous(limits = c(0, 56)),
      factor == "f2" ~ scale_x_continuous(limits = c(0, 48)),
      factor == "f3" ~ scale_x_continuous(limits = c(0, 12)),
      factor == "fullscore" ~ scale_x_continuous(limits = c(0, 135))
    )
  ) +
  # Achsenmanipulation & Beschriftung
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 40),
    breaks = seq(0, 40, 10)
  ) +
  scale_x_continuous(expand = c(0, 0)) +
  xlab("Score") + 
  ylab("Häufigkeit") +
  # Farben
  scale_fill_manual(values = c("royalblue4", "orangered2", "purple", "ivory4")) +
  scale_color_manual(values = c("royalblue4", "orangered2", "purple","ivory4")) +
  guides(color = "none", fill = "none") +
  # Theme
  theme_sjplot()

#### Inhaltliche Testwertanalysen ----
## Unter Betrachtung des Alters:

# Vorbereitung: Demographische Variablen zu Datensatz hinzufügen:
demographs <- Daten.brauchbar[,1:2]
data_final_with_demo <- cbind(data_item_final_scores, demographs) # ACHTUNG: Beide müssen die gleiche Anzahl an Versuchspersonen haben!
data_final_with_demo$Geschlecht <- factor(
  data_final_with_demo$Geschlecht , 
  levels = c(1, 2, 3), # Ursprüngliche Werte
  labels = c("männlich", "weiblich", "divers") # Neue Labels
)

## Scatterplot unter Betrachtung des Alters (= Beispiel für numerische Variable)
scatter <- ggplot(data_final_with_demo, aes(x = Alter, y = fullscore)) + 
  geom_point() +
  geom_smooth(method = "lm", color = "red") +      # Trendlinie (lineares Modell)
  labs(title = "Zusammenhang von Alter und FOMO mit Trendlinie",
       x = "Alter",
       y = "FOMO-Score")

scatter

scatter + theme_sjplot()
## Unter Betrachtung des Geschlechts (= Beispiel für kategoriale Variable):
# Nur die Verteilung unter Berücksichtigung des Geschlechts:
ggplot(
  data = data_final_with_demo,
  mapping = aes(x = Geschlecht,
                y = fullscore)
) + 
  geom_boxplot()

# Histogramme nebeneinander für die Geschlechter:
ggplot(
  data = data_final_with_demo,aes(x = fullscore)) +
  geom_histogram() +
  facet_wrap(~ Geschlecht)  # Facet für jede Geschlechtskategorie

## Unterteilung nach Geschlechtern unter Berücksichtigung des Alters:
# Variante 1:
ggplot(
  data = data_final_with_demo,
  mapping = aes(x = Alter,
                y = fullscore,
                color = Geschlecht,
                #  shape = geschlecht
  )
) + 
  geom_point()

# Variante 2:
ggplot(
  data = data_final_with_demo,
  mapping = aes(x = Alter,
                y = fullscore)
  
) + 
  geom_point() + 
  facet_grid(cols = vars(Geschlecht)) 

# Variante 3:
ggplot(
  data = data_final_with_demo,
  mapping = aes(x = Alter,
                y = fullscore,
                color = Geschlecht)
) + 
  geom_point() + 
  facet_grid(cols = vars(Geschlecht)) 

## Abschlussbemerkung zur Ergebnisdarstellung:
# GGPlot heißt ausprobieren, was gut aussieht! Es lassen sich unzählige Anpassungen vornehmen, die diesen Rahmen hier sprengen :)
# Hilfe findet ihr z.B. auf Pandar - aus Statistik 2: https://pandar.netlify.app/lehre/statistik-ii/grafiken-ggplot2/
# Oder auch von mir: https://pandar.netlify.app/workshops/refresher/refresher-day2/
# Oder im Internet: https://www.data-to-viz.com/ oder https://r-charts.com/ggplot2/

