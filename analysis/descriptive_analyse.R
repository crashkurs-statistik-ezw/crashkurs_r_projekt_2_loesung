## Datenbereinigung

# (Installiere und) lade folgende Pakete
library(tidyverse)
library(janitor)
library(haven)

# TODO: Diese Sektionen
# Daten einlesen
# Daten bereinigen
# Daten transformieren
# Daten modeln
# Daten visualisieren

# Daten einlesen ------------------------------------------------------

# 1.0 Daten einlesen
# * Lese den Datensatz data/student_data.csv ein
# * Speichere den Datensatz in der Variable student_data
student_data <- read_csv2(paste0(
  "~/Hiwijob 2022/Datensätze/student_data/",
  "student-mat.csv"
)) ## Pfad rauslassen
# TODO: Als Idee


# 1.1 Variablennamen reinigen ---------------------------------------------
# * Reinige die Variablen mit der Funktion clean_names() und
#   ueberschreibe die Variable student_data
# * Kodiere die Variable Pstatus um: T == together, A == apart
# * Behalte dabei den Variablennamen
student_data_cleaned <- student_data %>% ## Code rauslassen
  clean_names() %>% ## unterste zwei Zeilen selber ausfüllen lassen mit neuen und alten Werten
  mutate(
    Pstatus = case_when(
      Pstatus == "T" ~ "together",
      Pstatus == "A" ~ "apart"
    )
  )

# TODO: Dort, ab wann die Daten nicht mehr verändert werden
# Daten als csv exportieren
# * Exportiere den Datensatz in den Ordner data/cleaned und
# * Benenne die Daten als student_cleaned.csv
write_csv2(student_data, "student_cleaned.csv") ## Klammer leerlassen

# Daten in SPSS exportieren
# * Exportiere den gereinigten Datensatz mit der Funktion write_sav
#   unter dem Namen student_cleaned.sav in den Ordner data/export
write_sav(student_data, "student_cleaned.sav") ## Code rauslassen


## Deskriptive Statistik

# Zunächst wollen wir den Datensatz allgemein untersuchen und
# wichtige demografische Daten berechnen:
# * Wie viele Männer und Frauen sind im Datensatz?
student_data %>%
  count(sex, na.rm = TRUE)

# * Bestimme die Spannweite und den Mittelwert des Alters aller Probanden
range(student_data$age) ## Klammern leerlassen
mean(student_data$age, na.rm = TRUE)
# * Wie viele Probanden waren in Familien mit mehr als drei und wieviele mit
#   weniger/gleich drei Familienmitgliedern?
student_data %>% ## Code weglassen
  count(famsize)


## Gruppenvergleiche

# * Vergleiche, inwiefern sich Probanden in der Qualität ihrer familiären Bindungen
#   unterscheiden, wenn ihre Eltern zusammen oder getrennt leben. Berechne dazu den
#   Mittelwert für beide Gruppe mit den Funktionen group_by und summarise.
student_data %>% ## Klammern und neuen variabennamen ausfüllen lassen
  group_by(Pstatus) %>%
  summarise(
    mean_famrel = mean(famrel)
  )

# * Untersuche auch, wie sich Probanden aus kleinen und größeren Familien in ihrer
#   durchschnittlichen Mathenote unterscheiden. Erstelle dafür zunächst eine neue
#   Variable mit Hilfe der Funktion mutate und speichere den Output ab.
# * Haben Schüler*innen aus kleinen Familien bessere Noten?

# TODO: Prüfen, ob Ergebnisse stimmen. 
student_data <- student_mat %>%
  rowwise() %>% 
  mutate(
    mean_grade = (G1 + G2 + G3) / 3
  ) %>% 
  ungroup()

student_data %>% ##  nach group_by selber ausfüllen lassen
  group_by(famsize) %>%
  summarise(
    mean_grade = mean(mean_grade)
  )


## Explorative Visualisierungen

# Untersuche den Datensatz mit Hilfe von Visualisierungen noch genauer.
# * Erstelle ein aneinandergereihtes Balkendiagramm, das die Verteilung der Probanden
#   auf die verschiedenen Level von familiärer Bindungsqualität zeigt und zusätzlich
#   das Geschlecht beachtet.
# * Nutze position = "dodge", um die Balken nebeneinander zu reihen.
# * Füge einen sinnvollen Titel, Achsen- und Legendentitel ein.
# * Haben die männlichen Probanden bessere familiäre Bindungen als die weiblichen?
ggplot(student_data, aes(x = famrel, fill = sex)) + ## Titel selber bestimmen lassen
  geom_bar(position = "dodge") +
  labs(
    title = "Qualität der familiären Bindung",
    x = "Qualität der familiären Bindung",
    y = "Anzahl",
    fill = "Geschlecht"
  )


# * Erstelle ein weiteres aneinandergereihtes Balkendiagramm mit dem Bildungslevel
#   der Mütter auf der x-Achse und deren Beschäftigungsstatus.
# * Erstelle dafür eine neue bedingte Variable mit Hilfe von case_when.
# * Unterscheiden sich die arbeitenden Mütter von den nicht arbeitenden Mütter
#   in ihrem Bildungslevel?
student_data <- student_data %>% ## nach case_when selber ausfüllen lassen
  mutate(
    working_mother = case_when(
      Mjob == "at_home" ~ "no",
      Mjob == "health" ~ "yes",
      Mjob == "other" ~ "yes",
      Mjob == "services" ~ "yes",
      Mjob == "teacher" ~ "yes"
    )
  )

ggplot(student_data, aes(x = Medu, fill = working_mother)) + ## selber machen lassen
  geom_bar(position = "dodge") +
  labs(
    title = "Bildungslevel Mutter",
    x     = "Bildungslevel",
    y     = "Anzahl",
    fill  = "Working mother"
  )

# * Speichere beide Visualisierungen über die grafische Nutzeroberfläche
#   im R-Projekt ab. Gib den Dateien dabei sinnvolle Namen.
