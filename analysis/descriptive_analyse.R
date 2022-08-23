# (Installiere und) lade folgende Pakete
library(tidyverse)
library(janitor)
library(haven)

# 1.0 Daten einlesen ------------------------------------------------------

# * Lese den Datensatz data/student_data.csv ein
# * Speichere den Datensatz in der Variable student_data
student_data <- read_csv(paste0(
  "~/Hiwijob 2022/project_1_crashkurs_r/data/student_data.csv"
))


# 1.1 Daten bereinigen ---------------------------------------------

# * Reinige die Variablen mit der Funktion clean_names()
# * Kodiere die Variable Pstatus um: T == together, A == apart
# * Behalte dabei den Variablennamen
# * Speichere den bereinigten Datensatz als student_data_cleaned
student_data_cleaned <- student_data %>% 
  clean_names() %>%
  mutate(
    pstatus = case_when(
      pstatus == "T" ~ "together",
      pstatus == "A" ~ "apart"
    )
  )

# @Christian: wenn wir clean_names und mutate verbinden, muss ich pstatus
# kleinschreiben, obwohl es in der Originalversion groß ist. Das müsste man
# den Studierenden erklären oder die Funktionen doch nacheinander ausführen.


# 1.2 Daten transformieren ------------------------------------------------

# * Wie viele Männer und Frauen sind im Datensatz?
student_data_cleaned %>%
  count(sex, na.rm = TRUE)

# * Bestimme die Spannweite und den Mittelwert des Alters aller Probanden
range(student_data_cleaned$age)
mean(student_data_cleaned$age, na.rm = TRUE)

# * Wie viele Probanden waren in Familien mit mehr als drei und wieviele mit
#   weniger/gleich drei Familienmitgliedern?
student_data_cleaned %>%
  count(famsize)

# * Vergleiche, inwiefern sich Probanden in der Qualität ihrer familiären 
#   Bindungen unterscheiden, wenn ihre Eltern zusammen oder getrennt leben.
#   Berechne dazu den Mittelwert für beide Gruppe mit den Funktionen group_by
#   und summarise.
student_data_cleaned %>%
  group_by(pstatus) %>%
  summarise(
    mean_famrel = mean(famrel)
  )

# * Untersuche auch, wie sich Probanden aus kleinen und größeren Familien in 
#   ihrer durchschnittlichen Mathenote unterscheiden. Erstelle dafür zunächst
#   eine neue Variable mit Hilfe von mutate und speichere den Output ab.
# * Haben Schüler*innen aus kleinen Familien bessere Noten?

# TODO: Prüfen, ob Ergebnisse stimmen. @ Christian: ist geprüft,
# stimmen auch ohne rowwise 
student_data_cleaned <- student_data_cleaned %>%
  mutate(
    mean_grade = (g1 + g2 + g3) / 3
  )

student_data_cleaned %>%
  group_by(famsize) %>%
  summarise(
    mean_grade = mean(mean_grade)
  )


# 1.3 Daten visualisieren -----------------------------------------------------

# Untersuche den Datensatz mit Hilfe von Visualisierungen noch genauer.
# * Erstelle ein aneinandergereihtes Balkendiagramm, das die Verteilung der 
#   Probanden auf die verschiedenen Level von familiärer Bindungsqualität zeigt 
#   und zusätzlich das Geschlecht beachtet.
# * Nutze position = "dodge", um die Balken nebeneinander zu reihen.
# * Füge einen sinnvollen Titel, Achsen- und Legendentitel ein.
# * Haben die männlichen Probanden bessere familiäre Bindungen als die 
#   weiblichen?
ggplot(student_data_cleaned, aes(x = famrel, fill = sex)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Qualität der familiären Bindung",
    x     = "Qualität der familiären Bindung",
    y     = "Anzahl",
    fill  = "Geschlecht"
  )

# * Erstelle ein weiteres aneinandergereihtes Balkendiagramm mit dem 
#   Bildungslevel der Mütter auf der x-Achse und deren Beschäftigungsstatus.
# * Erstelle dafür eine neue bedingte Variable mit Hilfe von case_when.
# * Unterscheiden sich die arbeitenden Mütter von den nicht arbeitenden Mütter
#   in ihrem Bildungslevel?
student_data_cleaned <- student_data_cleaned %>%
  mutate(
    working_mother = case_when(
      mjob == "at_home" ~ "no",
      mjob == "health" ~ "yes",
      mjob == "other" ~ "yes",
      mjob == "services" ~ "yes",
      mjob == "teacher" ~ "yes"
    )
  )

ggplot(student_data_cleaned, aes(x = medu, fill = working_mother)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Bildungslevel Mutter",
    x     = "Bildungslevel",
    y     = "Anzahl",
    fill  = "Working mother"
  )

# * Speichere beide Visualisierungen über die grafische Nutzeroberfläche
#   im R-Projekt ab. Gib den Dateien dabei sinnvolle Namen.

# @Christian: Daten exportieren finde ich in keinem der Punkte aus R for Data
# Science wieder, vielleicht hast du eine bessere Idee zur Strukturierung

# * Exportiere den Datensatz in den Ordner data/cleaned und
# * Benenne die Daten als student_data_cleaned.csv
write_csv(student_data_cleaned, "student_data_cleaned.csv")

# * Um die Daten in SPSS zu nutzen, exportiere den gereinigten Datensatz mit der
#   Funktion write_sav unter dem Namen student_data_cleaned.sav in den Ordner 
#   data/export
write_sav(student_data_cleaned, "student_data_cleaned.sav")