# (Installiere und) lade folgende Pakete
library(tidyverse)
library(janitor)
library(haven)

# 1.0 Daten einlesen ------------------------------------------------------

# 1.0.0
# * Lese den Datensatz data/student_data.csv ein
# * Speichere den Datensatz in der Variable student_data
student_data <- read_csv("data/student_data.csv")


# 1.1 Daten bereinigen ---------------------------------------------

# 1.1.0
# * Wandle die Variablennamen mit clean_names in snake case um
# * Kodiere die Variable pstatus mit case_when um: T -> together, A -> apart
#   Speichere die umkodierte Variable unter dem gleichen Variablennamen pstatus
# * Verbinde beide Bereinigungsschritte mit dem Pipe-Operator
# * Berechne den Mittelwert der Mathenote unter der neuen Variable mean_grade_math
# * Speichere den bereinigten Datensatz in der Variable student_data_cleaned
student_data_cleaned <- student_data %>% 
  clean_names(case = "snake") %>%
  mutate(
    pstatus = case_when(
      pstatus == "T" ~ "together",
      pstatus == "A" ~ "apart"
    )
  ) %>%
  mutate(
    mean_grade_math = (g1 + g2 + g3) / 3
  )


# 1.2 Daten explorieren  ------------------------------------------------

# 1.2.0
# * Wie viele Maenner und Frauen sind im Datensatz?
student_data_cleaned %>%
  count(sex)

# 1.2.1
# * Bestimme die Spannweite und den Mittelwert des Alters aller SuS
range(student_data_cleaned$age)
mean(student_data_cleaned$age, na.rm = TRUE)

# 1.2.2
# * Wie viele SuS leben in Familien mit mehr als drei und wieviele mit
#   weniger oder gleich drei Familienmitgliedern?
student_data_cleaned %>%
  count(famsize)

# 1.2.3
# * Vergleiche, inwieweit sich die SuS in der Qualitaet ihrer familiaeren 
#   Bindungen unterscheiden, wenn ihre Eltern zusammen oder getrennt leben.  
# * Berechne den Mittelwert für beide Gruppen der Variable pstatus mit 
#   den Funktionen group_by und fasse sie zusammen.
student_data_cleaned %>%
  group_by(pstatus) %>%
  summarise(
    mean_famrel = mean(famrel)
  )

# 1.2.4
# * Untersuche, wie sich SuS aus kleinen und groeßeren Familien in 
#   ihrer durchschnittlichen Mathenote unterscheiden. 
# * Haben SuS aus kleinen Familien bessere Noten?
student_data_cleaned %>%
  group_by(famsize) %>%
  summarise(
    mean_grade_math = mean(mean_grade_math)
  )


# 1.3 Daten visualisieren -----------------------------------------------------

# 1.3.0
# Untersuche den Datensatz mit Hilfe eines Balkendiagramms
# * Erstelle ein aneinandergereihtes Balkendiagramm, das die Verteilung der 
#   SuS auf die verschiedenen Level von familiaerer Bindungsqualitaet d
#   auf der X-Achse darstellt und das Geschlecht durch die Farbe der 
#   Balken kennzeichnet
# * Nutze position = position_dodge(), um die Balken nebeneinander zu reihen.
# * Füge sinnvolle Achsen- und Legendentitel hinzu
# * Haben die Schueler bessere familiaere Bindungen als Schuelerinnen?
ggplot(student_data_cleaned, aes(x = famrel, fill = sex)) +
  geom_bar(position = position_dodge()) +
  labs(
    x     = "Qualität der familiären Bindung",
    y     = "Anzahl",
    fill  = "Geschlecht"
  ) +
  scale_y_continuous(expand = expansion(0)) +
  scale_fill_viridis_d(option = "cividis", begin = 0.3, end = 0.9)


# 1.3.1
# Speichere beide Visualisierungen im R-Projekt ab unter dem Pfad
# images/barbplot_mothers_education_status.png
ggsave("images/verteilung_bildungsqualitaet_geschlecht.png",
       width = 8, height = 5, dpi = 300)


# 1.3.2
# * Erstelle ein aneinandergereihtes Balkendiagramm mit dem Bildungslevel der 
#   Muetter auf der x-Achse und deren Beschaeftigungsstatus auf der Y-Achse.
# * Erstelle  eine neue bedingte Variable mit Hilfe von case_when.
# * Unterscheiden sich die arbeitenden Muetter von den nicht arbeitenden 
#   Muettern in ihrem Bildungslevel?
student_data_cleaned %>%
  mutate(
    working_mother = case_when(
      mjob %in% c("at_home") ~ "no",
      TRUE ~ "yes"
    )
  ) %>%  
  ggplot(aes(x = medu, fill = working_mother)) +
  geom_bar(position = "dodge") +
  labs(
    x     = "Bildungslevel der Mutter",
    y     = "Anzahl",
    fill  = "Working mother"
  ) +
  scale_y_continuous(expand = expansion(0)) +
  scale_fill_viridis_d(option = "cividis", begin = 0.3, end = 0.9)


# 1.3.3
# Speichere beide Visualisierungen im R-Projekt ab unter dem Pfad
# images/barbplot_mothers_education_status.png
ggsave("images/barbplot_mothers_education_status.png", width = 8,
       height = 5, dpi = 300)


# 1.4 Datenexport ---------------------------------------------------------

# 1.4.0
# * Exportiere den Datensatz in den Ordner data/cleaned
# * Speichere die Daten unter data/export/student_data_cleaned.csv
write_csv(student_data_cleaned, "data/export/student_data_cleaned.csv")


# 1.4.1
# * Um die Daten in SPSS zu nutzen, exportiere den gereinigten Datensatz mit der
#   Funktion write_sav
# * Speichere die Daten unter data/export/student_data_cleaned.sav
write_sav(student_data_cleaned, "data/export/student_data_cleaned.sav")
