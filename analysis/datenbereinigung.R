# 2.0 Setup -------------------------------------------------------------
# 2.0.1
# (Installiere und) lade die Pakete janitor und haven
library(janitor)
library(haven)

# 2.1 Daten einlesen ------------------------------------------------------

# 2.1.0
# * Lese den Datensatz data/student_data.csv ein
# * Speichere den Datensatz in der Variable student_data
student_data <- read_csv("data/student_data.csv")


# 2.2 Daten bereinigen ---------------------------------------------

# 2.2.0
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


# 2.3 Datenexport ---------------------------------------------------------

# 2.3.0
# * Exportiere den Datensatz in den Ordner data/cleaned
# * Speichere die Daten unter data/export/student_data_cleaned.csv
write_csv(student_data_cleaned, "data/export/student_data_cleaned.csv")


# 2.3.1
# * Um die Daten in SPSS zu nutzen, exportiere den gereinigten Datensatz mit der
#   Funktion write_sav
# * Speichere die Daten unter data/export/student_data_cleaned.sav
write_sav(student_data_cleaned, "data/export/student_data_cleaned.sav")
