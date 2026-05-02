library(tidyverse)
library(kableExtra)
library(scales)

# Parameter
darlehen <- 75000
zinssatz <- 0.035       # 3,5% Jahreszinssatz
monatsrate <- zinssatz / 12  # monatlicher Zinssatz
laufzeit <- 60          # Monate
annuitaet <- darlehen * (monatsrate * (1 + monatsrate)^laufzeit) / ((1 + monatsrate)^laufzeit - 1)

zielordner <-"C:/Users/sebse/Meine Ablage/01_Arbeit/03_STZKG/02_Geschäftsvorschläge/2605_Immokauf/92_Tabellen"
dateiname <-"TilgungsplanCC.tex"
vollstaendiger_pfad <- file.path(zielordner, dateiname)


# Tilgungsplan berechnen
plan <- data.frame(
  Monate    = 1:laufzeit,
  Rate      = NA_real_,
  Zinsen    = NA_real_,
  Tilgung   = NA_real_,
  Restschuld = NA_real_
)

rest <- darlehen
for(i in 1:laufzeit) {
  zins_anteil    <- rest * monatsrate
  tilgung_anteil <- annuitaet - zins_anteil
  rest           <- max(0, rest - tilgung_anteil)

  plan$Rate[i]       <- annuitaet
  plan$Zinsen[i]     <- zins_anteil
  plan$Tilgung[i]    <- tilgung_anteil
  plan$Restschuld[i] <- rest
}


plan_long <- plan %>%
  pivot_longer(cols = c(Zinsen, Tilgung), names_to = "Typ", values_to = "Betrag")

# 1. Hilfsfunktion für deutsches Zahlenformat
# (Punkt als Tausender-Trenner, Komma als Dezimal-Trenner)
format_de <- function(x) {
  format(round(x, 2), nsmall = 2, big.mark = ".", decimal.mark = ",", scientific = FALSE)
}


ggplot(plan_long, aes(x = Monate, y = Betrag, fill = Typ)) +
  geom_col(position = "stack", width = 0.7) +
  scale_y_continuous(labels = label_dollar(prefix = "€", big.mark = ".")) +
  scale_fill_manual(values = c("#2c3e50", "#e74c3c")) + # Edle Farben
  theme_minimal() +
  labs(title = "Annuitätentilgung: Zins vs. Tilgung",
       subtitle = paste("Monatliche Rate:", round(annuitaet, 2), "€"),
       x = "Monate", y = "Betrag in Euro")



plan %>%
  mutate(across(where(is.numeric), ~ round(., 2))) %>%
  kbl(format = "latex", 
      booktabs = TRUE, 
      caption = "Detaillierter Tilgungsplan",
      label = "tab:tilgungsplan",
      col.names = c("Monat", "Rate (€)", "Zinsen (€)", "Tilgung (€)", "Restschuld (€)")) %>%
  kable_styling(latex_options = c("striped", "hold_position"))


# 2. Den Tilgungsplan formatieren und als Longtable exportieren
tabelle_tex <- plan %>%
  # Alle numerischen Spalten außer "Monat" formatieren
  mutate(across(c(Rate, Zinsen, Tilgung, Restschuld), format_de)) %>%
  kbl(format = "latex", 
      longtable = TRUE,        # Ermöglicht Seitenumbrüche
      booktabs = TRUE, 
      col.names = c("Monat", "Rate", "Zinsen", "Tilgung", "Restschuld"),
      align = "r") %>%         # Rechtsbündig für bessere Lesbarkeit von Zahlen
  kable_styling(latex_options = c(
    "striped", 
    "repeat_header"            # Wiederholt den Header auf der neuen Seite bei Longtables
  ))

# Caption ans Ende der Tabelle verschieben (vor \end{longtable})
tabelle_tex_str <- sub(
  "\\\\end\\{longtable\\}",
  "\\\\caption{Detaillierter Tilgungsplan (Werte in Euro)}\n\\\\end{longtable}",
  as.character(tabelle_tex)
)

cat(tabelle_tex_str, file = vollstaendiger_pfad)
