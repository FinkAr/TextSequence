###Installation (falls noch nicht installiert) und Laden der notwendigen R-Pakete
packages = c("ggplot2", "reshape", "openxlsx", "stringr", "text.alignment")
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)
#Funktionen laden
source("functions/dotPlots.R")
source("functions/sw.R")
source("functions/smith_waterman_compl.R")

####Erstelle Dotplots 

#Settings für dotplots-Funktion mit Voreinstellungen:
##file = NULL; Input-File im csv oder xlsx-Format. Jeder Text muss in einer einzelnen Spalte dargestellt werden jeweils mit den Kürzeln für die Textabschnitte in den Zellen darunter
##sheet = wenn xlsx-File, dann Angabe des Excelsheets in der xlsx-Datei, dass die Daten enthält (Name oder Index möglich) 
##output.file = NULL; Name für Output-File im xlsx-Format
##gap.marker = "GP"; Marker für Lücken im Input-File; werden mit -1 in den Dotplots dargestellt; werden intern automatisch durchnummeriert.
##sep = F; Sollen alle Vergleiche in einer großen Tabelle (sep = F) oder in separaten Tabellen pro Textsequenz (sep = T) dargestellt werden? Wenn sep = F dauert die Berechnung deutlich länger
##colors = c("#ffa666","#ffd966", "#00BFFF") ; Farben für unterschiedliche Kategorien im Dotplot im HEX-Format. Reihenfolge ist Farbe für -1, Farbe für 0, Farbe für 1

dotplots(file = "data/Priamelueberlieferung_reduziert.xlsx", sheet = 2, output.file = "results/dotplots_redux.xlsx", gap.marker = "GP", sep = F, colors = c("#ffa666","#ffd966", "#00BFFF")) 

####Führe Smith-Waterman-Analyse durch

#Settings für sw mit Voreinstellungen:
##file = NULL; Input-File im csv oder xlsx-Format.
##sheet = wenn xlsx-File, dann Angabe des Excelsheets in der xlsx-Datei, dass die Daten enthält (Name oder Index möglich) 
##output.file = NULL; Name für Output-File im xlsx-Format
##gap.marker = "GP"; Marker für Lücken im Input-File. Werden intern automatisch durchnummeriert.
##rev = T; Soll auch auch mit der invertierten Reihenfolge verglichen werden? Ja -> rev = T; nein -> rev = F
##sec_order = F; second order Smith-Waterman;experminentell(!) -> Erneuter Vergleich der gefundenen Sequenzen miteinander (wenn sec_order = T; Achtung: erhöht die Berehcnungszeit deutlich). 

sw(file = "data/Priamelueberlieferung_reduziert.xlsx", sheet = 2, output.file = "results/sw_incl_rev_redux.xlsx", gap.marker = "GP",rev = T, sec_order = F)
