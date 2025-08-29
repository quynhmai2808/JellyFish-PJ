# JellyFish-PJ
R Shiny Dashboard Ideas

1. Datenmanagement
Daten-Upload (CSV/Excel/Database): Team kann neue Monats- oder Wochenberichte hochladen.
Automatisches Laden von Daten aus Ordnern (z. B. /reports/customer_name/monthly).
Filter nach Zeitraum: Auswahl nach Woche/Monat oder benutzerdefiniertem Zeitraum.
Berichtstyp auswählen: Muster A, Muster B, Muster C.
Kunde auswählen: Dropdown-Filter für Berichte je Kunde.

2. Dashboard-Darstellung
Tabellenansicht (DataTable) mit Feldern:
store (Filiale)
product_number (Produktnummer)
artikel
total_sticks (Gesamtmenge)
total_revenue (Gesamtumsatz)
month (Monat)
Interaktive Diagramme:
📊 Umsatz nach Monat (Linien- oder Balkendiagramm)
🛍️ Top-Produkte nach Umsatz (Balkendiagramm)
🏬 Vergleich Umsatz pro Filiale (gestapeltes Balkendiagramm)
📈 Entwicklung Gesamtmenge/Umsatz über Zeit (Liniendiagramm)

3. Erweiterte Analysen
KPI-Boxen (valueBox/infoBox):
Gesamtumsatz
Gesamtanzahl verkaufter Produkte (Sticks)
Anzahl aktiver Kunden / Filialen
Vergleich nach Berichtsmuster: Muster A vs Muster B vs Muster C.
Drill-down-Funktion: von Gesamtübersicht → Detail pro Produkt/Filiale.
Dynamische Filter: Auswahl nach mehreren Kriterien (Filiale + Monat + Berichtsmuster).
🔹 4. Berichtsexport & Teilen
Export als PDF/Excel.
Automatische Berichtserstellung mit rmarkdown (z. B. monatlich pro Kunde).
Download-Button: Kunden können Berichte direkt herunterladen.
🔹 5. Kundenmanagement (optional erweitert)
Login / Benutzer-Authentifizierung: jeder Kunde sieht nur seine eigenen Daten.
Rechteverwaltung (Admin / Kunde).
Download-Historie protokollieren.
🔹 6. Technische Umsetzung
Shiny + shinydashboard / bslib → modernes UI.
DT-Package → interaktive Tabellen mit Filter & Suche.
plotly / highcharter → interaktive Charts.
shinyWidgets → Dropdowns, Multi-Select, Date Range Picker.
shinyjs / shinyFiles → File-Handling.
