### Daten einlesen

# Herunterladen
download.file("https://goo.gl/whKjnl", destfile = "tips.csv")
# Einlesen in R
tips <- read.csv2("tips.csv")

# Paket mosaic laden
library(mosaic)

str(tips)


### Analyse kategorialer Daten

gf_bar( ~ sex, # (unabhängige) Variable, die analysiert wird
        data = tips) # Datensatz

prop( ~ sex, # (unabhängige) Variable, die analysiert wird
      data = tips) # Datensatz

prop( ~ sex, # (unabhängige) Variable, die analysiert wird
      success = "Female", # wovon den Anteil bestimmen
      data = tips) # Datensatz

tally( ~ sex, # Variable, die analysiert wird
       data = tips) # Datensatz

tally( ~ sex, # Variable, die analysiert wird
       format = "proportion", # Option: Anteile
       data = tips) # Datensatz

gf_bar( ~ sex # Variable, die analysiert wird
        | time, # Variable, nach der bedingt wird
        data = tips) # Datensatz

tally( ~ sex # Variable, die analysiert wird
       | time, # Variable, nach der bedingt wird
       data = tips) # Datensatz

tally( ~ sex # Variable, die analysiert wird
       | time, # Variable, nach der bedingt wird
       format = "proportion", # Option: Anteile
       data = tips) # Datensatz

tally( ~ smoker | day, format = "proportion", data = tips)

tally( ~ day | smoker, format = "proportion", data = tips)

mosaicplot(day ~ smoker, data = tips, color = TRUE)


### Analyse numerischer (metrischer) Daten

gf_histogram( ~ total_bill, data = tips, binwidth = 10, center = 5)

gf_histogram( ~ total_bill, # Variable, die analysiert wird
              binwidth = 5, # Breite einer Säule entspricht 5$
              data = tips) # Datensatz

pdata( ~ total_bill, q = 10, data = tips)

qdata( ~ total_bill, p = 0.9, data = tips)

mean( ~ total_bill, data = tips)

inspect(tips)

favstats( ~ total_bill, # Variable, die analysiert wird
          data = tips) # Datensatz

gf_boxplot(tip ~ 1, data = tips)

gf_boxplot(total_bill ~ # abhängige Variable
             sex, # unabhängige Variable
           data = tips) # Datensatz

favstats(total_bill ~ sex, data = tips)

gf_point(total_bill ~ sex, stat = "summary", size = 5, data = tips)

favstats(total_bill ~ 1, data = tips)

favstats(total_bill ~ size, data = tips)


### Zusammenhang zwischen numerischen Variablen

gf_point( tip # Variable auf y-Achse
          ~ total_bill, # Variable auf x-Achse
          data = tips) # Datensatz

cor(tip ~ total_bill, # Variablen
    data = tips) # Datensatz

### Daten: Rückblick und Ausblick

set.seed(1896)
Sarah_Raet <- do(1000) * rflip(n = 8)
prop( ~ heads, success = 8, data = Sarah_Raet)




