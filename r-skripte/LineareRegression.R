library(mosaic)
# download.file("https://goo.gl/whKjnl", destfile = "tips.csv")
tips <- read.csv2("tips.csv")


### Einfache lineare Regression

gf_point(tip ~ total_bill, data = tips)

# Speichere Ergebnis der Regression lm() in "erglm1"
erglm1 <- lm(tip ~ # abhängige Variable
               total_bill, # unabhängige Variable(n)
             data = tips) # Datensatz
erglm1

plotModel(erglm1)

coef(erglm1)

predict(erglm1, # Modell
        # Neue Beobachtung mit x=10:
        newdata = data.frame(total_bill = 10))

rsquared(erglm1)

### Inferenz in der Linearen Regression

# Original
tips %>% head()

# Reproduzierbarkeit
set.seed(1896)
# Resample
tips %>% head() %>% resample()

do(1) * lm(tip ~ total_bill, data = tips)

# Reproduzierbarkeit
set.seed(1896)
# Resample
do(3) * lm(tip ~ total_bill, data = resample(tips))

# Reproduzierbarkeit
set.seed(1896)
Bootvtlg <- do(10000) *
  lm(tip ~ total_bill, data = resample(tips))

gf_histogram( ~ total_bill, data = Bootvtlg)

sd( ~ total_bill, data = Bootvtlg)

quantile( ~ total_bill, data = Bootvtlg,
          probs = c(0.025, 0.975))

# Original
tips %>% head()

# Reproduzierbarkeit
set.seed(1896)
# Shuffle
tips %>% head() %>% shuffle()

# Original
do(1) * lm(tip ~ total_bill, data = tips)

# Reproduzierbarkeit
set.seed(1896)
# Resample
do(3) * lm(tip ~ shuffle(total_bill), data = tips)

set.seed(1896) # Reproduzierbarkeit
Nullvtlg <- do(10000) *
  lm(tip ~ shuffle(total_bill), data = tips)

gf_histogram( ~ total_bill, data = Nullvtlg)

coef(erglm1) # Koeffizienten
coef(erglm1)[2] # Steigung
abs(coef(erglm1))[2] # Absolutbetrag der Steigung
effektdach <- abs(coef(erglm1))[2] # Zuweisung
effektdach

Nullvtlg <- Nullvtlg %>%
  mutate(effekt0 = abs(total_bill))

prop( ~ (effekt0 >= effektdach), data = Nullvtlg)

summary(erglm1) # Modellzusammenfassung

### Voraussetzungen Lineare Regression

gf_point(resid(erglm1) ~ fitted(erglm1))

predict(erglm1, # Modell
        # Neue Beobachtung mit x=1000:
        newdata = data.frame(total_bill = 1000),
        # Prognoseintervall:
        interval = "prediction")

### Regression mit kategorialer unabhängiger Variable

mean(tip ~ smoker, data = tips)

diffmean(tip ~ smoker, data = tips)

gf_point(tip ~ smoker, data = tips,
         position = "jitter",
         width = 0.1, height = 0)

erglm2 <- lm(tip ~ smoker, data = tips)
summary(erglm2)

set.seed(1896)
Bootvtlg <- do(10000)* lm(tip ~ smoker, data = resample(tips))
gf_histogram( ~ smokerYes, data = Bootvtlg) %>%
  gf_vline(xintercept = ~0)
quantile( ~ smokerYes, probs = c(0.025, 0.975), data = Bootvtlg)

set.seed(1896)
Nullvtlg <- do(10000) * lm(tip ~ shuffle(smoker), data = tips)
dachbeta_smokerYes <- coef(lm(tip ~ smoker, data = tips))[2]
gf_histogram( ~ smokerYes, data = Nullvtlg) %>%
  gf_vline(xintercept = ~dachbeta_smokerYes)
quantile( ~ smokerYes, probs = c(0.025, 0.975), data = Nullvtlg)

lm(total_bill ~ day, data = tips)

prop(smoker ~ time,
     success = "Yes", data = tips)

diffprop(smoker ~ time,
         success = "Yes", data = tips)

lm( (smoker=="Yes") ~ time,
    data = tips) %>% coef()


### Multiple Regression

mean(tip ~ 1, data = tips)

lm(tip ~ 1, data = tips)

erglm3 <- lm(tip ~ # abbhängige Variable
               total_bill + smoker, # unabhängige Variablen
             data = tips) # Datensatz
summary(erglm3)

plotModel(erglm3)

set.seed(1896) # Reproduzierbarkeit
Bootvtlg <- do(10000) * lm(tip ~ total_bill + smoker,
                           data = resample(tips))
confint(Bootvtlg)


### Wechselwirkung

erglm4 <- lm(tip ~ total_bill + smoker + total_bill:smoker,
             data = tips)
plotModel(erglm4)

summary(erglm4)


