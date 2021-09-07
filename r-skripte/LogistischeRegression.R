# download.file("https://goo.gl/whKjnl", destfile = "tips.csv")
tips <- read.csv2("tips.csv")
library(mosaic) # Paket laden

gf_point( (sex=="Male") ~ total_bill,
          data = tips)

# Referenzkategorie festlegen
tips <- tips %>%
  mutate(sex = factor(tips$sex, levels = c("Female", "Male")))
# Kontrolle
levels(tips$sex)

ergglm1 <- glm(sex ~ # abhängige Variable
                 total_bill, # unabghängige Variable(n)
               data = tips, # Datensatz
               # Abhängige Variable binomial,
               # Verknüpfung Logit
               family = binomial("logit"))
summary(ergglm1)

plotModel(ergglm1)

set.seed(1896) # Reproduzierbarkeit
Bootvtlg <- do(10000) *
  glm(sex ~ total_bill,
      data = resample(tips),
      family = binomial("logit"))

gf_histogram( ~ total_bill, data = Bootvtlg)

qdata( ~ total_bill, data = Bootvtlg,
       p = c(0.025, 0.975))

set.seed(1896) # Reproduzierbarkeit
Nullvtlg <- do(10000) *
  glm(sex ~ shuffle(total_bill),
      data = tips,
      family = binomial("logit"))

gf_histogram( ~ total_bill, data = Nullvtlg)

prop( ~ abs(total_bill) >= coef(ergglm1)[2],
      data=Nullvtlg )

ergglm2 <- glm(sex ~ size,
               data = tips,
               family = binomial("logit"))
summary(ergglm2)

predict(ergglm2,
        newdata = data.frame(size = 4),
        type = "response")

ergglm3 <- glm(sex ~ time, data = tips,
               family = binomial("logit"))
summary(ergglm3)

exp(coef(ergglm3))

ergglm4 <- glm(sex ~
                 total_bill + size + time,
               data = tips,
               family = binomial("logit"))
exp(coef(ergglm4)) # Odds Ratio

summary(ergglm4)

