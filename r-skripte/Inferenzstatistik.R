### Konfidenzintervall

population <- rep(factor(c("f","r")), c(220000, 120000))
prop( ~ population, success = "r")

prop( ~ sample(population, size = 34), success = "r")
prop( ~ sample(population, size = 34), success = "r")

set.seed(1896) # Reproduzierbarkeit
Stiprovtlg <- do(10000)* prop( ~ sample(population, size = 34),
                               success = "r")

sd( ~ prop_r, data = Stiprovtlg)

gf_bar( ~ prop_r, data = Stiprovtlg)

stipro <- rep(factor(c("f","r")), c(22, 12))
stipro

resample(stipro)

set.seed(1896) # Reproduzierbarkeit
do(3)* prop( ~ resample(stipro), success = "r")

set.seed(1896)
Bootvtlg <- do(10000)* prop( ~ resample(stipro),
                             success = "r")

gf_bar( ~ prop_r, data = Bootvtlg)

quantile( ~ prop_r, data = Bootvtlg, probs = c(0.025, 0.975))


### Grundlagen des Hypothesenprüfens

rflip(n = 34, prob = 1/3)

set.seed(1896)
Nullvtlg <- do(10000) * rflip(n = 34, prob = 1/3)

gf_bar( ~ heads, data = Nullvtlg )

gf_bar( ~ heads,
        data = Nullvtlg)

prop( ~ heads >= 12,
      data = Nullvtlg)


### Effektgröße

# tips einlesen in R
tips <- read.csv2("tips.csv")

library(lsr)

cohensD(total_bill ~ smoker, data=tips)

