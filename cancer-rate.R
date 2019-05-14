library(tidyverse)

female_AT <- 9000000 * 0.52         # Anzahl Frauen in Österreich
female_AT_40 <- female_AT * 0.40    # Anzahl Frauen unter 40 Jahren
female_AT_40  

krank_neu <- 5000                   # Anzahl Brustkrebsneuerkrankungen in Österreich pro Jahr
krank_neu_40 <- krank_neu * 0.07    # Anzahl Brustkrebsneuerkrankungen bei Frauen unter 40 Jahren
krank_neu_40

prior_krank <- krank_neu / female_AT  # Primärwahrscheinlichkeit Brustkrebs bei Frauen
prior_krank

prior_krank_40 <- krank_neu_40 / female_AT_40  # Primärwahrscheinlichkeit Brustkrebs bei Frauen
                                               # unter 40 Jahren (Angaben in Prozent)
round(prior_krank_40 * 100, 4)

N <- 250000                          # Anzahl getestete Frauen unter 40

ergebnis <- sample(c("Krank", "Gesund"), N, replace = TRUE,
                   prob = c(prior_krank_40, 1 - prior_krank_40))

N_K <- sum(ergebnis == "Krank")      # hiervon Anzahl kranker Frauen
N_K

N_G <- sum(ergebnis == "Gesund")     # hiervon Anzahl gesunder Frauen
N_G                                  

test_genau <- 0.99                   # Testgenauigkeit Vorsorgeuntersuchung

test <- vector("numeric", N)

test[ergebnis == "Krank"] <- sample(c(1, 0), N_K, replace = TRUE,
                                      prob = c(test_genau, 1 - test_genau))
positive_positive <- sum(test[ergebnis == "Krank"])

test[ergebnis == "Gesund"] <- sample(c(0, 1), N_G, replace = TRUE,
                                      prob = c(test_genau, 1 - test_genau))
false_positive <- sum(test[ergebnis == "Gesund"])

table(ergebnis, test)

posterior_krank <- positive_positive / (positive_positive + false_positive)
round(posterior_krank * 100, 2)

