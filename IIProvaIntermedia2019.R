df <- read.csv("https://www.dropbox.com/s/sekbk47062l9doo/charcters_stats.csv?dl=1", header = T)

str(df)
summary(df)
# Il file e' stato letto bene e non presenta NA

df_good <- df[df$Alignment == "good",]
df_bad  <- df[df$Alignment == "bad",]

df_good[, "Alignment"] <- NULL
df_bad[, "Alignment"] <- NULL



# Si verifichi la correttezza del contenuto e si analizzino i dati. 
str(df_good)
summary(df_good)

str(df_bad)
summary(df_bad)
## Da qui proseguo solo con good, bad e' in fondo

########
# GOOD #
########

# Si rimuovano tutte le righe contenenti valori 0 o 1. 
# Quante righe abbiamo rimosso?

df_good1 <- df_good

dim(df_good1[df_good1$Intelligence == 0 | df_good1$Intelligence == 1,  ])
# per intelligence sono 133 righe
df_good2 <- df_good1[ - which(df_good1$Intelligence == 0 | df_good1$Intelligence == 1),  ]

dim(df_good2[df_good2$Combat == 0 | df_good2$Combat == 1,  ])
# per Combat non ci sono altre riche da rimuovere
# df_good3 <- df_good2[ - which(df_good2$Combat == 0 | df_good2$Combat == 1),  ]

# sono state rimosse 133 righe. Proseguo lavorando su df_good2

# Si calcolino mediana e range per tutte  le variabili numeriche.



median(df_good2$Intelligence)
median(df_good2$Strength)
median(df_good2$Speed)
median(df_good2$Durability)
median(df_good2$Power)
median(df_good2$Combat)
median(df_good2$Total)

range(df_good2$Intelligence)
range(df_good2$Strength)
range(df_good2$Speed)
range(df_good2$Durability)
range(df_good2$Power)
range(df_good2$Combat)
range(df_good2$Total)

#oppure
apply(df_good2[,-1],2,FUN = median)
apply(df_good2[,-1],2,FUN = range)

# Si visualizzi la distribuzione delle variabili Intelligence e Combat usando degli istogrammi.

par(mfrow = c(1,2))
hist(df_good2$Intelligence, main = "Intelligenza", xlab = "", freq = F)
hist(df_good2$Combat, main = "Combattimento", ylab = "", xlab = "", freq = F)

# Si consideri un modello di regressione lineare per analizzare l’andamento di Combat al variare 
# di Intelligence. Si visualizzino i dati, la retta di regressione, la distribuzione dei residui 
# e si confrontino i quantili dei residui con i quantili della normale. Si commenti il valore di R^2 ed i grafici.

reg1 <- lm( Combat ~ Intelligence, data = df_good2 )
summary(reg1)

par(mfrow = c(3,1), mar = c(2,2,1,1))
# Retta di regressione
plot( Combat ~ Intelligence, data = df_good2 )
abline(reg1$coefficients, col = "green")
# Pattern nei residui
plot(reg1$residuals, main = "Residui")
# Distribuzione in quantili 
qqnorm(reg1$residuals)
qqline(reg1$residuals)

# dal valore di R^2 e davisivamente possiamo noter come i modello
# non sia deguato a spiegare i dati. In particolare, possiamo notare che i 
# residui non mostrano pattern e i quantili seguono quelli della normale, 
# ma sono molto grandi. Inoltre possiamo notare come la variabile Comabat
# assuma valori che sembrano discreti, scenario non ideale per un modello 
# di regressione lineare.

#######
# BAD #
#######


# Si rimuovano tutte le righe contenenti valori 0 o 1. 
# Quante righe abbiamo rimosso?

df_bad1 <- df_bad

dim(df_bad1[df_bad1$Intelligence == 0 | df_bad1$Intelligence == 1,  ])
# per intelligence sono 44 righe
df_bad2 <- df_bad1[ - which(df_bad1$Intelligence == 0 | df_bad1$Intelligence == 1),  ]

dim(df_bad2[df_bad2$Combat == 0 | df_bad2$Combat == 1,  ])
# per Combat non ci sono altre riche da rimuovere

# sono state rimosse 44 righe. Proseguo lavorando su df_bad2

# Si calcolino mediana e range per tutte  le variabili numeriche.

median(df_bad2$Intelligence)
median(df_bad2$Strength)
median(df_bad2$Speed)
median(df_bad2$Durability)
median(df_bad2$Power)
median(df_bad2$Combat)
median(df_bad2$Total)

range(df_bad2$Intelligence)
range(df_bad2$Strength)
range(df_bad2$Speed)
range(df_bad2$Durability)
range(df_bad2$Power)
range(df_bad2$Combat)
range(df_bad2$Total)


# Si visualizzi la distribuzione delle variabili Intelligence e Combat usando degli istogrammi.

par(mfrow = c(1,2))
hist(df_bad2$Intelligence, main = "Intelligenza", xlab = "", freq = F)
hist(df_bad2$Combat, main = "Combattimento", ylab = "", xlab = "", freq = F)

# Si consideri un modello di regressione lineare per analizzare l’andamento di Combat al variare 
# di Intelligence. Si visualizzino i dati, la retta di regressione, la distribuzione dei residui 
# e si confrontino i quantili dei residui con i quantili della normale. Si commenti il valore di R^2 ed i grafici.

reg1 <- lm( Combat ~ Intelligence, data = df_bad2 )
summary(reg1)

par(mfrow = c(3,1), mar = c(2,2,1,1))
# Retta di regressione
plot( Combat ~ Intelligence, data = df_bad2 )
abline(reg1$coefficients, col = "green")
# Pattern nei residui
plot(reg1$residuals, main = "Residui")
# Distribuzione in quantili 
qqnorm(reg1$residuals)
qqline(reg1$residuals)

# dal valore di R^2 e davisivamente possiamo noter come i modello
# non sia deguato a spiegare i dati. In particolare, possiamo notare che i 
# residui non mostrano pattern ma sono grandi e non seguono i 
# quantili della normale. Inoltre possiamo notare come la variabile Comabat
# assuma valori che sembrano discreti, scenario non ideale per un modello 
# di regressione lineare.

##FR 5/6/2019


