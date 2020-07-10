
## (1 punto) Si importi il dataset in R e si verifichi la correttezza del contenuto.


# ho letto il dataset in df.

df <- read.csv("https://www.dropbox.com/s/rt1swt5frqlmw2n/heart.csv?dl=1", header = T )

str(df)
summary(df)

# Il dataset e' stato letto correttamente. 
# Alcune variabili potrebbero necessitare la conversione in factor,  
# ma lo faro' solo se necessario.

##(2 punti) Si dividano i dati un due dataset in base ai valori della
## variabile Target. Se Target e' uguale a 0, il paziente non ha avuto 
## malattie cardiache. Se Target e' uguale a 1 ne ha avute. 
## Le seguenti analisi si intendono su entrambi i dataset.

df_healthy <- df[df$target == 0,]
df_disease <- df[df$target == 1,]

## (3 punti) Per entrambi i dataset, si visualizzini potenziali outliers 
## della variabile chol usando i boxplot e si rimuovano le righe relative 
## agli outliers. Dopo si verifichi la corretta rimozione di tutti gli 
## outliers. Quante righe abbiamo rimosso? Si commentino i risultati. 
## Le seguenti analisi si intendono sui dataset dove sono state rimosse 
## queste righe.

par(mfrow = c(1,2), mar = c(2,2,1,1))
boxplot(df_healthy$chol)
boxplot(df_disease$chol)

# ci son odiversi potenziali outliers

## OPPURE
#par(mfrow = c(1,1), mar = c(2,2,1,1))
#boxplot(chol~target, data = df)

# rimuovo quelli di healthy che riconosco essere 
# i valori sopra 380 circa.

df_healthy1 <- df_healthy[df_healthy$chol < 380,]

# qui non e' chiarissimo il valore che separa i dati dagli outliers.
# allora calolo la lunghezza massima baffo:

lmax <- quantile(df_disease$chol)[4] + 1.5*IQR(df_disease$chol)

df_disease1 <- df_disease[df_disease$chol < lmax,]
## OPPURE
#library("car")
#boxplot(df_healthy$thalach)
# e si rimuovono gli indici


## Visualizzo i nuovi dataset per verifica
par(mfrow = c(1,2), mar = c(2,2,1,1))
boxplot(df_healthy1$chol)
boxplot(df_disease1$chol)

# in disease c'e' un nuovo outlier, questo e' possibile in quanto rimuovendo 
# elementi, l'IQR e' cambiato. Rimuovo anche questo

lmax1 <- quantile(df_disease1$chol)[4] + 1.5*IQR(df_disease1$chol)

df_disease2 <- df_disease1[df_disease1$chol < lmax1,]

# rinomino per usare lo stesso numero 
df_healthy2 <- df_healthy1
par(mfrow = c(1,2), mar = c(2,2,1,1))
boxplot(df_healthy2$chol)
boxplot(df_disease2$chol)

#Ora non ce ne sono piu'.

## (2 punti) Si calcolino media, mediana, quartili e 
## deviazione standard per le variabili chol e thalach.

#chol
summary(df_healthy2$chol)
summary(df_disease2$chol)

sd(df_healthy2$chol)
sd(df_disease2$chol)

#thalach
summary(df_healthy2$thalach)
summary(df_disease2$thalach)

sd(df_healthy2$thalach)
sd(df_disease2$thalach)

## (2 punti) Si calcolino gli intervalli di confidenza bilateri
## (95%) per la variabile thalach dei dataset sani e malati. 
## Si commentino i risultati.

t.test(df_healthy2$thalach)
t.test(df_disease2$thalach)

# dall'analisi degli intervalli di conferenza si nota
# che quelli del gruppo dei sano abbiano media piu' bassa dei 
# malati. Sembra quindi che il valore di massima frequenza cardiaca sia 
# piu' basso nei sani

## (1 punto) Si determini se la differenza delle medie e' statisticamente 
## significativa e si commenti il risultato.

t.test(df_healthy2$thalach , df_disease2$thalach)

# il p-valore e' molto piccolo e suggerisce che la differenza non sia data dal caso.
# potevamo intuirlo anche dal risultato precedente. 



