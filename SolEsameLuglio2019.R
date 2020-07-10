
## (2 punto) Si importi il dataset in R e si verifichi la correttezza del contenuto. 
## Si sostituiscano eventuali errori con la mediana della stessa variabile.

df <- read.csv("https://www.dropbox.com/s/02oyr3iixa64m6o/Metro_Interstate_Traffic_Volume.csv?dl=1", header = T )

str(df)

summary(df)

# Il dataset e' stato letto correttamente, tuttavia il minimo della temperatura risulta 0 Kelvin, che e' fisicamente impoosibile.
# trovo e rimuovo la riga corrispondente a tali misurazioni

df$temp[df$temp ==0] <- median(df$temp[df$temp >0], na.rm = T)


##(2 punti) Si crei una nuova variabile TempC in cui si trasforma la variabile Temp da gradi Kelvin a Celsius. Inoltre si crei una nuova variabile 
## rain_YN uguale a 0 se rain_1h e' uguale a 0, e sia uno altrimenti. Si trasformi la variabile date_time nel tipo data e ora con time zone GMT -5

df$TempC <- df$temp -273.15

df$Rain_YN <- ifelse(df$rain_1h > 0, 1, 0)

df$date_time <- as.POSIXct(df$date_time, "%Y-%m-%d %H:%M:%S" , tz = "GMT-5" )

## (3 punti) Per le misure con e senza pioggia, si calcolino per la temperatura (in C) e per il volume di traffico il minimo, massimo, media e mediana
## Inoltre si visualizzino gli stessi dati (al variare della pioggia) usando i boxplot. 
summary(df$TempC[df$Rain_YN==0])
summary(df$TempC[df$Rain_YN==1])

summary(df$traffic_volume[df$Rain_YN==0])
summary(df$traffic_volume[df$Rain_YN==1])

par(mfrow = c(1,2), mar = c(2,2,1,1))
boxplot(TempC ~ Rain_YN, data = df, main = 'Temp (C)')
boxplot(traffic_volume ~ Rain_YN, data = df, main = 'Traffico')

## (1 punto) Si determini se la differenza delle medie del volume di traffico nei giorni con/senza pioggia e' statisticamente 
## significativa. Si commenti il risultato e si discuta cosa dicono le analisi fatte fin'ora rispetto al pensiero comune che 
## risutla esserci piu traffico quando piove.

t.test(df$traffic_volume[df$Rain_YN ==0], df$traffic_volume[df$Rain_YN ==1])

## (2 punti) Si definisca un dataset contenento il massimo del volume di traffico aggregato per la stessa data e su questo nuovo dataset
## si definisca una nuova variabile contenente il giorno della settimana relativo alla data. 
## Si visualizzi la distribuzione del massimo del traffico con un istogramma e con dei boxplot la distribuzione nei vari giorni della settimana.
## Quale e' il giorno con maggior traffico?

df1 <- df
df1$date <- as.Date(df1$date_time)
df2 <- aggregate( traffic_volume ~date, data = df1, FUN= max)
df2$weekday <- weekdays(df2$date)

par(mfrow = c(1,2), mar = c(2,2,1,1))
hist(df2$traffic_volume, title = 'Temp (C)')
boxplot(traffic_volume ~ weekday, data = df2, main = 'Traffico')

# Wed




