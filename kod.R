library(cluster)
library(psych)
library(ggplot2)
library(dplyr)
library(factoextra)

dane <- read.csv("C:/Users/Pawe³/Desktop/footballers.csv", sep= ";", dec = ",")
rownames(dane) <- dane$Player
dane <- dane[,-1]
dane$Passes.per.match <- as.numeric(dane$Passes.per.match)

#brawo Pawe³ ! #10
par(mfrow=c(3,3))
boxplot(dane$Goals, xlab = "Liczba bramek", col = "red")
boxplot(dane$Assists, xlab = "Assists", col = "yellow")
boxplot(dane$Crosses, xlab = "Crosses", col = "blue")
boxplot(dane$Goals, xlab = "Liczba bramek", col = "red")
boxplot(dane$Assists, xlab = "Assists", col = "yellow")
boxplot(dane$Crosses, xlab = "Crosses", col = "blue")
boxplot(dane$Goals, xlab = "Liczba bramek", col = "red")
boxplot(dane$Assists, xlab = "Assists", col = "yellow")
boxplot(dane$Crosses, xlab = "Crosses", col = "blue")

#spr korelacji
korelacja <- round(cor(dane),2)
korelacja

#spr wspolczynnika zmiennosci
wz<- function(x) {sd(x)/mean(x)}
wspZM <- round(sapply(dane, wz),3)
wspZM

#skalowanie
dane_st <- scale(dane)

#opis danych
boxplot(dane_st)
summary(dane_st)

kM <- kmeans(dane_st, centers=3,nstart = 20)
pM <- pam(dane_st, k=3)
wynik <- cbind(kM$cluster,pM$clustering)
dane <- cbind(dane, wynik)

fviz_nbclust(dane_st, pam, method = "wss") 
fviz_nbclust(dane_st, kmeans, method = "wss")
fviz_nbclust(dane_st, hcut, method = "silhouette")
kM <- kmeans(dane_st, centers=4,nstart = 20)
pM <- pam(dane_st, k=4)
fviz_cluster(kM, data = dane)

library(ggdendro)
ggdendrogram(podzialW)

plot(podzialW)
rect.hclust(podzialW, k = 3, border = 2:4)

mss <- function(dane, rodzaj_zm, wagi = NULL){
  n_row <- nrow(dane)
  n_col <- ncol(dane)
  
  #zamiana zmiennych na stymulanty
  for (i in 1:n_col){
    if (rodzaj_zm[i] == "D") {dane[,i] <- -dane[,i]}
    else if (rodzaj_zm[i] != "S") {
      dane[,i] <- -abs(dane[,i] - as.double(rodzaj_zm[i]))
    }
  } 
  
  #standaryzacja zmiennych
  dane <- scale(dane)
  
  if (!is.null(wagi)) {
    for (i in 1:n_col){
      dane[,i] <- dane[,i]*wagi[i]
    }
  }
  
  #obliczam wektor bêd¹cy sum¹ po zmiennych
  s_rang <- NULL
  for (i in 1:n_row){
    s_rang <- c(s_rang,sum(dane[i,1:n_col]))
  }
  
  #zmienne pomocnicze do oblczenia wskaŸnika ciut szybciej
  minn <- min(s_rang)
  pom <- s_rang - minn
  mianownik <- max(pom)
  
  #obliczam wskaŸnik i zapisuje go do zmiennej wynik
  wynikk <- NULL
  for (i in 1:n_row){
    a <- (s_rang[i]-minn)/mianownik
    wynikk <- rbind(wynikk,a)
  }
  
  #nadaje nazwy wierszy takie jak w danych wejsciowych 
  rownames(wynikk) <- rownames(dane)
  
  #sortowanie od najlepszego obiektu
  wynikk <- wynikk[order(wynikk[,1], decreasing = TRUE),]
}

rank1 <- mss(dane, rodzaj_zm = c("S","D","S","S","S",
                                 "S","S","S","S"))

wagi <- c(0.9, 0.1, 0.25, 0.85, 0.35, 0.55, 0.2, 0.6, 0.75)

rank2 <- mss(dane, rodzaj_zm = c("S","D","S","S","S",
                                 "S","S","S","S"),
             wagi =  wagi)

library(factoextra)
hellwig <- function(dane, rodzaj_zm, wagi = NULL){
  n_row <- nrow(dane)
  n_col <- ncol(dane)
  
  #standaryzacja
  dane <- scale(dane)
  
  wzorzec <- NULL
  #tworzenie wzorca
  for (i in 1:n_col){
    if (rodzaj_zm[i]=="S") {wzorzec[i] <- max(dane[,i])}
    else if (rodzaj_zm[i]=="D"){wzorzec[i] <- min(dane[,i])}
  }
  
  dane <- rbind(dane,wzorzec)
  distances <- NULL
  for (i in 1:n_row){
    h <- get_dist(dane[i,], dane[n_row+1,])
    distances <- c(distances, h)
  }
  
  d0 <- mean(distances) + 2*sd(distances)
  
  Si <- NULL
  for (i in 1:n_row){
    Si <- c(Si,1 - (distances[i]/d0))
  }
  
  wynik <- NULL
  wynik <- cbind(wynik, Si)
  rownames(wynik) <- rownames(dane)
  wynik <- wynik[order(wynik[,1], decreasing = TRUE),]
}

hrank <- hellwig(dane, rodzaj_zm = c("S","D","S","S","S",
                            "S","S","S","S"))
View(hrank)


sortHellwig <- function(df, wzorzec) {
  row <- nrow(df)
  col <- ncol(df)
  len <- length(wzorzec)
  
  for(i in 1:len) {
    if(wzorzec[i]!="+" && wzorzec[i]!="-") {
      wzorzec[i] = (as.numeric(wzorzec[i])-mean(df[,i+1]))/sd(df[,i+1])
    }
  }
  
  df[,2:col] <- scale(df[,2:col])
  
  for(i in 1:len) {
    if(wzorzec[i] == "+") {
      wzorzec[i] = max(df[,i+1])
    } else if(wzorzec[i]=="-") {
      wzorzec[i] = min(df[,i+1])
    } else{
    }
  }
  
  
  wzorzec <- as.numeric(c(NA, wzorzec))
  df <- rbind(df,wzorzec)
  
  for(i in 1:row) {
    df[i,col+1] <- dist(df[i,2:col], df[row+1,2:col])
  }
  
  
  df[,col+1] <- as.vector(df[,col+1] )
  
  
  d <- mean(df[1:row,col+1]) + 2*sd(df[1:row,col+1])
  
  for(i in 1:row) {
    df[i,col+2] <- 1-df[i,col+1]/d
  }
  
  df[,col+2] <- as.vector(df[,col+2] )
  
  names(df)[col+1] <- "odleglosc"
  names(df)[col+2] <- "wynik"
  
  arrange(df[1:row,], desc(wynik))
  
}
danes <- cbind(rownames(dane),dane)
sortHellwig(danes, c("+","-","+","+","+","+","+","+","+"))
