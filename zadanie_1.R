
### 1. ogólną postać szeregów czasowych cen i wolumenu (maksymalnie wierna kopia załączonego Zadanie_2_wizualizacja_szeregow_czasowych.jpeg)
rm(list=ls())

library(ggplot2)
library(dplyr)

d <- read.csv(file="https://stooq.pl/q/d/l/?s=wig&i=d", sep=",",dec=".")

d$Data <- as.Date(d$Data)

library(RCurl)
myurl <- "http://administracja.sgh.waw.pl/pl/dpir/obowiazki/PublishingImages/ksiega2019/SGHgodloRGB.png"

library(png)
img <-  readPNG(getURLContent(myurl))

library(ggplot2)
sgh <- grid::rasterGrob(img, interpolate=TRUE)

p <- ggplot(d, aes(x=d$Data, y=d$Wolumen)) +
   geom_line() + 
   annotation_custom(sgh, xmin = 1990, ymin = 500000000) +
   xlab("") +
   ylab(" ") +
   ggtitle("Wolumen")+
   theme_classic()
x11();print(p)

library(reshape2)
df <- data.frame(x = d$Data, Otwarcie=d$Otwarcie, Najwyzszy=d$Najwyzszy, Najnizszy=d$Najnizszy, Zamkniecie=d$Zamkniecie)
df <- df[complete.cases(df), ]
df2 <- melt(df, id = "x")
r <- ggplot(df2, aes(x=x, y=value, color = variable)) +
   geom_line() + 
   annotation_custom(sgh, xmin = 1990, ymin = 50000) +
   scale_color_brewer(palette = "Set1") +
   xlab("") +
   ylab(" ") +
   labs(color='Legenda')+
   ggtitle("Notowania WIG")+
   theme_classic()
x11();print(r)

x11();grid.arrange(
   arrangeGrob(r,p,ncol=1)
)

### 2. histogram różnic kursów zamknięcia i otwarcia
rm(list = ls())
library(ggplot2) 
d <- read.csv(file="https://stooq.pl/q/d/l/?s=wig&i=d", sep=",",dec=".")

library(RCurl)
myurl <- "http://administracja.sgh.waw.pl/pl/dpir/obowiazki/PublishingImages/ksiega2019/SGHgodloRGB.png"

library(png)
img <-  readPNG(getURLContent(myurl))

library(ggplot2)
sgh <- grid::rasterGrob(img, interpolate=TRUE)

wyk <- (
   ggplot(d, aes(d$Zamkniecie-d$Otwarcie))#,group=cut, fill=cut) 
   +
      geom_histogram(bins=50, fill="red", colour = "black") +
      annotation_custom(sgh, xmin = -1000, ymin = 2000) 
)
wyk2 <- wyk + labs(title = "Histogram roznic kursow zamkniecia i otwarcia", x="Roznica kursow zamkniecia i otwarcia", y="Czestosc")
x11();print(wyk2)

### 3. boxplot różnic kursów max i min dla typów dni
rm(list = ls())
library(ggplot2) 
library(RCurl)
library(png)
library(cowplot)
d <- read.csv(file="https://stooq.pl/q/d/l/?s=wig&i=d", sep=",",dec=".")

TypDnia <- as.POSIXlt(d$Data,tz="GMT")$wday

myurl <- "http://administracja.sgh.waw.pl/pl/dpir/obowiazki/PublishingImages/ksiega2019/SGHgodloRGB.png"
img <-  readPNG(getURLContent(myurl))
sgh <- grid::rasterGrob(img, interpolate=TRUE)

wyk <- (
   ggplot(d, aes(x=TypDnia, y=d$Najwyzszy-d$Najnizszy, group=factor(TypDnia)#, fill=class
   ))
   +
     geom_boxplot(
       fill = "orange",
       colour = "green",
       outlier.colour = "red",
       outlier.shape = 5)  
 )

wyk2 <- wyk + labs(title = "Boxplot roznic kursow najwyzszych i najnizszych dla typow dni", x="Typ dnia", y="Najwyzszy - Najnizszy")
wyk3 <- ggdraw(wyk2)
wyk4 <- wyk3 + draw_grob(sgh, 0.8, 0.7, 0.1, 0.1)
x11();print(wyk4)
 
### 4. wykres kołowy volumenu w podziale na typy dni
rm(list = ls())
library(ggplot2) 
library(RCurl)
library(png)
library(cowplot)
d <- read.csv(file="https://stooq.pl/q/d/l/?s=wig&i=d", sep=",",dec=".")

d$TypDnia <- as.POSIXlt(d$Data,tz="GMT")$wday
e <- d[order(d$TypDnia),]

myurl <- "http://administracja.sgh.waw.pl/pl/dpir/obowiazki/PublishingImages/ksiega2019/SGHgodloRGB.png"
img <-  readPNG(getURLContent(myurl))
sgh <- grid::rasterGrob(img, interpolate=TRUE)

bp <- ggplot(d, aes(x="", y=e$Wolumen, fill=e$TypDnia))+ 
   geom_bar(width = 1, stat = "identity")+
   scale_fill_gradient(low="blue", high="red") 

pie <- bp + coord_polar("y", start=0) + labs(fill="Typ dnia", title = "Wykres kolowy wolumenu w podziale na typy dni", x="", y="Wolumen")
pie2 <- ggdraw(pie)
pie3 <- pie2 + draw_grob(sgh, 0.75, 0.75, 0.1, 0.1)
x11(); print(pie3)
