## Telepítendő csomagok: xts readxl
.First <- function(){Sys.setenv(TZ='UTC')}

## A meteorológiai állomás 10 perces elemi adatai, illetve az azokból képzett
## statisztikák innen tölthetők le:
## http://met.boreas.hu/erti/
## wgettel leszedve az Excels könyvtárba letolt.txt letolt2.txt

# xls files
ttfile <- dir("Excels", patt="[0-9].xls", full.names=TRUE)

beolv_boreasxls <- function(file) {
    require(xts)
    require(readxl)
    tbl_data <- read_xls(file, range=cell_cols("A:T"))
    df_data <- as.data.frame(tbl_data)
    xts(df_data[,-1],df_data[,1])
}

## Minden excel egy listába
rawlist <- list()
for(tti in 1:length(ttfile))
    rawlist[[tti]] <- beolv_boreasxls(ttfile[tti])

## tti listaelem kirajzolása
ttnames <- names(rawlist[[1]])
ttname <- 1
plot(rawlist[[tti]][,ttname], main=ttnames[ttname]);ttname <- ttname+1

## Hibák javítása Improvement.R fájlban.
## Itt kell futtatni a külön vett javításokat!

tti <- 1
## Napi csapadék és napsütés összeg
tt1 <- apply.daily(rawlist[[tti]][,11], sum)
tt2 <- apply.daily(rawlist[[tti]][,19], sum)
## Napi átlagok
tt3 <- apply.daily(rawlist[[tti]][,-c(5,7:16,18:19)], mean)
## Napi min és max hőmérséklet
tt4 <- apply.daily(rawlist[[tti]][,1], min)
names(tt4) <- "Temp-min"
tt5 <- apply.daily(rawlist[[tti]][,1], max)
names(tt5) <- "Temp-max"
## Összefűzés
daily <- merge.xts(tt1,tt2,tt3,tt4,tt5)

for(tti in 2:length(rawlist)){
## Napi csapadék és napsütés összeg
tt1 <- apply.daily(rawlist[[tti]][,11], sum)
tt2 <- apply.daily(rawlist[[tti]][,19], sum)
## Napi átlagok
tt3 <- apply.daily(rawlist[[tti]][,-c(5,7:16,18:19)], mean)
## Napi min és max hőmérséklet
tt4 <- apply.daily(rawlist[[tti]][,1], min)
names(tt4) <- "Temp-min"
tt5 <- apply.daily(rawlist[[tti]][,1], max)
names(tt5) <- "Temp-max"
## Összefűzés
daily <- c(daily, merge.xts(tt1,tt2,tt3,tt4,tt5))
}

### Csapadék
## Teljes sor
plot(daily[,1], type="h")

## Az ideiglenes beolvasás törlése (memória takarékosság miatt)
rm(rawlist)

## Kiegészítés teljes adatsorrá
## csv beolvasás
ttpotlas <- read.csv2("KaszoPotlas.csv")
## Dátum karakter formázás
ttpotlas[,1] <- gsub("\\.", "-", as.character(ttpotlas[,1]))
## Üres mátrix a megfelelő sor, oszlopszámmal
ttpotlasjav <- matrix(NA, nrow=nrow(ttpotlas), ncol=ncol(coredata(daily[1])))
## Nevek létrehozása
colnames(ttpotlasjav) <- colnames(coredata(daily[1]))
## Adatok bemásolása a megfelelő helyrre
ttpotlasjav[,c(3,1)] <- as.matrix(ttpotlas[,-1])
## xts létrehozása
ttpotlasjav.xls <- xts(ttpotlasjav, as.POSIXct(paste0(ttpotlas[,1]," 23:50:00")))
## Az idősor kiegészítése
daily <- c(ttpotlasjav.xls, daily)

## Éves összeg kontroll

apply.yearly(daily[,1],sum)
## Havi összegek az elemzéshez
prec.month <- apply.monthly(daily[,1],sum)
plot(prec.month, type="h")
## Napi adatsor mentése
write.zoo(daily[,1], file = "KaszoNapiCsapi.csv", dec = ",")


## Hőmérséklet
## Napi átlag lefutása
plot(daily[, "Temp.2m.C"])
## Napi adatsor mentése
write.zoo(round(daily[, "Temp.2m.C"],2), file = "KaszoNapiTemp.csv", dec = ",")
## Minimum-maximum is rajta
plot(daily[, "Temp.2m.C"], ylim = c(min(daily[, "Temp.min"], na.rm=T), max(daily[, "Temp.max"],na.rm=T)), col="ivory4")
lines(daily[, "Temp.min"],col="ivory2")
lines(daily[, "Temp.max"],col="black")

dailytemps.withDate <- xts(coredata(daily[, c("Temp.2m.C", "Temp.min", "Temp.max")]),
                           order.by = as.Date(index(daily)))
## Napi átlag a napi maximummal-minimummal
plot.tempmaxmin <- function(x, ylab="Hőmérséklet") {
    par(las=2, mar=c(4.1,3.9,.5,.5))
    maxdata <- max(x, na.rm = TRUE)
    mindata <- min(x, na.rm = TRUE)
    ttpolyhoz <- data.frame(x = c(index(x),index(x)[nrow(x):1]),
                        y = c(coredata(x[, 2]), coredata(x[, 3])[nrow(x):1]))
    ttpolyhoz <- na.omit(ttpolyhoz)
    plot(index(x),x[, 1], ylim = c(mindata, maxdata),
         type="n", xaxs="i", xaxt="n",
         xlab = "", ylab = ylab)
    ts.start <- as.Date(trunc(as.POSIXct(start(x)), unit="months"))
    ts.end <- as.Date(trunc(as.POSIXct(end(x)), unit="months"))
    ts.month <- seq(ts.start,ts.end, by="months")[-1]
    grid(nx=NA, ny=NULL, lty = "solid")
    axis(1, at = ts.month, lab=F, col="lightgray", tck=1)
    axis(1, at = ts.month, lab=F)
    par(mgp=c(3,0.4,0))
    axis.Date(1, x = ts.month, at=ts.month+15, format="%b", tcl=0)
    par(mgp=c(3,2,0),las=0)
    year.start <- as.numeric(format(ts.start, format="%Y"))
    year.end <- as.numeric(format(ts.end, format="%Y"))
    date.to.year <- as.Date(paste0(year.start:year.end,"-01-01"))
    axis.Date(1,date.to.year, at=date.to.year + c(250,rep(180,4)),padj=1, tcl=0)
    polygon(ttpolyhoz, col = "ivory4", border = NA)
    lines(index(x),x[, 1], col="black", lwd=2 )
    box()
    par(mgp=c(3,1,0))
}

plot.tempmaxmin(dailytemps.withDate)

## Controll
## lines(index(dailytemps.withDate),dailytemps.withDate[, 2],col="red")
## lines(index(dailytemps.withDate),dailytemps.withDate[, 3],col="red")

## Indexeles
plot(daily['2015-10-01/2016-09-30',1], type="h")
## Hidrologiai ev osszeg
sum(daily['2014-10-01/2015-09-30',1])
sum(daily['2015-10-01/2016-09-30',1])
sum(daily['2016-10-01/2017-09-30',1])
sum(daily['2017-10-01/2018-09-30',1])

plot(daily['2014-10-01/2015-09-30',1],major.ticks="months",type = "h")
plot(daily['2015-10-01/2016-09-30',1], type = "h")
plot(daily['2016-10-01/2017-09-30',1], type = "h")
plot(daily['2017-10-01/2018-09-30',1], type = "h", main="", yaxis.sname=FALSE, on=NA)
title("Napi csapadék és átlaghőmérséklet alakulása", sub= "2014-15 hidrológiai év")
?plot.xts
plot(index(daily['2017-10-01/2018-09-30']),coredata(daily['2017-10-01/2018-09-30',1]),type="h",ylab="",xlab="")
axis.POSIXct()

sum(daily['2014-10-01/2015-09-30',1]  > 20)
sum(daily['2015-10-01/2016-09-30',1]  > 20)
sum(daily['2016-10-01/2017-09-30',1]  > 20)
sum(daily['2017-10-01/2018-09-30',1]  > 20)

sum(daily['2015-04-01/2015-09-30',1])
sum(daily['2016-04-01/2016-09-30',1])
sum(daily['2017-04-01/2017-09-30',1])
sum(daily['2018-04-01/2018-09-30',1])

## vegetációs időszak plotok
plot(daily['2015-04-01/2015-09-30',1], type = "h")
plot(daily['2016-04-01/2016-09-30',1], type = "h")
plot(daily['2017-04-01/2017-09-30',1], type = "h")
plot(daily['2018-04-01/2018-09-30',1], type = "h")
# 20 mm-nél nagyobb csapadék
sum(daily['2015-04-01/2015-09-30',1] > 20)
sum(daily['2016-04-01/2016-09-30',1] > 20)
sum(daily['2017-04-01/2017-09-30',1] > 20)
sum(daily['2018-04-01/2018-09-30',1] > 20)
# minimum h
lines(daily['2018-04-01/2018-09-30', "Temp.min"],col="ivory4")

##havi hőm
apply.monthly(daily[,"Temp.2m.C"],mean)
monthly.full<-apply.monthly(daily[,"Temp.2m.C"],mean)
matrix(monthly.full[-49],ncol=4)
rowMeans(matrix(monthly.full[-49],ncol=4))
points(rowMeans(matrix(monthly.full[-49],ncol=4)),ylab = "Hőmérséklet  [°C]", xlab = "Hónapok", cex=1.5)

##
head(daily)
apply.monthly(daily[,"Temp.min"],mean)
monthly.fullmin<-apply.monthly(daily[,"Temp.min"],mean)
matrix(monthly.fullmin[-49],ncol=4)
rowMeans(matrix(monthly.fullmin[-49],ncol=4))
points(rowMeans(matrix(monthly.fullmin[-49],ncol=4)),ylab = "Hőmérséklet  [°C]", xlab = "Hónapok", cex=1.5)
#
apply.monthly(daily[,"Temp.max"],mean)
monthly.fullmax<-apply.monthly(daily[,"Temp.max"],max)
matrix(monthly.fullmax[-49],ncol=4)
rowMeans(matrix(monthly.fullmax[-49],ncol=4))
points(rowMeans(matrix(monthly.fullmax[-49],ncol=4)),ylab = "Hőmérséklet  [°C]", xlab = "Hónapok", cex=1.5)
#


##havi csap
apply.monthly(daily[,1],sum)
monthly.fullcs<-apply.monthly(daily[,1],sum)
matrix(monthly.fullcs[-49],ncol=4)
rowMeans(matrix(monthly.fullcs[-49],ncol=4))
plot(rowMeans(matrix(monthly.fullcs[-49],ncol=4)),ylab = "Csapadék  [mm]", xlab = "Hónapok",type="h")


##

sum(daily["2014-10-01/2014-10-31",1] > 0)
sum(daily["2014-11-01/2014-11-30",1] > 0)
sum(daily["2014-12-01/2014-12-31",1] > 0)
sum(daily["2015-01-01/2015-01-31",1] > 0)
sum(daily["2015-02-01/2015-02-28",1] > 0)
sum(daily["2015-03-01/2015-03-31",1] > 0)
sum(daily["2015-04-01/2015-04-30",1] > 0)
sum(daily["2015-05-01/2015-05-31",1] > 0)
sum(daily["2015-06-01/2015-06-30",1] > 0)
sum(daily["2015-07-01/2015-07-31",1] > 0)
sum(daily["2015-08-01/2015-08-31",1] > 0)
sum(daily["2015-09-01/2015-09-30",1] > 0)

## 2015-16 hidrológiai év
sum(daily["2015-10-01/2015-10-31",1] > 0)
sum(daily["2015-11-01/2015-11-30",1] > 0)
sum(daily["2015-12-01/2015-12-31",1] > 0)
sum(daily["2016-01-01/2016-01-31",1] > 0)
sum(daily["2016-02-01/2016-02-29",1] > 0)
sum(daily["2016-03-01/2016-03-31",1] > 0)
sum(daily["2016-04-01/2016-04-30",1] > 0)
sum(daily["2016-05-01/2016-05-31",1] > 0)
sum(daily["2016-06-01/2016-06-30",1] > 0)
sum(daily["2016-07-01/2016-07-31",1] > 0)
sum(daily["2016-08-01/2016-08-31",1] > 0)
sum(daily["2016-09-01/2016-09-30",1] > 0)

## 2016-17 hidrológai év
sum(daily["2016-10-01/2016-10-31",1] > 0)
sum(daily["2016-11-01/2016-11-30",1] > 0)
sum(daily["2016-12-01/2016-12-31",1] > 0)
sum(daily["2017-01-01/2017-01-31",1] > 0)
sum(daily["2017-02-01/2017-02-28",1] > 0)
sum(daily["2017-03-01/2017-03-31",1] > 0)
sum(daily["2017-04-01/2017-04-30",1] > 0)
sum(daily["2017-05-01/2017-05-31",1] > 0)
sum(daily["2017-06-01/2017-06-30",1] > 0)
sum(daily["2017-07-01/2017-07-31",1] > 0)
sum(daily["2017-08-01/2017-08-31",1] > 0)
sum(daily["2017-09-01/2017-09-30",1] > 0)

sum(daily["2017-10-01/2017-10-31",1] > 0)
sum(daily["2017-11-01/2017-11-30",1] > 0)
sum(daily["2017-12-01/2017-12-31",1] > 0)
sum(daily["2018-01-01/2018-01-31",1] > 0)
sum(daily["2018-02-01/2018-02-28",1] > 0)
sum(daily["2018-03-01/2018-03-31",1] > 0)
sum(daily["2018-04-01/2018-04-30",1] > 0)
sum(daily["2018-05-01/2018-05-31",1] > 0)
sum(daily["2018-06-01/2018-06-30",1] > 0)
sum(daily["2018-07-01/2018-07-31",1] > 0)
sum(daily["2018-08-01/2018-08-31",1] > 0)
sum(daily["2018-09-01/2018-09-30",1] > 0)

sum(daily["2014-10-01/2015-09-30",1] > 0)
sum(daily["2015-10-01/2016-09-30",1] > 0)
sum(daily["2016-10-01/2017-09-30",1] > 0)
sum(daily["2017-10-01/2018-09-30",1] > 0)
sum(daily["2014-10-01/2018-09-30",1] > 0)

## 2015/16 havonta másként
apply.monthly(daily["2015-10-01/2016-09-30",1] > 0,sum)
