## Telepítendő csomagok: xts readxl
.First <- function(){Sys.setenv(TZ='UTC')}

## A meteorológiai állomás 10 perces elemi adatai, illetve az azokból képzett
## statisztikák innen tölthetők le:
## http://met.boreas.hu/erti/
## wgettel leszedve az Excels könyvtárba letolt.txt letolt2.txt

# xls files
ttfile <- dir("Excels", patt="xls", full.names=TRUE)

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
plot(daily[, "Temp.2m.C"], ylim = c(min(daily[, "Temp.min"]), max(daily[, "Temp.max"])), col="ivory4")
lines(daily[, "Temp.min"],col="ivory2")
lines(daily[, "Temp.max"],col="black")

## Indexeles
plot(daily['2015-10-01/2016-09-30',1], type="h")
## Hidrologiai ev osszeg
sum(daily['2015-10-01/2016-09-30',1])
