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
tt3 <- apply.daily(rawlist[[tti]][,-c(5,7:19)], mean)
## Összefűzés
daily <- merge.xts(tt1,tt2,tt3)

for(tti in 2:length(rawlist)){
## Napi csapadék és napsütés összeg
tt1 <- apply.daily(rawlist[[tti]][,11], sum)
tt2 <- apply.daily(rawlist[[tti]][,19], sum)
## Napi átlagok
tt3 <- apply.daily(rawlist[[tti]][,-c(5,7:19)], mean)
## Összefűzés
daily <- c(daily, merge.xts(tt1,tt2,tt3))
}
