## Telepítendő csomagok: xts readxl

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

ttbeolv2 <- beolv_boreasxls(ttfile[2])
ttnames <- names(ttbeolv2)

plot(ttbeolv2[,tti], main=ttnames[tti]);tti <- tti+1
