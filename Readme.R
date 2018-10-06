## A meteorológiai állomás 10 perces elemi adatai, illetve az azokból képzett
## statisztikák innen tölthetők le:
## http://met.boreas.hu/erti/
## wgettel leszedve az Excels könyvtárba letolt.txt letolt2.txt

## Loading
library("readxl")
# xls files
ttfile <- dir("Excels", patt="xls", full.names=TRUE)

## my_data <- read_excel(ttfile[1],range=cell_cols("A:T"))
my_data <- read_xls(ttfile[1],range=cell_cols("A:T"))

my_data.df <- as.data.frame(my_data)

plot(my_data.df[,1],my_data.df[,2],type="l")

library(xts)

my_data <- read_xls(ttfile[2],range=cell_cols("A:T"))
my_data.df <- as.data.frame(my_data)
my_data.xts <- xts(my_data.df[,-1],my_data.df[,1])

## Végignézés
ttnames <- names(my_data.xts)
tti <- 1

plot(my_data.xts[,tti], main=ttnames[tti]);tti <- tti+1
