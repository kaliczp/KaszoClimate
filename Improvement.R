##Hibák
## 2015-01 nagy szelek
tti <- 4;ttname <- 4
plot(rawlist[[tti]][,ttname], main=ttnames[ttname])
rawlist[[tti]][rawlist[[tti]][,ttname] > 100,ttname] <- NA
rawlist[[tti]][,ttname] <- na.approx(rawlist[[tti]][,ttname])
## Sugárzás??
ttname <- 17
plot(rawlist[[tti]][,ttname], main=ttnames[ttname])
rawlist[[tti]][rawlist[[tti]][,ttname] > 400,ttname]
plot(rawlist[[tti]]['2015-01-04',ttname]) ## 500+ januárban lehet?
## 600 + sec!!!

## 2015-10 nagy szelek
tti <- 13;ttname <- 4
plot(rawlist[[tti]][,ttname], main=ttnames[ttname])
rawlist[[tti]][rawlist[[tti]][,ttname] > 100,ttname] <- NA
rawlist[[tti]][,ttname] <- na.approx(rawlist[[tti]][,ttname])

## 2016-02 nagy szelek
tti <- 17;ttname <- 4
plot(rawlist[[tti]][,ttname], main=ttnames[ttname])
rawlist[[tti]][rawlist[[tti]][,ttname] > 100,ttname] <- NA
rawlist[[tti]][,ttname] <- na.approx(rawlist[[tti]][,ttname])

## 2016-11 nagy szelek
tti <- 26;ttname <- 4
plot(rawlist[[tti]][,ttname], main=ttnames[ttname])
rawlist[[tti]][rawlist[[tti]][,ttname] > 100,ttname] <- NA
rawlist[[tti]][,ttname] <- na.approx(rawlist[[tti]][,ttname])
ttname <- 5
rawlist[[tti]][rawlist[[tti]][,ttname] < 0,ttname] <- NA
rawlist[[tti]][,ttname] <- na.approx(rawlist[[tti]][,ttname])

## 2017-03 Extrém kis pára
tti <- 30;ttname <- 2
plot(rawlist[[tti]][,ttname], main=ttnames[ttname])
rawlist[[tti]][rawlist[[tti]][,ttname] < 10,ttname] <- NA
rawlist[[tti]][,ttname] <- na.approx(rawlist[[tti]][,ttname])

## 2017-04 Extrém kis pára
tti <- 31;ttname <- 2
plot(rawlist[[tti]][,ttname], main=ttnames[ttname])
rawlist[[tti]][rawlist[[tti]][,ttname] < 10,ttname] <- NA
rawlist[[tti]][,ttname] <- na.approx(rawlist[[tti]][,ttname])
## Légnyomás
ttname <- 3
rawlist[[tti]][rawlist[[tti]][,ttname] > 1050,ttname] <- NA
rawlist[[tti]][,ttname] <- na.approx(rawlist[[tti]][,ttname])

## 2017-05 nagy szelek
tti <- 32;ttname <- 4
plot(rawlist[[tti]][,ttname], main=ttnames[ttname])
rawlist[[tti]][rawlist[[tti]][,ttname] > 100,ttname] <- NA
rawlist[[tti]][,ttname] <- na.approx(rawlist[[tti]][,ttname])
ttname <- 5
plot(rawlist[[tti]][,ttname], main=ttnames[ttname])
rawlist[[tti]][rawlist[[tti]][,ttname] < 0,ttname] <- NA
rawlist[[tti]][,ttname] <- na.approx(rawlist[[tti]][,ttname])

## 2018-02-27
## Nagyon nagy csapadék intenzitás 257.1 mm/h
tti <- 41

## 2018-05 nagy szelek
tti <- 44;ttname <- 4
plot(rawlist[[tti]][,ttname], main=ttnames[ttname])
rawlist[[tti]][rawlist[[tti]][,ttname] > 100,ttname] <- NA
rawlist[[tti]][,ttname] <- na.approx(rawlist[[tti]][,ttname])
ttname <- 5
plot(rawlist[[tti]][,ttname], main=ttnames[ttname])
rawlist[[tti]][rawlist[[tti]][,ttname] < 0,ttname] <- NA
rawlist[[tti]][,ttname] <- na.approx(rawlist[[tti]][,ttname])

## 2018-07 nagy szelek
tti <- 46;ttname <- 4
plot(rawlist[[tti]][,ttname], main=ttnames[ttname])
rawlist[[tti]][rawlist[[tti]][,ttname] > 100,ttname] <- NA
rawlist[[tti]][,ttname] <- na.approx(rawlist[[tti]][,ttname])
ttname <- 5
plot(rawlist[[tti]][,ttname], main=ttnames[ttname])
rawlist[[tti]][rawlist[[tti]][,ttname] < 0,ttname] <- NA
rawlist[[tti]][,ttname] <- na.approx(rawlist[[tti]][,ttname])

## 2018-10 nagy szelek
tti <- 49;ttname <- 4
plot(rawlist[[tti]][,ttname], main=ttnames[ttname])
rawlist[[tti]][rawlist[[tti]][,ttname] > 100,ttname] <- NA
rawlist[[tti]][,ttname] <- na.approx(rawlist[[tti]][,ttname])
ttname <- 5
plot(rawlist[[tti]][,ttname], main=ttnames[ttname])
rawlist[[tti]][rawlist[[tti]][,ttname] < 0,ttname] <- NA
rawlist[[tti]][,ttname] <- na.approx(rawlist[[tti]][,ttname])
