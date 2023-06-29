# To clean global environment
# rm(list=ls())


# LIBRARIES


## 1. IMPORTING DATA
# read.csv() -> Separator: Comma-separated, Decimals: point, Header: yes
dfRow <- read.csv(choose.files(), na.strings = c("", " ", "NA", " NA ", "<NA>"))


## 2. DATA SANITY CHECKS
## 2.1. Structure of dataframe.
str(dfRow) # 'data.frame':	13027 obs. of  12 variables
summary(dfRow)
head(dfRow)

## 2.1.1 Descriptive analysis. Character or Factor.
for(i in 1:length(colnames(dfRow))){
  solution <- paste(length(unique(dfRow[[i]])), "unique values in column names:", colnames(dfRow)[i])
  
  print(solution)
}

unique(dfRow$Period) # -> Invalid values: -
unique(dfRow$Year) # -> Invalid values: -
unique(dfRow$Month) # -> Invalid values: -
unique(dfRow$Flow) # -> Invalid values: -
unique(dfRow$Port) # -> Invalid values: -
unique(dfRow$Region) # -> Invalid values: -
unique(dfRow$Value..CN..) # -> Invalid values: "Confidential"
unique(dfRow$Value..US..) # -> Invalid values: "Confidential"
unique(dfRow$Price..CN..GJ.) # -> Invalid values: "Confidential"
unique(dfRow$Price..US..MMBTU.) # -> Invalid values: "Confidential"

dfRow[dfRow$Year==1985, c("Period", "Year", "Month")] # -> dfRow$Period can be avoided

## 2.1.2 Final dataframe.
df1 <- transform(dfRow,
                 year=as.integer(dfRow$Year),
                 month=factor(dfRow$Month, levels=month.name, ordered=T),
                 flow=factor(dfRow$Flow),
                 port=factor(dfRow$Port),
                 region=factor(dfRow$Region),
                 volumen_10k_m3=dfRow$Volume..10.3m3.,
                 volume_MCF=dfRow$Volume..MCF.,
                 value_US=as.numeric(dfRow$Value..US..),
                 avgPrice_US_MMBTU=as.numeric(dfRow$Price..US..MMBTU.))

df1 <- df1[,-(1:12)]

str(df1)
summary(df1)
head(df1)

## 2.2. Checking irregular values. 
## 2.2.1 Deleting "Total" value from region and port.
levels(df1$port) # -> Invalid values: "Total"
levels(df1$region) # -> Invalid values: "Total"

df1 <- df1[which(df1$region!="Total"),]
df1[which(df1$region=="Total"),]
df1[which(df1$port=="Total"),]

## 2.2.2 Checking min values in value_US, avgPrice_US_MMBTU, volumen_10k_m3
summary(df1[,c('value_US','avgPrice_US_MMBTU','volumen_10k_m3','volume_MCF')])

df1[which(df1$value_US==0), c('year','month')] # -> cero value comes from 1985 to 1990
df1[which(df1$avgPrice_US_MMBTU==0), c('year','month')] # -> cero value comes from 1985 to 1990 and one in 2019
df1[which(df1$volumen_10k_m3==0), c('year','month')] # no rows, but summary min=0?
df1[which(df1$volume_MCF==1), c('year','month')] # no rows, but summary min=1?

# We'll filter from 1991 to avoid blanks, zeros.
df1 <- df1[df1$year>1990,]

summary(df1)
str(df1)


## 3. Creating a new csv File

write.csv(df1, file="D:\\Maestrias, Diplomaturas, Cursos\\Programming\\Python\\Projects\\Machine_Learning\\Natural gas exports and imports\\natural-gas-exports-and-imports-monthly(df1).csv",
          na="", row.names=F)
