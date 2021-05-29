#------------entering the database for analysis-----------------------------------------
# include the archive in "C:/bechdel.xlsx"
#install.packages("readxl")# in case of necessary!
library(readxl)#acessing the library for readering of the archive .xlsx
# Part 1: Data setup
bechdel <- data.frame(read_excel("C:/bechdel.xlsx")[-1,1:10])
bechdel <- bechdel[order(bechdel$Year),] #sorting the data set
head(bechdel)
year <- as.numeric(bechdel$Year);table(year);length(year) #generates the "year" variable
order(year)
sort(year)
imdb <- as.character(bechdel$IMDB.code)#generates the "imdb" variable
# construct IMDB url
IMDB <- 0
for(i in 1:dim(bechdel)[1]){
  IMDB[i] <- paste0("http://www.imdb.com/title/",imdb[i])
}
title <- "Movie test"  #generates the title
  
budget <- as.numeric(bechdel$Budget) #generates the "budget" variable

domgross <- as.numeric(bechdel$Dom.gross) #contain (NAs); generates the "domgross" variable

intgross <- as.numeric(bechdel$Int.Gross) #contain (NAs); generates the "intgross" variable

period_code <- as.character(bechdel$Code) #generates the "period_code" variable

budget_2013 <- as.numeric(bechdel$budget.2013.dollars) #generates the "budget_2013" variable

domgross_2013 <- as.numeric(bechdel$Dom.gross.2013.dollars) #contain (NAs); generates the "domgross_2013" variable

intgross_2013 <- as.numeric(bechdel$Int.gross.2013.dollars) #contain (NAs); generates the "intgross_2013" variable

binary <- bechdel$Simple.pass...fail
for(i in 1:length(binary)){
  if(binary[i] == "PASS"){binary[i] = as.numeric(1)}
  else{binary[i] = as.numeric(0)}
                          } # assign value "0" to "FAIL" and "1" to "PASS"
binary <- as.numeric(binary) # convert to numeric the value 0 and 1

table(binary) # showing the table of binary


#Part 2: Data analysis
# i) Summarise the data
lista <- list(year,budget,domgross,intgross,budget_2013,
     domgross_2013,intgross_2013,binary)
summarise <- list(year=0,budget=0,domgross=0,intgross=0,
                  budget_2013=0,domgross_2013=0,intgross_2013=0,binary=0)
for(i in 1:length(lista)){
  summarise[[i]] <- summary(lista[[i]])
}
summarise$year
par(mfrow=c(2,4))
summarise$budget;boxplot(summarise$budget,col="grey",main="Budget")
sd(budget) #standart deviation to budget

summarise$domgross;boxplot(summarise$domgross, col="blue",main="Domgross")
sd(domgross) #standart deviation to domgross

summarise$intgross;boxplot(summarise$intgross, col="green",main="Intgross")
sd(intgross) #standart deviation to intgross

summarise$budget_2013;boxplot(summarise$budget_2013, col="red",main="Budget 2013")
sd(budget_2013) #standart deviation to budget_2013

summarise$domgross_2013;boxplot(summarise$domgross_2013, col="purple",main="Domgross 2013")
sd(domgross_2013) #standart deviation to domgross_2013

summarise$intgross_2013;boxplot(summarise$intgross_2013, col="pink",main="Intgross 2013")
sd(intgross_2013) #standart deviation to intgross_2013

dataframe <- data.frame(year,budget,domgross,intgross,budget_2013,
                        domgross_2013,intgross_2013,binary)
par(mfrow=c(1,1))

P_PASS_1970_1975 = sum(dataframe[year >= 1970 & year < 1975,]$binary== 1)/
  length(dataframe[year >= 1970 & year < 1975,]$binary== 1)
P_FAIL_1970_1975 = 1-P_PASS_1970_1975

P_PASS_1975_1980 = sum(dataframe[year >= 1975 & year < 1980,]$binary== 1)/
  length(dataframe[year >= 1975 & year < 1980,]$binary== 1)
P_FAIL_1975_1980 = 1-P_PASS_1975_1980

P_PASS_1980_1985 = sum(dataframe[year >= 1980 & year < 1985,]$binary== 1)/
  length(dataframe[year >= 1980 & year < 1985,]$binary== 1)
P_FAIL_1980_1985 = 1-P_PASS_1980_1985

P_PASS_1985_1990 = sum(dataframe[year >= 1985 & year < 1990,]$binary== 1)/
  length(dataframe[year >= 1985 & year < 1990,]$binary== 1)
P_FAIL_1985_1990 = 1-P_PASS_1985_1990

P_PASS_1990_1995 = sum(dataframe[year >= 1990 & year < 1995,]$binary== 1)/
  length(dataframe[year >= 1990 & year < 1995,]$binary== 1)
P_FAIL_1990_1995 = 1-P_PASS_1990_1995

P_PASS_1995_2000 = sum(dataframe[year >= 1995 & year < 2000,]$binary== 1)/
  length(dataframe[year >= 1995 & year < 2000,]$binary== 1)
P_FAIL_1995_2000 = 1-P_PASS_1995_2000

P_PASS_2000_2005 = sum(dataframe[year >= 2000 & year < 2005,]$binary== 1)/
  length(dataframe[year >= 2000 & year < 2005,]$binary== 1)
P_FAIL_2000_2005 = 1-P_PASS_2000_2005

P_PASS_2005_2010 = sum(dataframe[year >= 2005 & year < 2010,]$binary== 1)/
  length(dataframe[year >= 2005 & year < 2010,]$binary== 1)
P_FAIL_2005_2010 = 1-P_PASS_2005_2010

P_PASS_2010_2015 = sum(dataframe[year >= 2010 & year <= 2015,]$binary== 1)/
  length(dataframe[year >= 2010 & year <= 2015,]$binary== 1)
P_FAIL_2010_2015 = 1-P_PASS_2010_2015

PASS <- round(c(P_PASS_1970_1975,
          P_PASS_1975_1980,
          P_PASS_1980_1985,
          P_PASS_1985_1990,
          P_PASS_1990_1995,
          P_PASS_1995_2000,
          P_PASS_2000_2005,
          P_PASS_2005_2010,
          P_PASS_2010_2015),2)

FAIL <- round(c(P_FAIL_1970_1975,
           P_FAIL_1975_1980,
           P_FAIL_1980_1985,
           P_FAIL_1985_1990,
           P_FAIL_1990_1995,
           P_FAIL_1995_2000,
           P_FAIL_2000_2005,
           P_FAIL_2005_2010,
           P_FAIL_2010_2015),2)


PASS_names <- c("1975_1980",
                "1980_1985",
                "1985_1990",
                "1990_1995",
                "1995_2000",
                "2000_2005",
                "2005_2010",
                "2010_2015")

# What percentage of movies passed the test, based on the binary definition?
P_PASS <- sum(binary)/length(binary)
# The percentage of movies passed the test is
P_PASS

# How does the percentage of movies passing change over time?
#YES, do watch the Histogram, he show one change in each period.

hist(year,main = "Frequency of Movie Produced by Year")
legend("topleft",legend = paste(PASS_names, " = ", PASS,"%"),
       cex=.575,title = "Percentage of PASS in \n Each Interval")
legend("top",legend = paste(PASS_names, " = ", FAIL,"%"),
       cex=.575,title =  "Percentage of FAIL in \n Each Interval")

year_movie <- seq(1970,2013,1);year_movie

table(dataframe$year)
Num_Pass <- 0
Num_Fail <- 0
percentage <- 0

for(i in 1:length(year_movie)){
Num_Pass[i] <- sum(dataframe[year==year_movie[i],]$binary)
Num_Fail[i] <-length(dataframe[year==year_movie[i],]$binary)-Num_Pass[i]
#percentage of movie what PASS in each year
percentage[i] <- Num_Pass[i]/(Num_Pass[i]+Num_Fail[i])
}

plot(year_movie,percentage,type = "l",lty=2,col="blue",
     main="Percentage of Movie What PASS In Each Year",
     xlab = "Year of Movie",ylab = "Percentage in Each Year")

plot(year_movie,Num_Pass,type = "l",lty=2,col="blue",
     main="Number of Movie what PASS In Each Year",
     xlab = "Year of Movie",ylab = "Quanties of Movie")

#Time Series of Proportion of Movie
plot(year_movie,Num_Pass/(sum(Num_Pass)),type = "l",lty=2,col="blue",
     main=" Time Series of Proportion of Movie what PASS",
     xlab = "Year of Movie",ylab = "Proportion")

# ii) Propose a model that will predict
#install.packages("asbio")
library(asbio)
lms <- lm(binary~budget+domgross+intgross)
model_predict <- step(lms,direction = "backward")# Model Predictive


#verify the locat of the archive
teste_csv <- read.csv("C:/test.csv",header = T,dec=".",sep=",")
head(teste_csv)

uns <- rep(1,dim(teste_csv)[1])
matr <- matrix(c(uns,teste_csv$budget,teste_csv$domgross,
                 teste_csv$intgross),
               dim(teste_csv)[1],4,byrow = F);matr


predicti <-  model_predict$coefficients%*%t(matr)[,1:dim(teste_csv)[1]]

classify <- 0

for(i in 1:dim(teste_csv)[1]){
      ifelse(predicti[,i] >= 0.50000000, classify[i] <- 1, classify[i] <- 0)
      }
mdb <- cbind.data.frame(imdb = teste_csv$imdb,classify)
head(mdb)
write.csv(mdb,file = "Users/c4r10s/Documents/mdb.csv")

















