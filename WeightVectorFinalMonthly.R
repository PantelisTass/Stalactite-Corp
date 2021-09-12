# Web Scraping: List of S&P 500 stocks
library(rvest)
library(magrittr) # needs to be run every time you start R and want to use %>%
library(quantmod)
library(comprehenr) # used for list comprehension

# get the URL for the wikipedia page with all SP500 symbols
url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"
# use that URL to scrape the SP500 table using rvest
tickers <- url %>%
  # read the HTML from the webpage
  read_html() %>%
  # one way to get table
  #html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>%
  # easier way to get table
  html_nodes(xpath = '//*[@id="constituents"]') %>% 
  html_table()
#create a vector of tickers
sp500tickers <- tickers[[1]]
sp500tickers$Symbol
sp500tickers = sp500tickers %>% mutate(Symbol = case_when(Symbol == "BRK.B" ~ "BRK-B",
                                                          Symbol == "BF.B" ~ "BF-B",
                                                          TRUE ~ as.character(Symbol)))
sp500 = sp500tickers$Symbol

#a way to get stocks through user input (NOT WORKING)
getSymbolsList = function(startDate, endDate){
  List <-c()
  x = TRUE
  while(x == TRUE){
    stockSymbol <- readline()
    stockSymbol = toupper(stockSymbol)
    if(stockSymbol == "QUIT."){
      x = FALSE
      break;
    }
    tryCatch(stonk = try(getSymbols(stockSymbol, from = startDate, to = endDate, warnings=FALSE, auto.assign = FALSE), periodicity = "monthly"),
             stop("Error at getSymbols"),
             error = function(e)
             {
               print(e$message)
               #"smthn happened") # or whatever error handling code you want
             }
    )
    dataFramey <- c(stockSymbol)
    print('Stock added!')
  }
  return(List)
}

giveSymbolsList = function(){
  x <- read.delim("/Users/pantelistassopoulos/Desktop/stockSymbolsFile.txt", header = TRUE, sep = "\t",)
  #x <- scan("C:\\Users\\USER\\Desktop\\stockSymbolsFile.txt", what = list(stockNo = 0, stockName = ""))
  x = x[ , order(colnames(x))]
  return(x)
}

Listy = giveSymbolsList()
print(Listy)

HistoricalPricesColumns = function(symbols, startDate, endDate){
  
  options("getSymbols.warning4.0"=FALSE)
  
  Cols = c()
  
  
  for(i in 1:ncol(symbols)){
    Cols = cbind(Cols, as.vector(getSymbols(colnames(symbols[i]), from = startDate, to = endDate, warnings = FALSE, auto.assign = FALSE, periodicity="monthly")[,2]))
                 
  }
  df = data.frame(Cols)
  #for(i in 1:nrow(symbols)){
  #  names(df)[i] = symbols[i,]
  #}
  
  colnames(df) = colnames(symbols)
  return(df)
}

priceTable = HistoricalPricesColumns(Listy, '2010-01-01', '2021-08-30')


library(quantmod)
options("getSymbols.warning4.0"=FALSE)


# Covariance Matrix Computations

#covariance matrix: 
corr = function(x,y){
  return(cov(x,y)/sqrt(var(x)*var(y)))
}
Cov_Matrix = function(df){
  matrix = matrix(0, nrow = ncol(df), ncol = ncol(df))
  for (i in 1:ncol(df)){
    for(j in 1:ncol(df)){
      matrix[i,j] = corr(df[,i], df[,j])
    }
  }
  covMat = as.data.frame(matrix)
  colnames(covMat) = colnames(df) 
  rownames(covMat) = colnames(df)
  return(covMat)
}


df <- data.frame(priceTable)
Cov_Matrix(df)
#produces a random sample of symbols from sp500 without replacement
#List = sample(sp500, 10, replace = FALSE, prob = NULL)
#print(priceTable))
final = Cov_Matrix(vec)
write.csv(final,"C:\\Users\\USER\\Desktop\\Test1.csv")
Expected_Return_Rates = matrix(0, nrow = 1, ncol = ncol(df))
#print(Expected_Return_Rates)
for (i in 1:ncol(priceTable)){
  vec = priceTable[2:nrow(priceTable),i]-priceTable[1:nrow(priceTable)-1,i]
  
  for (j in 1:nrow(priceTable)-1){
    vec[j] = vec[j]/priceTable[j,i]
  }
  Expected_Return_Rates[1,i] = sum(vec)/nrow(priceTable)
}
colnames(Expected_Return_Rates) = colnames(df) 
#print(Expected_Returns)
mu = Expected_Return_Rates[1,]
e = rep(1, ncol(priceTable))
V = Cov_Matrix(df)
A = t(e)%*%solve(V)%*%e
B = t(mu)%*%solve(V)%*%e
C = t(mu)%*%solve(V)%*%mu
Mu = 0.2
lambda_1_mu = (C-Mu*B)/(A*C-B*B)
lambda_2_mu = (Mu*A-B)/(A*C-B*B)
print(lambda_1_mu[1])
w = lambda_1_mu[1]*solve(V)%*%e+lambda_2_mu[1]*solve(V)%*%mu
write.csv(w,"WeightVector2_Mu=0,2_Monthly.csv")
print(sum(w))

Current_Stocks = to_vec(for(i in 1:10) getQuote(colnames(df)[i], warnings = FALSE, auto.assign = FALSE)[,2])

Account_Value = 100000
Numbers_Markowitz = matrix(0, nrow = 1, ncol = length(Listy))
Numbers_Markowitz[1,] = floor(Account_Value/(1+0.5*abs(sum(w[which(w<0)])))*w/Current_Stocks)
colnames(Numbers_Markowitz) = colnames(df) 
write.csv(Numbers_Markowitz,"Numbers_Markowitz_Mu=0,2_Monthly.csv")
