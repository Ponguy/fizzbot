# Intro ------------

rm(list = ls())
library(httr)
library(magrittr)
library(zoo)
root <- "https://api.noopschallenge.com"

# Qfun ----

qget <- function(url){
  fiz_loc <- paste0(root,url)
  GET(fiz_loc) %>% content() %>% return
}

qpost <- function(url, a){
  p <- POST(paste0(root,url), body = a, encode = "json") 
  p %>% content() %>% return
}

# FizzFun -----

fizzfun <- function(q){
  
  q$rules %<>% unlist
  divisors <- q$rules[q$rules %>% names %in% "number"] %>% as.numeric()
  response <- q$rules[q$rules %>% names %in% "response"]
  
  a <- list()
  
  for( i in index(q$numbers)){
    
    n <- q$numbers[i] %>% unlist
    
    test <- sapply(divisors,
                   function(x)mod(n,x)) == 0
    
    if(!any(test)){#none are True, neither fizz nor buzz
      a[[i]] <- n
    }else{
      a[[i]] <- paste(response[test], collapse = "") 
    }
  }
  a %<>% paste(collapse = " ") %>% list(answer = .)
  return(a)
}

get_post <- function(url){
  
 repeat {
  sub <- qget(url) %>% #get the q
  fizzfun() %>% # compute the answer
  qpost(url,.) # post the answer
  
  url <- sub$nextQuestion
  
  if(!is.null(sub$grade)){ #if it is not null, then we have reached the final iteration
    return(sub)
  }
 }
}
# Q0 -----

q1_sub <- qget("/fizzbot")$nextQuestion %>% 
qpost(.,list(answer = "R"))

get_post(q1_sub$nextQuestion)
