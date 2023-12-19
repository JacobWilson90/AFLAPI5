#'Champion Data AFL API
#'
#'Hit the Champion Data AFL with a given endpoint
#'@param endpoint The full path of an endpoint including path and query parameters, excluding the base_url and version components. eg: "Leagues" rather than "https://api.afl.championdata.io/v1/Leagues"
#'@param envir A short code representing the environment to hit. Empty string = Production, 'sandbox' = Sandbox environment, 'dev' = Development environment (Internal Champion Data use only).
#'@param base_url The base URL for the API. Defaults to the AFL API.
#'@param version The version number of the API. Defaults to 1.
#'@param df Logical. Convert to data.frame? Note: Will only work for endpoints with a flat JSON response.
#'@return A numerical value for league ID.
#'@examples
#'cdAPI('matches/113770401')
#'@export
cdAPI <- function(endpoint,envir=c('','sandbox','dev'),base_url='https://api.afl.championdata.io',version='v1',df=TRUE){
  
  api_url <- gsub('afl-.ch','afl.ch',gsub('afl',paste('afl',envir[1],sep='-'),base_url))
  r       <- GET(modify_url(api_url,path=paste(version,endpoint,sep='/')),authenticate(api_un,api_pw))
  
  if(r$status==403){
    message("Match yet to start")
    return(NULL)
  }
  if(df){
    if(length(content(r))>1){
      out <- data.frame(fromJSON(content(r,'text'),flatten=TRUE))
    }else{
      out <- data.frame(unlist(content(r)))
    }
    return(out)
  }else{
    return(r)
  }
}

#'to_minsec
#'
#'Convert an integer of seconds elapsed to a text string with minutes and seconds.
#'@param x Seconds, as an integer.
#'@param leadingZero Logical. Add a leading zero to minute values less than 10?
#'@return A text string in the form "MI:SS" - eg. "9:54" if @param leadingZero is FALSE, "09:54" if @param leadingZero is TRUE.
#'@examples
#'to_minsec(594)
#'@export
to_minsec <- function(x,leadingZero=FALSE){
  require(tidyverse)
  mins <- floor(x/60)
  secs <- x %% 60
  if(leadingZero){
    mins_fill <- if_else(mins<10,paste("0",mins,sep=''),as.character(mins))
  }else{
    mins_fill <- mins
  }
  secs_fill <- if_else(secs<10,paste("0",secs,sep=''),as.character(secs))
  output <- paste(mins_fill,secs_fill,sep=':')
  return(output)
}

