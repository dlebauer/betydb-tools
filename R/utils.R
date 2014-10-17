##' Check that traits have required covariates
##'
##' @title count traits with no covariates
##' @param var 
##' @param cov 
##' @return result of expect_equal
##' @author David LeBauer
expect_covs<- function(var, cov){
  query <-  paste("select count(traits.id) from traits where checked >=0 and variable_id in (select id from variables where name = '", var,"') and traits.id not in (select trait_id from covariates where covariates.variable_id in (select id from variables where name in (", vecpaste(cov),")));", sep = '')
  x <- as.numeric(query.bety(query, con = newcon()))
  return(expect_equal(x,0))
}

variable.has.ncitations <- function(variable, id = TRUE, url = FALSE, title = FALSE) {
  varinfo <- variable.has.nrecords(variable)
  if (sum(unlist(varinfo[1:3])) > 0) {
    result <- query.bety(paste('select distinct citation_id, citations.title, citations.url from traits
                                join citations on traits.citation_id = citations.id
                                where variable_id = ', varinfo$variables.id,';'))
  } else {
    result <- data.frame(NA)
    warning(paste('no results for variable', variable)) 
  }
  if(!is.na(result[1,1])) {
    if(id) print(result$citation_id)
    if(title) print(result$title)
    if(url) print(result$url)
  }
  return(result)
}

variable.has.nrecords <- function(variable) {
  ## Check if variable is in variables table
  check.var <- ifelse(!is.numeric(variable),
                      query.bety(paste("select count(1)
                                        from variables where name = '",variable,"';", sep = '')),
                      query.bety(paste("select count(1)
                                        from variables where id = ",variable,";", sep = '')))
  if(check.var > 0) {
    
    variables.id <- ifelse (!is.numeric(variable),
                            query.bety(paste("select id from variables
                                            where name = '", variable,"';", sep = ''))[1,1],
                            variable)
    variables.name <- query.bety(paste("select name from variables where id = ", variables.id,";"))[1,1]
    ntraits     <- query.bety(paste('select count(*) from traits
                                     where variable_id = ', variables.id,';'))[1,1]
    ncovariates <- query.bety(paste('select count(*) from covariates
                                     where variable_id = ', variables.id,';'))[1,1]
    npriors     <- query.bety(paste('select count(*) from priors
                                     where variable_id = ', variables.id,';'))[1,1]
    units       <- query.bety(paste('select units from variables
                                     where id = ', variables.id,';'))[1,1]
    ans <- (list(ntraits = replaceNAwith0(ntraits),
                 ncovariates = replaceNAwith0(ncovariates),
                 npriors = replaceNAwith0(npriors),
                 variables.id = variables.id,
                 variables.name = variables.name,
                 units = units))
  } else {
    warning(paste(variable, 'not in BETY'))
    ans <- NULL
  }
  return(ans)
}

variable.summary <- function(variable) {
  print(t(as.data.frame(variable.has.nrecords(variable))))
}


replaceNAwith0 <- function(result) {
  result <- ifelse(!is.numeric(result), 0, result)
  return(result)
}
##' Backup ebi_production to ebi_analysis
##'
##' Executes the db_copy.sh script on ebi-forecast
##' @title db_copy
##' @return nothing, copies ebi_production db to ebi_analysis db on ebi-forecast server
##' @author David LeBauer
db_copy <- function(){
  system("ssh ebi-forecast.igb.illinois.edu 'db_copy.sh'")
}


##' Remove 0000's, 00's from dates 
##'
##' replaces unknown years -> NA; unknown month -> Jan; unknown day -> 01 
##' @title newdate
##' @param date
##' @export
##' @return transformed date
##' @author David LeBauer
newdate <- function(date) {
  date <- substr(date, 1, 10)
  date <- gsub("0000", "NA", date)#year
  date <- gsub("-00-", "-01-", date)#month
  date <- gsub("-00", "-01", date)#day
  date[grepl("NA", date)] <- NA
  return(date)
}



##' Function to remove NA values from database queries
##' @title transform NA's
##' @param dt input data
##' @return dt with NA's replaced
##' @export
##' @author David LeBauer
transform.nas.dt <- function(dt){
  dt$control[is.na(dt$control)] <- 1
  dt$site_id[is.na(dt$site_id)] <- 0
  dt$n[is.na(dt$n)] <- 1
  dt$n[dt$n ==1 & is.na(dt$stat)] <- 2
  dt$greenhouse[is.na(dt$greenhouse)] <- 0
  return(dt)
}


##' Prepare trait data for JAGS meta-analysis
##' 
##' Convert queried data to format required by JAGS meta-analysis model
##' @title JagifyDT
##' @param dt input trait data
##' @return dt as data.table result transformed to meet requirements of PEcAn meta-analysis model
##' @export 
##' @author David LeBauer
jagify.dt <- function(dt){
  dt <- transform.nas.dt(dt)
  ans <- dt[control == 1, 
            list(yield_id,
                 mean, stat, n, 
                 site_id = as.integer(factor(site_id, unique(site_id))),
                 greenhouse = as.integer(factor(greenhouse, unique(greenhouse))),
                 citation_id)]
  
  if(any(ans$stat < 0, na.rm = TRUE)){
    stop("negative values of SE calculated")
  }
  return(ans)
  
}