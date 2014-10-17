##' .. content for \descrption{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Query data by trait 
##' @param trait 
##' @param con 
##' @return data frame with species, mean, se, and n 
##' @author David LeBauer
query.traitdata <- function(trait){
  query <- paste("select sp, mean, statname, stat, n from traitsview where trait = '", trait, "';", sep = '')
  ans <- query.base(query)
  ans <- transformstats(ans)
  ans <- ans[, c('sp', 'mean', 'stat', 'n')]
  colnames(ans) <- c('species', 'mean', 'se', 'n')
  return(ans)
}
##' import key tables
##'
##' returns tables to the working environment
##' @title Pull Tables
##' @param tables 
##' @return list of tables from BETYdb as data.tables
##' @export
##' @author David LeBauer
pull.tables <- function(tables = c("yields", "traits", "managements_treatments", "managements", "treatments", "species")){
  betydb <- lapply(tables, pull.table)
}

pull.table <- function(table){
  ans <- query.base(paste("select * from", table, ";"))
  ans.dt <- data.table(ans)
  return(ans.dt)
}
                        
