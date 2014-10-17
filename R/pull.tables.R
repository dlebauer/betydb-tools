##' Query all data from traitsview by trait
##'
##' @title Query data by trait 
##' @param trait 
##' @export
##' @return data frame with species, mean, se, and n 
##' @author David LeBauer
query.traitdata <- function(trait){
  query <- paste("select sp, mean, statname, stat, n from traitsview where trait = '", trait, "';", sep = '')
  ans <- query.base(query)
  ans <- pecan.transformstats(ans)
  ans <- ans[, c('sp', 'mean', 'stat', 'n')]
  colnames(ans) <- c('species', 'mean', 'se', 'n')
  return(ans)
}


##' Import tables from BETYdb as data.frames
##'
##' called by \code{\link{pull.data.tables}}
##' @title Pull tables
##' @param dbtabnames names of tables to pull
##' @param ... additional arguments to \code{\link{query.bety}}
##' @return list of data.frames pulled from database
##' @export
##' @author David LeBauer
pull.tables <- function(dbtabnames = c('yields', 'managements_treatments', 'managements',
                                       'species', 'sites', 'traits', 'treatments', 
                                       'covariates', 'variables'),
                        con = con){
  dbtables <- list()
  for(i in dbtabnames){
    dbtables[[i]] <- query.base(paste("select * from ", i, ";"), con = con)
    if("date" %in% colnames(dbtables[[i]])){
      transform(dbtables[[i]], date = gsub("-00", "-01", date))
    }
  }
  renamed <- rename.keys(dbtables)
  return(renamed)
}

##' Import tables from BETYdb as data.tables 
##'
##' @title Pull Data Tables 
##' @param ... arguments passed to \code{\link{pull.tables}}
##' @return list of BETYdb tables imported as data.tables 
##' @export
##' @author David LeBauer
pull.data.tables <- function(...){
  require(data.table)
  df.tables <- pull.tables(...)
  dt.tables <- list()
  for(i in names(df.tables)){
    dt.tables[[i]] <- data.table(df.tables[[i]])
    if(!grepl("view", i)){
      newkey <- paste(substring(i, 1, nchar(i)-1), "_id", sep = "")
      if(!grepl("_", i)){## exclude lookup tables with "_"; grepl returns logical
        setkeyv(dt.tables[[i]], newkey) 
      }
    }
  }
  return(dt.tables)
}
                                 
rename.keys <- function(dbtables){
  for (tabname in names(dbtables)){
    newtabkey <- paste(substring(tabname, 1, nchar(tabname)-1), "_id", sep = "") 
    colnames(dbtables[[tabname]])[colnames(dbtables[[tabname]]) == "id"] <- newtabkey
  }
  return(dbtables)
}


######################## MANAGEMENT FUNCTIONS #################################
## these functions are analogs to the covariate functions in pecan, but need work 
append.management <- function(data, managements.data = list(...)){
  merged <- merge(data, managements.data, by = "treatment_id", all.x = TRUE, all.y = FALSE)
  return(merged)
}
query.managements <- function(yield_ids = "0", mgmttype = "planting", table = "yields"){
  mgmt.query <- paste("select y.id as yield_id, mt.treatment_id, mt.management_id,",
                      "m.mgmttype, m.date, m.level, m.units", 
                      "from managements as m",
                      "join managements_treatments as mt on m.id = mt.management_id",
                      "join yields as y on y.treatment_id = mt.treatment_id",
                      "where mgmttype like ", vecpaste(mgmttype) ," and",
                      "y.id in (", vecpaste(yield_ids),");")

  managements = query.base(mgmt.query)
  return(managements)
}

