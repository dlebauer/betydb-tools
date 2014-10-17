##' Extract yield data from BETYdb
##'
##' query.yield.data extracts yield data from BETYdb for a given set of species,
##' converts all statistics to summary statistics, and prepares a dataframe for use in meta-analysis.
##' @name query.yield.data
##' @param spstr is the species.id integer or string of 
##' @name
##' integers associated with the species
##' @export
##' @return dataframe ready for use in meta-analysis
query.yield.data <- function(spstr){  
  if(is.list(con)){
    print("query.yield.data")
    print("WEB QUERY OF DATABASE NOT IMPLEMENTED")
    return(NULL)
  } 
  
  data <- query.yields(spstr, con=con)
  result <- data

  ## if result is empty, stop run
  print(result)
  if(nrow(result)==0) {
    return(NA)
  }
  jagged <- jagify(result)
  renamed <- rename.jags.columns(jagged)
  return(renamed)
}
 

##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title Query Yields
##' @param spstr 
##' @param ... 
##' @return queries yield data for a given species string
##' @export
##' @author David LeBauer
query.yields <- function(spstr){
  result <- query.base(paste("select yields.id as yield_id, treatment_id as trt_id, site_id, citation_id, greenhouse, scientificname, genus, AcceptedSymbol, commonname, mean, statname, stat, n from yields join species on yields.specie_id = species.id join sites on yields.site_id = sites.id join treatments on yields.treatment_id = treatments.id where specie_id in (", spstr,");"))
  return(result)
}

query.mgmt  <- function(...){
  q <- query.bety("select y.id as yield_id, t.id as trt_id, t.name, t.control, m.citation_id, m.date, m.dateloc, m.mgmttype, m.level, m.units, m.notes from managements as m join managements_treatments as mt on mt.management_id = m.id join treatments as t on mt.treatment_id = t.id join yields as y on y.treatment_id = t.id;")
}


##' Retrieve Yield data with meta-data
##'
##' @title get yield DT
##' @param dt 
##' @return data table
##' @export
##' @author David LeBauer
get.yield.DT <- function(list = bety){

  yields <- bety$yields[,list(citation_id, site_id, specie_id, cultivar_id, treatment_id,
                              date = newdate(date), dateloc, 
                              statname, stat, mean, n, notes, yield_id)]
  setkey(yields, yield_id)
  test_that("yields are reasonable",
            expect_true(yields[,max(mean)] < 500))
  
  species <- bety$species[,list(specie_id, scientificname, genus)]
  setkey(species, specie_id)
  traits  <- transform(bety$traits, newdate(date))
  treatments <- bety$treatments
  
  sites  <- bety$sites[,list(site_id, lat, lon, city, sitename, greenhouse = as.numeric(greenhouse))]
  setkey(sites, 'site_id')
  
  managements <- bety$managements[, list(management_id, citation_id, dateloc, mgmttype, level, units, notes, date = newdate(date))]
  setkey(managements, management_id)
  
  managements_treatments <- bety$managements_treatments[,list(management_id, treatment_id)]
  setkey(managements_treatments, management_id)
  
  test_that("all tables have appropriate key",{
    expect_equal(key(yields), "yield_id")
    expect_equal(key(traits), "trait_id")
    expect_equal(key(sites),  "site_id")
    expect_equal(key(managements), "management_id")
    expect_equal(key(treatments), "treatment_id")
    expect_equal(key(species), "specie_id")
  })
  
  ##################
  ##   Yields     ##
  ##################
  
  ## join tables to make something useful:
  yield2 <- merge(yields, species, by = "specie_id", all.x = TRUE)
  yield3 <- merge(yield2, treatments, by = "treatment_id", all.x = TRUE)
  yield4 <- merge(yield3, sites, by = "site_id", all.x = TRUE)
  
  Y <- transform(yield4,
                 trt_id = ifelse(is.na(treatment_id), 0, treatment_id),
                 control = ifelse(is.na(control), 1, control))
  return(Y)
}