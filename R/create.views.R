##' create planting, seeding, coppice views
##'
##' Loads and submits scripts to generate views. *views.sql files are located in inst/extdata and can be accessed with \code{system.file("traitsview.sql", package = "db")}
##' @title Create Views
##' @param views names of views to create (must have assocaites 'view'.sql in inst/extdata
##' @param con database connection
##' @return nothing, creates views in database as a side effect
##' @export
##' @author David LeBauer
##' @references Stack Overflow question \href{http://stackoverflow.com/questions/11418920/how-to-send-a-query-from-a-sql-file/}{how to send a query from a .sql file?}
##' @examples
##' \dontrun{create.views(views = c("mgmtview"))}
create.views <- function(views = c("traitsview", "yieldsview", "mgmtview")){
  for(view in views){
    file <- system.file(paste("extdata/", view, ".sql", sep = ""), package = "pecandb")
    createview.sql <- scan(file, sep = "\n", what = 'character')
    sapply(createview.sql, function(x) query.base(x))
  }
}
