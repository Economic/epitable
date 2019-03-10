#' @title A EPI HTML Table Making Function
#'
#' @description This is the table making function.
#' @param x is the data
#' @param rownamesvar variable name in x that identifies the rownames (either rownamesvar or rownames is required)
#' @param rownames vector that identifies the rownames (either rownames or rownamesvar is required)
#' @param colnames vector of column names
#' @param colgroups is a list of two specifically named vectors, names and pattern, that determine the column group names and the spanning pattern. The spanning pattern is the number of columns grouped under each column group. For example, colgroups = list(names = c("a group" "another group"), pattern = c(3, 2)) establishes two column groups, spanning 3 and 2 columns, respectively. See the vignettes for more examples.
#' @param rowlevels is a vector of integers indicating the indentation level (row-level) of each row. EPI's CSS allows for row levels 1 through 4.
#' @param header Set to FALSE if you want suppress the column headers.
#' @param file is the filename for saving the table snippet as a file. By default, this is NULL and epitable() does not write to a file.
#' @param selfcontained If writing to file, the default selfcontained=FALSE will write a table snippet. selfcontained=TRUE writes a complete html page, with header, body, etc.
#' @param extrarowX is used to add an extra row before row X of the table. See the vignettes for examples.
#' @keywords html tables
#' @export
#' @importFrom magrittr %>% %<>%
#' @importFrom readr read_file
#' @importFrom dplyr select pull mutate
#' @importFrom rlang enquo
#' @examples
#' # The output will print to the Rstudio viewer:
#' epitable(tradebalance, rownamesvar = industry)
#'
# epitable creates the table
epitable <- function(x,
                     rownamesvar,
                     rownames,
                     colnames=NULL,
                     colgroups=NULL,
                     select=NULL,
                     rowlevels=NULL,
                     header=TRUE,
                     file=NULL,
                     selfcontained=FALSE,
                     ...
                     ) {

  # checks on rownamesvar and rownames
  if (missing(rownamesvar) && missing(rownames)) {
    stop("You need to specify either rownamesvar or rownames")
  }
  if (!missing(rownamesvar) && !missing(rownames)) {
    stop("You cannot specify both rownamesvar and rownames. Pick one or the other.")
  }
  # if using rownamesvar clean up x so that it does not have rownamesvar
  # and assign rownames
  if (!missing(rownamesvar)) {
    if (!(deparse(substitute(rownamesvar)) %in% colnames(x))) {
      stop("rownamesvar needs to be a variable of your data")
    }
    rownames <- x %>% select(!!enquo(rownamesvar)) %>% pull()
    x %<>% select(-!!enquo(rownamesvar))
  }
  if (!missing(rownames) && (nrow(x) != length(rownames))) {
    stop("rownames needs to have the same number of rows as your data")
  }

  # select specified columns
  if (!is.null(select)) {
    x %<>% select(select)
  }

  # define table header
  the_header <- epitable_header(x = x, colnames = colnames, colgroups = colgroups, header = header)

  # define table body
  the_body <- epitable_body(x = x, rownames = rownames, rowlevels = rowlevels, ...)

  # create table
  the_table <- paste0("<table>\n")
  the_table %<>% paste0(the_header,"\n")
  the_table %<>% paste0(the_body,"\n")
  the_table %<>% paste0("</table>")

  # if writing to file
  if(!is.null(file)) {
    if (selfcontained) {
      htmlpage <- paste0(
        selfcontained_top(),
        the_table,
        selfcontained_bottom()
      )
      cat(htmlpage,file = file)
    } else cat(the_table, file = file)
  }

  class(the_table) = c("epitable")
  return(the_table)
}


#' @rdname epitable
#' @importFrom knitr knit_print
#' @importFrom knitr asis_output
#' @export
knit_print.epitable<- function(x, ...){
  paste(selfcontained_bodypre,x,selfcontained_bodypost) %>% asis_output()
}

#' @rdname epitable
#' @param useViewer When wrapping epitable in a print() command, set to FALSE to show snippet in console.
#' @export
#' @examples
#' # The output will print to the console:
#' print(epitable(tradebalance, rownamesvar = industry), useViewer=FALSE)
print.epitable <- function(x, useViewer=TRUE) {
  # taken from https://stackoverflow.com/a/22871109

  # create temp self-contained html page for viewer
  htmlpage <- paste0(
    selfcontained_top(),
    x,
    selfcontained_bottom()
  )
  htmlfile <- tempfile(fileext=".html")
  cat(htmlpage, file=htmlfile)

  if (useViewer) {
    # if useViewer then show in viewer or browser
    viewer <- getOption("viewer")
    if (!is.null(viewer)) {
      viewer(htmlfile)
    } else {
      utils::browseURL(htmlfile)
    }
  } else {
    # otherwise print snippet to console
    cat(x)
  }

  invisible(x)
}


#' @title Append multiple epitables
#'
#' @description Append multiple epitables together into a single table.
#' @param ... Comma separated tables.
#' @keywords html tables
#' @export
epitable_append <- function(..., file) {
  # confirm all arguments have class epitable
  verified <- lapply(list(...), verify_class_epitable) %>% unlist() %>% all()

  if (!verified) {
    stop("All arguments must have class epitable.")
  }

  x <- paste(...,sep="\n")
  stripped <- gsub("<table>", "", x)
  stripped <- gsub("</table>", "", stripped)
  the_table <- paste("<table>",stripped,"</table>",sep="\n")
  class(the_table) = c("epitable")
  return(the_table)

  # if writing to file
  if(!is.null(file)) {
    if (selfcontained) {
      htmlpage <- paste0(
        selfcontained_top(),
        the_table,
        selfcontained_bottom()
      )
      cat(htmlpage,file = file)
    } else cat(the_table, file = file)
  }
}

