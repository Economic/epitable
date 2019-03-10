epitable_header <- function(x, colnames, colgroups, header) {
  if (!header) {
    the_header <- NULL
  } else {
    # create column groups
    the_colgroups <- add_colgroups(colgroups = colgroups)

    # create column names
    the_colnames <- add_colnames(x = x, colnames = colnames)

    # create table header
    the_header <- paste0("\n<thead>")
    the_header %<>% paste0(the_colgroups)
    the_header %<>% paste0(the_colnames)
    the_header %<>% paste0("\n</thead>")

    return(the_header)
  }
}


add_colnames <- function(x, colnames) {
  # begin column names
  the_colnames <- paste0("\n<tr>")

  # blank col above rownamesvar for now
  rownameheader <- ""
  the_colnames %<>% paste0("<th scope=\"col\">", rownameheader, "</th>")

  if (is.null(colnames)) {
    colnames <- colnames(x)
  }

  style<-paste("style=\"text-align: right;\"")
  for (col_j in 1:length(colnames)) {
    the_colnames %<>% paste("<th scope=\"col\"",style,">", colnames[col_j], "</th>")
  }
  # end column names
  the_colnames %<>% paste0("</tr>")

  return(the_colnames)
}


add_colgroups <- function(colgroups) {

  if (is.null(colgroups)) {
    the_colgroups <- NULL
  } else {
    colgroupsnames <- colgroups$names
    colgroupspattern <- colgroups$pattern

    # begin column groups
    the_colgroups <- paste0("\n<tr>")

    # blank first column
    the_colgroups %<>% paste0("\n<th scope=\"colgroup\">","</th>")

    for (col_j in 1:length(colgroupspattern)) {
      if (col_j == 1) {
        class<-NULL
      } else class<-paste("class=\"table-division-left\" ")

      the_colgroups %<>% paste0("\n<th ", class, "colspan=\"", colgroupspattern[col_j], "\" scope=\"colgroup\">", colgroupsnames[col_j], "</th>")
    }

    # end column groups
    the_colgroups %<>% paste0("</tr>")
  }

}
