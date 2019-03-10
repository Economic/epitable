epitable_body <- function(x, rownames, rowlevels, ...) {

  dotsargs <- list(...)

  # begin table body
  the_body <- paste("\n<tbody>")

  # loop over rows
  for (row_i in 1:nrow(x)) {

    # add extra row if necessary

    # set up arguments for extra row
    extrarowlist <- dotsargs[[paste0("extrarow",row_i)]]
    colgroupspattern <- extrarowlist$colgroups$pattern
    colgroupsnames <- extrarowlist$colgroups$names
    if (!is.null(colgroupspattern) && !is.null(colgroupsnames)) {
      extracolgroups <- TRUE
    } else extracolgroups <- FALSE

    if (!is.null(extrarowlist)) {
      # begin extra row

      if (is.null(extrarowlist$pseudoheader)) {
        extrarowlist$pseudoheader <- FALSE
      }
      if (extrarowlist$pseudoheader) {
        trclass<-paste0(" class=\"table-pseudo-header\"")
      } else trclass <-NULL

      the_body %<>% paste0("\n<tr",trclass,">")

      # add name to column 0
      if (!extracolgroups) {
        colspan0length <- length(x[1,]) + 1
      } else colspan0length <- 1
      colspanstr <- paste0(" colspan=\"", colspan0length, "\"")


      the_body %<>% paste0("\n<th", colspanstr, " scope=\"row\">", extrarowlist$name, "</th>")


      if (extracolgroups) {
        for (col_j in 1:length(colgroupspattern)) {
          the_body %<>% paste0("\n<th colspan=\"", colgroupspattern[col_j], "\" style=\"text-align: center;\">", colgroupsnames[col_j], "</th>")
        }
      }

      # end extra row
      the_body %<>% paste("\n</tr>")
    }



    # begin actual row
    if (is.null(rowlevels)) {
      the_body %<>% paste0("\n<tr>")
    } else if (rowlevels[row_i] == 1) {
      the_body %<>% paste0("\n<tr>")
    } else {
      the_body %<>% paste0("\n<tr class=\"row-level",rowlevels[row_i],"\">")
    }

    # row header
    style<-paste("style=\"min-width:12em;\" ")
    the_body %<>% paste0("\n<th ",style,"scope=\"row\">", rownames[row_i], "</th>")

    style<-paste("style=\"font-variant-numeric: tabular-nums; text-align: right;\"")

    # loop over columns
    for (col_j in 1:length(x[row_i,])) {
      the_body %<>% paste("\n<td",style,">",x[[row_i,col_j]], "</td>")
    }

    # end row
    the_body %<>% paste("\n</tr>")
  }

  # end table body
  the_body %<>% paste("\n</tbody>")

  return(the_body)

}

