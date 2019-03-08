# load EPI CSS
epicss <-
  system.file("extdata", "epi-chart.css" ,package = "epitable") %>%
  read_file() %>%
#  paste("html * {font-family: 'Proxima Nova', 'Lato' !important;}\n") %>%
  paste(".figure.figure-theme-clean{margin:0;border-top:none;}\n") %>%
  paste(".figure.figure-theme-clean .figInner{border-bottom:none;}\n") %>%
  paste(".figure .figInner table th[scope=\"col\"][colspan],.figure .figInner table th[scope=\"colgroup\"][colspan]{text-transform:none}\n")


# load complete test table for testing purposes
testtable <- system.file("extdata", "testtable.html", package = "epitable")

selfcontained_bodypre <- paste("<div class=\"figure figure-table figure-theme-clean\"><div class=\"figInner\">","\n")
selfcontained_bodypost <- paste("</div></div>\n")

# top matter for self-contained webpage
selfcontained_top <- function() {
  htmlpage <- paste("<html>",
                    "<head>",
                    "<meta http-equiv=\"Content-type\" content=\"text/html;charset=UTF-8\">",
                    "<style>",
                    epicss,
                    "</style>",
                    "</head>",
                    "<body>",
                    selfcontained_bodypre,
                    "",
                    sep="\n")
  return(htmlpage)
}

# bottom matter for self-contained webpage
selfcontained_bottom <- function() {
  htmlpage <- paste("",
                    selfcontained_bodypost,
                    "</body>",
                    "</html>",
                    sep="\n")
  return(htmlpage)
}

table_meat <- function(x,
                       rownames,
                       rowlevels,
                       colnames,
                       colgroups,
                       header
                       ) {

  table_input <- x %>% as.data.frame()

  if (header) {
    # begin table header
    the_header <- paste0("\n<thead>")

    if (!is.null(colgroups)) {
      colgroupsnames <- colgroups$names
      colgroupspattern <- colgroups$pattern

      # begin column groups
      the_header %<>% paste0("\n<tr>")
      # blank first column
      the_header %<>% paste0("\n<th scope=\"colgroup\">","</th>")
      for (col_j in 1:length(colgroupspattern)) {
        if (col_j == 1) {
          class<-NULL
        } else class<-paste("class=\"table-division-left\" ")

        the_header %<>% paste0("\n<th ", class, "colspan=\"", colgroupspattern[col_j], "\" scope=\"colgroup\">", colgroupsnames[col_j], "</th>")
      }

      # end column groups
      the_header %<>% paste0("</tr>")
   }

    # begin column names
    the_header %<>% paste0("\n<tr>")
    # blank col above rownamesvar for now
    rownameheader <- ""
    the_header %<>% paste0("<th scope=\"col\">", rownameheader, "</th>")
    if (is.null(colnames)) {
      colnames <- colnames(x)
    }
    style<-paste("style=\"text-align: right;\"")
    for (col_j in 1:length(colnames)) {
      the_header %<>% paste("<th scope=\"col\"",style,">", colnames[col_j], "</th>")
    }
    # end column names
    the_header %<>% paste0("</tr>")

    # end table header
    the_header %<>% paste0("\n</thead>")
  } else {
    the_header <- NULL
  }

  the_table <- the_header

  # begin table body
  the_table %<>% paste("\n<tbody>")

  # loop over rows
  for (row_i in 1:nrow(x)) {

    # begin row
    if (is.null(rowlevels)) {
      the_table %<>% paste0("\n<tr>")
    } else if (rowlevels[row_i] == 1) {
      the_table %<>% paste0("\n<tr>")
    } else {
      the_table %<>% paste0("\n<tr class=\"row-level",rowlevels[row_i],"\">")
    }

    # row header
    the_table %<>% paste0("\n<th scope=\"row\">", rownames[row_i], "</th>")

    style<-paste("style=\"font-variant-numeric: tabular-nums; text-align: right;\"")

    # loop over columns
    for (col_j in 1:length(table_input[row_i,])) {
      the_table %<>% paste("\n<td",style,">",table_input[[row_i,col_j]], "</td>")
    }

    # end row
    the_table %<>% paste("\n</tr>")
  }

  # end table body
  the_table %<>% paste("\n</tbody>")

  return(the_table)
}


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
                     selfcontained=FALSE
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

  if (!is.null(select)) {
    x %<>% select(select)
  }

  # make the meat of the table
  the_meat <-
    table_meat(
      x,
      rownames = rownames,
      rowlevels = rowlevels,
      colnames = colnames,
      colgroups = colgroups,
      header = header
    )

  # create the table snippet
  # begin the table
  the_table <- paste("<table>\n")

  # add the meat
  the_table %<>% paste0(the_meat,"\n")

  # end the table
  the_table %<>% paste("\n</table>")

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


verify_class_epitable <- function(x) {
  y <- class(x) == "epitable"
  return(y)
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

# examples
# basic table
#epitable(1, example)

# write the table
#epitable("y'all", example, file = "test.html")
#epitable("y'all", example, file = "test_selfcontained.html", selfcontained=TRUE)
#cat(epitable(222), file="test.html")

# view the snippet of table in console
#print(epitable(3, example), useViewer=FALSE)

