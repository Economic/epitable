# load EPI CSS
epicss <-
  system.file("extdata", "epi-chart.css" ,package = "epitable") %>%
  read_file()

# load complete test table for testing purposes
testtable <- system.file("extdata", "testtable.html", package = "epitable")

# top matter for self-contained webpage
selfcontained_top <- function(x) {
  htmlpage <- paste("<html>",
                    "<head>",
                    "<meta http-equiv=\"Content-type\" content=\"text/html;charset=UTF-8\">",
                    "<style>",
                    epicss,
                    "html * {font-family: 'Proxima Nova', 'Lato' !important;}",
                    "</style>",
                    "</head>",
                    "<body>",
                    "<div class=\"figure figure-table figure-theme-clean\"><div class=\"figBorder\"><div class=\"figInner\"><h4>boring chart</h4><div class=\"table-wrapper\">",
                    "",
                    sep="\n")
  return(htmlpage)
}

# bottom matter for self-contained webpage
selfcontained_bottom <- function(x) {
  htmlpage <- paste("",
                    "</div></div></div></div>",
                    "</body>",
                    "</html>",
                    sep="\n")
  return(htmlpage)
}



table_meat <- function(x, rownamevar, rowlevels=NULL) {

  table_input <- x

  # adjust dataframe to rownames
  rownames <- table_input %>% select(!!enquo(rownamevar))
  table_input %<>% select(-!!enquo(rownamevar))

  # begin table header
  the_table <- paste0("\n<thead>","\n<tr><th scope=\"col\">hello</th></tr>")

  # end table header
  the_table %<>% paste0("\n</thead>")

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

    the_table %<>% paste0("\n<th scope=\"row\">", rownames[row_i,], "</th>")
    the_table %<>% paste("\n<td style=\"text-align: right;\">$403.2</td>")
    the_table %<>% paste("\n<td>100.0%</td>")
    the_table %<>% paste("\n<td style=\"text-align: right;\">$111.1</td>")
    the_table %<>% paste("\n<td>100.0%</td>")
    the_table %<>% paste("\n<td style=\"text-align: right;\">$-292.1</td>")
    the_table %<>% paste("\n<td style=\"text-align: right;\">100.0%</td>")

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
#' @param file is the filename for saving the table snippet as a file. By default, this is NULL and epitable() does not write to a file.
#' @param selfcontained If writing to file, the default selfcontained=FALSE will write a table snippet. selfcontained=TRUE writes a complete html page, with header, body, etc.
#' @param example Just for testing purposes.
#' @keywords html tables
#' @export
#' @importFrom magrittr %>% %<>%
#' @importFrom readr read_file
#' @importFrom dplyr select
#' @importFrom rlang enquo
#' @examples
#' epitable()
#'
# epitable creates the table
epitable <- function(x,
                     rownamevar,
                     rowlevels=NULL,
                     file=NULL,
                     selfcontained=FALSE,
                     example=FALSE) {

    # just for testing
  if(example) {
    the_table <- paste("<table>\n",x)
    the_table %<>% paste(read_file(testtable))
    the_table %<>% paste("\n</table>")
  } else {
    # create the table snippet
    # begin the table
    the_table <- paste("<table>\n")
    # add the meat of the table
    the_table %<>% paste0(table_meat(x, rownamevar=!!enquo(rownamevar), rowlevels=rowlevels),"\n")
    # end the table
    the_table %<>% paste("\n</table>")
  }

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
  asis_output(x)
}

#' @rdname epitable
#' @param useViewer Set to false to show snippet in console.
#' @export
print.epitable <- function(x, useViewer = TRUE, ...) {
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
#' @examples
#' epitable_append()
epitable_append <- function(...) {
  # confirm all arguments have class epitable
  verified <- lapply(list(...), verify_class_epitable) %>% unlist() %>% all()

  if (verified) {
    x <- paste(...,sep="\n")
    stripped <- gsub("<table>", "", x)
    stripped <- gsub("</table>", "", stripped)
    the_table <- paste("<table>",stripped,"</table>",sep="\n")
    class(the_table) = c("epitable")
    return(the_table)
  } else {
    stop("All arguments must have class epitable.")
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

