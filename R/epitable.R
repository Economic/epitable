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


#' @title A EPI HTML Table Making Function
#'
#' @description This is the table making function.
#' @param x is the data
#' @param file is the filename for saving the table snippet as a file. By default, this is NULL and epitable() does not write to a file.
#' @param selfcontained If writing to file, the default selfcontained=FALSE will write a table snippet. selfcontained=TRUE writes a complete html page, with header, body, etc.
#' @param example Just for testing purposes.
#' @keywords html tables
#' @export
#' @import magrittr
#' @import readr
#' @examples
#' epitable()
#'
# epitable creates the table
epitable <- function(x, file=NULL, selfcontained=FALSE, example=FALSE) {

  # create the table snippet
  if(example) {
    thetable <- paste("<table>\n",x)
    thetable %<>% paste(read_file(testtable))
    thetable %<>% paste("\n</table>")
  } else {
    thetable <- paste("<table>\n",x,"\n")
    thetable %<>% paste("</table>")
  }

  # if writing to file
  if(!is.null(file)) {
    if (selfcontained) {
      htmlpage <- paste0(
        selfcontained_top(),
        thetable,
        selfcontained_bottom()
      )
      cat(htmlpage,file = file)
    } else cat(thetable, file = file)
  }

  class(thetable) = c("epitable")
  return(thetable)
}


#' @rdname epitable
#' @param useViewer If you are using RStudio there is a viewer thar can render
#'  the table within that is envoced if in \code{\link[base]{interactive}} mode.
#'  Set this to \code{FALSE} if you want to remove that  functionality. You can
#'  also force the function to call a specific viewer by setting this to a
#'  viewer function, e.g. \code{useViewer = utils::browseURL} if you want to
#'  override the default RStudio viewer. Another option that does the same is to
#'  set the \code{options(viewer=utils::browseURL)} and it will default to that
#'  particular viewer (this is how RStudio decides on a viewer).
#'  \emph{Note:} If you want to force all output to go through the
#'  \code{\link[base]{cat}()} the set \code{\link[base]{options}(htmlTable.cat = TRUE)}.
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
    thetable <- paste("<table>",stripped,"</table>",sep="\n")
    class(thetable) = c("epitable")
    return(thetable)
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

