# load EPI CSS
epicss <-
  system.file("extdata", "epi-chart.css" ,package = "epitable") %>%
  read_file() %>%
  #  paste("html * {font-family: 'Proxima Nova', 'Lato' !important;}\n") %>%
  paste(".figure.figure-theme-clean{margin:0;border-top:none;}\n") %>%
  paste(".figure.figure-theme-clean .figInner{border-bottom:none;}\n") %>%
  paste(".figure .figInner table th[scope=\"col\"][colspan],.figure .figInner table th[scope=\"colgroup\"][colspan]{text-transform:none}\n")

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
