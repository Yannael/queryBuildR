#' <Add Title>
#'
#' <Add Description>
#'
#' @import htmlwidgets
#'
#' @export
#'

library(htmlwidgets)

idToName <- function(x) {
  s <- strsplit(x, "_")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

getFiltersFromTable<-function(data) {
  filters<-list()

  namesCol<-colnames(data)
  niceNames<-as.vector(sapply(namesCol,idToName))

  for (i in 1:ncol(data)) {

    filterCol<-
      switch(class(data[,i]),
             character={
               if (length(unique(data[,i]))>50) {
                 list(
                   id= tolower(gsub(" ","",namesCol[i])),
                   label= niceNames[i],
                   type= 'string',
                   default_value=data[1,i],
                   operators=list('equal','not_equal','contains', 'is_empty', 'is_not_empty'))
               }
               else {
                 values<-setdiff(unique(data[,i]),"")
                 list(
                   id= tolower(gsub(" ","",namesCol[i])),
                   label= niceNames[i],
                   type= 'string',
                   input='select',
                   values=values,
                   default_value=values[1],
                   operators=list('equal','not_equal','contains', 'is_empty', 'is_not_empty'))
               }
             },
             integer=list(
               id= tolower(gsub(" ","",namesCol[i])),
               label= niceNames[i],
               type= 'integer',
               default_value=0,
               operators=list('equal','not_equal',  'less', 'less_or_equal', 'greater','greater_or_equal','between')),
             numeric=list(
               id= tolower(gsub(" ","",namesCol[i])),
               label= niceNames[i],
               type= 'double',
               default_value=0,
               validation=list(
                 min= 0,
                 step= 0.01
               ),
               operators=list('equal','not_equal',  'less', 'less_or_equal', 'greater','greater_or_equal','between'))
      )
    filters<-c(filters,list(filterCol))
  }
  filters
}


queryBuildR <- function(rules,filters, width = NULL, height = NULL) {

  x = list(
    rules = rules,
    filters=filters
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'queryBuildR',
    x,
    width = width,
    height = height,
    package = 'queryBuildR'
  )
}

#' Widget output function for use in Shiny
#'
#' @export
queryBuildROutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'queryBuildR', width, height, package = 'queryBuildR')
}

#' Widget render function for use in Shiny
#'
#' @export
renderQueryBuildR <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, queryBuildROutput, env, quoted = TRUE)
}
