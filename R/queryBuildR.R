#' Returns a template for the Query Builder filter on the basis of a Data Frame
#'
#' @param data Data Frame from which the list of filters will be created.
#'
#' @import htmlwidgets
#'
#' @examples
#' #### Iris data example.
#' # Load data
#' data(iris)
#' getFiltersFromTable(iris)
#' @export
#'
getFiltersFromTable<-function(data,column_opts=NULL) {

  if(is.null(column_opts)) column_opts=list()

  filters<-list()
  namesCol<-colnames(data)
  niceNames<-as.vector(sapply(namesCol,function(x) gsub('[_.]',' ' ,x)))

  for (i in 1:ncol(data)) {

    filterCol<-
      switch(class(data[,i]),
             character={
               char_list=list(
                 id= namesCol[i],
                 label= niceNames[i],
                 type= 'string',
                 placeholder='Enter search term',
                 operators=switch(as.numeric(is.null(column_opts[[namesCol[i]]][['operators']]))+1,
                                  column_opts[[namesCol[i]]][['operators']],
                                  list('equal','not_equal','contains', 'in', 'not_in','begins_with', 'ends_with','is_null', 'is_not_null')
                                  )

                 )

               if(length(column_opts[[namesCol[i]]][['plugin']])>0){
                    if(column_opts[[namesCol[i]]][['plugin']]=='selectize'){
                      char_list=c(char_list,list(
                       input=ifelse(is.null(column_opts[[namesCol[i]]][['input']]),'select',column_opts[[namesCol[i]]][['input']]),
                       plugin=ifelse(is.null(column_opts[[namesCol[i]]][['plugin']]),'selectize',column_opts[[namesCol[i]]][['plugin']]),
                       plugin_config=list(
                                    valueField="id",
                                    labelField="name",
                                    sortField="name",
                                    searchField="name",
                                    create=TRUE,
                                    options= jsonlite::toJSON(data.frame(id=1:length(unique(data[,namesCol[i]])),name=unique(data[,namesCol[i]]),stringsAsFactors = FALSE,row.names = NULL))
                               )))
                    }
                 }

               char_list


               },

             factor={
               list(
                 id= namesCol[i],
                 label= niceNames[i],
                 type= ifelse(is.null(column_opts[[namesCol[i]]][['type']]),'string',column_opts[[namesCol[i]]][['type']]),
                 input=ifelse(is.null(column_opts[[namesCol[i]]][['input']]),'select',column_opts[[namesCol[i]]][['input']]),
                 values=setdiff(levels(data[,i]),""),
                 placeholder='Enter search term',
                 operators=switch(as.numeric(is.null(column_opts[[namesCol[i]]][['operators']]))+1,
                                  column_opts[[namesCol[i]]][['operators']],
                                  list('equal','not_equal','contains', 'in', 'not_in','is_null', 'is_not_null'))
                 )
             },

             integer=list(
               id= namesCol[i],
               label= niceNames[i],
               type= ifelse(is.null(column_opts[[namesCol[i]]][['type']]),'integer',column_opts[[namesCol[i]]][['type']]),
               input=ifelse(is.null(column_opts[[namesCol[i]]][['input']]),'number',column_opts[[namesCol[i]]][['input']]),
               default_value=data[1,i],
               operators=switch(as.numeric(is.null(column_opts[[namesCol[i]]][['operators']]))+1,
                                column_opts[[namesCol[i]]][['operators']],
                                list('equal','not_equal','less', 'less_or_equal', 'greater','greater_or_equal','between','in', 'not_in','is_null', 'is_not_null'))
               ),

             numeric=list(
               id= namesCol[i],
               label= niceNames[i],
               type= 'double',
               default_value=data[1,i],
               operators=
                 switch(as.numeric(is.null(column_opts[[namesCol[i]]][['operators']]))+1,
                        column_opts[[namesCol[i]]][['operators']],
                        list('equal','not_equal',  'less', 'less_or_equal', 'greater','greater_or_equal','between','is_null', 'is_not_null'))
             )
      )
    filters<-c(filters,list(filterCol))
  }
  filters
}

#' Create a Query Builder widget
#'
#' @param rules List initializing the set of rules for the Query Builder widget.
#' @param filters List specifying the variables, conditions and value ranges for building the queries.
#'
#' @import htmlwidgets
#'
#' @examples
#' #### Iris data example.
#' # Load data
#' data(iris)
#' filters<-getFiltersFromTable(iris)
#' # Create Query Builder widget
#' queryBuildR(filters)
#'
#' queryBuildR(getFiltersFromTable(iris,
#'                                 column_opts = list(Species=list(type='select',
#'                                                                 plugin='selectize',
#'                                 operators=list('equal','not_equal','contains', 'in', 'not_in')))
#'                                 )
#'            )
#'
#' @export
#'
queryBuildR <- function(filters,rules=NULL,plugins=NULL,width = NULL, height = NULL) {

  x = list(
    rules = rules,
    filters=filters,
    plugins=plugins
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

#' @export
queryBuildROutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'queryBuildR', width, height, package = 'queryBuildR')
}

#' @export
renderQueryBuildR <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, queryBuildROutput, env, quoted = TRUE)
}
