# queryBuildR

This package provides a function `queryBuildR` to create complex SQL query with [JQuery Query Builder](https://github.com/mistic100/jQuery-QueryBuilder).

## Install

Using devtools:

```
 devtools::install_github('Yannael/queryBuildR')
```

## Usage

```
# Load data
data(iris)
# Rename columns and get filters
colnames(iris)<-c("sepal_length","sepal_width","petal_length","petal_width","species")
filters<-getFiltersFromTable(iris)
# Set initial rules to NULL
rules<-NULL
# Create Query Builder widget
queryBuildR(rules,filters)
```

## Shiny

See example at https://github.com/Yannael/shinyQueryBuildR


