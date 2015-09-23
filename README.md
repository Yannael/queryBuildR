# queryBuildR

This package provides a function `queryBuildR` to create complex SQL query with [JQuery Query Builder](http://mistic100.github.io/jQuery-QueryBuilder).

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

Example of integration with Shiny: https://github.com/Yannael/shinyQueryBuildR

Online examples at:
* http://yleborgne.net/shinyQueryBuildRDemo
* http://yleborgne.net/opented


