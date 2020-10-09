dat <- data.frame(id = letters[1:10], x = 1:10, y = 11:20)
dat

# head() - shows first 6 rows
# tail() - shows last 6 rows
# dim() - returns the dimensions of data frame (i.e. number of rows and number of columns)
# nrow() - number of rows
# ncol() - number of columns
# str() - structure of data frame - name, type and preview of data in each column
# names() or colnames() - both show the names attribute for a data frame
# sapply(dataframe, class) - shows the class of each column in the data frame

is.list(dat)
class(dat)
dat[1, 3]
dat[["y"]]
dat$y
str(iris)
