R.utils::use("async")

expr <- AsyncTask({ x <- 1 })
print(expr)

