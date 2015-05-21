source("cachematrix.R")

timed.solver <- function(msg, cache.matrix) {
  message(msg)
  print(system.time(cacheSolve(cache.matrix)))
}

set.seed(1234)
dimension <- 1000
input.matrix <- matrix(rnorm(1:dimension^2), dimension, dimension)

cache.matrix <- makeCacheMatrix(input.matrix)

timed.solver("First test, should not use cached version", cache.matrix)


timed.solver("Second test, should use cached version", cache.matrix)

cache.matrix$set(matrix(4:1,2,2))
timed.solver("Third test, changing the matrix should invalidate the cached version",
             cache.matrix)

timed.solver("Fourth test, now the inverse should be cached again",cache.matrix)

cache.matrix$set(matrix(4:1,2,2))
timed.solver("Fifth test, setting the matrix with an identical matrix should not invalidate the cache",
             cache.matrix)