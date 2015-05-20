source("cachematrix.R")

input.matrix <- matrix(1:4, 2, 2)

cache.matrix <- makeCacheMatrix(input.matrix)

message("First test, should not use cached version")
cacheSolve(cache.matrix)

message("Second test, should use cached version")
cacheSolve(cache.matrix)

message("Third test, changing the matrix should invalidate the cached version")
cache.matrix$set(matrix(4:1,2,2))
cacheSolve(cache.matrix)

message("Fourth test, now the inverse should be cached again")
cacheSolve(cache.matrix)

message("Fifth test, setting the matrix with an identical matrix should not invalidate the cache")
cache.matrix$set(matrix(4:1,2,2))
cacheSolve(cache.matrix)