generate_data = function(n,p){
  vec = rnorm(n)
  mat = matrix(vec, n,p)
  return(list(covariates= mat, responses=vec))
}

