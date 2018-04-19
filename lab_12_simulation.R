generate_data = function(n,p){
  vec = rnorm(n)
  mat = matrix(rnorm(n*p), n,p)
  return(list(covariates= mat, responses=vec))
}

