generate_data = function(n,p){
  vec = rnorm(n)
  mat = matrix(rnorm(n*p), n,p)
  return(list(covariates= mat, responses=vec))
}

model_select = function(covariates, responses, cutoff){
  n = length(responses)
  vec = vector(length=n)
  lin = summary(lm(responses ~ covariates))
  for (i in 1:n) {
    vec[i] = (lin[[i]]$coefficients)[,4][2]
  }
  new = summary(lm(responses ~ covariates[,vec<=cutoff]))
  if(sum(vec<= cutoff) == 0) return(c())
  vec2 = vector(sum(vec <= cutoff))
  for (i in 1:sum(vec <= cutoff)) {
    vec2[i] = (new[[i]]$coefficients )[,4][2]
  }
  return(vec2)
}

run_simulation = function(n_trials,n,p,cutoff){
  ret = c()
  for (i in 1:n_trials) {
    gen = generate_data(n,p)
    mod = model_select(gen$covariates, gen$responses, cutoff)
    ret = c(ret,mod)
  }
  hist(ret)
}
