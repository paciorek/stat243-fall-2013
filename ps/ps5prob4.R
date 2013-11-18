load("ps5prob5.RData")

names(meanpts) <- c("time", "value")
# this creates the function, meanfunc()
meanfunc <- with(list(spf = splinefun(meanpts$time, meanpts$value, method = "natural"), 
                 minday = min(meanpts$time), 
                 maxday = max(meanpts$time)),
function(theta, times){
  rescaled <- times/theta["lambda"]
  if(any(rescaled < minday | rescaled > maxday))
    warning("Extrapolating beyond the range of the template data")
  theta["kappa"] * spf(rescaled)
})

# now you can evaluate the mean function as "meanfunc(theta, time)" where
# theta should be a named vector of parameters, and
# time should be a vector of time values at which you want to evaluate the mean

# if you want theta to be a list or to be referred to by number you can modify the code in the function body above

