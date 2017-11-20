library(tuneR)
# Example
supp = seq(1,7, length.out = 100)

ex = (seq(1, 7, length.out = 100)^1.25*sin(seq(1, 7, length.out = 100)*1.5) -
  (seq(1,7,length.out = 100)-1)^1.05*2) * .05 + 2.32

# plot(supp, ex, type = "l")

list = lapply(ex, function(x) sine(x*440+440, duration = .1, xunit = "time"))

ex.wav = do.call(bind, list)

writeWave(ex.wav, "./ex.wav")


#
# Standard Normal
supp = seq(-4, 4, length.out = 100)
st.norm.dens = dnorm(supp)

plot(supp, st.norm.dens, type = "l")

list = lapply(st.norm.dens, function(x) sine(x*440+440, duration = .1, xunit = "time"))

st.norm.wav = do.call(bind, list)

writeWave(st.norm.wav, "./st_norm_dens.wav")

# Beta 2, 2
supp = seq(-0.1, 1.1, length.out = 100)

beta.dens = dbeta(supp, 2, 2)

plot(supp, beta.dens, type = "l")

list = lapply(beta.dens, function(x) sine(x*440+440, duration = .1, xunit = "time"))

beta.wav = do.call(bind, list)

writeWave(beta.wav, "./beta_dens.wav")

# Gamma 2,1
supp = seq(0, 8, length.out = 100)
gamma.dens = dgamma(supp, 2, 1)

plot(supp, gamma.dens, type = "l")

list = lapply(gamma.dens, function(x) sine(x*440+440, duration = .1, xunit = "time"))

gamma.wav = do.call(bind, list)

writeWave(gamma.wav, "./gamma_dens.wav")

# Stepwise constant
supp = seq(0, 8, length.out = 100)
step.const = function(x) {
  if(x>0 & x <= 2) {
    1
  } else if( x >2 & x <= 4) {
    2 
  } else if(x>4 & x  <= 6) {
    3
  } else {
    1
  }
}


stepconst = sapply(supp, step.const)

plot(supp, stepconst, type = "l")

list = lapply(stepconst, function(x) sine(x*440, duration = .1, xunit = "time"))

step.const.wav = do.call(bind, list)

writeWave(step.const.wav, "./step_const.wav")


# sine
supp = seq(0,2*pi, length.out = 100)

sine.fun =  sin(4*supp)

plot(supp, sine.fun, type = "l")

list = lapply(sine.fun, function(x) sine(x*440+440, duration = .1, xunit = "time"))

sine.wav = do.call(bind, list)

writeWave(sine.wav, "./sine.wav")

# LMU

lmu.func <- function(x) {
  if (x>=0 & x <= .25) {
    1-4*x
  } else if (x>.25 & x <= 1.25) {
    0
  } else if (x>1.25 & x <=1.75) {
    2*(x-1.25)
  } else if (x>1.75 & x <= 2.25) {
    1-2*(x-1.75)
  } else if (x>2.25 & x <= 2.75) {
    2*(x-2.25)
  } else if (x>2.75 & x <= 3.25) {
    1-2*(x-2.75)
  } else {
    (x - 4)^2 / .5625
  }
}

supp = seq(0, 4.75, length.out = 100)

lmu = sapply(supp, lmu.func) / 2

list = lapply(lmu, function(x) sine(x*440+440, duration = .1, xunit = "time"))

lmu.wav = do.call(bind, list)

writeWave(lmu.wav, "./lmu.wav")
