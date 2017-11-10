library(tuneR)

# Standard Normal
supp = seq(-4, 4, length.out = 50)
st.norm.dens = dnorm(supp)

plot(supp, st.norm.dens, type = "l")

list = lapply(st.norm.dens, function(x) sine(x*440+440, duration = 7500, xunit = "samples"))

st.norm.wav = do.call(bind, list)

writeWave(st.norm.wav, "./st_norm_dens.wav")

# Beta 2, 2
supp = seq(-0.1, 1.1, length.out = 50)

beta.dens = dbeta(supp, 2, 2)

plot(supp, beta.dens, type = "l")

list = lapply(beta.dens, function(x) sine(x*440+440, duration = 7500, xunit = "samples"))

beta.wav = do.call(bind, list)

writeWave(beta.wav, "./beta_dens.wav")

# Gamma 2,1
supp = seq(0, 8, length.out = 50)
gamma.dens = dgamma(supp, 2, 1)

plot(supp, gamma.dens, type = "l")

list = lapply(gamma.dens, function(x) sine(x*440+440, duration = 7500, xunit = "samples"))

gamma.wav = do.call(bind, list)

writeWave(gamma.wav, "./gamma_dens.wav")
