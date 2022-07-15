n_month <- 33
pop <- rep(0, n_month)
yield <- rep(0, n_month)
season <- rep('S', n_month)
season[c(8:12, 20:24)] <- 'W'
pop[1] <- 1
for(mo in 1:(n_month-1)) {
  pop[mo+1] <- exp(-0.09*(season[mo]=='S') - 0.06*(season[mo]=='W') - F) * pop[mo]
  yield[mo] <- exp(-F) * pop[mo] * (season[mo]=='S')
}
# Plus group???