library(reshape2)
library(plyr)

d = read.table("http://www.public.iastate.edu/~dnett/microarray/MyostatinTime2Data.txt", header=TRUE)

m = melt(d, id="geneID", variable.name="sample", value.name="y")
m$treatment = factor(gsub("\\d", "", m$sample))
m$rep = as.numeric(gsub("\\D", "", m$sample))
m$rep[m$rep>5] = m$rep[m$rep>5]-5

#ggplot(m, aes(x=y,y=..density..))+geom_histogram()+facet_grid(treatment~rep)

# Independent analysis
sm = ddply(m, .(geneID), function(x) {
  tmp = t.test(y~treatment, data=x, var.equal=TRUE)
  data.frame(pvalue = tmp$p.value,
             difference = tmp$statistic,
             width = diff(tmp$conf.int))
})
n1=n2=5
sm$sd = sm$width/2/qt(.975,n1+n2-2)/sqrt(1/n1+1/n2)


hist(sm$pvalue, 100, prob=TRUE)

# Estimated differences have approximately a t_10 distribution
hist(sm$difference, 100, prob=TRUE)
v = 10
mn = mean(sm$difference)
s = sd(sm$difference)/ (v/(v-2))
curve(dt((x-mn)/s,v)/s, add=TRUE, col="red", lwd=2)

# Estimated standard deviations are bimodal
hist(sm$sd, 100, prob=TRUE)

# Estimated sd are inversely related to the magnitude of the estimated differences
plot(sd~abs(difference), sm)
lines(smooth.spline(abs(sm$difference), sm$sd), col="red")

# Standard deviation decreases with gene signal
rM = rowMeans(d[,-1])
plot(sm$sd~rM)
lines(smooth.spline(rM, sm$sd), col="red")

