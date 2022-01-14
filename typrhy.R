rm(list = ls())
# setwd("F:/TYPRHY")
setwd("~/Desktop/TYPRHY")
library(ggplot2)
library(plyr)
library(zoo)

# load typing data
load("txt_ikis.RData")

txtikis = subset(txtikis, ikis < 1000 & ikis > 0)

summary(txtikis$ikis)

ggplot(txtikis, aes(ikis))+ geom_density()
# find how to extract the density to plot it for individual points

# density of cumsum (from beginning of sentence -- check beginning of word)
b = ddply(txtikis, .(Subject, icur), mutate, ikisum = cumsum(ikis))

# a = subset(txtikis, Subject %in% c(1,2))
# b = ddply(a, .(Subject, icur), mutate, ikisum = cumsum(ikis))

a = subset(txtikis, count == 2, select = c("Subject", "icur", "ikis"))
colnames(a)[3] = "iki1" 

c = merge(b, a)
c$ikirel = c$ikisum - c$iki1

ggplot(c, aes(ikisum))+geom_density()
summary(c$ikisum)

call = c
c = subset(c, ikisum < 2000)

# ggplot(c, aes(ikirel))+ geom_point()

ggplot(c, aes(ikirel))+ geom_density()
ggplot(c, aes(ikirel))+ geom_density()+xlim(80,750)

ggplot(c, aes(ikirel))+ geom_histogram()+xlim(20,1000)

# density by subject
c1 = c
c1$Subject = as.numeric(as.character(c1$Subject))
c1 = subset(c1, Subject < 50)
ggplot(c1, aes(ikirel, color = as.factor(Subject)))+ geom_density()
ggplot(c1, aes(ikirel, color = as.factor(Subject)))+ geom_density()+xlim(80,750)

c2 = subset(c1, Subject < 20)
ggplot(c2, aes(ikirel))+ geom_density()+facet_wrap(~Subject)+xlim(20,800)


# fake time-series and FFT
call = call[order(call$Subject, call$icur, call$count),]

# by subject
# for (s in call$Subject) {
all = data.frame()
for (s in seq(1,3)) {
  call1 = subset(call, Subject == s)
  dd = call1[,c("Subject", "icur", "ikis", "ikisum")]
  dd$data = 1
  # dd = data.frame(ikisum = call$ikisum, data = 1)
  for (ii in seq(1,max(dd$icur))){
     ddi = subset(dd, icur == ii)
     dd2 = data.frame(ikisum = seq(1,max(ddi$ikisum)))
     dd3 = merge(ddi, dd2, all = T)
     dd3$data = ifelse(is.na(dd3$data), 0, dd3$data)
     dd3$Subject = unique(ddi$Subject)
     dd3$icur = unique(ddi$icur)
     all = rbind.fill(all, dd3)
  }
}

all1 = subset(all, Subject == 1 & icur == 1)
ggplot(all1, aes(data, x = ikisum))+geom_line()

plot(head(all$ikisum,5000),head(all$data,5000))

f = fft(all$data)


plot.frequency.spectrum <- function(X.k, xlimits=c(0,length(X.k))) {
  plot.data  <- cbind(0:(length(X.k)-1), Mod(X.k))
  
  # TODO: why this scaling is necessary?
  plot.data[2:length(X.k),2] <- 2*plot.data[2:length(X.k),2] 
  
  plot(plot.data, t="h", lwd=2, main="", 
       xlab="Frequency (Hz)", ylab="Strength", 
       xlim=xlimits, ylim=c(0,max(Mod(plot.data[,2]))))
}

plot.frequency.spectrum(f, xlimits = c(0,25))
# from http://www.di.fc.ul.pt/~jpn/r/fourier/fourier.html

# Over all subjects
rm(list = c("b", "c", "txtikis", "a"))
call = call[order(call$Subject, call$icur, call$count),]
call$ikisumall = cumsum(call$ikis)

call$Subject = as.numeric(as.character(call$Subject))
call = subset(call, Subject < 300)
call$data = 1

dd = data.frame(ikisumall = seq(1,max(call$ikisumall)))
# dd2 = merge(dd, call, all = T)
dd2 = merge(dd, call[,c("ikisumall", "data")], all = T)
dd2$data = ifelse(is.na(dd2$data), 0, dd2$data)

# dd$data = 1
# dd2 = data.frame(ikisum = seq(1,max(ddi$ikisum)))
# dd3 = merge(ddi, dd2, all = T)
# dd3$data = ifelse(is.na(dd3$data), 0, dd3$data)
# dd3$Subject = unique(ddi$Subject)
# dd3$icur = unique(ddi$icur)

# all1 = subset(all, Subject == 1 & icur == 1)
ggplot(head(dd2, 100), aes(data, x = ikisumall))+geom_line()
plot(head(dd2$ikisumall,5000),head(dd2$data,5000))

f = fft(dd2$data)
plot.frequency.spectrum(f, xlimits = c(1,25))


# TO DO:
# clean data (take for instance selected ppts in FAST2)
# figure out how to extract density/kernel for fig1
# figure out what to do with fft
