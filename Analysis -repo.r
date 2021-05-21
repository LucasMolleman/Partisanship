### load data ###
a<-read.csv('clean_data.csv', header=TRUE)
population <- read.csv('states_pop.txt', header = TRUE, sep=',')
outcomes<-read.csv('electionOutcome.txt', header = TRUE, sep=',')
library('tidyverse')
a<-a %>%  left_join(population, by = 'state')
a<-a %>%  left_join(outcomes, by = 'state')

# load libraries
library('lme4')

### code who observes whom ###
a$case<-paste(substr(a$ownParty,1,1),substr(a$partyOther,1,1), sep='') ;

### remove the filler trials
a<-subset(a, a$treatment<3)

### remove trials where experimental conditions were not met
a<-subset(a, a$target_found==1)

### remove trials where social information was the same as e1 (so no meaningful adjustments could be made)
a<-subset(a, a$e1!=a$social_info)

### create a numerical variable for confidence in initial estimate being low (1) or high (2)
a$uncertainty <- ifelse(a$confidence == 'btnLow', 1, 2) 

### are ego and alter from the same party?
a$sameParty<-ifelse(a$case=='DD' | a$case=='RR', 1,0)

### did social information point in the direction favoured by ego?
a$socInfoHigher<-ifelse(a$e1 < a$social_info, 1, 0)
a$favourability<-ifelse(a$ownParty=='Democratic', a$socInfoHigher, 1-a$socInfoHigher)

a$favourability_alternative<-ifelse(a$ownParty=='Democratic' & a$social_info > 50, 1, ifelse(a$ownParty=='Republican' & a$social_info < 50, 1, 0))

### did ego and alter predict the same winner (i.e. are they on the same side of the 50-50 line)?
a$sameWinner<-ifelse(a$e1 < 50 & a$social_info < 50, 1, 0)
a$sameWinner<-ifelse(a$e1 > 50 & a$social_info > 50, 1, a$sameWinner)
a$sameWinner<-ifelse(a$social_info == 50 | a$e1==50, NA, a$sameWinner)



### inspect raw distribution of adjustments ###
summary(a$s)

f<-rep(0, 31)
for (i in -10:20){
	f[i+11]<-length(which(round(a$s * 10) == i))
}
f<-f/sum(f)

### plot raw distribution of adjustments (Fig. S5) ###
par(cex.lab=1.5, cex.axis=1.5, lend=1, las=1, mar=c(5,5,1,1), yaxs='i')
plot(0, type='n', xlim=c(-1,2), ylim=c(0, 0.35), xlab='Adjustment', ylab='Frequency', axes=FALSE)
rect(-10,0,0,0.3, col='grey90', border=FALSE)
rect(1,0,10,0.3, col='grey90', border=FALSE)
axis(1, at=-10:10/2)
axis(2, at=0:3/10)

for (i in -10:20){
	x1<- i/10 - 0.05
	x2<-i/10+0.05
	col1<-'grey80'
	if (i==0) col1<-'forestgreen';
	if (i > 0 && i < 10) col1<-'darkgoldenrod'
	if (i==10) col1<-'violet'
	rect(x1,0,x2, f[i+11], col=col1)
}

# how often did people move away, stay, compromise, copy and overshoot?
move_away<-length(which(a$s<0))
stay<-length(which(a$s==0))
copy<-length(which(a$s==1))
overshoot<-length(which(a$s>1))
compromise<-length(which(!is.na(a$s))) - move_away - stay - copy - overshoot
distr<-c(move_away, stay, compromise, copy, overshoot)/length(which(!is.na(a$s)))

xs<--1:3/2
for (i in 1:5) text(xs[i], 0.33, round(distr[i]*100), cex=1.5)

arrows(0.5, 0, 0.5, 0.3, lty=2, lwd=2, code=0)

### split out this distribution by case (who observes whom; Fig. S6) ###

par(mfrow=c(2,2))
for (ca in unique(a$case)){

	b<-subset(a, a$case==ca)
	f<-rep(0, 31)
	for (i in -10:20){
		f[i+11]<-length(which(round(b$s * 10) == i))
	}
	f<-f/sum(f)

	### plot raw distribution of adjustments ###
	par(cex.lab=1.5, cex.axis=1.5, lend=1, las=1, mar=c(5,5,1,1), yaxs='i')
	plot(0, type='n', xlim=c(-1,2), ylim=c(0, 0.4), xlab='Adjustment', ylab='', axes=FALSE, main='')
	rect(-10,0,0,0.3, col='grey90', border=FALSE)
	rect(1,0,10,0.3, col='grey90', border=FALSE)
	axis(1, at=-10:10/2)
	axis(2, at=0:3/10)

	for (i in -10:20){
		x1<- i/10 - 0.05
		x2<-i/10+0.05
		col1<-'grey80'
		if (i==0) col1<-'forestgreen';
		if (i > 0 && i < 10) col1<-'darkgoldenrod'
		if (i==10) col1<-'violet'
		rect(x1,0,x2, f[i+11], col=col1)
	}

	# how often did people move away, stay, compromise, copy and overshoot?
	move_away<-length(which(b$s<0))
	stay<-length(which(b$s==0))
	copy<-length(which(b$s==1))
	overshoot<-length(which(b$s>1))
	compromise<-length(which(!is.na(b$s))) - move_away - stay - copy - overshoot
	distr<-c(move_away, stay, compromise, copy, overshoot)/length(which(!is.na(b$s)))

	xs<--1:3/2
	for (i in 1:5) text(xs[i], 0.33, round(distr[i]*100), cex=1.5)
	
	arrows(0.5, 0, 0.5, 0.3, lty=2, lwd=2, code=0)

}


a$move_away<-ifelse(a$s<0,1,0)
m1<-glmer(move_away ~ ownParty * partyOther + (1|ID), data=a, family='binomial')
summary(m1)


#### 

### initial predictions for each state. plot distribution and show means per party (Fig. S2)

m1<-lmer(e1 ~ ownParty + state + (1|ID), data=a)
summary(m1)
m2<-lmer(e2 ~ ownParty + state + (1|ID), data=a)
summary(m2)



outcomes<-outcomes[order(outcomes$percDem, decreasing=TRUE),]
stateNamesOrdered<-outcomes[,1]
yy<-21
par(xaxs='i',yaxs='i', mfrow=c(1,1), cex.lab=1.5, cex.axis=1.5, lend=1)
plot(0, type='n', ylim=c(1,22), xlim=c(-10,100), xlab='', ylab='', axes=FALSE)
axis(1, at=0:10*10)
cnt<-1

na<-outcomes$abbrev

#rev(c('WY','WV','OK', 'KE','AL','SC','IO','OH','TX','FL','NC','AZ','GE','PE','NV','NY','CA','HA','MA','DC'))

for (st in stateNamesOrdered){
	b<-subset(a, a$state==st)
	
	col1<-'blue'
	if(cnt>=10) col1<-'red'
	b$e1r<-100-b$e1
	w<-summary(b$e1r)
	m<-mean(b$e1r, na.rm=TRUE)
	sd<-sd(b$e1r, na.rm=TRUE)
	arrows(m-sd, yy-0.5, m+sd, yy-0.5, code=0, lwd=2)
	rect(w[2], yy-0.9, w[5], yy-0.1, col='white', lwd=2)
	rect(w[2], yy-0.9, w[5], yy-0.1, lwd=2, col=adjustcolor(col1, alpha=0.5))
	arrows(m, yy-0.9, m, yy-0.1, code=0, lwd=4)
	
	for (i in 1:nrow(b)){
		ypos<-yy-0.8+0.8*runif(1)
		xpos<-b$e1r[i] - 0.5+runif(1)
#		points(xpos, ypos, pch=16, cex=0.5, col=adjustcolor('black', alpha=0.3))
	}
	
	# means per party
	d<-subset(b, b$ownParty=='Republican')
	m_R<-mean(d$e1r, na.rm=TRUE)
	d<-subset(b, b$ownParty=='Democratic')
	m_D<-mean(d$e1r, na.rm=TRUE)
	
	arrows(m_R,yy-0.5, m_D, yy-0.5, code=0, lwd=2)
	points(m_R,yy-0.5, pch=23, bg='red')
	points(m_D, yy-0.5, pch=23, bg='blue')
	
	
	text(-5, yy-0.5, na[cnt], cex=1.3)
	
	r<-100-outcomes[cnt,2]
	points(r,yy-0.5, pch=24, bg='white', lwd=2, cex=1.2)
	
	yy<-yy-1;
	cnt<-cnt+1;
}
arrows(50,-1,50,30, lty=2, lwd=2, code=0)

### President by partisanship ###
cols<-c('blue', 'red')
b<-subset(a, a$period==20)
b<-subset(b, !is.na(b$president))
par(mfrow=c(1,2), yaxs='i', xaxs='i');
cnt<-1;

means<-c(); sds<-c()

for (party in c('Democratic', 'Republican')){
	plot(0, type='n', xlab='', ylab='', xlim=c(0,100), ylim=c(0,0.38), axes=FALSE)
	axis(1)
	axis(2, labels=FALSE)
	d<-subset(b, b$ownParty==party)
	
	means<-c(means, mean(d$president, na.rm=TRUE))
	sds<-c(sds, sd(d$president, na.rm=TRUE))
	
	
	f<-rep(0,10)
	for (ind in 1:nrow(d)){
		x<-1+floor(d$president[ind]/10)
		if (x==11) x<-10;
		f[x]<-f[x]+1;
	}
	f<-f/sum(f)
	for (i in 1:10){
		rect((i-1)*10, 0, i*10, f[i], col=cols[cnt], lwd=2)
	}
	box()
	cnt<-cnt+1;
}
means
sds


###### Confidence and error by state #####

a$e1error<-a$e1-a$percDem
mean(abs(a$e1error), na.rm=TRUE)

population<-population[order(population$population),]
stateNamesOrdered<-population[,1]


### check relation population size and fraction 'uncertain' first predictions
par(xaxs='r', yaxs='i', mfrow=c(1,1), cex.lab=1.5, cex.axis=1.5, lend=1)
plot(0, type='n', ylim=c(0.5,1), xlim=c(0,44), xlab='', ylab='', axes=FALSE)
axis(1, at=0:10*10)
axis(2, at=0:10/10)
cnt<-1

na<-population$abbreviation

pops<-c(); pUnc<-c()
for (st in stateNamesOrdered){
	b<-subset(a, a$state==st)
	
	xpos<-b$population[1]
	ypos<-mean(b$uncertainty-1)
	
	pops<-c(pops, xpos)
	pUnc<-c(pUnc, ypos)
	
	text(xpos, ypos, na[cnt], cex=1)
	cnt<-cnt+1;
}

m1<-lm(pUnc ~ pops)
abline(m1, lty=2, lwd=2)
summary(m1)


### check relation population size and error in first predictions
plot(0, type='n', ylim=c(0,0.5), xlim=c(0,44), xlab='', ylab='', axes=FALSE)
axis(1, at=0:10*10)
axis(2, at=0:10/10)
cnt<-1

pops<-c(); meanErr<-c()
for (st in stateNamesOrdered){
	b<-subset(a, a$state==st)
	
	xpos<-b$population[1]
	ypos<-mean(abs(b$e1error)/100)
	
	pops<-c(pops, xpos)
	meanErr<-c(meanErr, ypos)
	
	text(xpos, ypos, na[cnt], cex=1)

	yy<-yy-1;
	cnt<-cnt+1;
}
m1<-lm(meanErr ~ pops)
abline(m1, lty=2, lwd=2)
summary(m1)

### check relation participant's certainty (confidence) and accuracy
a$absErr1<-abs(a$e1error)
median(subset(a, a$uncertainty==1)$absErr1)
median(subset(a, a$uncertainty==2)$absErr1)
m2<-lmer( log(absErr1+0.1) ~ uncertainty + (1|ID), data=a)
summary(m2)



### for the main analyses, we focus only on cases where the second prediction was a weighted average of the first prediciton and social information
### remove cases where e2 was not a weighted estimate of e1 and social information
a$s<-ifelse(a$s < 0, NA, a$s)
a$s<-ifelse(a$s > 1, NA, a$s)

m1<-lmer(e1 ~ ownParty + state + (1|ID), data=a)
summary(m1)
m2<-lmer(e2 ~ ownParty + state + (1|ID), data=a)
summary(m2)



### calculate the grand mean of adjustments
mean(a$s, na.rm=TRUE)



### create a matrix with individual-level summary data
mat<-matrix(nrow=0, ncol=10)
mat<-data.frame(mat)
for (ind in unique(a$ID)){
	b<-subset(a, a$ID==ind)
		
	d<-subset(b, b$partyOther=='Democrat')
	S_D<-mean(d$s, na.rm=TRUE)
	d<-subset(b, b$partyOther=='Republican')
	S_R<-mean(d$s, na.rm=TRUE)

	d<-subset(b, b$uncertainty==1)
	S_lowconf<-mean(d$s, na.rm=TRUE)
	d<-subset(b, b$uncertainty==2)
	S_highconf<-mean(d$s, na.rm=TRUE)

	S_overall<-mean(b$s, na.rm=TRUE)

	gen<-0;
	gend<-b$gender[1]
	gen<-ifelse( gend=='male',0, ifelse(gend=='female',1,NA))
	
	# party: 1=Democratic, 2=Republican
	party<-ifelse(b$ownParty[1]=='Democratic', 1,2)
	
	r<-c(ind, b$age[1], gen, party, b$identification[1], S_D, S_R, S_overall, S_lowconf, S_highconf)
	mat<-rbind(mat,r)
}


names(mat)<-c('ID', 'age', 'gender', 'ownParty', 'partizanDegree', 'S_Dem', 'S_Rep', 'S_overall', 'S_lowconf', 'S_highconf')


demographics<-matrix(0, nrow=2, ncol=8)
for (party in unique(mat$ownParty)){
	b<-subset(mat, mat$ownParty==party)
	demographics[party, 1]<-party
	demographics[party, 2]<-mean(b$age, na.rm=T)
	demographics[party, 3]<-sd(b$age, na.rm=T)
	demographics[party, 4]<-min(b$age, na.rm=T)
	demographics[party, 5]<-max(b$age, na.rm=T)
	demographics[party, 6]<-length(which(b$gender==0))
	demographics[party, 7]<-length(which(b$gender==1))
	demographics[party, 8]<-nrow(b)
}
demographics<-data.frame(demographics)
names(demographics)<-c('party', 'mean age', 's.d. age','min age', 'max age', 'males', 'females', 'total')
demographics


### calculate the average 'mean adjustment' (that is, means per participant; this might deviate from the overall grand mean because some participants might contribute more to the grand mean as they had more cases where 0<=s<=1)
mean(mat$S_overall, na.rm=TRUE)
hist(mat$S_overall)

### plot ingroup vs outgroup biases

a$copartisan<-ifelse(a$case=='DD' | a$case=='RR', 1,0)
mean(subset(a,a$copartisan==1)$s, na.rm=TRUE)
mean(subset(a,a$copartisan==0)$s, na.rm=TRUE)

 

ingroupS<-ifelse(mat$ownParty==1, mat$S_Dem, mat$S_Rep)
outgroupS<-ifelse(mat$ownParty==2, mat$S_Dem, mat$S_Rep)
x<-cbind(ingroupS, outgroupS)

cols<-c("#98a441","#ba6437","#965da7", 'green', 'purple')
par(mfrow=c(1,2), xaxs='r', yaxs='r', cex.lab=1.5, cex.axis=1.5, las=1, lend=1, mar=c(2,1,1,1))
plot(0, type='n', xlim=c(0, 1), ylim=c(0.5, 2.5), axes=FALSE, ylab='', xlab='')
axis(1)
#axis(1, at=c(1,2))
for (gr in 1:2){
	b<-x[,gr]
		
	yp<-3-gr # y position (ingroup up top)
	
	m<-mean(b, na.rm=TRUE)
	med<-median(b, na.rm=TRUE)
	sd1<-sd(b, na.rm=TRUE)
	se<-sd1/sqrt(length(which(!is.na(b))))
	
	summ<-summary(b)
	
	arrows(m-sd1, yp+0.1, m+sd1, yp+0.1, lwd=5, code=0)


	rect(summ[2], yp, summ[5], yp+0.2, col='white', lwd=2)
	rect(summ[2], yp,summ[5], yp+0.2,  col=adjustcolor(cols[gr], alpha=0.5), lwd=2)
	
	arrows(m, yp, m,yp+0.2,  lwd=5, code=0)
	arrows(med, yp, med,yp+0.2,  lwd=2, code=0, lty=1)
	
		
	for (ind in 1:nrow(x)){
		points(x[ind], yp - 0.05 - runif(1) * 0.25, pch=16, col=adjustcolor(cols[gr], alpha=0.3), cex=0.8)
	}
}
arrows(0.5, 0, 0.5,5,  lwd=2, lty=2)
box()

### what percentage of participants discounted social information?
length(which(x[,1] < 0.5)) / length(!is.na(x[,1]))
length(which(x[,2] < 0.5)) / length(!is.na(x[,2]))

a$expertise<-a$expertise*10
a$population<-a$population/10

## preregistered models

m1a<-lmer(s ~ sameParty * favourability + sameParty * confidence+(1|ID), data=a)

m1<-lmer(s ~ sameParty * favourability+(1|ID), data=a)
m2<-lmer(s ~ sameParty * favourability + sameWinner + confidence + expertise + population+ (1|ID), data=a)
m3<-lmer(s ~ sameParty * favourability + age + gender + (1|ID), data=a)

## output preregistered models
stargazer(m1,m2,m3,type="html" ,ci=TRUE, ci.level=0.95, title="", align=TRUE, star.cutoffs=c(0.05,0.01,0.001), single.row=TRUE,
 report=('vcsp'),
	out="Table S2.html")


### exploratory models ###


### split out for Democrats and Republicans
m2d<-lmer(s ~ sameParty * favourability + sameWinner + confidence + expertise + population+ (1|ID), data=subset(a, a$ownParty=='Democratic'))
summary(m1d)

### split out for Democrats and Republicans
m2r<-lmer(s ~ sameParty * favourability + sameWinner + confidence + expertise + population+ (1|ID), data=subset(a, a$ownParty=='Republican'))
summary(m1r)



#### plot main model (model m2)

summary(m2)
plot(0, type='n', xlim=c(-0.05, 0.07), ylim=c(0.5, 1.6), axes=FALSE, ylab='', xlab='')
axis(1)
box()

arrows(0, -1, 0, 4, lty=2, lwd=2, code=0)
yy<-1.4
cc<-summary(m2)$coefficients
orderOfEffects<-c(2,3,8,5)
na<-c('intercept', 'Ingroup Source (IS)', 'Favored Direction (FD)', 'Same majority', 'Uncertainty', 'Expertise', 'State Size',  'IS x FD')
for (k1 in 1:4){
	k<-orderOfEffects[k1];
	arrows(cc[k,1]-1.96*cc[k,2], yy, cc[k,1]+1.96*cc[k,2], code=0, lwd=2)
	points(cc[k,1], yy, pch=22, bg=cols[3], lwd=2, cex=2)

#	if (k1==2) rect(-0.01, yy+0.05, 0.01, yy+0.15, col='white', border='white')
	text(cc[k,1], yy+0.18, na[k], pos=1, cex=1.2)

#	if (k<8) text(-0.05, yy, na[k], pos=4, cex=1.2)
#	if (k==8) text(0.015, yy, na[k], pos=4, cex=1.2)
	yy<-yy-0.28
}



###


mat$dif<-mat$S_Rep - mat$S_Dem;

### plot of partisan bias for each party
cols<-c('blue', 'red')
par(mfrow=c(1,2), xaxs='r', yaxs='r', cex.lab=1.5, cex.axis=1.5, las=1, lend=1, mar=c(2,1,1,1))
plot(0, type='n', xlim=c(-0.6, 0.6), ylim=c(0.4, 2.5), axes=FALSE, ylab='', xlab='', )
axis(1)
arrows(0,0,0,5, lty=2, lwd=2)
for (party in 1:2){
	b<-subset(mat, mat$ownParty==party)
	ypos<-3-party
	
	m<-mean(b$dif, na.rm=TRUE)
	med<-median(b$dif, na.rm=TRUE)
	sd1<-sd(b$dif, na.rm=TRUE)
	se<-sd1/sqrt(length(which(!is.na(b$dif))))
	
	summ<-summary(b$dif)
	
	arrows(m-sd1,  ypos+0.1, m+sd1,ypos+0.1, lwd=5, code=0)
	
	
	rect( summ[2],ypos, summ[5],ypos+0.2, col='white', lwd=2)
	rect( summ[2],  ypos,summ[5],ypos+0.2, col=adjustcolor(cols[party], alpha=0.5), lwd=2)
	
	arrows(m, ypos, m,  ypos+0.2,lwd=5, code=0)
	arrows(med, ypos, med,  ypos+0.2,lwd=2, code=0)
	
	for (ind in 1:nrow(b)){
		points(b$dif[ind], ypos - 0.05 - runif(1) * 0.25, pch=16, col=adjustcolor(cols[party], alpha=0.5), cex=0.8)
	}
}
box()
###


#### plot main model (model m2) split out for Republicans and Democrats

summary(m2)
plot(0, type='n', xlim=c(-0.08, 0.09), ylim=c(0.5, 1.6), axes=FALSE, ylab='', xlab='')
axis(1, at=-5:5/50)
arrows(0, -1, 0, 4, lty=2, lwd=2, code=0)
box()


yy<-1.43
ccR<-summary(m2r)$coefficients
ccD<-summary(m2d)$coefficients

orderOfEffects<-c(2,3,8,5)
na<-c('intercept', 'Ingroup Source (IS)', 'Favored Direction (FD)', 'Same majority', 'Uncertainty', 'Expertise', 'State Size',  'IS x FD')
for (k1 in 1:7){
	k<-orderOfEffects[k1];
	
	
	arrows(ccD[k,1]-1.96*ccD[k,2], yy+0.025, ccD[k,1]+1.96*ccD[k,2], code=0, lwd=1.5)
	points(ccD[k,1], yy+0.025, pch=21, bg='blue', lwd=2, cex=1.2)

	arrows(ccR[k,1]-1.96*ccR[k,2], yy-0.025, ccR[k,1]+1.96*ccR[k,2], code=0, lwd=1.5)
	points(ccR[k,1], yy-0.025, pch=24, bg='red', lwd=2, cex=1.2)

	if (k1==2) rect(-0.01, yy+0.06, 0.01, yy+0.15, col='white', border='white')	
	text(cc[k], yy+0.18, na[k], pos=1, cex=1.2)
#	if (k<8) text(-0.085, yy, na[k], pos=4, cex=1.2)
#	if (k==8) text(0.01, yy-0.03, na[k], pos=4, cex=1.2)
	yy<-yy-0.28
}





### did the bias of either party differ from 0 (where 0 means no bias)?
t.test(subset(mat,mat$ownParty==1)$dif, alternative='less')
t.test(subset(mat,mat$ownParty==2)$dif, alternative='greater')


### put partisanship on a continuum (strong Dems at -7 and strong Dems at +7) and see whether it explains any bias in social information use
mat$contDegr<-ifelse(mat$ownParty==1, -mat$partizanDegree, mat$partizanDegree)
m1<-lm(dif ~ contDegr , data=mat)
summary(m1)

means<-rep(0,2); sds<-rep(0,2); Ns<-rep(0,2)
for (party in 1:2){
	b<-subset(mat, mat$ownParty==party)
	means[party]<-mean(b$partizanDegree, na.rm=TRUE)
	sds[party]<-sd(b$partizanDegree, na.rm=TRUE)
	Ns[party]<-nrow(b)
}
means
sds
Ns


plot(0, type='n', xlim=c(-0.6, 0.6), ylim=c(-7.5, 9.5), ylab='', xlab='', axes=FALSE)
axis(4, 7:1, at=-7:-1)
axis(4, at=1:7)
axis(1, at=-6:6/10)

for (partyAffil in c(-7:-1, 1:7)){
	b<-subset(mat, mat$contDegr==partyAffil)
	
	ypos<- -partyAffil;
	
	m<-mean(b$dif, na.rm=TRUE)
	med<-median(b$dif, na.rm=TRUE)
	sd1<-sd(b$dif, na.rm=TRUE)
	se<-sd1/sqrt(length(which(!is.na(b$dif))))
	
	summ<-summary(b$dif)
	
	arrows(m-sd1,  ypos, m+sd1,ypos, lwd=2, code=0)

	col1<-'blue'
	if (partyAffil>0) col1<-'red'
	col1<-adjustcolor(col1, alpha= abs(partyAffil)/10)
		
	
	rect(summ[2],ypos-0.2, summ[5],ypos+0.2,  col='white', lwd=2)
	rect(summ[2],  ypos-0.2, summ[5],ypos+0.2, col=col1, lwd=2)
	
	arrows(m, ypos+0.2,m, ypos-0.2,  lwd=5, code=0)
	
#	arrows(party, med, party+0.2,med,  code=0, lwd=2)
		
	for (ind in 1:nrow(b)){
		points(b$dif[ind], ypos - 0.5 + runif(1) * 0.2, pch=16, col=col1, cex=0.8)
	}
}
arrows(0,-10,0,10, lty=2, lwd=2, code=0)
#abline(m1, lty=2, lwd=2)
box()

m1Dem<-lm(dif~partizanDegree, data=subset(mat, mat$ownParty==1))
summary(m1Dem)
m1Rep<-lm(dif~partizanDegree, data=subset(mat, mat$ownParty==2))
summary(m1Rep)

#### plot the distribution of partisanship (Fig. S1) ###

f<-rep(14,0)
cnt<-1;
for (partyAffil in c(-7:-1, 1:7)){
	b<-subset(mat, mat$contDegr==partyAffil)
	f[cnt]<-nrow(b)
	cnt<-cnt+1;	
}
# normalize each within their own group
f[1:7]<-f[1:7]/sum(f[1:7])
f[8:14]<-f[8:14]/sum(f[8:14])

par(mfrow=c(1,1), mar=c(5,4,2,2), yaxs='i', xaxs='i')
plot(0, type='n', xlim=c(-8,8), ylim=c(0,0.35), axes=FALSE, xlab='', ylab='')
axis(1, at=-7:-1, labels=7:1)
axis(1, at=1:7)

axis(2, at=0:10/10)
cnt<-1
for (partyAffil in c(-7:-1, 1:7)){

	col1<-'blue'
	if (partyAffil>0) col1<-'red'
	col1<-adjustcolor(col1, alpha= abs(partyAffil)/10)
		

	rect(partyAffil-0.5, 0, partyAffil+0.5, f[cnt], col='white', lwd=2)
	rect(partyAffil-0.5, 0,  partyAffil+0.5, f[cnt],col=col1, lwd=2)
		
	cnt<-cnt+1;
}



####inspect correlations biases per party

par(mfrow=c(1,2), xaxs='r', yaxs='r')
b<-subset(mat, mat$ownParty==1)
plot(b$S_Dem, b$S_Rep, xlim=c(0,1), ylim=c(0,1), main='Democrats', pch=16, col=adjustcolor('blue', alpha=0.4), xlab='', ylab='')
arrows(-1,-1,2,2, code=0)
cor.test(b$S_Dem, b$S_Rep)


b<-subset(mat, mat$ownParty==2)
plot(b$S_Dem, b$S_Rep, xlim=c(0,1), ylim=c(0,1), main='Republicans', pch=16, col=adjustcolor('red', alpha=0.4), xlab='', ylab='')
arrows(-1,-1,2,2, code=0)

cor.test(b$S_Dem, b$S_Rep)


### (preregistered) statistical analyses ####
library('lme4')
library('lmerTest')
library('stargazer')

### first look at ingroup bias and favourability effect
m0<-lmer(s ~ sameParty * favourability+(1|ID), data=a)
summary(m0)
### first look at ingroup bias and favourability effect
m0d<-lmer(s ~ sameParty * favourability+(1|ID), data=subset(a, a$ownParty=='Democratic'))
summary(m0d)
### first look at ingroup bias and favourability effect
m0r<-lmer(s ~ sameParty * favourability+(1|ID), data=subset(a, a$ownParty=='Republican'))
summary(m0r)

### for writing this with stargazer, we need to change the class of the model objects
class(m0) <- "lmerMod"
class(m0d) <- "lmerMod"
class(m0r) <- "lmerMod"

stargazer(m0,m0d, m0r,type="html" ,ci=TRUE, ci.level=0.95, title="", align=TRUE, star.cutoffs=c(0.05,0.01,0.001), single.row=TRUE,
 report=('vcsp'),
	out="Table S3.html")



### main model
m1<-lmer(s ~ sameParty * favourability + sameWinner + confidence + expertise + population+ (1|ID), data=a)
summary(m1)

### for writing this with stargazer, we need to change the class of the model objects
class(m1) <- "lmerMod"
class(m1d) <- "lmerMod"
class(m1r) <- "lmerMod"

stargazer(m1,m1d, m1r,type="html" ,ci=TRUE, ci.level=0.95, title="", align=TRUE, star.cutoffs=c(0.05,0.01,0.001), single.row=TRUE,
 report=('vcsp'),
	out="Table S3.html")




### main model
m2<-lmer(s ~ sameParty * favourability + age + gender + identification_continous + (1|ID), data=a)
summary(m2)

### for writing this with stargazer, we need to change the class of the model objects
class(m0) <- "lmerMod"
class(m1) <- "lmerMod"
class(m2) <- "lmerMod"

stargazer(m0,m1,type="html" ,ci=TRUE, ci.level=0.95, title="", align=TRUE, star.cutoffs=c(0.05,0.01,0.001), single.row=TRUE,
 report=('vcsp'),
	out="Table 1.html")

