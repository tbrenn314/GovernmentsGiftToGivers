################################################################################
# GovernmentsGiftToGivers_table_calculations.R
#
# Replication code for all tables and computed numbers in:
#   "The Government's Gift to Givers: Donating Appreciated Stock"
#   by Thomas J. Brennan and David M. Schizer
#   Florida Tax Review (forthcoming)
#
# USAGE:
#   source("GovernmentsGiftToGivers_table_calculations.R")
#
# OUTPUT:
#   - Prints every table in the paper to the console, labeled by table number
#     and by the Part of the paper in which it appears
#   - Leaves the data frames (tab_1, tab_2, ...) and the kable-formatted
#     tables (tab_1s, tab_2s, ...) in the global environment
#   - Prints session info for reproducibility
#
# METHOD:
#   Option values use the Black-Scholes formulas for European options on a
#   non-dividend-paying stock (derivmkts::bscall, bsput, cashcall).  The
#   risk-free rate is 5% with annual compounding, i.e., log(1.05) = 4.88%
#   continuously compounded.  All options are at the money (strike = initial
#   share price = $10) unless otherwise noted.
#
# REQUIRED PACKAGES: derivmkts, knitr
#   install.packages(c("derivmkts", "knitr"))
################################################################################


library(derivmkts)
library(knitr)

# params for numerical examples
A  = 1000       # amount of simple contribution
TT = 0.40       # tax rate on ordinary income
KK = 0.25       # tax rate on net capital gains
CC = 0.21       # tax rate for corporations
vv = 0.40       # stock volatility (base case)
rr = log(1.05)  # risk-free return, ctsly cmpd
dd = 0          # dividend rate
shprc = 10      # initial share price
nshrs = A/shprc # number of shares to donate
vvvec = c(0.20,
	  0.40,
	  0.60) # vector of volatilities
hhvec = c(1,
	  5,
	  10)   # vector of horizon times
hhvec2 = c(1:10) # more detailed vector of horizon times


# introductory material (before part I): PV of $1,000 deduction in one year
v_intro_pv = A*exp(-rr)
s_intro_pv = sprintf('%.2f',v_intro_pv)

# Part 3A2 / Table 1
# table for deferral with bond
tab_1 = data.frame(c(A*TT,
		     A*TT*exp(-rr)),
                   c(A*(1-TT),
		     A-A*TT*exp(-rr)))
row.names(tab_1) = c('Current Cash Gift',
		     'Gift of Bond in One Year')
names(tab_1)     = c('Tax Benefit',
		     'Cost to Reach Target')
tab_1s = knitr::kable(tab_1,digits=2)

# Part 4A3 / Table 2
# table for one-year at-the-money call prices
tab_2 = data.frame(rbind(
			 bscall(s=shprc,k=shprc,r=rr,v=vvvec[1],tt=hhvec[1],d=0)*c(1,CC*100,TT*100),
			 bscall(s=shprc,k=shprc,r=rr,v=vvvec[2],tt=hhvec[1],d=0)*c(1,CC*100,TT*100),
			 bscall(s=shprc,k=shprc,r=rr,v=vvvec[3],tt=hhvec[1],d=0)*c(1,CC*100,TT*100)
			 )
                  )
row.names(tab_2) = c(paste(vvvec[1]*100,'%',sep=''),
		     paste(vvvec[2]*100,'%',sep=''),
		     paste(vvvec[3]*100,'%',sep=''))
names(tab_2)     = c('1 Share',
		     paste(CC*100,'Shares'),
		     paste(TT*100,'Shares'))
tab_2s = knitr::kable(tab_2,digits=2)

# Part 4A4 / Tables 3abc
# tables for five-year and ten-year at-the-money call prices
tab_3a = data.frame(rbind(
			 bscall(s=shprc,k=shprc,r=rr,v=vvvec[1],tt=hhvec,d=0)*1,
			 bscall(s=shprc,k=shprc,r=rr,v=vvvec[2],tt=hhvec,d=0)*1,
			 bscall(s=shprc,k=shprc,r=rr,v=vvvec[3],tt=hhvec,d=0)*1
			 )
                  )
row.names(tab_3a) = c(paste(vvvec[1]*100,'%',sep=''),
		     paste(vvvec[2]*100,'%',sep=''),
		     paste(vvvec[3]*100,'%',sep=''))
names(tab_3a)     = paste(hhvec,'-Year',sep='')
tab_3as = knitr::kable(tab_3a,digits=2)

tab_3b = data.frame(rbind(
			 bscall(s=shprc,k=shprc,r=rr,v=vvvec[1],tt=hhvec,d=0)*CC*100,
			 bscall(s=shprc,k=shprc,r=rr,v=vvvec[2],tt=hhvec,d=0)*CC*100,
			 bscall(s=shprc,k=shprc,r=rr,v=vvvec[3],tt=hhvec,d=0)*CC*100
			 )
                  )
row.names(tab_3b) = c(paste(vvvec[1]*100,'%',sep=''),
		     paste(vvvec[2]*100,'%',sep=''),
		     paste(vvvec[3]*100,'%',sep=''))
names(tab_3b)     = paste(hhvec,'-Year',sep='')
tab_3bs = knitr::kable(tab_3b,digits=2)

tab_3c = data.frame(rbind(
			 bscall(s=shprc,k=shprc,r=rr,v=vvvec[1],tt=hhvec,d=0)*TT*100,
			 bscall(s=shprc,k=shprc,r=rr,v=vvvec[2],tt=hhvec,d=0)*TT*100,
			 bscall(s=shprc,k=shprc,r=rr,v=vvvec[3],tt=hhvec,d=0)*TT*100
			 )
                  )
row.names(tab_3c) = c(paste(vvvec[1]*100,'%',sep=''),
		     paste(vvvec[2]*100,'%',sep=''),
		     paste(vvvec[3]*100,'%',sep=''))
names(tab_3c)     = paste(hhvec,'-Year',sep='')
tab_3cs = knitr::kable(tab_3c,digits=2)



# Part 4B / Table 4 (deferral cost, T=40%; paper reports years 1, 5, 10)
tab_4 = data.frame(hhvec2,
		   rep(A*TT,length(hhvec2)),
		   exp(rr*hhvec2),
		   A*TT*exp(-rr*hhvec2),
		   A*TT-A*TT*exp(-rr*hhvec2))
row.names(tab_4) = hhvec2
names(tab_4)     = c('Years','Deduction','Discount','PV','Deferral Cost')
tab_4s = knitr::kable(tab_4,digits=c(0,0,4,2,2))

# Part 4B / Table 5 (deferral cost, 21%; paper reports years 1, 5, 10)
tab_5 = data.frame(hhvec2,
		   rep(A*CC,length(hhvec2)),
		   exp(rr*hhvec2),
		   A*CC*exp(-rr*hhvec2),
		   A*CC-A*CC*exp(-rr*hhvec2))
row.names(tab_5) = hhvec2
names(tab_5)     = c('Years','Deduction','Discount','PV','Deferral Cost')
tab_5s = knitr::kable(tab_5,digits=c(0,0,4,2,2))

# Part 4C5 / Table 6 [divided as 6a and 6b for double content of cells]
tab_6a = data.frame(
		   bsput(s=shprc,k=shprc,r=rr,v=vvvec,tt=hhvec[1],d=0)*1,
		   bsput(s=shprc,k=shprc,r=rr,v=vvvec,tt=hhvec[2],d=0)*1,
		   bsput(s=shprc,k=shprc,r=rr,v=vvvec,tt=hhvec[3],d=0)*1
		   )
row.names(tab_6a) = c(paste(vvvec[1]*100,'%',sep=''),
		     paste(vvvec[2]*100,'%',sep=''),
		     paste(vvvec[3]*100,'%',sep=''))
names(tab_6a)     = c(paste(hhvec[1],'Y',sep=''),
		     paste(hhvec[2],'Y',sep=''),
		     paste(hhvec[3],'Y',sep=''))
tab_6as = knitr::kable(tab_6a,digits=2)

tab_6b = data.frame(
		   bsput(s=shprc,k=shprc,r=rr,v=vvvec,tt=hhvec[1],d=0)*15,
		   bsput(s=shprc,k=shprc,r=rr,v=vvvec,tt=hhvec[2],d=0)*15,
		   bsput(s=shprc,k=shprc,r=rr,v=vvvec,tt=hhvec[3],d=0)*15
		   )
row.names(tab_6b) = c(paste(vvvec[1]*100,'%',sep=''),
		     paste(vvvec[2]*100,'%',sep=''),
		     paste(vvvec[3]*100,'%',sep=''))
names(tab_6b)     = c(paste(hhvec[1],'Y',sep=''),
		     paste(hhvec[2],'Y',sep=''),
		     paste(hhvec[3],'Y',sep=''))
tab_6bs = knitr::kable(tab_6b,digits=2)


# Part 4E1b / Tables 7-9: individuals with long-term gain (calls on 40 shares,
# deferral cost, puts on 15 shares); 1-, 5-, and 10-year delay
# Part 4E1b / Table 7
tab_7 = data.frame(
		bscall(s=shprc,k=shprc,r=rr,v=vvvec,tt=hhvec[1],d=0)*(TT*nshrs),
		rep(A*TT*(1-exp(-rr*hhvec[1])),length(vvvec)),
		bsput(s=shprc,k=shprc,r=rr,v=vvvec,tt=hhvec[1],d=0)*(TT-KK)*nshrs
		)
tab_7 = cbind(tab_7,tab_7[,1]-tab_7[,2]-tab_7[,3])
tab_7 = cbind(tab_7,tab_7[,4]/(TT*A)*100)
row.names(tab_7) = c(paste(vvvec[1]*100,'%',sep=''),
		     paste(vvvec[2]*100,'%',sep=''),
		     paste(vvvec[3]*100,'%',sep=''))
names(tab_7) = c('Extra Ded',
	         'Deferral Cost',
	         'Loss Cost',
		 'Net Adv',
		 '% Benefit')
tab_7s = knitr::kable(tab_7,digits=2)
		   
# Part 4E1b / Table 8
tab_8 = data.frame(
		bscall(s=shprc,k=shprc,r=rr,v=vvvec,tt=hhvec[2],d=0)*(TT*nshrs),
		rep(A*TT*(1-exp(-rr*hhvec[2])),length(vvvec)),
		bsput(s=shprc,k=shprc,r=rr,v=vvvec,tt=hhvec[2],d=0)*(TT-KK)*nshrs
		)
tab_8 = cbind(tab_8,tab_8[,1]-tab_8[,2]-tab_8[,3])
tab_8 = cbind(tab_8,tab_8[,4]/(TT*A)*100)
row.names(tab_8) = c(paste(vvvec[1]*100,'%',sep=''),
		     paste(vvvec[2]*100,'%',sep=''),
		     paste(vvvec[3]*100,'%',sep=''))
names(tab_8) = c('Extra Ded',
	         'Deferral Cost',
	         'Loss Cost',
		 'Net Adv',
		 '% Benefit')
tab_8s = knitr::kable(tab_8,digits=2)

# Part 4E1b / Table 9
tab_9 = data.frame(
		bscall(s=shprc,k=shprc,r=rr,v=vvvec,tt=hhvec[3],d=0)*(TT*nshrs),
		rep(A*TT*(1-exp(-rr*hhvec[3])),length(vvvec)),
		bsput(s=shprc,k=shprc,r=rr,v=vvvec,tt=hhvec[3],d=0)*(TT-KK)*nshrs
		)
tab_9 = cbind(tab_9,tab_9[,1]-tab_9[,2]-tab_9[,3])
tab_9 = cbind(tab_9,tab_9[,4]/(TT*A)*100)
row.names(tab_9) = c(paste(vvvec[1]*100,'%',sep=''),
		     paste(vvvec[2]*100,'%',sep=''),
		     paste(vvvec[3]*100,'%',sep=''))
names(tab_9) = c('Extra Ded',
	         'Deferral Cost',
	         'Loss Cost',
		 'Net Adv',
		 '% Benefit')
tab_9s = knitr::kable(tab_9,digits=2)
		   
# Part 4E2b / Tables 10-12: individuals with short-term gain (no put cost)
# Part 4E2b / Table 10
tab_10 = data.frame(
		bscall(s=shprc,k=shprc,r=rr,v=vvvec,tt=hhvec[1],d=0)*(TT*nshrs),
		rep(A*TT*(1-exp(-rr*hhvec[1])),length(vvvec))
		)
tab_10 = cbind(tab_10,tab_10[,1]-tab_10[,2])
tab_10 = cbind(tab_10,tab_10[,3]/(TT*A)*100)
row.names(tab_10) = c(paste(vvvec[1]*100,'%',sep=''),
		     paste(vvvec[2]*100,'%',sep=''),
		     paste(vvvec[3]*100,'%',sep=''))
names(tab_10) = c('Extra Ded',
	         'Deferral Cost',
		 'Net Adv',
		 '% Benefit')
tab_10s = knitr::kable(tab_10,digits=2)
		   
# Part 4E2b / Table 11
tab_11 = data.frame(
		bscall(s=shprc,k=shprc,r=rr,v=vvvec,tt=hhvec[2],d=0)*(TT*nshrs),
		rep(A*TT*(1-exp(-rr*hhvec[2])),length(vvvec))
		)
tab_11 = cbind(tab_11,tab_11[,1]-tab_11[,2])
tab_11 = cbind(tab_11,tab_11[,3]/(TT*A)*100)
row.names(tab_11) = c(paste(vvvec[1]*100,'%',sep=''),
		     paste(vvvec[2]*100,'%',sep=''),
		     paste(vvvec[3]*100,'%',sep=''))
names(tab_11) = c('Extra Ded',
	         'Deferral Cost',
		 'Net Adv',
		 '% Benefit')
tab_11s = knitr::kable(tab_11,digits=2)

# Part 4E2b / Table 12
tab_12 = data.frame(
		bscall(s=shprc,k=shprc,r=rr,v=vvvec,tt=hhvec[3],d=0)*(TT*nshrs),
		rep(A*TT*(1-exp(-rr*hhvec[3])),length(vvvec))
		)
tab_12 = cbind(tab_12,tab_12[,1]-tab_12[,2])
tab_12 = cbind(tab_12,tab_12[,3]/(TT*A)*100)
row.names(tab_12) = c(paste(vvvec[1]*100,'%',sep=''),
		     paste(vvvec[2]*100,'%',sep=''),
		     paste(vvvec[3]*100,'%',sep=''))
names(tab_12) = c('Extra Ded',
	         'Deferral Cost',
		 'Net Adv',
		 '% Benefit')
tab_12s = knitr::kable(tab_12,digits=2)
		   
# Part 4E3b / Tables 13-15: corporations (calls on 21 shares, no put cost)
# Part 4E3b / Table 13
tab_13 = data.frame(
		bscall(s=shprc,k=shprc,r=rr,v=vvvec,tt=hhvec[1],d=0)*(CC*nshrs),
		rep(A*CC*(1-exp(-rr*hhvec[1])),length(vvvec))
		)
tab_13 = cbind(tab_13,tab_13[,1]-tab_13[,2])
tab_13 = cbind(tab_13,tab_13[,3]/(CC*A)*100)
row.names(tab_13) = c(paste(vvvec[1]*100,'%',sep=''),
		     paste(vvvec[2]*100,'%',sep=''),
		     paste(vvvec[3]*100,'%',sep=''))
names(tab_13) = c('Extra Ded',
	         'Deferral Cost',
		 'Net Adv',
		 '% Benefit')
tab_13s = knitr::kable(tab_13,digits=2)
		   
# Part 4E3b / Table 14
tab_14 = data.frame(
		bscall(s=shprc,k=shprc,r=rr,v=vvvec,tt=hhvec[2],d=0)*(CC*nshrs),
		rep(A*CC*(1-exp(-rr*hhvec[2])),length(vvvec))
		)
tab_14 = cbind(tab_14,tab_14[,1]-tab_14[,2])
tab_14 = cbind(tab_14,tab_14[,3]/(CC*A)*100)
row.names(tab_14) = c(paste(vvvec[1]*100,'%',sep=''),
		     paste(vvvec[2]*100,'%',sep=''),
		     paste(vvvec[3]*100,'%',sep=''))
names(tab_14) = c('Extra Ded',
	         'Deferral Cost',
		 'Net Adv',
		 '% Benefit')
tab_14s = knitr::kable(tab_14,digits=2)

# Part 4E3b / Table 15
tab_15 = data.frame(
		bscall(s=shprc,k=shprc,r=rr,v=vvvec,tt=hhvec[3],d=0)*(CC*nshrs),
		rep(A*CC*(1-exp(-rr*hhvec[3])),length(vvvec))
		)
tab_15 = cbind(tab_15,tab_15[,1]-tab_15[,2])
tab_15 = cbind(tab_15,tab_15[,3]/(CC*A)*100)
row.names(tab_15) = c(paste(vvvec[1]*100,'%',sep=''),
		     paste(vvvec[2]*100,'%',sep=''),
		     paste(vvvec[3]*100,'%',sep=''))
names(tab_15) = c('Extra Ded',
	         'Deferral Cost',
		 'Net Adv',
		 '% Benefit')
tab_15s = knitr::kable(tab_15,digits=2)
		   
# Part 5A4 / Tables 16-18: monetized value at 40% volatility (sell calls,
# buy bond to offset deferral cost, buy puts if long-term gain)
# Part 5A4 / table 16
tab_16 = data.frame(
		bscall(s=shprc,k=shprc,r=rr,v=vvvec[2],tt=hhvec,d=0)*(TT*nshrs),
		-A*TT*(1-exp(-rr*hhvec)),
		-bsput(s=shprc,k=shprc,r=rr,v=vvvec[2],tt=hhvec,d=0)*(TT-KK)*nshrs
		)
tab_16 = cbind(tab_16,tab_16[,1]+tab_16[,2]+tab_16[,3])
tab_16 = cbind(tab_16,tab_16[,4]/(TT*A)*100)
row.names(tab_16) = paste(hhvec,'Y',sep='')
names(tab_16) = c('Sell Calls',
	         'Buy Bond',
	         'Buy Puts',
		 'Net Adv',
		 '% Benefit')
tab_16s = knitr::kable(tab_16,digits=2)
		   
# Part 5A4 / table 17
tab_17 = data.frame(
		bscall(s=shprc,k=shprc,r=rr,v=vvvec[2],tt=hhvec,d=0)*(TT*nshrs),
		-A*TT*(1-exp(-rr*hhvec))
		)
tab_17 = cbind(tab_17,tab_17[,1]+tab_17[,2])
tab_17 = cbind(tab_17,tab_17[,3]/(TT*A)*100)
row.names(tab_17) = paste(hhvec,'Y',sep='')
names(tab_17) = c('Sell Calls',
	         'Buy Bond',
		 'Net Adv',
		 '% Benefit')
tab_17s = knitr::kable(tab_17,digits=2)
		   
# Part 5A4 / table 18
tab_18 = data.frame(
		bscall(s=shprc,k=shprc,r=rr,v=vvvec[2],tt=hhvec,d=0)*(CC*nshrs),
		-A*CC*(1-exp(-rr*hhvec))
		)
tab_18 = cbind(tab_18,tab_18[,1]+tab_18[,2])
tab_18 = cbind(tab_18,tab_18[,3]/(CC*A)*100)
row.names(tab_18) = paste(hhvec,'Y',sep='')
names(tab_18) = c('Sell Calls',
	         'Buy Bond',
		 'Net Adv',
		 '% Benefit')
tab_18s = knitr::kable(tab_18,digits=2)
		   
# Part 6C4 / Tables 19-21: unified instrument.  Strike solves
#   K*exp(-r*t) + call(K) - put(K)/(1-lossrate) = K,
# and the discount is (shprc - strike)*nshrs*(1-taxrate).
# Part 6C4 / table 19abc [discounts using cost of unified instrument; indiv TT/KK]
ff = function(kk,ss,vv,rr,dd,tt) {
  prc = kk*exp(-rr*tt) +
	  bscall(k=kk,s=ss,v=vv,r=rr,d=dd,t=tt)  - 
	  bsput(k=kk,s=ss,v=vv,r=rr,d=dd,t=tt)/(1-KK)
  ans = prc - kk
  return(ans)
}

strike_mat = matrix(NA,length(vvvec),length(hhvec))
for (i in 1:length(vvvec)) {
	for (j in 1:length(hhvec)) {
		ur = uniroot(ff,c(0.1,2.0)*shprc,ss=shprc,vv=vvvec[i],rr=rr,dd=dd,tt=hhvec[j])
		strike_mat[i,j] = ur$root
	}
}
tab_19a = data.frame(rbind(
		(shprc-strike_mat[1,])*nshrs*(1-TT),
		(shprc-strike_mat[1,])*nshrs*(1-TT)/(A*TT)*100
		)
          )
row.names(tab_19a) = c(paste('Discount Below $',A*(1-TT),sep=''),
		     paste('Pct Increase Above $',A*TT,' Benefit',sep=''))
names(tab_19a)     = paste(hhvec,'Year')
tab_19as = knitr::kable(tab_19a,digits=2)
tab_19b = data.frame(rbind(
		(shprc-strike_mat[2,])*nshrs*(1-TT),
		(shprc-strike_mat[2,])*nshrs*(1-TT)/(A*TT)*100
		)
          )
row.names(tab_19b) = c(paste('Discount Below $',A*(1-TT),sep=''),
		     paste('Pct Increase Above $',A*TT,' Benefit',sep=''))
names(tab_19b)     = paste(hhvec,'Year')
tab_19bs = knitr::kable(tab_19b,digits=2)
tab_19c = data.frame(rbind(
		(shprc-strike_mat[3,])*nshrs*(1-TT),
		(shprc-strike_mat[3,])*nshrs*(1-TT)/(A*TT)*100
		)
          )
row.names(tab_19c) = c(paste('Discount Below $',A*(1-TT),sep=''),
		     paste('Pct Increase Above $',A*TT,' Benefit',sep=''))
names(tab_19c)     = paste(hhvec,'Year')
tab_19cs = knitr::kable(tab_19c,digits=2)

# Part 6C4 / table 20abc [discounts using cost of unified instrument; indiv TT/KK=TT]
ff = function(kk,ss,vv,rr,dd,tt) {
  prc = kk*exp(-rr*tt) +
	  bscall(k=kk,s=ss,v=vv,r=rr,d=dd,t=tt)  - 
	  bsput(k=kk,s=ss,v=vv,r=rr,d=dd,t=tt)/(1-TT)
  ans = prc - kk
  return(ans)
}

strike_mat = matrix(NA,length(vvvec),length(hhvec))
for (i in 1:length(vvvec)) {
	for (j in 1:length(hhvec)) {
		ur = uniroot(ff,c(0.1,2.0)*shprc,ss=shprc,vv=vvvec[i],rr=rr,dd=dd,tt=hhvec[j])
		strike_mat[i,j] = ur$root
	}
}
tab_20a = data.frame(rbind(
		(shprc-strike_mat[1,])*nshrs*(1-TT),
		(shprc-strike_mat[1,])*nshrs*(1-TT)/(A*TT)*100
		)
          )
row.names(tab_20a) = c(paste('Discount Below $',A*(1-TT),sep=''),
		     paste('Pct Increase Above $',A*TT,' Benefit',sep=''))
names(tab_20a)     = paste(hhvec,'Year')
tab_20as = knitr::kable(tab_20a,digits=2)
tab_20b = data.frame(rbind(
		(shprc-strike_mat[2,])*nshrs*(1-TT),
		(shprc-strike_mat[2,])*nshrs*(1-TT)/(A*TT)*100
		)
          )
row.names(tab_20b) = c(paste('Discount Below $',A*(1-TT),sep=''),
		     paste('Pct Increase Above $',A*TT,' Benefit',sep=''))
names(tab_20b)     = paste(hhvec,'Year')
tab_20bs = knitr::kable(tab_20b,digits=2)
tab_20c = data.frame(rbind(
		(shprc-strike_mat[3,])*nshrs*(1-TT),
		(shprc-strike_mat[3,])*nshrs*(1-TT)/(A*TT)*100
		)
          )
row.names(tab_20c) = c(paste('Discount Below $',A*(1-TT),sep=''),
		     paste('Pct Increase Above $',A*TT,' Benefit',sep=''))
names(tab_20c)     = paste(hhvec,'Year')
tab_20cs = knitr::kable(tab_20c,digits=2)

# Part 6C4 / table 21abc [discounts using cost of unified instrument; corp CC/CC]
ff = function(kk,ss,vv,rr,dd,tt) {
  prc = kk*exp(-rr*tt) +
	  bscall(k=kk,s=ss,v=vv,r=rr,d=dd,t=tt)  - 
	  bsput(k=kk,s=ss,v=vv,r=rr,d=dd,t=tt)/(1-CC)
  ans = prc - kk
  return(ans)
}

strike_mat = matrix(NA,length(vvvec),length(hhvec))
for (i in 1:length(vvvec)) {
	for (j in 1:length(hhvec)) {
		ur = uniroot(ff,c(0.1,2.0)*shprc,ss=shprc,vv=vvvec[i],rr=rr,dd=dd,tt=hhvec[j])
		strike_mat[i,j] = ur$root
	}
}
tab_21a = data.frame(rbind(
		(shprc-strike_mat[1,])*nshrs*(1-CC),
		(shprc-strike_mat[1,])*nshrs*(1-CC)/(A*CC)*100
		)
          )
row.names(tab_21a) = c(paste('Discount Below $',A*(1-CC),sep=''),
		     paste('Pct Increase Above $',A*CC,' Benefit',sep=''))
names(tab_21a)     = paste(hhvec,'Year')
tab_21as = knitr::kable(tab_21a,digits=2)
tab_21b = data.frame(rbind(
		(shprc-strike_mat[2,])*nshrs*(1-CC),
		(shprc-strike_mat[2,])*nshrs*(1-CC)/(A*CC)*100
		)
          )
row.names(tab_21b) = c(paste('Discount Below $',A*(1-CC),sep=''),
		     paste('Pct Increase Above $',A*CC,' Benefit',sep=''))
names(tab_21b)     = paste(hhvec,'Year')
tab_21bs = knitr::kable(tab_21b,digits=2)
tab_21c = data.frame(rbind(
		(shprc-strike_mat[3,])*nshrs*(1-CC),
		(shprc-strike_mat[3,])*nshrs*(1-CC)/(A*CC)*100
		)
          )
row.names(tab_21c) = c(paste('Discount Below $',A*(1-CC),sep=''),
		     paste('Pct Increase Above $',A*CC,' Benefit',sep=''))
names(tab_21c)     = paste(hhvec,'Year')
tab_21cs = knitr::kable(tab_21c,digits=2)


# Appendix A / table A1: grossed-up binary calls (1-year, 40% vol)
p_of_atleast_strike <- function(k) {
  ans = cashcall(k=k,s=shprc,v=vvvec[2],r=rr,d=dd,tt=hhvec[1])*exp(rr*hhvec[1])
  return(ans)
}
p_vec = c(0.2,0.5,0.8)
k_vec = NA*p_vec
for (i in 1:length(p_vec)) {
  k_vec[i] = uniroot(function(k) p_of_atleast_strike(k) - p_vec[i],
		     interval=c(shprc/100, shprc*100),
		     tol = 1e-10)$root
}
D_vec = (1-KK)*exp(rr) + KK*(1-p_vec)
P_vec = p_vec/exp(rr)
G_vec = P_vec*(1-KK)*exp(rr)/D_vec
R_vec = KK*(1-p_vec)/D_vec
N_vec = A*(1-TT)/P_vec
save_vec = A*(1-TT)*R_vec
pct_vec  = (1-TT)*R_vec/TT

tab_A1 = data.frame(
		    p=100*p_vec,
		    X=k_vec,
		    G=G_vec,
		    N=N_vec,
		    disc=save_vec,
		    pct=100*pct_vec
		    )
tab_A1s = knitr::kable(tab_A1,digits=c(0,2,3,0,2,1))


################################################################################
## PRINT ALL RESULTS
################################################################################
show = function(title, tbl) {
  cat('\n', strrep('=', 78), '\n', title, '\n', strrep('=', 78), sep='')
  print(tbl)
}

cat('\nReplication output: "The Government\'s Gift to Givers"\n')
cat('Brennan & Schizer, Florida Tax Review (forthcoming)\n')
cat('\nIntroduction: PV of $1,000 deduction in one year = $', s_intro_pv, '\n', sep='')

show('Table 1 [Part III.A.2]: Tax Benefit of Current and Future Cash Gifts', tab_1s)
show('Table 2 [Part IV.A.3]: Ex Ante Value of Deducting Appreciation (1-Year Calls)', tab_2s)
show('Table 3a [Part IV.A.4]: One Option (Benchmark)', tab_3as)
show('Table 3b [Part IV.A.4]: 21 Options (Corporations)', tab_3bs)
show('Table 3c [Part IV.A.4]: 40 Options (Individuals)', tab_3cs)
show('Table 4 [Part IV.B]: Deferral Cost, T=40% (paper reports years 1, 5, 10)', tab_4s)
show('Table 5 [Part IV.B]: Deferral Cost, 21% (paper reports years 1, 5, 10)', tab_5s)
show('Table 6 (per share; not in paper) [Part IV.C.5]: ATM Put Values', tab_6as)
show('Table 6 [Part IV.C.5]: Loss Devaluation Cost (Puts on 15 Shares)', tab_6bs)
show('Table 7 [Part IV.E.1.b]: Net Advantage, Long-Term Gain, 1-Year Delay', tab_7s)
show('Table 8 [Part IV.E.1.b]: Net Advantage, Long-Term Gain, 5-Year Delay', tab_8s)
show('Table 9 [Part IV.E.1.b]: Net Advantage, Long-Term Gain, 10-Year Delay', tab_9s)
show('Table 10 [Part IV.E.2.b]: Net Advantage, Short-Term Gain, 1-Year Delay', tab_10s)
show('Table 11 [Part IV.E.2.b]: Net Advantage, Short-Term Gain, 5-Year Delay', tab_11s)
show('Table 12 [Part IV.E.2.b]: Net Advantage, Short-Term Gain, 10-Year Delay', tab_12s)
show('Table 13 [Part IV.E.3.b]: Net Advantage, Corporations, 1-Year Delay', tab_13s)
show('Table 14 [Part IV.E.3.b]: Net Advantage, Corporations, 5-Year Delay', tab_14s)
show('Table 15 [Part IV.E.3.b]: Net Advantage, Corporations, 10-Year Delay', tab_15s)
show('Table 16 [Part V.A.4]: Monetized Value, Individuals, Long-Term Gain', tab_16s)
show('Table 17 [Part V.A.4]: Monetized Value, Individuals, Short-Term Gain', tab_17s)
show('Table 18 [Part V.A.4]: Monetized Value, Corporations', tab_18s)
show('Table 19a [Part VI.C.4]: Unified Instrument, Long-Term Gain, 20% Vol', tab_19as)
show('Table 19b [Part VI.C.4]: Unified Instrument, Long-Term Gain, 40% Vol', tab_19bs)
show('Table 19c [Part VI.C.4]: Unified Instrument, Long-Term Gain, 60% Vol', tab_19cs)
show('Table 20a [Part VI.C.4]: Unified Instrument, Short-Term Gain, 20% Vol', tab_20as)
show('Table 20b [Part VI.C.4]: Unified Instrument, Short-Term Gain, 40% Vol', tab_20bs)
show('Table 20c [Part VI.C.4]: Unified Instrument, Short-Term Gain, 60% Vol', tab_20cs)
show('Table 21a [Part VI.C.4]: Unified Instrument, Corporations, 20% Vol', tab_21as)
show('Table 21b [Part VI.C.4]: Unified Instrument, Corporations, 40% Vol', tab_21bs)
show('Table 21c [Part VI.C.4]: Unified Instrument, Corporations, 60% Vol', tab_21cs)
show('Table A1 [Appendix]: Grossed-Up Binary Calls', tab_A1s)

cat('\n', strrep('=', 78), '\nSESSION INFO\n', strrep('=', 78), '\n', sep='')
print(sessionInfo())
