load(paste0('/mnt/ExtraDrive1/Work/desktop_data/2022_papers/',
	'HSsurv2023/data/', 'akpv_datacube.rda'))

figpath = paste0('/mnt/ExtraDrive1/Work/desktop_data/2022_papers/',
	'HSsurv2023/inst/scripts/DataCube/figures/')

stockid_names = data.frame(stockid = 1:12, stocknames = 
	c('Aleutians',
	'Pribilofs',
	'Bristol Bay',
	'North Kodiak',
	'South Kodiak',
	'Prince William Sound',
	'Cook Inlet/Shelikof Strait',
	'Glacier Bay/Icy Strait',
	'Lynn Canal/Stephens Passage',
	'Sitka/Chatham Strait',
	'Dixon/Cape Decision',
	'Clarence Strait')
)
	
#-------------------------------------------------------------------------------
#                 Fig_abundance_by_stock
#-------------------------------------------------------------------------------

plot_abundance = function(stock_id){
	pop = matrix(NA, nrow = 1000, ncol = 27)
	for(i in 1:1000) pop[i,] = 
		apply(akpv_datacube[[i]][attr(akpv_datacube[[i]], 'stockid') == 
			stock_id,], 2, sum)
	bot = apply(pop, 2, quantile, prob = .025)
	top = apply(pop, 2, quantile, prob = .975)
	par(mar = c(5,5,5,1))
	plot(c(1,27), c(min(bot),max(top)), type = 'n',
		xaxt = 'n', ylab = 'Estimated Abundance', cex.main = 1.5,
		xlab = 'Year', cex.lab = 2, cex.axis = 1.5,
		main = paste0('Stock ', stock_id, ': ', 
			stockid_names[stock_id,'stocknames']))
	axis(1, at = c(5, 10, 15, 20, 25), labels = c(2000, 2005, 2010, 2015, 2020),
		cex.axis = 1.5)
	points(apply(pop,2,mean), pch = 19, cex = 2)
	for(i in 1:27)
		lines(c(i,i),c(bot[i],top[i]), lty = 1, lwd = 2)
}
	
pdf(paste0(figpath, 'Fig_abundance_by_stock.pdf'), width = 13, height = 17)

	layout(matrix(1:12, nrow = 4, ncol = 3, byrow = TRUE))

	plot_abundance(stock_id = 1)
	plot_abundance(stock_id = 2)
	plot_abundance(stock_id = 3)
	plot_abundance(stock_id = 4)
	plot_abundance(stock_id = 5)
	plot_abundance(stock_id = 6)
	plot_abundance(stock_id = 7)
	plot_abundance(stock_id = 8)
	plot_abundance(stock_id = 9)
	plot_abundance(stock_id = 10)
	plot_abundance(stock_id = 11)
	plot_abundance(stock_id = 12)
	
	layout(1)
	
dev.off()


#-------------------------------------------------------------------------------
#                 Fig_8yr_trend_by_stock
#-------------------------------------------------------------------------------

plot_trend_absolute = function(stock_id){
	pop = matrix(NA, nrow = 1000, ncol = 27)
	for(i in 1:1000) pop[i,] = 
		apply(akpv_datacube[[i]][
			attr(akpv_datacube[[i]], 'stockid') == stock_id,], 2, sum)
  maxi = 27
  trendlen = 8
  linTrendMat = NULL
  for(i in 1:(maxi - trendlen + 1))
    linTrendMat = cbind(linTrendMat,
      apply(pop,1,function(v){coef(lm(y~x, 
        data.frame(x=1:8,y = v[i:(i + trendlen - 1)])))[2]}))
  bot = apply(linTrendMat, 2, quantile, prob = .025)
  top = apply(linTrendMat, 2, quantile, prob = .975)
  par(mar = c(5,5,5,1))
    plot(c(1,length(top)), c(min(bot),max(top)), type = 'n',
      xaxt = 'n', ylab = 'Trailing 8-Year Trend (Seals/Year)', 
      cex.main = 1.5, xlab = 'Year', cex.lab = 2, cex.axis = 1.5,
			main = paste0('Stock ', stock_id, ': ', 
				stockid_names[stock_id,'stocknames']))
    axis(1, at = c(1, 5, 10, 15, 20), 
			labels = c(2003, 2007, 2012, 2017, 2022),
			cex.axis = 1.5)
    lines(c(1, 22), c(0,0), lty = 2, lwd = 3, col ='red')
    points(apply(linTrendMat, 2, mean), pch = 19, cex = 2)
    for(i in 1:length(top))
      lines(c(i,i), c(bot[i], top[i]), lty = 1, lwd = 2)
}


pdf(paste0(figpath, 'Fig_8yr_trend_by_stock.pdf'), width = 13, height = 17)

	layout(matrix(1:12, nrow = 4, ncol = 3, byrow = TRUE))
	
	plot_trend_absolute(stock_id = 1)
	plot_trend_absolute(stock_id = 2)
	plot_trend_absolute(stock_id = 3)
	plot_trend_absolute(stock_id = 4)
	plot_trend_absolute(stock_id = 5)
	plot_trend_absolute(stock_id = 6)
	plot_trend_absolute(stock_id = 7)
	plot_trend_absolute(stock_id = 8)
	plot_trend_absolute(stock_id = 9)
	plot_trend_absolute(stock_id = 10)
	plot_trend_absolute(stock_id = 11)
	plot_trend_absolute(stock_id = 12)

	layout(1)
	
dev.off()

#-------------------------------------------------------------------------------
#                 Fig_8yr_precent_trend_by_stock
#-------------------------------------------------------------------------------

plot_trend_percent = function(stock_id){
	pop = matrix(NA, nrow = 1000, ncol = 27)
	for(i in 1:1000) pop[i,] = 
		apply(akpv_datacube[[i]][
			attr(akpv_datacube[[i]], 'stockid') == stock_id,], 2, sum)
  maxi = 27
  trendlen = 8
  propTrendMat = NULL
  for(i in 1:(maxi - trendlen + 1))
    propTrendMat = cbind(propTrendMat,
      100*(exp(apply(pop,1,function(v){coef(lm(I(log(y))~x, 
        data.frame(x=1:8,y = v[i:(i + trendlen - 1)])))[2]}))-1))
  bot = apply(propTrendMat,2,quantile, prob = .025)
  top = apply(propTrendMat,2,quantile, prob = .975)
  par(mar = c(5,5,5,1))
    plot(c(1,length(top)), c(min(bot),max(top)), type = 'n',
      xaxt = 'n', ylab = 'Trend (%/Year)', cex.main = 1.5,
      xlab = 'Year', cex.lab = 2, cex.axis = 1.5,
			main = paste0('Stock ', stock_id, ': ', 
				stockid_names[stock_id,'stocknames']))
    axis(1, at = c(1,5,10,15,20), labels = c(2003, 2007, 2012, 2017, 2022),
     cex.axis = 1.5)
    lines(c(1,22),c(0,0), lty = 2, lwd = 3, col ='red')
    points(apply(propTrendMat,2,mean), pch = 19, cex = 2)
    for(i in 1:length(top))
      lines(c(i,i),c(bot[i],top[i]), lty = 1, lwd = 2)
}

pdf(paste0(figpath, 'Fig_8yr_precent_trend_by_stock.pdf'), 
	width = 13, height = 17)

	layout(matrix(1:12, nrow = 4, ncol = 3, byrow = TRUE))

	plot_trend_percent(stock_id = 1)
	plot_trend_percent(stock_id = 2)
	plot_trend_percent(stock_id = 3)
	plot_trend_percent(stock_id = 4)
	plot_trend_percent(stock_id = 5)
	plot_trend_percent(stock_id = 6)
	plot_trend_percent(stock_id = 7)
	plot_trend_percent(stock_id = 8)
	plot_trend_percent(stock_id = 9)
	plot_trend_percent(stock_id = 10)
	plot_trend_percent(stock_id = 11)
	plot_trend_percent(stock_id = 12)

	layout(1)
	
dev.off()

#-------------------------------------------------------------------------------
#                 Fig_CV_by_stock
#-------------------------------------------------------------------------------

plot_CV = function(stock_id){
	pop = matrix(NA, nrow = 1000, ncol = 27)
	for(i in 1:1000) pop[i,] = 
		apply(akpv_datacube[[i]][
			attr(akpv_datacube[[i]], 'stockid') == stock_id,], 2, sum)
  CV = sqrt(apply(pop,2,var))/apply(pop,2,mean)
  plot(1:length(CV), CV, 
      xaxt = 'n', ylab = 'CV', cex.main = 1.5,
      xlab = 'Year', cex.lab = 2, cex.axis = 1.5, type = 'l', lwd = 3,
			main = paste0('Stock ', stock_id, ': ', 
				stockid_names[stock_id,'stocknames']))
  axis(1, at = c(5,10,15,20, 25), labels = c(2000, 2005, 2010, 2015, 2020),
   cex.axis = 1.5)
}

pdf(paste0(figpath, 'Fig_CV_by_stock.pdf'), 
	width = 13, height = 17)

	layout(matrix(1:12, nrow = 4, ncol = 3, byrow = TRUE))

	plot_CV(stock_id = 1)
	plot_CV(stock_id = 2)
	plot_CV(stock_id = 3)
	plot_CV(stock_id = 4)
	plot_CV(stock_id = 5)
	plot_CV(stock_id = 6)
	plot_CV(stock_id = 7)
	plot_CV(stock_id = 8)
	plot_CV(stock_id = 9)
	plot_CV(stock_id = 10)
	plot_CV(stock_id = 11)
	plot_CV(stock_id = 12)

	layout(1)
	
dev.off()

#-------------------------------------------------------------------------------
#                 Fig_statewide
#-------------------------------------------------------------------------------

pdf(paste0(figpath, 'Fig_statewide.pdf'), width = 11, height = 8.5)

	layout(matrix(1:4, nrow = 2, ncol = 2, byrow = TRUE))

# State-wide Abundance
	pop = matrix(NA, nrow = 1000, ncol = 27)
	for(i in 1:1000) pop[i,] = 
		apply(akpv_datacube[[i]], 2, sum)
	bot = apply(pop, 2, quantile, prob = .025)
	top = apply(pop, 2, quantile, prob = .975)
	par(mar = c(5,5,5,1))
	plot(c(1,27), c(min(bot),max(top)), type = 'n',
		xaxt = 'n', ylab = 'Estimated Abundance', cex.main = 1.5,
		xlab = 'Year', cex.lab = 2, cex.axis = 1.5,
		main = 'State-wide Abundance')
	axis(1, at = c(5, 10, 15, 20, 25), labels = c(2000, 2005, 2010, 2015, 2020),
		cex.axis = 1.5)
	points(apply(pop,2,mean), pch = 19, cex = 2)
	for(i in 1:27)
		lines(c(i,i),c(bot[i],top[i]), lty = 1, lwd = 2)

# State-wide CV
  CV = sqrt(apply(pop,2,var))/apply(pop,2,mean)
  plot(1:length(CV), CV, 
      xaxt = 'n', ylab = 'CV', cex.main = 1.5,
      xlab = 'Year', cex.lab = 2, cex.axis = 1.5,
      main = 'CV by Year', type = 'l', lwd = 3)
  axis(1, at = c(5,10,15,20, 25), labels = c(2000, 2005, 2010, 2015, 2020),
   cex.axis = 1.5)

# State-wide Trailing 8-year Trend
 
  maxi = 27
  trendlen = 8
  linTrendMat = NULL
  for(i in 1:(maxi - trendlen + 1))
    linTrendMat = cbind(linTrendMat,
      apply(pop,1,function(v){coef(lm(y~x, 
        data.frame(x=1:8,y = v[i:(i + trendlen - 1)])))[2]}))
  bot = apply(linTrendMat,2,quantile, prob = .025)
  top = apply(linTrendMat,2,quantile, prob = .975)
  par(mar = c(5,5,5,1))
    plot(c(1,length(top)), c(min(bot),max(top)), type = 'n',
      xaxt = 'n', ylab = 'Trend (Seals/Year)', cex.main = 1.5,
      xlab = 'Year', cex.lab = 2, cex.axis = 1.5,
      main = paste0('Trailing ', trendlen, '-Year Trend by Year'))
    axis(1, at = c(1,5,10,15,20), labels = c(2003, 2007, 2012, 2017, 2022),
     cex.axis = 1.5)
    lines(c(1,22),c(0,0), lty = 2, lwd = 3, col ='red')
    points(apply(linTrendMat,2,mean), pch = 19, cex = 2)
    for(i in 1:length(top))
      lines(c(i,i),c(bot[i],top[i]), lty = 1, lwd = 2)

# moving trailing 8-yr trend estimates multiplicative        
  maxi = 27
  trendlen = 8
  propTrendMat = NULL
  for(i in 1:(maxi - trendlen + 1))
    propTrendMat = cbind(propTrendMat,
      100*(exp(apply(pop,1,function(v){coef(lm(I(log(y))~x, 
        data.frame(x=1:8,y = v[i:(i + trendlen - 1)])))[2]}))-1))
  bot = apply(propTrendMat,2,quantile, prob = .025)
  top = apply(propTrendMat,2,quantile, prob = .975)
  par(mar = c(5,5,5,1))
    plot(c(1,length(top)), c(min(bot),max(top)), type = 'n',
      xaxt = 'n', ylab = 'Trend (%/Year)', cex.main = 1.5,
      xlab = 'Year', cex.lab = 2, cex.axis = 1.5,
      main = paste0('Trailing ', trendlen, '-Year Trend by Year'))
    axis(1, at = c(1,5,10,15,20), labels = c(2003, 2007, 2012, 2017, 2022),
     cex.axis = 1.5)
    lines(c(1,22),c(0,0), lty = 2, lwd = 3, col ='red')
    points(apply(propTrendMat,2,mean), pch = 19, cex = 2)
    for(i in 1:length(top))
      lines(c(i,i),c(bot[i],top[i]), lty = 1, lwd = 2)

	layout(1)

dev.off()
