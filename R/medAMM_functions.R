

causal.calc = function(datas, confounders, outcome, mainpred, mediators, survey=FALSE, survey.diagnose=FALSE) {
	
#Fit a logistic regression model predicting black (1) or white (0) from age and sex (our only confounders). 
	if(survey){
	options(survey.lonely.psu="adjust")
	model_check = suppressWarnings(svyglm(as.formula(paste(outcome, "~", mainpred, "+",  paste(confounders,collapse="+"))), family = "binomial", design=datas))
	if(model_check$coef[2] < 0) {
		print("Warning: Your main predictor is such that group 1 has a lower prevalence of outcome. This code is intended for a setting where group 1 has a higher prevalence. Weird things may happen. This package FLIPS it, please interpret accordingly. ")
		datas$variables$newpred = 1-datas$variables[,names(datas$variables) == mainpred]
		mainpred = "newpred"
	}
	model1 = suppressWarnings(svyglm(as.formula(paste(mainpred, "~",paste(confounders,collapse="+"))), family = "binomial", design=datas))
	#For each person, calculate the probability of white given their confounders. This is the same as asking Stata to use that regression model to give you predicted probabilities for each person given their age and race. Stata will give you the P(outcome = 1) which is black, but we want white. So take these predicted probabilities it gives you, say Pi, save these Pi’s somewhere. Now calculate 1-Pi. This is what you want. Let’s call this Wi because I have it for each person i.
	pi =  predict(model1, type = "response")
	wi =  1- pi
	#Calculate the P(white) in this subgroup – this is just the % of white in this black+white subset. Call this K.
	K = 1- svymean(as.formula(paste("~",mainpred)), design = datas)[1]
	# For each person calculate K/Wi. This is a weight.
	my.weight = K/wi
	if(survey.diagnose) print(summary(my.weight))
	#Take all the white kids and calculate a straight-up weighted average of disease prevalence in Stata where you give it all the survey things, BUT the survey weights you give it should be the original survey weights multiplied by these weights you just made, K/Wi. Call this weighted average A.
	new.data = datas
	new.data$variables$my.weight =my.weight
	new.data$variables$my.weight.surv = new.data$variables$my.weight*new.data$variables$new_weight
	data.surv.new <- s(id=~newpsu,strata=~newstrat, weights=~my.weight.surv, nest=TRUE, data=new.data$variables)
	data.surv.new$variables$subwant = data.surv.new$variables[,names(data.surv.new$variables) == mainpred]
	data.surv.white=subset(data.surv.new, subwant==0)
	A = svymean(as.formula(paste("~",outcome)), design = data.surv.white, na.rm=T)[1]
	# Now we will do the same for black. Take your P’s that you calculated in 2. Now calculate the P(black) in this subgroup. Call this K*.
	Kstar = svymean(as.formula(paste("~",mainpred)), design = datas)[1]
	#For each person, calculate K*/Pi. This is a weight. 
	my.weight.star = Kstar/pi
	if(survey.diagnose) print(summary(my.weight.star))
	#Take all the black kids and calculate a straight-up weighted average of disease prevalence in Stata where you give it all the survey things, BUT the survey weights you give it should be the original survey weights multiplied by these weights you just made, K*/Pi. Call this weighted average B.
	new.data = datas
	new.data$variables$my.weight =my.weight.star
	new.data$variables$my.weight.surv = new.data$variables$my.weight*new.data$variables$new_weight
	data.surv.new <- svydesign(ids=~newpsu,strata=~newstrat, weights=~my.weight.surv, nest=TRUE, data=new.data$variables)
	data.surv.new$variables$subwant = data.surv.new$variables[,names(data.surv.new$variables) == mainpred]
	data.surv.race=subset(data.surv.new, subwant==1)
	B = svymean(as.formula(paste("~",outcome)), design = data.surv.race, na.rm=T)[1]
	#Now fit a logistic regression model predicting disease prevalence from black/white, age, sex, and your mediator (it can be 1 mediator or all of them, doesn’t matter).
	model = suppressWarnings(svyglm(as.formula(paste(outcome, "~", mainpred, "+", paste(mediators,collapse="+"), "+", paste(confounders,collapse="+"))), family = "binomial", design=datas))
	#Take all the white kids and calculate their predicted probability of disease P(disease=1) given their age, sex, their true mediator value, BUT SET THEIR RACE TO BLACK. This is predicting the expected disease prevalence had they been black. Let’s call these predicted values Yi.

data.surv.white$variables[,names(data.surv.new$variables) == mainpred] = rep(1, length(data.surv.white$variables[,names(data.surv.new$variables) == mainpred]))
	yi = predict(model, newdata = data.surv.white$variables, type = "response")
	data.surv.white$variables$yi = yi
	#Take all the white kids and calculate a straight-up weighted average of Yi in Stata where you give it all the survey things, BUT the survey weights you give it should be the original survey weights multiplied by the first weights you made, K/Wi. Call this weighted average C.
	C = svymean(yi, design = data.surv.white)[1]
	#The natural direct effect is C-A. The natural indirect effect is B-C. The total effect is B-A. The proportion explained by this mediator (or mediators) is (B-C)/(B-A).
	direct.effect = C-A
	indirect.effect = B-C
	total = direct.effect+indirect.effect
	prop.mediated = indirect.effect/total
	return(list("direct.effect" = as.numeric(direct.effect), "indirect.effect" = as.numeric(indirect.effect), "total" = as.numeric(total), "prop.mediated" = as.numeric(prop.mediated)))
	}
	else {
		model_check = suppressWarnings(glm(as.formula(paste(outcome, "~", mainpred, "+",  paste(confounders,collapse="+"))), family = "binomial", data=datas))
	if(model_check$coef[2] < 0) {
		print("Warning: Your main predictor is such that group 1 has a lower prevalence of outcome. This code is intended for a setting where group 1 has a higher prevalence. Weird things may happen. This package FLIPS it, please interpret accordingly. ")
		datas$newpred = 1-datas[,names(datas) == mainpred]
		mainpred = "newpred"
	}
		model1 = suppressWarnings(glm(as.formula(paste(mainpred, "~",paste(confounders,collapse="+"))), family = "binomial", data=datas))
	pi =  predict(model1, type = "response")
	wi =  1- pi
	K = 1- mean(datas[,names(datas) == mainpred])
	my.weight = K/wi
	if(survey.diagnose) print(summary(my.weight))
	data.surv.new = cbind(datas,my.weight)
	data.surv.new = cbind(data.surv.new[,names(data.surv.new) == mainpred], data.surv.new[,names(data.surv.new) == outcome],data.surv.new$my.weight)
	A = weighted.mean(x=data.surv.new[data.surv.new[,1] ==0, 2], w = data.surv.new[data.surv.new[,1] ==0, 3]) 
	Kstar = mean(datas[,names(datas) == mainpred])
	my.weight.star = Kstar/pi
	if(survey.diagnose) print(summary(my.weight.star))
	data.surv.new = cbind(datas,my.weight.star)
	data.surv.new = cbind(data.surv.new[,names(data.surv.new) == mainpred], data.surv.new[,names(data.surv.new) == outcome],data.surv.new$my.weight.star)
	B = weighted.mean(x=data.surv.new[data.surv.new[,1] ==1, 2], w = data.surv.new[data.surv.new[,1] ==1, 3]) 
	model = suppressWarnings(glm(as.formula(paste(outcome, "~", mainpred, "+", paste(mediators,collapse="+"), "+", paste(confounders,collapse="+"))), family = "binomial", data=datas))
	data.surv.new = datas[datas[,names(datas) == mainpred]==0,]

	data.surv.new[,names(data.surv.new) == mainpred] = rep(1, length(data.surv.new[,names(data.surv.new) == mainpred]))
	yi = predict(model, newdata = data.surv.new, type = "response")
	C = mean(yi)

	direct.effect = C-A
	indirect.effect = B-C
	total = direct.effect+indirect.effect
	prop.mediated = indirect.effect/total
	return(list("direct.effect" = as.numeric(direct.effect), "indirect.effect" = as.numeric(indirect.effect), "total" = as.numeric(total), "prop.mediated" = as.numeric(prop.mediated)))
	}
	
}


make.row.causal = function(datas, confounders, outcome, mainpred, mediators, survey=FALSE, values = FALSE, conf=TRUE,survey.diagnose=FALSE) {
	main = unlist(causal.calc(datas=datas, confounders = confounders, outcome=outcome, mainpred=mainpred, mediators=mediators, survey=survey, survey.diagnose = survey.diagnose))
	
	#remember I only use full data when pulling in original weights
	#does bootstrap, sets seed
	if(conf)	{
	set.seed(100)
	boot.num=300
	hold.per = matrix(nrow=boot.num, ncol = 4)
	for(i in 1:boot.num) {
		ss=sample(1:dim(datas)[1],dim(datas)[1], replace=TRUE)
		sub = datas[ss,]
		hold.per[i,] = as.vector(unlist(causal.calc(datas=sub, confounders = confounders, outcome=outcome, mainpred=mainpred, mediators=mediators,survey=survey,survey.diagnose= survey.diagnose))	)	
	}
	rr = c(round(main[1:3], 3), paste(100*round(main[4], 2), "% (", 100*round(quantile(hold.per[,4],0.025),2),"%, ", 100*round(quantile(hold.per[,4],0.975),2),"%)", sep=""))
	}
	if(!conf) {
		rr = c(round(main[1:3], 3), paste(100*round(main[4], 2), "%",sep=""))
	}
	if(values & conf){
		return(list("rr" = rr, "values" = c(100*round(main[4],2),100*round(quantile(hold.per[,4],0.025),2),100*round(quantile(hold.per[,4],0.975),2))))
	}
	if(!values | !conf) {return(rr)}
}



causal.calc.all = function(datas, confounders, outcome, mainpred, mediator.names, survey=FALSE, survey.diagnose=FALSE, conf=TRUE, plot=TRUE, plot.labels = NULL, plot.order=NULL) {
	new.tab=c()
	val.tab=c()
	for(j in 1:length(mediator.names)) {
		if(plot){
			row.want = make.row.causal(datas=datas, outcome = outcome, mainpred = mainpred, confounders = confounders, mediators = mediator.names[j],survey=survey, survey.diagnose=survey.diagnose, values = TRUE, conf=TRUE)
			new.tab = rbind(new.tab,row.want$rr)
			val.tab = rbind(val.tab, row.want$values)}
		if(!plot){
			row.want = make.row.causal(datas=datas, outcome = outcome, mainpred = mainpred, confounders = confounders, mediators = mediator.names[j],survey=survey, survey.diagnose=survey.diagnose, values = FALSE, conf=conf)
			new.tab = rbind(new.tab,row.want)}
	print(paste("Done with ", mediator.names[j]))
	}
	if(plot){
		row.all = make.row.causal(datas=datas, outcome = outcome, mainpred = mainpred, confounders = confounders, mediators = mediator.names,survey=survey, survey.diagnose=survey.diagnose, values = TRUE, conf=TRUE)
		val.tab = rbind(val.tab, row.all$values)
		new.tab = rbind(new.tab,row.all$rr)
		num.plot = dim(val.tab)[1]
		if(is.null(plot.labels)) {plot.labels = mediator.names}
		plot.frame <- data.frame(
  x = letters[1:num.plot],
  y   = val.tab[,1],
  yhi = val.tab[,3],
  ylo = val.tab[,2], 
  lab = c(plot.labels,"ALL MEDIATORS TOGETHER"),
  grouping = factor(c(1:(num.plot))))
		if(!is.null(plot.order)){plot.frame$x <- reorder(plot.frame$x, plot.order)}
		a <- ggplot(plot.frame, aes(x=x, y=y, ymin=ylo, ymax=yhi, colour=grouping)) +  geom_pointrange(shape=16, size=0.5, position=position_dodge(width=c(0.1))) + coord_flip() + geom_hline(yintercept=0, lty=2) + xlab('Variable') + scale_x_discrete(breaks=NULL) + scale_y_continuous(limits = c(min(val.tab[,2]),max(val.tab[,3])+50)) + ylab("Proportion Mediated, 95% CI") + xlab("") + ggtitle("Mediation Summary") + 	theme(legend.position="none") + geom_text(data=plot.frame,  size=3,aes(x = x, y = max(val.tab[,3]) +1, label = lab, hjust=0, 	fontface="italic"))
		print(a) 
		}
	if(!plot){
		row.all = make.row.causal(datas=datas, outcome = outcome, mainpred = mainpred, confounders = confounders, mediators = mediator.names,survey=survey, survey.diagnose=survey.diagnose, values = FALSE, conf=conf)
	new.tab = rbind(new.tab,row.all)	}
	row.names(new.tab) = c(mediator.names, "All mediators")
	return(list("results" = new.tab, "values" = val.tab))
}

causal.calc.plot.only = function(values, mediator.names, plot.labels = NULL, plot.order=NULL) {
		val.tab=values
		num.plot = dim(val.tab)[1]
		if(is.null(plot.labels)) {plot.labels = mediator.names}
		plot.frame <- data.frame(
  x = letters[1:num.plot],
  y   = val.tab[,1],
  yhi = val.tab[,3],
  ylo = val.tab[,2], 
  lab = c(plot.labels,"ALL MEDIATORS TOGETHER"),
  grouping = factor(c(1:(num.plot))))
		if(!is.null(plot.order)){plot.frame$x <- reorder(plot.frame$x, plot.order)}
		a <- ggplot(plot.frame, aes(x=x, y=y, ymin=ylo, ymax=yhi, colour=grouping)) +  geom_pointrange(shape=16, size=0.5, position=position_dodge(width=c(0.1))) + coord_flip() + geom_hline(yintercept=0, lty=2) + xlab('Variable') + scale_x_discrete(breaks=NULL) + scale_y_continuous(limits = c(min(val.tab[,2]),max(val.tab[,3])+50)) + ylab("Proportion Mediated, 95% CI") + xlab("") + ggtitle("Mediation Summary") + 	theme(legend.position="none") + geom_text(data=plot.frame,  size=3,aes(x = x, y = max(val.tab[,3]) +1, label = lab, hjust=0, 	fontface="italic"))
		print(a) 
}


