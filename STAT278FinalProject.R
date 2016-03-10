# Kevin O'Connor
# STAT27850


# Calculating probability of false classification X_i with l labels and true class probability p_i based on naive selection rule.
probcalc <- function(k, l, p_i){
	return((-1)^(k+1)*choose(l-1,k)*(1-p_i)^k)
}
return(sum(probcalc(1:9, 10, 0.9)) #0.6125795
return(sum(probcalc(1:9, 10, 0.99))) #0.08648275



# Defining selection rules.
# Naive selection simply returns the class which corresponds to the maximum element of the vector of estimated pvalues.  
naiveselect <- function(pvec){
	return(which(pvec==max(pvec)))
}
# Selection Rule 1 returns the class corresponding to the maximum element of the vector of estimated pvalues as long as it is greater than p_o.  
select1 <- function(pvec, p_o){
	if (max(pvec) > p_o){
		return(which(pvec==max(pvec)))
	}
	else {
		return(0) #null classification
	}
}
# Selection Rule 2 returns the class corresponding to the maximum element of the vector of estimated pvalues as long as it is at least d_o greater than the next largest element.
select2 <- function(pvec, d_o){
	max = max(pvec)
	maxind = which(pvec==max(pvec))[1]
	pvec[maxind] = 0
	if (max - max(pvec) > d_o){
		return(maxind)
	}
	else {
		return(0)
	}
}
# Selection Rule 3 returns the class corresponding to the maximum element of the vector of estimated pvalues as long as all other pvalues are no greater than epsilon.
select3 <- function(pvec, e){
	maxind = which(pvec==max(pvec))[1]
	pvec[maxind] = 0
	if (max(pvec) < e){
		return(maxind)
	}
	else {
		return(0)
	}
}



# Testing selection rules using artificial data.
m = 10; sdev = sqrt(5); mu_range = c(-100, 100)
mu = runif(m, min=mu_range[1], max=mu_range[2]) # generate class means
data = c(); labels = c()
for (i in 1:10){
	data = c(data, rnorm(100, mean=mu[i], sd=sdev)) # generate data from class i
	labels = c(labels, rep(i, 100)) # construct vector of true labels
}

# Function for generating a vector of estimated p-values.
gen_pvec <- function(val, means){
	return(2*(1 - pnorm(abs(rep(val, length(means)) - means)/sdev)))
}

# Predicting labels for the generated data.
p_o = 0.8; d_o = 0.2; e = 0.1
pred_labels_naive = c(); false_class_naive = 0
pred_labels_1 = c(); false_class_1 = 0
pred_labels_2 = c(); false_class_2 = 0
pred_labels_3 = c(); false_class_3 = 0
for (i in 1:length(data)){
	pvec = gen_pvec(data[i], mu)
	pred_labels_naive = c(pred_labels_naive, naiveselect(pvec))
	pred_labels_1 = c(pred_labels_1, select1(pvec, p_o))
	pred_labels_2 = c(pred_labels_2, select2(pvec, d_o))
	pred_labels_3 = c(pred_labels_3, select3(pvec, e))
	# Counting false classifications
	if (pred_labels_naive[i] != labels[i]){false_class_naive = false_class_naive + 1}
	if (pred_labels_1[i] != labels[i] && pred_labels_1[i] != 0){false_class_1 = false_class_1 + 1}
	if (pred_labels_2[i] != labels[i] && pred_labels_2[i] != 0){false_class_2 = false_class_2 + 1}
	if (pred_labels_3[i] != labels[i] && pred_labels_3[i] != 0){false_class_3 = false_class_3 + 1}
}

# Printing stats.
print("Naive selection:")
cat("Classifications: ", length(pred_labels_naive[pred_labels_naive != 0]))
cat("Null Classifications: ", length(pred_labels_naive[pred_labels_naive == 0]))
cat("False Classifications: ", false_class_naive)
cat("FCR: ", false_class_naive/length(pred_labels_naive[pred_labels_naive != 0]))
print(""); print("")
print("Selection Rule 1:")
cat("Classifications: ", length(pred_labels_1[pred_labels_1 != 0]))
cat("Null Classifications: ", length(pred_labels_1[pred_labels_1 == 0]))
cat("False Classifications: ", false_class_1)
cat("FCR: ", false_class_1/length(pred_labels_1[pred_labels_1 != 0]))
print(""); print("")
print("Selection Rule 2:")
cat("Classifications: ", length(pred_labels_2[pred_labels_2 != 0]))
cat("Null Classifications: ", length(pred_labels_2[pred_labels_2 == 0]))
cat("False Classifications: ", false_class_2)
cat("FCR: ", false_class_2/length(pred_labels_2[pred_labels_2 != 0]))
print(""); print("")
print("Selection Rule 3:")
cat("Classifications: ", length(pred_labels_3[pred_labels_3 != 0]))
cat("Null Classifications: ", length(pred_labels_3[pred_labels_3 == 0]))
cat("False Classifications: ", false_class_3)
cat("FCR: ", false_class_3/length(pred_labels_3[pred_labels_3 != 0]))



# Examining the FCR for a range of threshold values.
mu = runif(m, min=mu_range[1], max=mu_range[2]) # generate class means
data = c(); labels = c()
for (i in 1:10){
	data = c(data, rnorm(100, mean=mu[i], sd=sdev)) # generate data from class i
	labels = c(labels, rep(i, 100)) # construct vector of true labels
}

# Saving randomly generated means.
sink("STAT278FinalProjectMeans.txt")
print(mu)
sink()

# Saving data.
sink("STAT278FinalProjectData1.txt")
print(data)
sink()

p_o_vec = seq(0,1,0.01)
d_o_vec = seq(0,1,0.01)
e_vec = seq(0,1,0.01)
FCR_naive = c(); class_vec_naive = c(); false_class_vec_naive = c() 
FCR_1 = c(); class_vec_1 = c(); false_class_vec_1 = c()
FCR_2 = c(); class_vec_2 = c(); false_class_vec_2 = c()
FCR_3 = c(); class_vec_3 = c(); false_class_vec_3 = c()
for (j in 1:length(p_o_vec)){
	p_o = p_o_vec[j]
	d_o = d_o_vec[j]
	e = e_vec[j]
	pred_labels_naive = c(); false_class_naive = 0
	pred_labels_1 = c(); false_class_1 = 0
	pred_labels_2 = c(); false_class_2 = 0
	pred_labels_3 = c(); false_class_3 = 0
	for (i in 1:length(data)){
		pvec = gen_pvec(data[i], mu)
		pred_labels_naive = c(pred_labels_naive, naiveselect(pvec))
		pred_labels_1 = c(pred_labels_1, select1(pvec, p_o))
		pred_labels_2 = c(pred_labels_2, select2(pvec, d_o))
		pred_labels_3 = c(pred_labels_3, select3(pvec, e))
		# Counting false classifications
		if (pred_labels_naive[i] != labels[i]){false_class_naive = false_class_naive + 1}
		if (pred_labels_1[i] != labels[i] && pred_labels_1[i] != 0){false_class_1 = false_class_1 + 1}
		if (pred_labels_2[i] != labels[i] && pred_labels_2[i] != 0){false_class_2 = false_class_2 + 1}
		if (pred_labels_3[i] != labels[i] && pred_labels_3[i] != 0){false_class_3 = false_class_3 + 1}
	}
	FCR_naive = c(FCR_naive, false_class_naive/length(pred_labels_naive[pred_labels_naive != 0]))
	FCR_1 = c(FCR_1, false_class_1/length(pred_labels_1[pred_labels_1 != 0]))
	FCR_2 = c(FCR_2, false_class_2/length(pred_labels_2[pred_labels_2 != 0]))
	FCR_3 = c(FCR_3, false_class_3/length(pred_labels_3[pred_labels_3 != 0]))
}

# Plotting the FCR
pdf("STAT278FinalProjectPlot1.pdf", width=7, height=5)
plot(FCR_naive, main="FCR for Naive Selection", ylim=c(0,1), xlab="", ylab="FCR")
dev.off()
pdf("STAT278FinalProjectPlot2.pdf", width=7, height=5)
plot(p_o_vec, FCR_1, main="FCR for Selection Rule I", ylim=c(0,1), xlab="p_o", ylab="FCR")
abline(h=FCR_naive[1])
dev.off()
pdf("STAT278FinalProjectPlot3.pdf", width=7, height=5)
plot(FCR_2, main="FCR for Selection Rule II", ylim=c(0,1), xlab="d_o", ylab="FCR")
abline(h=FCR_naive[1])
dev.off()
pdf("STAT278FinalProjectPlot4.pdf", width=7, height=5)
plot(FCR_3, main="FCR for Selection Rule III", ylim=c(0,1), xlab="e", ylab="FCR")
abline(h=FCR_naive[1])
dev.off()








