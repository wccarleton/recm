library(stringr)
oxcal_out <- readLines("../Data/OxCal/rects_rdates_1000.js")
postprob_lines <- grep("ocd\\[[0-9]*\\].likelihood.prob=",oxcal_out)
start_dates <- grep("ocd\\[[0-9]*\\].likelihood.start=",oxcal_out)
post_prob_dates <- postprob_lines[5:length(postprob_lines)]
post_prob_kde <- postprob_lines[1]
post_prob_kde_prNorm <- grep("ocd\\[1\\].likelihood.probNorm=",oxcal_out)

c14startdates <- start_dates[5:length(start_dates)]
kdestartdate <- start_dates[1]

oxcalParsePosts <- function(x){
   c14post <- str_extract_all(x, "\\=\\[.*]")
   c14post <- sub("\\=\\[","c\\(",c14post)
   c14post <- sub("\\]","\\)",c14post)
   c14post <- eval(parse(text=c14post))
   return(c14post)
}

kdepost <- oxcalParsePosts(oxcal_out[post_prob_kde])
kdepost_prNorm <- as.numeric(str_extract_all(oxcal_out[post_prob_kde_prNorm],"[+-]?([0-9]*[.])?[0-9]+")[[1]][2])

c14post <- lapply(oxcal_out[post_prob_dates],oxcalParsePosts)

#n dates
ndates <- length(c14post)

#get start dates
c14_sdates <- lapply(str_extract_all(oxcal_out[c14startdates],"[-][:alnum:]*|[:alnum:]*"),function(x){as.numeric(x[[10]])})

kde_sdate <- unlist(lapply(str_extract_all(oxcal_out[kdestartdate],"[-][:alnum:]*|[:alnum:]*"),function(x){as.numeric(x[[10]])}))

#sum_sdate <- lapply(str_extract_all(oxcal_out[sumstartdate],"[-][:alnum:]*|[:alnum:]*"),function(x){as.numeric(x[[10]])})


#build c14 posterior dataframes
t_seq_length <- lapply(c14post,length)

t_seq <- lapply(1:ndates,function(x,t_seq_length,sdates){c(seq(sdates[[x]],sdates[[x]] + t_seq_length[[x]] - 1))},t_seq_length=t_seq_length,sdates=c14_sdates)

c14post <- lapply(1:ndates,function(x,p,ts){cbind(ts[[x]],p[[x]])},p=c14post,ts=t_seq)

#build sum dataframe

#sumpost <- data.frame(Density=sumpost,
#                     Date=seq(unlist(sum_sdate),length.out=length(sumpost)) )

#build kde dataframe

kdepost <- data.frame(Density=kdepost,
                     Date=seq(kde_sdate,length.out=length(kdepost),by=10) )

###Curve
curve_lines <- grep("calib\\[0\\].bp=\\[",oxcal_out)
curve_start <- grep("calib\\[0\\].start=",oxcal_out)
curve_sigma <- grep("calib\\[0\\].sigma=\\[",oxcal_out)

curve_fun_bp <- oxcalParsePosts(oxcal_out[curve_lines][2])
curve_fun_bp_sig <- oxcalParsePosts(oxcal_out[curve_sigma][2])
curve_fun_bcad_start <- as.numeric(str_extract_all(oxcal_out[curve_start[1]], "[+-]?([0-9]*[.])?[0-9]+")[[1]][2])
cal_curve_df <- data.frame(RCBP=curve_fun_bp,
                           RCBP_l=curve_fun_bp - curve_fun_bp_sig,
                           RCBP_u=curve_fun_bp + curve_fun_bp_sig,
                           BCAD=seq(curve_fun_bcad_start,1951,10))

###KDE_s
#oxcal_out_kde <- readLines("~/OxCal/unif_2_caldates_kde.js")
kde_ens_lines <- grep("kde_ensembles.push",oxcal_out)

oxcalParseKDE <- function(x){
   kde_ens <- str_extract_all(x, "\\:\\[.*]")
   kde_ens <- sub("\\:\\[","c\\(",kde_ens)
   kde_ens <- sub("\\]","\\)",kde_ens)
   kde_ens <- eval(parse(text=kde_ens))
   return(kde_ens)
}

kde_ensembles <- lapply(oxcal_out[kde_ens_lines],oxcalParseKDE)

kde_sdates <- str_extract_all(oxcal_out[kde_ens_lines], "start\\:[+-]?([0-9]*[.])?[0-9]+")
kde_sdates <- as.numeric(str_extract_all(kde_sdates,"[+-]?([0-9]*[.])?[0-9]+"))

oxcalParseKDEPr <- function(x){
   kde_pr <- str_extract_all(x, "probNorm:\\d*[\\.]\\d{1,10}")
   kde_pr <- str_extract_all(kde_pr, "\\d*[\\.]\\d{1,10}")
   kde_pr <- eval(parse(text=kde_pr))
   return(kde_pr)
}
kde_prNorms <- unlist(lapply(oxcal_out[kde_ens_lines],oxcalParseKDEPr))



nkdes <- length(kde_sdates)
t_seq_length <- lapply(kde_ensembles,length)
t_seq <- lapply(1:nkdes,function(x,t_seq_length,kde_sdates){c(seq(kde_sdates[[x]],kde_sdates[[x]] + (t_seq_length[[x]] - 1)*10,10))},t_seq_length=t_seq_length,kde_sdates=kde_sdates)

kde_ensembles <- lapply(1:nkdes,function(x,p,tseq){cbind(tseq[[x]],p[[x]])},p=kde_ensembles,tseq=t_seq)
