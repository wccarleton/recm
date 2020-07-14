library(stringr)
oxcal_out <- readLines("~/OxCal/hirec_exp_pos.js")
postprob_lines <- grep("ocd\\[[0-9]*\\].posterior.prob=",oxcal_out)
start_dates <- grep("ocd\\[[0-9]*\\].posterior.start=",oxcal_out)
post_prob_dates <- postprob_lines[5:length(postprob_lines)]
post_prob_kde <- postprob_lines[1]
post_prob_kde_prNorm <- grep("ocd\\[1\\].likelihood.probNorm=",oxcal_out)

#c14startdates <- start_dates[5:length(start_dates)]
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
