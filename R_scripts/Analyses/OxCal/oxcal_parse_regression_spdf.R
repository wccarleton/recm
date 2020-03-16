library(stringr)
oxcal_out <- readLines("~/OxCal/rects_rdates_1000_Kennett_neg.js")
postprob_lines <- grep("ocd\\[[0-9]*\\].likelihood.prob=",oxcal_out)
start_dates <- grep("ocd\\[[0-9]*\\].likelihood.start=",oxcal_out)
#post_prob_dates <- postprob_lines[1:length(postprob_lines)]
resolution <- 1

c14startdates <- start_dates[1:length(start_dates)]

oxcalParsePosts <- function(x){
   c14post <- str_extract_all(x, "\\=\\[.*]")
   c14post <- sub("\\=\\[","c\\(",c14post)
   c14post <- sub("\\]","\\)",c14post)
   c14post <- eval(parse(text=c14post))
   return(c14post)
}

c14post <- lapply(oxcal_out[postprob_lines],oxcalParsePosts)

#n dates
ndates <- length(c14post)

#get start dates
c14_sdates <- lapply(str_extract_all(oxcal_out[c14startdates],"[-][:alnum:]*|[:alnum:]*"),function(x){as.numeric(x[[10]])})


#build c14 posterior dataframes
t_seq_length <- lapply(c14post,length)

t_seq <- lapply(1:ndates,function(x,t_seq_length,sdates){seq(sdates[[x]],sdates[[x]] + (t_seq_length[[x]] - 1)*resolution,by=resolution)} ,t_seq_length=t_seq_length,sdates=c14_sdates)

c14post <- lapply(1:ndates,function(x,p,ts){cbind(ts[[x]],p[[x]])},p=c14post,ts=t_seq)
