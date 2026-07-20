# load the datacube
load("C://smk/akpv_datacube.rda")

# check out the structure of the datacube
class(akpv_datacube) # it is a list
str(akpv_datacube[[1]]) # the first element is a 194 x 27 matrix
length(akpv_datacube)  # there are 1000 list items, each element of which is 
#  a 194 x 27 matrix
attributes(akpv_datacube[[1]]) # notice the attributes of each matrix
rownames(akpv_datacube[[1]]) # the polyid's are the row names
colnames(akpv_datacube[[1]]) # the years are the column names
attr(akpv_datacube[[1]], 'stockid') # there is an extra attribute for 
#  stockid, which is equal to length as rownames, and in the same order

# the datacube is composed of 1000 matrices, where each matrix is an MCMC sample
# if we want to see the MCMC samples for the first year for the first site
unlist(lapply(akpv_datacube,function(x){x[1,1]}))

# we can also use names rather than numbers.  Here is polyid "OA00" for 1997
unlist(lapply(akpv_datacube,function(x){x["OA00","1997"]}))
#From these 1000 samples, you can make an inference on them such as means, standard deviations, quantiles, etc., per site per year

# find all polyids for a given stock in any matrix using stockid attribute
attr(akpv_datacube[[1]], 'stockid') == 6
# make a subset of those for the first MCMC sample in year 1996
akpv_datacube[[1]][attr(akpv_datacube[[1]], 'stockid') == 6, "1996"]
# sum those to get the first MCMC sample for total abundance of stock 6 in 1996
sum(akpv_datacube[[1]][attr(akpv_datacube[[1]], 'stockid') == 6, "1996"])
# get the sum for the 2nd MCMC sample 
i = 2 # MCMC sample
sum(akpv_datacube[[i]][attr(akpv_datacube[[i]], 'stockid') == 6, "1996"])


#Sites can also be grouped in any way that you want, then compute something on that group. and then make your inference.  
#For example, suppose that we want the mean of the total for sites 1 and 2 in year 1.  First create a 2 column matrix, one column for each site,
temp = cbind(unlist(lapply(DataCube_Aleutians,function(x){x[1,1]})),
             unlist(lapply(DataCube_Aleutians,function(x){x[2,1]})) )
#then 
temp1 = apply(temp,1,sum)
#will sum the two sites together for each MCMC sample, so finally you can make your inference (mean, standard deviation, quantiles) from temp1

as.data.table(x, keep.rownames = TRUE)[, lapply(.SD, sum), by = rn]
rowsum(df, row.names(x))
aggregate(. ~ year, data=akpv_datacube, FUN=mean)


abundance_base <- akpv_datacube %>% 
  data.frame() %>%
  bind_rows(.id = "location") %>%
  rownames_to_column() %>%
  rename(polyid = rowname) %>%
  select(-location) %>%
  pivot_longer(
    cols = starts_with("X"), 
    names_to = "year",
    values_to = "abund_est") %>%
  mutate(year = as.integer(substring(year, 2, 5)))
  
  
  
  
  #unnest() %>%
  group_by(dimnames[[1]], dimnames[[2]]) %>% 
  filter(n()>1)%>%
  summarize(y=list(combn(plot,2, paste, collapse="_"))) %>% 
  unnest %>%
  group_by(region,y) %>% 
  summarize(freq=n())