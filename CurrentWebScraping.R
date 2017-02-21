
##Create the URLs for each playlist
library(dplyr)
library(rvest)
hours <- 0:23
days <- c(paste('0',1:9,sep=''),as.character(10:31))
months <- c(paste('0',1:9,sep=''),'10','11','12')
year <- '2016'
dategrid <- expand.grid(list(y=year,m=months,d=days,h=hours))
dategrid <- dategrid %>% mutate(exactdate = paste(y,'-',m,'-',d,'/',h,sep=''))
head(dategrid)
urls <- paste('http://www.thecurrent.org/playlist/',dategrid$exactdate,sep='')
head(urls)


##Now scrape.
##CSS tags found via selector gadget
##http://selectorgadget.com/



ta.tag <- '.title , .artist a'
timetag <- '#pjaxReplaceTarget time'

###############
#Testing out a Title Tag with a single URL
url <- urls[1]
read_html(url)%>%html_nodes(ta.tag)%>%html_text(trim=TRUE)

#Need to do some cleaning on the title/artist output to get titles
clean.one.node <- function(node,index) {
  return(node[index])
}
clean.many.nodes <- function(nodes,index) {
  split.nodes <- strsplit(nodes,'\n')
  output <- unlist(lapply(split.nodes,clean.one.node, index = index))
  output.trim <- str_trim(output, side = 'both')
  return(output.trim)
}

url <- urls[5]
output <- read_html(url)%>%html_nodes(ta.tag)%>%html_text(trim=TRUE)
clean.many.nodes(output,index=1) #Titles
clean.many.nodes(output,index=2) #Artists


##Now for the times...
read_html(url)%>%html_nodes(timetag)%>%html_text(trim=TRUE)


#########PUt it all together!

process.one.url <- function(url) {
  htmlstuff <- read_html(url)
  ta <- htmlstuff%>%html_nodes(ta.tag)%>%html_text(trim=TRUE)
  time <- htmlstuff%>%html_nodes(timetag)%>%html_text(trim=TRUE)
  titles <- clean.many.nodes(ta,index=1) #Titles
  artists <- clean.many.nodes(ta,index=2) #Titles
 return(cbind(time, titles, artists))
}

process.10.urls <- sapply(urls[1:10],process.one.url)
prettydata <- do.call(rbind,process.10.urls)
prettydata




