##Bruce Scrape
library(httr)
library(rvest)
library(htmltools)
library(magittr)
library(dplyr)
library(selectr)
library(stringr)

all<-data.frame()



##sites<-paste(url,i,sep="")



##Don't have:
##11,19,28,39,60,71,84,97,112,123,136,152,165,181,194,209,232,246,259,264,28

for (i in c(11,19,28,39,60,71,84,97,112,123,136,152,165,181,194,209,232,246,259,264,28)) {

url<-'http://www.azlyrics.com/s/brucespringsteen.html'
x <- GET(url, add_headers('user-agent' = 'r'))
  
webpath<-x$url%>%includeHTML%>%read_html()

site<-paste("///div/div[3]/a","[")  
site<-paste(site,19,sep="")
site<-paste(site,"]",sep="")
name<-webpath%>%html_nodes(xpath=site)
name1<-sub(".*brucespringsteen","", name)
name2<-gsub( ".html.*$", "", name1)
name2<-gsub("</a>.*$","",name2)%>%as.character()%>%trimws()
name3<-paste(name2,".html",sep="")
name3<-as.character(name3)
url1<-paste('http://www.azlyrics.com/lyrics/brucespringsteen',name3,sep="")

closeAllConnections()

x1 <- GET(url1, add_headers('user-agent' = 'r'))
webpath1<-x1$url%>%includeHTML%>%read_html()

lyrics<-webpath1%>%html_nodes(xpath="/html/body/div[3]/div/div[2]/div[5]")%>%html_text()
lyrics1<-gsub("[\r\n]", " ", lyrics)
lyrics1<-gsub("[\r]", "",lyrics1)

lyrics2<-sub(" \ " ,"",lyrics1)

lfile<-paste("file",19,".txt",sep="")

write(lyrics2,lfile)

closeAllConnections()

}

##"C:/Users/JustinL/Dropbox/Datava/BruceBot/file5.txt"


//*[@id="listAlbum"]/a[11]