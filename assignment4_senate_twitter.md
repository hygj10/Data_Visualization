---
title: "Assignment 3: U.S. Senators on Twitter"
author: Thomas Brambor
date: 2017-04-04
always_allow_html: yes
output: 
  html_document:
    keep_md: true
---

Network Analysis of U.S. Senate Tweets
================================



## Overview

Twitter is a great tool to analyze the public interactions of political actors. For this assignment, I want you to use the information about who follows whom on Twitter as well as past tweets of the current U.S. Senate members to analyze how they interact and what they tweet about. 

## Data

#### Twitter Handles of Senators

Twitter does not allow us to search for past tweets based on keywords, location, or topics (hashtags). However, we are able to obtain the past tweets of users if we specify their Twitter handle. The file `senators_twitter.csv` contains the Twitter handles of the current U.S. Senate members (obtained from [SocialSeer](https://www.socialseer.com/resources/us-senator-twitter-accounts/)). We will focus on the Senators' _official Twitter accounts_ (as opposed to campaign or staff members). I have also added information on the party affiliation of the Senators from [here](https://ballotpedia.org/List_of_current_members_of_the_U.S._Congress).

#### Followers

The file `senators_follow.csv` contains an edge list of connections between each pair of senators who are connected through a follower relationship (this information was obtained using the function `rtweet::lookup_friendships`). The file is encoded such that the `source` is a follower of the `target`. You will need to use the subset of `following = TRUE` to identify the connections for which the `source` follows the `target`.

#### Tweets by Senators

To make your life a bit easier, I have also already downloaded all available tweets for these Twitter accounts using the following code. You **do not need to repeat this step**. Simply rely on the file `senator_tweets.RDS` in the exercise folder.


```r
library(tidyverse)
library(lubridate)

# Read in the Senator Data
senate <- read_csv("senators_twitter.csv")

# Get Tweets
senator_tweets <- get_timelines(user = senate$`Official Twitter`,
    n = 3200, ## number of tweets to download (max is 3,200)
    )

saveRDS(senator_tweets, "senator_tweets.RDS")
```
lir

```r
# Read in the Tweets
senator_tweets <- readRDS("senator_tweets.RDS")

# How limiting is the API limit?
senator_tweets %>% 
  group_by(screen_name) %>% 
  summarize(n_tweet = n(),
            oldest_tweet = min(created_at)) %>%
  arrange(desc(oldest_tweet))
```

The data contains about 170k tweets and about 40 variables. Please note, that the API limit of 3,200 tweets per twitter handle actually cuts down the time period we can observe the most prolific Twitter users in the Senate down to only about one year into the past.

## Tasks for the Assignment

```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.4.4
```

```r
library(tidyverse)
```

```
## Loading tidyverse: tibble
## Loading tidyverse: tidyr
## Loading tidyverse: readr
## Loading tidyverse: purrr
## Loading tidyverse: dplyr
```

```
## Warning: package 'tibble' was built under R version 3.4.4
```

```
## Warning: package 'tidyr' was built under R version 3.4.4
```

```
## Warning: package 'readr' was built under R version 3.4.3
```

```
## Warning: package 'dplyr' was built under R version 3.4.3
```

```
## Conflicts with tidy packages ----------------------------------------------
```

```
## filter(): dplyr, stats
## lag():    dplyr, stats
```

```r
library(ggnetwork)
```

```
## Warning: package 'ggnetwork' was built under R version 3.4.4
```

```r
library(stringr)
```

```
## Warning: package 'stringr' was built under R version 3.4.4
```

```r
library(visNetwork)
```

```
## Warning: package 'visNetwork' was built under R version 3.4.4
```

```r
library(igraph)
```

```
## Warning: package 'igraph' was built under R version 3.4.4
```

```
## 
## Attaching package: 'igraph'
```

```
## The following objects are masked from 'package:dplyr':
## 
##     as_data_frame, groups, union
```

```
## The following objects are masked from 'package:purrr':
## 
##     compose, simplify
```

```
## The following object is masked from 'package:tidyr':
## 
##     crossing
```

```
## The following object is masked from 'package:tibble':
## 
##     as_data_frame
```

```
## The following objects are masked from 'package:stats':
## 
##     decompose, spectrum
```

```
## The following object is masked from 'package:base':
## 
##     union
```

```r
library(network)
```

```
## Warning: package 'network' was built under R version 3.4.4
```

```
## network: Classes for Relational Data
## Version 1.13.0.1 created on 2015-08-31.
## copyright (c) 2005, Carter T. Butts, University of California-Irvine
##                     Mark S. Handcock, University of California -- Los Angeles
##                     David R. Hunter, Penn State University
##                     Martina Morris, University of Washington
##                     Skye Bender-deMoll, University of Washington
##  For citation information, type citation("network").
##  Type help("network-package") to get started.
```

```
## 
## Attaching package: 'network'
```

```
## The following objects are masked from 'package:igraph':
## 
##     %c%, %s%, add.edges, add.vertices, delete.edges,
##     delete.vertices, get.edge.attribute, get.edges,
##     get.vertex.attribute, is.bipartite, is.directed,
##     list.edge.attributes, list.vertex.attributes,
##     set.edge.attribute, set.vertex.attribute
```

```r
library(ggrepel)
```

```
## Warning: package 'ggrepel' was built under R version 3.4.4
```

```r
library(ggnet)
library(GGally)
```

```
## Warning: package 'GGally' was built under R version 3.4.4
```

```
## 
## Attaching package: 'GGally'
```

```
## The following objects are masked from 'package:ggnet':
## 
##     ggnet, ggnet2
```

```
## The following object is masked from 'package:dplyr':
## 
##     nasa
```

```r
library(sna)
```

```
## Warning: package 'sna' was built under R version 3.4.4
```

```
## Loading required package: statnet.common
```

```
## 
## Attaching package: 'statnet.common'
```

```
## The following object is masked from 'package:base':
## 
##     order
```

```
## sna: Tools for Social Network Analysis
## Version 2.4 created on 2016-07-23.
## copyright (c) 2005, Carter T. Butts, University of California-Irvine
##  For citation information, type citation("sna").
##  Type help(package="sna") to get started.
```

```
## 
## Attaching package: 'sna'
```

```
## The following objects are masked from 'package:igraph':
## 
##     betweenness, bonpow, closeness, components, degree,
##     dyad.census, evcent, hierarchy, is.connected, neighborhood,
##     triad.census
```

```r
library(svgPanZoom)    
```

```
## Warning: package 'svgPanZoom' was built under R version 3.4.4
```

```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:igraph':
## 
##     %--%
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
library(networkD3)
```

```
## Warning: package 'networkD3' was built under R version 3.4.4
```

```r
library(ggridges)
```

```
## Warning: package 'ggridges' was built under R version 3.4.4
```

```r
library(ggthemes)
```

```
## Warning: package 'ggthemes' was built under R version 3.4.4
```

```r
library(ggjoy)
```

```
## Warning: package 'ggjoy' was built under R version 3.4.4
```

```
## The ggjoy package has been deprecated. Please switch over to the
## ggridges package, which provides the same functionality. Porting
## guidelines can be found here:
## https://github.com/clauswilke/ggjoy/blob/master/README.md
```

```r
library(waffle)
```

```
## Warning: package 'waffle' was built under R version 3.4.4
```

```r
library(ggrepel)
library(scales)
```

```
## 
## Attaching package: 'scales'
```

```
## The following object is masked from 'package:purrr':
## 
##     discard
```

```
## The following object is masked from 'package:readr':
## 
##     col_factor
```

```r
library(rtweet)
```

```
## Warning: package 'rtweet' was built under R version 3.4.4
```

### 1. Who follows whom?

#### a) Network of Followers

Read in the edgelist of follower relationships from the file `senators_follow.csv`. Create a directed network graph. Identify the three senators who are followed by the most of their colleagues (i.e. the highest "in-degree") and the three senators who follow the most of their colleagues (i.e. the highest "out-degree"). [Hint: You can get this information simply from the data frame or use `igraph` to calculate the number of in and out connections: `indegree = igraph::degree(g, mode = "in")`.] Visualize the network of senators. In the visualization, highlight the party ID of the senator nodes with an appropriate color (blue = Democrat, red = Republican) and size the nodes by the centrality of the nodes to the network. Briefly comment.

```r
sn <- read.csv("senators_follow.csv")
```

```r
true_follow <- sn[sn$following == TRUE,]
true_follow$following <- NULL
true_follow$followed_by <- NULL
seninfo <- read.csv("senators_twitter.csv")
```



```r
ell <- true_follow
net1 <- network(ell, directed = TRUE)

following <- table(ell$source)
followed <- table(ell$target)

ing3<-tail(sort(following),3)
ed3<-tail(sort(followed),3)

ing3 <- as.data.frame(ing3)
ed3 <- as.data.frame(ed3)

set.seed(1100)
ggnet2(net1, alpha = 0.75, color = ifelse(network.vertex.names(net1) %in% seninfo$Official.Twitter[seninfo$Party.affiliation == "Democratic Party"], "#2E86C1", ifelse(network.vertex.names(net1) %in% seninfo$Official.Twitter[seninfo$Party.affiliation == "Republican Party"], "tomato", "#27AE60")),size = "degree", edge.alpha = 0.5, edge.size = 0.1 ) + guides(size = FALSE)
```

![](assignment4_senate_twitter_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
#It is noticeable that Democrats tend to follow other Democrats more and Republicans tend to follow other Republicans more
```



```r
g <- ggplot(ed3, aes(Var1, Freq, fill=Var1))
g + geom_bar(stat="identity", position = "dodge") + ggtitle("Senators followed by most Senators") + coord_cartesian(ylim=c(92,100))+
  ylab("Number of followers") + xlab("Senators") + theme_fivethirtyeight()+ 
scale_fill_manual("legend", values = c("pink", "orange", "yellow"))
```

![](assignment4_senate_twitter_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
g1 <- ggplot(ing3, aes(Var1, Freq, fill=Var1))
g1 + geom_bar(stat="identity", position = "dodge") + ggtitle("Senators following most Senators") + coord_cartesian(ylim=c(70,90))+
  ylab("Number following") + xlab("Senators")+ theme_fivethirtyeight()+ 
scale_fill_manual("legend", values = c("pink", "orange", "yellow"))
```

![](assignment4_senate_twitter_files/figure-html/unnamed-chunk-7-2.png)<!-- -->

```r
#These are the senators who are most followed y thei colleagues
```


#### b) Communities

Now let's see whether party identification is also recovered by an automated mechanism of cluster identification. Use the `cluster_walktrap` command in the `igraph` package to find densely connected subgraphs. 


```r
ell <- true_follow
rt_graphh <- graph_from_data_frame(d=ell, directed=T)

wcc <- cluster_walktrap(rt_graphh)
memberss <- membership(wcc)
d3_rtt <- igraph_to_networkD3(rt_graphh, group = memberss)
set.seed(1009)
plot(simplify(rt_graphh), group = memberss, vertex.color=memberss)

#The function was able to adequately separate the two parties
```

Based on the results, visualize how well this automated community detection mechanism recovers the party affiliation of senators. This visualization need not be a network graph. Comment briefly. 

### 2. What are they tweeting about?

From now on, rely on the information from the tweets stored in `senator_tweets.RDS`.

#### a) Most Common Topics over Time

Remove all tweets that are re-tweets (`is_retweet`) and identify which topics the senators tweet about. Rather than a full text analysis, just use the variable `hashtags` and identify the most common hashtags over time. Provide a visual summary.

```r
senator_tweets <- readRDS("senator_tweets.RDS")

# How limiting is the API limit?
senator_tweets %>% 
  group_by(screen_name) %>% 
  summarize(n_tweet = n(),
            oldest_tweet = min(created_at)) %>%
  arrange(desc(oldest_tweet))
```

```
## # A tibble: 100 x 3
##    screen_name    n_tweet oldest_tweet       
##    <chr>            <int> <dttm>             
##  1 SenDougJones       178 2018-01-05 18:18:46
##  2 SenTinaSmith       814 2018-01-03 16:21:07
##  3 JohnCornyn        3193 2017-05-31 02:56:50
##  4 SenWhitehouse     3206 2017-05-08 18:44:06
##  5 senorrinhatch     3212 2017-05-01 14:36:04
##  6 brianschatz       3211 2017-04-23 21:15:28
##  7 senrobportman     3237 2017-04-19 15:39:38
##  8 SenMarkey         3241 2017-03-21 18:44:15
##  9 CoryBooker        3206 2017-03-09 21:57:18
## 10 SenJeffMerkley    3248 2017-03-07 01:40:21
## # ... with 90 more rows
```

```r
noret <- senator_tweets[senator_tweets$is_retweet == "FALSE",]
noretweet <- senator_tweets[senator_tweets$is_retweet == "FALSE",]
noret$created_at <- substr(noret$created_at, 1, 7)

allhash <- unnest(noret, hashtags)
```

```r
allhash <- allhash[order(allhash$created_at),]
newhash <- tail(allhash, -7680)

tabl <- as.data.frame(table(newhash$hashtags, newhash$created_at))
colnames(tabl) <- c('Hashtag', 'Date', 'Count')
tabl <- tail(tabl, -2)
tabl <- tabl[order(tabl$Date),]
tabb <- table(newhash$hashtags) 

headhash <- allhash[newhash$hashnum > 1,]
```

```
## Warning: Unknown or uninitialised column: 'hashnum'.
```

```
## Warning: Length of logical index must be 1 or 287964, not 0
```

```r
poph <- as.data.frame(head(sort(tabb, decreasing = TRUE), 10))


p5 <- ggplot(tabl[tabl$Hashtag %in% poph$Var1,], aes(x = Date, y = Count, group=1))

(p5 <- p5 + geom_line() +
   facet_wrap(~Hashtag, ncol = 5)) + theme_wsj() +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) + ggtitle("Trend of popular hashtags")
```

![](assignment4_senate_twitter_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
#These show the trends for different hashtags since 2013, when hashtags became more widely used
```

#### b) Democrats vs. Republicans

Some tweets are as old as 10 years but for some prolific users we observe a much shorter time span of Twitter activity. Feel free to subset the data to only include more recent tweets. Using the party ID variable (`Party affiliation`), identify how the choice of topics tweeted about (again using using hashtags) differs by party and visualize that information.

```r
newhashwp <- merge(x = newhash, y = seninfo[ , c("Official.Twitter","Party.affiliation")], by.x = "screen_name", by.y = "Official.Twitter", all.x=TRUE)

tabl2 <- as.data.frame(table(newhashwp$hashtags, newhashwp$Party.affiliation))
tabl2 <- tail(tabl2, -2)
colnames(tabl2) <- c('Hashtag', 'Party', 'Count')
```


```r
ggplot(tabl2[tabl2$Hashtag %in% poph$Var1,],aes(x=Hashtag,y=Count,fill=Party))+
  geom_bar(stat="identity",position="dodge")+ theme(axis.text.x = element_text(angle = 90, hjust = 1))+ scale_fill_manual(values=c("#2874A6", "#27AE60", "#CB4335")) + ggtitle("Number of Popular Hashtags by Party")+
  theme_fivethirtyeight()
```

![](assignment4_senate_twitter_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
#popular tags by party since 2013
```

#### c) Gun Control I - Dems vs. Reps

The democratic party seems broadly more supportive of gun control legislation. Try to identify a set of 5-10 hashtags that signal support for gun control legislation (e.g. "`NeverAgain`", `#guncontrol`, `#guncontrolnow`, `#Enough`) and others that are expressing support for the right to own guns (e.g. `#2ndamendment`, `#NRA`, `#liberals`). The site [ritetag.com](https://ritetag.com/best-hashtags-for/gun%20control) can help with that task. Using the subset of senator tweets that included these hashtags, show whether and how senators from different parties talk differently about the issue of gun legislation.  


```r
nogun <- c("NeverAgain", "guncontrol", "guncontrolnow", "Enough", "gunviolence", "gunsense", "GunReformNow",
           "BoycottNRA","guncontrolsaveslives", "firearmsafety", "FireArmSafety", "MarchForOurLives", "GunControl", "GunViolence",
           "NotOneMore", "GunSafety", "gunsafety", "GunSense")
yesgun <- c("2ndamendment", "NRA", "liberals", "2ndAmendment", "2ndamdt", "2ndAmdt","2ndamendent", "2A", "ArmedCitizen", "GunRights",
            "gunrights", "firearms", "nra", "armedcitizen", "IAmTheNRA", "Freedom", "AR15", "DefendTheSecond", "GunOwner", "GunsSaveLives", "ProGun", "SelfDefense", "progun", "freedom", "defendthesecond", "gunssavelives")


sum(tabl2$Party == "Democratic Party" &  tabl2$Hashtag %in% nogun)
```

```
## [1] 13
```

```r
sum(tabl2$Party == "Democratic Party" &  tabl2$Hashtag %in% yesgun)
```

```
## [1] 12
```

```r
sum(tabl2$Party == "Republican Party" &  tabl2$Hashtag %in% nogun)
```

```
## [1] 13
```

```r
sum(tabl2$Party == "Republican Party" &  tabl2$Hashtag %in% yesgun)
```

```
## [1] 12
```

```r
sum(tabl2$Party == "Independent" &  tabl2$Hashtag %in% nogun)
```

```
## [1] 13
```

```r
sum(tabl2$Party == "Independent" &  tabl2$Hashtag %in% yesgun)
```

```
## [1] 12
```

```r
noguntab <- tabl2[tabl2$Hashtag %in% nogun,]
yesguntab<- tabl2[tabl2$Hashtag %in% yesgun,]
nogunrep <- noguntab[noguntab$Party == "Republican Party",]
nogundem <- noguntab[noguntab$Party == "Democratic Party",]
yesgundem <- yesguntab[yesguntab$Party == "Democratic Party",]
yesgunrep <- yesguntab[yesguntab$Party == "Republican Party",]

nnr <- sum(nogunrep$Count)
nnd <- sum(nogundem$Count)
nyd <- sum(yesgundem$Count)
nyr <- sum(yesgunrep$Count)


nos <- c(nnd, nnr)
names(nos) <- c("Democratic Party", "Republican Party")

yess <- c(nyd, nyr)
names(yess) <- c("Democratic Party", "Republican Party")

waffle(nos, rows=20,size=1,legend_pos = "top",
       title="Number of Hashtags for gun control by party",
        colors=c("#2874A6", "#CB4335"))
```

![](assignment4_senate_twitter_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

```r
waffle(yess, rows=10,size=1,legend_pos = "top",
       title="Number of Hashtags against gun control by party",
        colors=c("#2874A6", "#CB4335"))
```

![](assignment4_senate_twitter_files/figure-html/unnamed-chunk-13-2.png)<!-- -->

```r
#Shows the distribution of each party members for gun control legislation
```


#### d) Gun Control II - Parkland Shooting

On February 14, 2018, a mass shooting occurred at Marjory Stoneman Douglas High School in Parkland, Florida. Provide some visualization of how senators responded to the event in their Twitter communication. 


```r
parkland <- noretweet[substr(noretweet$created_at,1,10) == '2018-02-14',]
days <- noretweet
days$created_at <- substr(noretweet$created_at, 1, 10)
days <- unnest(days, hashtags)
```


```r
days <- days[substr(days$created_at,3,4) == '18',]
days <- days[substr(days$created_at,6,7) == '02',]
numdays <- as.data.frame(table(days$created_at))
tagdays <- as.data.frame(table(days$hashtags, days$created_at))

ggplot(numdays, aes(Var1, Freq)) + geom_line(aes(group=1)) + xlab("") + ylab("Number of Hashtags")+ theme_solarized()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ ggtitle("Number of Popular Hashtags by Month since 2013")
```

![](assignment4_senate_twitter_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

```r
nogundays <- tagdays[tagdays$Var1 %in% nogun,]
dta.sum <- aggregate(x = nogundays$Freq,
                     FUN = sum,
                     by = list(Group.date = nogundays$Var2))

yesgundays <- tagdays[tagdays$Var1 %in% yesgun,]
yessum <- aggregate(x = yesgundays$Freq,
                     FUN = sum,
                     by = list(Group.date = yesgundays$Var2))


ggplot() + geom_line(data=dta.sum, aes(Group.date, x),group=1, color='#2874A6', size=1.5) + geom_line(data=yessum, aes(Group.date, x),group=1, color = '#CB4335',size=1.5) + xlab("") + ylab("Number of Hashtags")+ theme_solarized()+theme(axis.text.x = element_text(angle = 90, hjust = 1))+ ggtitle("Number of hashtags in favor of gun control in February") + geom_vline(xintercept = 14, color = 'red')+ geom_text(data=data.frame(x=11.5,y=9.5), aes(x, y), label='Day of Shooting', vjust=-1, color='red')  
```

![](assignment4_senate_twitter_files/figure-html/unnamed-chunk-15-2.png)<!-- -->

```r
#The number of popular hashtags does not seem to have been affcted greatly, but hashtags for gun control increased.
```

### 3. Are you talking to me?

Often tweets are simply public statements without addressing a specific audience. However, it is possible to interact with a specific person by adding them as a friend, becoming their follower, re-tweeting their messages, and/or mentioning them in a tweet using the `@` symbol.  

#### a) Identifying Re-Tweets

Select the set of re-tweeted messages from other senators and identify the source of the originating message. Calculate by senator the amount of re-tweets they received and from which party these re-tweets came. Essentially, I would like to visualize whether senators largely re-tweet their own party colleagues' messages or whether there are some senators that get re-tweeted on both sides of the aisle. Visualize the result. 


```r
retweets <- senator_tweets[senator_tweets$is_retweet == TRUE,]


#get IDs of those who got retweeted
retweets$from <-substring(gsub(":.*","",retweets$text),5)

ret_sen <- retweets[retweets$from %in% retweets$screen_name,]

newretsen <- merge(x = ret_sen, y = seninfo[ , c("Official.Twitter","Party.affiliation")], by.x = "screen_name", by.y = "Official.Twitter", all.x=TRUE)

names(newretsen)[names(newretsen) == 'Party.affiliation'] <- "Party"

newretsen <- merge(x = newretsen, y = seninfo[ , c("Official.Twitter","Party.affiliation")], by.x = "from", by.y = "Official.Twitter", all.x=TRUE, all.y=TRUE)
names(newretsen)[names(newretsen) == 'Party.affiliation'] <- "retweet_Party"

el <- newretsen[,c("from","screen_name", "Party")]
rt_graph <- graph_from_data_frame(d=el, directed=T)
```

```
## Warning in graph_from_data_frame(d = el, directed = T): In `d' `NA'
## elements were replaced with string "NA"
```

```r
wc <- cluster_walktrap(rt_graph)
members <- membership(wc)
d3_rt <- igraph_to_networkD3(rt_graph, group = members)

d3_rt$nodes$group <- ifelse(d3_rt$nodes$name %in% seninfo$Official.Twitter[seninfo$Party.affiliation == "Democratic Party"],
                            1,2)

forceNetwork(Links = d3_rt$links, Nodes = d3_rt$nodes, 
             Source = 'source', Target = 'target', 
             NodeID = 'name', Group = 'group', colourScale = JS("d3.scaleOrdinal().domain([1,2]).range(['#2874A6', '#CB4335']);") )
```

<!--html_preserve--><div id="htmlwidget-05264d3395b3e872e84d" style="width:672px;height:480px;" class="forceNetwork html-widget"></div>
<script type="application/json" data-for="htmlwidget-05264d3395b3e872e84d">{"x":{"links":{"source":[33,76,13,86,77,41,33,40,78,77,77,21,75,9,83,1,52,45,83,83,52,6,86,6,49,52,38,99,6,6,30,74,32,59,6,6,76,38,103,28,52,6,16,76,59,6,63,52,65,52,76,32,33,32,6,17,96,2,76,41,21,4,6,73,101,1,21,4,77,6,7,6,10,89,10,4,102,23,90,10,102,4,22,12,89,21,39,39,102,102,39,9,6,90,77,6,90,104,10,4,6,104,10,78,48,90,10,10,6,62,6,12,21,77,58,6,21,6,73,26,19,102,6,90,1,102,20,6,27,10,92,6,37,37,37,23,100,46,12,27,28,77,12,61,25,37,58,45,44,37,59,37,96,37,37,37,37,12,61,37,101,77,44,69,96,37,39,37,73,16,96,96,83,5,6,37,83,45,20,96,37,85,96,77,54,45,83,5,37,37,37,37,37,60,86,77,37,51,96,86,39,37,20,44,99,12,100,37,39,37,73,37,39,37,77,37,37,37,10,86,4,71,104,4,4,57,81,51,10,4,4,82,4,90,4,77,20,4,65,89,39,12,9,104,39,9,101,4,90,6,22,60,69,102,10,51,90,21,7,36,23,68,90,101,58,60,27,90,47,100,58,69,36,21,7,101,101,7,46,100,9,36,7,7,7,73,20,101,37,21,1,71,71,71,38,10,77,51,9,38,10,0,9,10,90,65,86,1,65,58,89,83,12,21,47,4,10,92,25,9,10,92,33,4,4,9,39,47,10,10,53,27,4,71,12,92,47,4,90,68,47,101,71,4,6,90,4,4,4,22,104,90,47,47,6,90,12,6,47,10,10,6,10,35,92,56,71,71,23,22,4,73,57,9,10,10,33,47,47,47,23,104,4,10,71,10,47,104,39,4,22,27,90,6,9,10,47,4,90,6,92,55,71,7,10,71,10,4,90,39,25,47,43,6,6,71,29,57,22,4,10,10,43,10,90,104,0,21,27,36,90,101,10,4,42,71,39,90,10,47,10,90,23,4,27,71,47,73,21,6,27,29,47,22,47,79,6,27,0,7,90,71,10,89,58,10,82,102,4,6,29,4,92,93,104,102,4,4,90,90,77,77,77,77,103,77,77,22,77,22,77,77,5,104,77,77,22,52,77,77,77,77,22,23,22,10,22,65,30,77,61,77,22,22,22,104,22,77,22,22,5,77,77,53,43,95,48,41,95,95,85,28,21,17,85,77,53,5,77,95,57,41,16,78,31,43,95,28,77,10,85,95,82,43,54,85,57,53,74,85,85,78,43,53,12,28,85,63,21,95,32,5,94,94,59,102,54,45,59,61,32,16,73,52,10,45,17,99,99,99,99,32,44,85,99,51,44,99,99,99,99,99,8,99,44,83,20,103,74,44,50,20,76,0,30,95,17,65,94,86,94,100,88,94,20,77,65,17,78,44,96,45,83,83,77,100,52,34,78,86,83,20,66,65,24,34,45,69,17,15,0,100,83,65,52,65,76,0,17,32,96,94,12,17,32,20,32,52,94,65,99,103,102,19,103,34,32,12,32,94,65,94,0,83,82,94,52,19,94,94,67,20,85,17,45,26,96,20,20,20,20,95,62,31,62,35,31,95,17,62,95,66,31,16,71,25,62,95,35,52,34,45,52,95,35,43,31,32,0,43,95,95,95,82,17,52,83,99,52,52,20,83,61,83,83,6,82,65,101,0,25,9,104,104,22,37,104,42,83,27,0,73,21,71,39,104,44,4,21,104,102,104,31,67,101,29,0,27,34,50,61,77,79,58,6,34,25,58,27,55,9,27,39,21,83,55,34,0,9,51,29,104,29,45,37,21,34,34,10,51,104,104,37,104,38,21,39,104,22,13,39,6,102,104,27,77,39,56,81,37,34,104,21,104,58,0,9,27,30,25,66,77,34,34,34,51,46,36,68,55,90,77,34,51,21,12,21,16,45,104,79,51,55,51,104,104,9,72,104,12,21,21,104,34,25,29,21,21,77,72,104,43,104,51,42,43,57,25,104,53,34,104,81,39,81,92,21,77,81,6,6,102,23,47,12,53,21,60,101,10,101,60,55,22,60,27,12,38,55,34,35,6,60,101,65,72,77,81,25,6,101,42,60,69,39,37,53,79,35,12,69,81,21,25,9,60,9,21,60,72,81,81,27,6,53,25,77,53,102,58,9,60,39,51,43,37,53,47,93,35,73,90,53,60,60,12,39,77,69,60,101,73,77,58,71,90,90,69,55,25,69,90,39,102,39,73,69,90,25,57,92,28,90,4,5,60,37,81,25,53,39,47,43,90,101,79,101,65,57,71,92,0,101,47,7,22,72,57,90,33,39,58,43,104,25,53,90,46,53,35,90,36,25,101,25,90,69,90,9,28,50,90,73,93,37,5,5,79,48,48,48,48,15,43,48,32,46,21,96,96,21,35,55,32,48,27,48,28,48,48,48,27,103,19,48,27,48,32,16,96,27,32,36,48,48,48,48,48,21,77,96,36,36,32,36,48,27,21,59,48,96,49,27,48,48,32,48,48,48,48,32,48,48,96,27,45,48,22,40,48,18,87,14,64,3,91,80,84,47,71,90,89,22,4,27,47,23,27,42,25,102,35,102,42,27,73,89,35,69,27,90,33,33,79,103,6,32,81,46,83,6,65,81,6,27,6,31,6,6,81,26,26,27,26,27,6,58,79,10,58,21,27,6,26,79,53,27,82,94,27,82,81,98,58,82,90,46,27,27,27,27,22,33,33,73,22,27,58,89,103,23,22,26,81,25,27,27,46,58,25,27,25,27,25,73,46,22,89,27,6,27,24,22,70,45,95,17,95,95,54,32,32,45,85,86,32,99,34,38,81,22,75,65,49,100,41,16,33,100,75,38,100,99,75,16,102,102,102,103,100,43,5,52,95,100,49,44,85,52,5,49,32,95,63,30,95,95,59,52,22,32,85,39,52,32,30,103,32,83,5,77,44,30,2,32,32,40,40,82,95,49,52,52,44,52,83,32,49,34,31,34,30,32,17,32,34,2,32,52,70,49,40,30,32,32,32,74,49,85,38,104,38,38,99,34,19,38,37,104,59,34,43,38,41,41,38,39,49,38,38,38,43,41,38,38,38,38,59,21,59,43,38,38,38,22,41,48,38,38,38,90,77,43,59,38,13,102,59,38,104,38,38,38,38,13,37,5,38,38,38,38,77,54,38,22,39,29,99,39,19,49,49,38,38,38,38,38,104,38,41,41,41,39,38,74,28,81,30,30,52,75,30,77,65,40,34,83,77,74,52,98,78,77,74,77,16,85,52,34,38,17,90,96,86,20,15,77,41,61,52,95,30,61,74,63,85,99,52,100,16,74,77,44,24,17,45,17,103,74,17,74,45,16,45,90,90,59,93,90,92,89,25,89,6,90,102,27,37,59,102,89,92,92,92,96,32,16,96,30,100,96,89,30,28,61,77,85,24,96,32,96,96,44,30,96,96,103,32,96,32,32,45,96,96,28,24,25,96,28,96,54,31,40,96,1,44,45,78,96,32,32,31,61,77,99,55,29,96,96,32,19,32,45,32,96,32,90,69,22,89,27,69,43,5,47,101,39,23,51,81,90,104,37,72,50,25,37,46,21,69,101,47,102,73,39,101,37,5,7,36,6,104,70,37,81,97,21,104,37,23,6,9,101,37,73,58,36,5,37,37,37,37,5,27,37,37,37,29,5,37,79,31,37,27,21,37,22,37,5,42,101,70,37,37,90,37,73,37,39,77,2,37,58,5,53,37,25,90,37,37,37,6,27,25,37,6,5,81,37,70,37,25,25,37,68,6,35,58,6,53,37,44,37,90,23,37,73,5,9,104,37,37,102,0,39,73,37,53,37,37,104,36,39,5,37,37,73,27,5,96,37,37,37,53,36,50,37,37,35,37,37,39,5,37,68,6,58,37,21,5,37,5,37,57,90,37,6,37,39,81,37,5,37,37,37,37,37,6,37,5,93,25,37,60,0,71,66,31,19,46,72,46,65,31,43,71,31,31,31,46,71,46,6,21,30,31,31,31,66,31,46,2,31,31,31,50,31,72,95,50,25,31,65,77,66,31,31,50,31,67,53,102,69,55,81,73,77,6,77,104,42,21,60,81,4,77,53,92,7,92,12,21,36,37,79,47,68,6,68,27,22,60,68,23,47,6,102,69,36,68,81,57,47,58,21,101,69,68,68,69,25,73,5,68,6,60,4,77,60,60,81,6,69,60,77,47,27,8,36,102,22,102,53,68,6,21,60,22,32,28,0,100,74,74,44,74,74,32,74,74,74,94,74,74,44,32,74,74,74,85,74,74,74,74,74,74,30,74,74,74,44,74,74,74,74,74,28,44,74,32,100,74,74,74,74,74,74,75,72,28,74,52,85,78,0,52,52,88,0,0,96,85,85,52,61,78,32,103,78,45,77,50,68,50,37,50,39,53,50,50,81,37,102,50,53,59,50,50,102,86,50,66,86,50,86,25,58,60,39,19,12,92,50,102,50,58,61,54,81,53,23,51,10,60,75,71,77,33,21,86,71,73,38,77,25,51,38,51,54,22,71,77,39,63,28,51,32,32,5,5,38,104,100,77,5,49,17,40,37,49,5,59,28,28,77,39,32,77,30,100,32,32,13,5,51,85,77,30,83,32,28,32,52,94,52,28,96,96,96,21,52,99,96,32,34,43,0,88,32,20,88,52,34,52,0,43,15,96,95,32,103,52,52,63,52,34,43,41,74,49,15,32,40,96,34,9,34,16,34,96,52,17,34,32,0,9,39,12,69,104,39,39,69,27,46,39,20,89,90,16,39,46,39,94,6,69,89,102,39,58,89,69,46,89,21,89,58,58,104,58,39,7,39,69,94,102,94,104,92,69,39,89,22,39,53,81,73,102,39,101,53,73,101,39,89,27,89,27,92,4,22,39,79,12,12,77,39,90,36,79,5,46,50,93,39,103,101,37,42,50,101,12,93,77,8,101,21,72,39,50,93,95,77,39,104,6,89,89,77,6,43,102,4,89,73,90,90,51,104,36,104,89,104,6,90,77,6,21,21,72,104,10,53,39,60,23,90,102,92,27,24,24,24,24,22,24,24,31,24,24,24,24,24,40,24,24,74,24,20,24,50,24,24,24,24,24,27,24,24,24,24,34,34,31,20,24,34,24,24,24,31,24,24,24,24,24,27,24,24,24,65,24,32,24,31,45,103,28,24,24,24,24,55,24,24,24,24,24,45,24,31,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,49,31,49,49,43,62,44,0,49,21,78,66,28,78,44,49,99,78,78,85,49,103,49,49,74,32,99,0,40,78,44,78,74,78,49,49,78,52,45,77,78,77,49,78,73,75,68,42,77,77,42,42,77,36,81,42,38,59,86,101,71,35,22,102,102,42,52,46,42,46,4,39,37,89,49,71,59,43,31,22,46,31,61,89,8,75,39,53,12,39,44,54,77,46,42,22,43,44,44,51,50,103,43,43,22,65,57,0,65,65,59,73,55,0,22,44,103,59,44,35,43,43,70,44,68,43,43,21,43,51,44,44,13,52,45,85,62,52,52,52,20,52,61,52,96,32,85,32,52,45,52,52,52,52,52,85,48,96,52,52,52,52,52,52,94,52,45,52,78,52,52,10,45,10,77,52,32,59,30,52,52,52,52,52,96,52,52,20,52,61,52,79,61,43,45,59,52,52,52,52,54,52,54,52,52,85,52,85,86,52,52,52,85,52,61,52,52,74,52,85,52,52,52,52,103,52,52,52,52,52,52,52,52,1,59,52,59,52,48,103,65,52,52,52,52,45,52,52,52,65,45,52,17,85,52,45,52,52,52,52,52,41,52,52,52,44,8,41,65,52,52,52,59,52,17,45,52,52,52,45,85,26,45,36,90,6,90,73,90,25,37,27,81,7,46,81,81,102,81,22,37,53,46,77,77,81,57,104,30,95,37,59,66,95,95,95,32,95,32,95,66,76,95,95,99,99,95,95,16,95,95,95,83,65,78,96,95,95,63,32,95,95,32,95,95,32,32,30,8,32,95,32,45,95,95,28,32,95,32,77,95,32,95,16,32,95,95,32,55,81,10,55,25,25,25,55,34,51,27,27,36,81,61,61,71,55,65,55,25,21,104,60,104,92,71,73,60,47,0,61,25,35,90,24,53,22,34,25,39,30,0,71,86,6,73,104,35,60,6,81,81,27,38,5,38,7,93,21,44,86,68,27,0,7,81,92,77,6,38,42,27,77,21,90,22,21,55,38,27,34,44,68,42,61,27,38,68,38,27,9,89,93,42,19,43,32,12,101,101,27,39,92,6,47,22,21,79,47,29,7,39,73,90,53,39,92,65,102,39,0,101,27,104,27,73,22,92,101,102,73,90,73,53,90,39,92,92,22,25,81,25,92,69,27,90,69,102,102,97,39,42,102,27,22,47,47,6,5,79,69,77,33,73,68,25,73,55,103,103,52,42,85,28,99,85,85,65,55,52,55,20,54,55,55,32,5,78,40,54,96,55,41,96,99,70,70,63,85,59,74,41,86,103,96,96,103,77,77,28,52,28,78,96,77,19,44,99,44,44,99,44,70,48,63,94,44,45,44,62,75,44,44,44,44,103,44,52,52,44,45,44,54,44,77,62,44,44,44,44,44,13,16,22,82,32,71,58,69,71,32,66,12,69,43,29,58,69,77,22,23,71,22,69,85,62,51,32,27,62,32,22,62,23,65,45,71,66,29,42,39,53,38,33,39,104,39,39,39,39,90,39,73,57,39,79,39,39,39,42,102,90,57,39,57,58,104,68,39,53,39,39,73,8,42,33,7,77,53,39,90,39,60,60,6,90,90,57,46,92,39,57,53,46,33,93,93,5,6,93,93,96,5,93,61,21,5,5,93,93,59,81,53,61,2,61,6,71,61,33,71,71,93,0,104,33,104,50,71,0,71,70,6,94,50,20,21,77,77,102,77,77,0,39,58,33,39,27,90,25,54,39,39,102,71,77,90,38,55,101,39,90,102,92,39,90,65,102,97,19,97,68,81,89,89,90,101,37,69,92,90,58,89,89,1,90,33,68,9,69,102,27,43,39,9,92,10,27,27,22,21,101,25,24,10,10,81,90,53,27,27,33,37,68,37,102,27,39,77,39,95,21,82,69,47,27,68,81,101,81,25,53,40,40,40,30,59,17,77,85,16,77,30,40,41,103,100,77,104,78,40,54,19,40,77,52,77,82,11,85,40,100,43,40,28,16,78,40,44,41,78,32,40,40,40,49,40,17,95,40,43,17,32,11,28,78,28,20,40,48,75,100,9,40,92,44,40,40,24,43,40,54,77,78,78,65,40,34,59,40,65,40,32,32,52,85,32,77,16,77,63,24,43,40,43,40,24,32,77,49,63,77,28,78,77,77,28,17,32,85,40,52,40,30,40,100,61,44,28,65,77,43,16,54,17,12,13,54,59,65,82,77,65,24,52,40,40,40,78,43,29,0,45,27,0,95,28,29,77,77,0,95,22,48,60,63,96,63,5,83,63,39,21,59,63,12,32,32,32,77,43,12,17,86,32,5,32,77,15,13,32,49,52,49,85,32,74,49,32,65,32,17,74,17,86,33,71,69,43,60,92,60,61,69,79,79,69,12,57,77,25,60,43,6,22,79,69,23,6,69,90,69,69,60,69,57,102,25,42,35,100,22,102,90,27,22,47,73,90,53,53,73,90,70,79,57,66,37,33,53,25,90,97,73,47,58,58,33,70,79,92,53,58,58,46,102,73,21,0,81,62,101,73,6,22,53,90,46,81,81,102,90,50,89,42,42,66,73,0,55,7,90,53,5,55,92,37,71,90,73,25,90,47,70,33,57,90,27,97,53,90,23,57,73,101,68,35,90,6,73,73,81,70,23,27,42,35,73,53,23,36,67,46,29,27,19,53,104,53,73,97,25,39,104,33,27,7,71,42,27,31,68,102,39,36,69,46,36,55,102,73,90,47,55,81,53,27,27,81,10,27,94,102,44,60,100,35,102,73,27,25,27,90,58,25,47,36,60,55,90,46,42,47,39,47,66,6,81,97,73,35,73,73,55,22,27,27,22,98,93,27,22,60,22,5,69,53,9,60,57,22,33,77,37,94,1,1,45,81,94,94,11,94,94,99,26,39,62,46,40,94,100,45,46,46,20,73,77,26,46,45,46,46,94,27,100,46,1,20,39,26,94,94,94,94,45,100,46,74,54,10,13,83,83,83,83,38,83,17,32,83,52,83,1,82,21,83,83,100,83,83,83,83,77,98,83,33,32,83,85,20,83,52,83,83,83,55,83,67,83,83,83,83,44,32,83,83,83,83,83,32,83,83,1,83,83,83,83,83,94,83,83,83,83,83,83,83,27,83,83,59,83,83,83,96,5,45,34,86,65,21,20,83,32,83,83,83,83,74,17,83,83,83,32,86,83,83,83,5,83,83,83,32,83,83,83,83,83,83,83,83,83,32,83,44,83,85,32,83,52,83,83,83,83,61,83,83,83,83,83,55,83,44,30,65,83,103,52,85,83,83,32,24,86,83,83,83,45,92,83,20,83,83,83,19,83,83,83,32,83,96,83,12,83,83,83,32,32,83,27,83,83,1,83,26,83,65,83,99,83,83,20,20,103,32,32,83,83,83,83,83,83,96,78,83,96,83,32,83,83,12,83,83,32,52,83,12,83,32,1,26,32,67,82,17,83,83,32,30,32,35,83,83,83,83,45,83,83,83,83,83,83,65,62,83,38,32,26,83,83,83,38,83,32,83,21,83,83,83,100,83,1,83,83,83,83,83,83,101,96,83,83,83,83,12,74,83,83,83,83,83,20,83,86,52,32,43,32,103,103,40,5,43,103,52,52,52,34,62,61,52,61,61,13,52,52,77,61,52,28,16,103,62,5,61,52,28,45,32,28,52,78,52,0,28,52,32,5,52,44,52,32,34,103,74,96,103,54,78,28,99,99,52,5,52,52,5,61,103,103,52,61,49,61,40,52,62,34,52,52,54,103,13,28,96,44,45,23,45,102,52,57,77,86,28,77,74,12,88,33,77,6,6,86,77,57,90,71,102,73,53,92,33,58,35,104,4,71,73,46,101,73,71,101,79,71,102,101,81,79,81,101,81,46,35,90,101,101,73,25,79,22,73,35,79,81,89,89,57,25,22,57,81,79,73,71,79,33,101,89,10,89,57,39,73,73,25,89,73,53,73,46,50,101,82,10,90,57,73,7,46,72,35,79,39,102,102,102,81,101,73,79,90,73,27,33,81,21,81,35,0,71,56,89,58,71,37,21,7,42,6,47,57,22,4,90,35,21,81,47,6,21,39,4,39,47,42,102,39,57,27,101,27,71,35,47,6,47,39,27,81,68,4,47,56,4,58,89,9,72,81,92,7,92,79,23,57,57,9,7,58,69,25,27,47,56,27,33,25,79,102,70,101,36,47,27,47,93,92,6,22,27,89,93,57,22,58,57,42,6,81,10,60,102,67,21,42,77,22,37,4,7,81,27,6,47,23,92,56,21,27,21,81,47,47,27,101,81,79,7,6,47,6,23,42,73,68,72,6,69,56,37,79,35,51,47,9,27,69,22,69,6,53,6,68,23,25,69,27,27,4,21,58,27,36,81,102,4,81,72,37,47,68,47,47,25,4,69,79,22,101,22,39,21,27,27,22,81,47,73,10,79,60,42,36,93,68,71,81,42,27,37,42,69,42,25,42,27,58,72,58,22,81,25,60,27,25,73,96,9,60,27,57,51,23,101,77,4,60,25,69,69,10,37,6,37,67,39,81,92,46,6,42,7,7,55,25,4,81,4,69,42,51,69,58,22,27,81,89,42,101,69,21,25,58,81,79,22,32,6,21,81,77,33,101,52,28,95,95,95,95,95,95,86,58,6,10,100,39,86,86,86,23,59,86,53,10,27,27,27,72,86,90,56,70,70,6,28,73,10,89,27,28,28,39,90,70,28,42,70,23,46,66,59,70,43,86,5,68,90,38,58,46,70,70,57,70,42,70,27,73,27,7,90,70,94,94,94,28,94,57,94,94,82,94,94,94,82,94,82,86,94,94,82,20,82,32,94,26,45,41,82,17,94,65,103,63,94,45,82,17,17,94,20,94,94,94,94,45,54,45,95,54,54,54,54,54,95,54,34,95,54,37,54,54,95,54,41,34,34,17,85,5,5,100,34,8,52,32,50,34,32,45,17,37,34,45,85,32,41,17,34,34,45,33,0,54,77,8,74,34,32,2,0,45,32,17,34,8,32,44,52,32,41,99,62,44,24,34,2,78,34,45,30,34,48,5,100,20,34,34,45,32,0,34,52,86,17,45,99,63,44,34,5,5,52,34,34,34,32,28,75,52,32,32,78,30,44,85,93,34,45,34,44,44,85,34,17,34,45,34,54,70,44,41,45,100,17,8,103,32,77,44,70,86,45,17,30,34,34,17,32,100,70,34,34,34,34,34,34,100,17,34,22,49,33,34,32,52,104,13,45,28,85,29,33,34,54,86,77,44,8,57,20,17,63,28,101,96,34,59,32,45,45,0,96,74,17,43,97,97,73,32,97,50,77,16,52,16,77,34,16,34,16,16,62,45,63,34,34,17,32,32,88,77,50,54,32,28,32,5,100,32,77,74,34,77,17,7,43,92,90,92,72,90,33,27,73,58,25,27,73,89,53,25,43,46,89,58,57,25,90,79,92,73,58,21,89,71,90,89,73,58,27,102,90,81,90,37,21,43,77,72,53,6,72,90,81,72,43,72,57,102,72,73,43,72,90,72,37,93,33,42,39,71,90,92,56,6,90,25,42,23,58,57,90,43,89,81,79,0,69,37,25,57,58,21,77,1,42,90,27,72,9,83,46,4,39,68,4,22,72,81,60,4,77,69,77,47,86,90,9,47,46,97,42,50,35,57,42,46,72,102,25,33,90,6,9,37,36,53,77,90,77,90,77,43,25,47,90,81,4,27,31,36,51,69,53,69,90,90,101,73,43,9,23,101,10,92,7,63,77,39,58,86,90,27,25,42,72,73,53,101,5,46,37,38,22,23,60,90,53,37,6,101,90,36,57,25,0,0,57,57,103,0,63,16,23,62,24,82,44,70,63,71,27,85,70,63,13,88,28,70,63,17,51,51,27,70,0,5,5,41,99,51,28,103,48,41,82,96,71,30,51,59,70,73,5,99,74,62,51,52,54,83,70,101,63,61,32,45,13,51,78,32,65,17,51,45,85,71,21,55,21,68,21,21,77,21,73,37,81,102,21,42,12,52,12,55,77,37,21,21,37,77,59,21,86,79,31,5,5,71,39,89,55,21,90,77],"target":[0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,8,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,11,11,11,11,11,11,11,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,18,18,18,18,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,105,105,105,105,105,105,105,105,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,28,28,28,28,28,28,28,28,28,28,28,28,28,28,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,35,35,35,35,35,35,35,35,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,39,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,43,43,43,43,43,43,43,43,43,43,43,43,43,43,43,43,43,43,43,43,43,43,43,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,47,47,47,47,47,47,47,47,47,47,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,51,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,54,55,55,55,55,55,55,55,55,55,55,55,55,55,55,55,55,55,55,55,55,55,55,55,55,55,55,55,55,55,55,55,55,55,55,55,55,55,55,55,55,55,55,56,57,57,57,57,57,57,57,57,57,57,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,59,59,59,59,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,62,62,62,62,62,62,62,62,62,62,62,62,62,63,63,63,63,63,63,63,63,63,63,63,63,63,63,63,63,63,63,63,63,63,63,63,63,63,63,63,63,63,63,63,63,63,63,63,63,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,66,67,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,69,69,69,69,69,69,69,69,69,69,69,69,69,69,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,71,71,71,71,71,71,71,71,71,71,71,71,71,71,71,71,71,71,71,72,72,72,72,72,72,72,72,72,72,72,72,72,72,72,72,72,72,72,72,72,72,72,72,72,72,72,72,72,72,72,72,72,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,75,75,75,75,75,75,75,75,75,75,75,75,75,75,76,76,76,76,76,76,76,76,76,76,76,76,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,78,78,78,78,78,78,78,78,78,78,78,78,78,78,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,82,82,82,82,82,82,82,82,82,82,82,82,82,82,82,82,82,82,82,82,82,82,82,82,82,82,82,82,82,82,82,82,82,82,82,82,82,82,82,82,82,82,82,82,82,82,82,82,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,86,86,86,86,86,86,86,88,88,88,88,88,88,88,88,88,88,88,88,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,89,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,90,91,91,91,91,91,91,91,91,92,92,92,92,92,92,92,92,92,92,92,92,92,92,92,92,92,92,93,93,93,93,93,93,93,93,93,93,93,93,93,93,93,93,93,93,93,93,93,93,93,93,93,93,93,93,93,93,93,93,93,93,93,93,93,93,93,93,93,93,93,93,94,94,94,94,94,94,94,94,94,94,94,94,94,94,94,94,94,94,94,94,94,94,94,94,94,94,94,94,94,94,94,94,94,94,94,94,94,94,94,94,94,94,94,94,95,95,95,95,95,95,95,95,95,95,95,95,95,95,95,95,95,95,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,96,97,97,97,98,98,98,98,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,99,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,100,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,101,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,102,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,103,104,104,104,104,104,104,104,104,104,104,104,104,104,104,104,104,104,104,104,104,104,104,104,104,104,104,104,104,104,104,104,104,104,104,104,104,104,104],"colour":["#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666","#666"]},"nodes":{"name":["amyklobuchar","BenSasse","BillCassidy","BobCorker","brianschatz","ChrisCoons","ChrisMurphyCT","ChrisVanHollen","ChuckGrassley","clairecmc","CoryBooker","GrahamBlog","JeffFlake","JerryMoran","jiminhofe","JimInhofe","JohnBoozman","JohnCornyn","joniernst","lisamurkowski","marcorubio","MarkWarner","MartinHeinrich","maziehirono","MikeCrapo","PattyMurray","RandPaul","RonWyden","RoyBlunt","Sen_JoeManchin","SenAlexander","SenAngusKing","SenateMajLdr","SenatorBaldwin","SenatorBurr","SenatorCantwell","SenatorCardin","SenatorCarper","SenatorCollins","SenatorDurbin","SenatorEnzi","SenatorFischer","SenatorHassan","SenatorHeitkamp","SenatorIsakson","SenatorLankford","SenatorLeahy","SenatorMenendez","SenatorRisch","SenatorRounds","SenatorShaheen","SenatorTester","SenatorTimScott","SenatorTomUdall","SenatorWicker","SenBennetCO","SenBillNelson","SenBlumenthal","SenBobCasey","SenCapito","SenCortezMasto","SenCoryGardner","SenDanSullivan","sendavidperdue","SenDavidPerdue","SenDeanHeller","SenDonnelly","SenDougJones","SenDuckworth","SenFeinstein","SenGaryPeters","SenGillibrand","SenJackReed","SenJeffMerkley","SenJohnBarrasso","SenJohnHoeven","SenJohnKennedy","SenJohnMcCain","SenJohnThune","SenKamalaHarris","senmarkey","SenMarkey","SenMikeLee","senorrinhatch","SenOrrinHatch","SenPatRoberts","senrobportman","SenRobPortman","SenRonJohnson","SenSanders","SenSchumer","SenShelby","SenSherrodBrown","SenStabenow","SenTedCruz","SenThadCochran","SenThomTillis","SenTinaSmith","SenToddYoung","SenTomCotton","SenToomey","SenWarren","SenWhitehouse","SteveDaines","timkaine","NA"],"group":[1,2,2,2,1,1,1,1,2,1,1,2,2,2,2,2,2,2,2,2,2,1,1,1,2,1,2,1,2,1,2,2,2,1,2,1,1,1,2,1,2,2,1,1,2,2,1,1,2,2,1,1,2,1,2,1,1,1,1,2,1,2,2,2,2,2,1,1,1,1,1,1,1,1,2,2,2,2,2,1,1,2,2,2,2,2,2,2,2,2,1,2,1,1,2,2,2,1,2,2,2,1,1,2,1,2]},"options":{"NodeID":"name","Group":"group","colourScale":"d3.scaleOrdinal().domain([1,2]).range(['#2874A6', '#CB4335']);","fontSize":7,"fontFamily":"serif","clickTextSize":17.5,"linkDistance":50,"linkWidth":"function(d) { return Math.sqrt(d.value); }","charge":-30,"opacity":0.6,"zoom":false,"legend":false,"arrows":false,"nodesize":false,"radiusCalculation":" Math.sqrt(d.nodesize)+6","bounded":false,"opacityNoHover":0,"clickAction":null}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

```r
#You can see the division between the parties. Each party tends to retweet more of their own party's tweets
```



#### b) Identifying Mentions

Identify the tweets in which one senator mentions another senator directly (the variable is `mentions_screen_name`). For this example, please remove simple re-tweets (`is_retweet == FALSE`). Calculate who re-tweets whom among the senate members. Convert the information to an undirected graph object in which the number of mentions is the strength of the relationship between senators. Visualize the network graph using the party identification of the senators as a group variable (use blue for Democrats and red for Republicans) and some graph centrality measure to size the nodes. Comment on what you can see from the visualization.


```r
mentrel <- noret[,c("screen_name", "mentions_screen_name")]
mentsen <- unnest(mentrel, mentions_screen_name)
mentsen <- mentsen[mentsen$mentions_screen_name %in% senator_tweets$screen_name,]
```

```r
net2 <- network(mentsen, directed = TRUE)
ggnetwork(net2, weights = "Frequency")
```

```
## Warning in fortify.network(x, ...): duplicated edges detected
```

```
##               x         y  na.x    vertex.names        xend       yend
## 1     0.5228768 0.6516892 FALSE    amyklobuchar 0.522876847 0.65168917
## 2     0.7021885 0.9950414 FALSE        BenSasse 0.702188545 0.99504145
## 3     0.7779185 0.6162086 FALSE     BillCassidy 0.777918481 0.61620861
## 4     0.0000000 0.0000000 FALSE       BobCorker 0.000000000 0.00000000
## 5     0.2226792 0.8702001 FALSE     brianschatz 0.222679244 0.87020013
## 6     0.5793850 0.4664930 FALSE      ChrisCoons 0.579385040 0.46649305
## 7     0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.379417542 0.65183730
## 8     0.2943128 0.6971773 FALSE  ChrisVanHollen 0.294312757 0.69717732
## 9     0.8283623 0.6576046 FALSE   ChuckGrassley 0.828362259 0.65760460
## 10    0.4614245 0.5950597 FALSE       clairecmc 0.461424518 0.59505968
## 11    0.3917261 0.7061237 FALSE      CoryBooker 0.391726137 0.70612369
## 12    0.5414873 1.0000000 FALSE      GrahamBlog 0.541487257 1.00000000
## 13    0.6726420 0.6722629 FALSE       JeffFlake 0.672642042 0.67226291
## 14    0.5617509 0.8235947 FALSE      JerryMoran 0.561750926 0.82359465
## 15    0.8209075 0.7607588 FALSE       JimInhofe 0.820907521 0.76075878
## 16    0.7689116 0.5193352 FALSE     JohnBoozman 0.768911559 0.51933517
## 17    0.7426211 0.4831933 FALSE      JohnCornyn 0.742621150 0.48319331
## 18    1.0000000 0.8907920 FALSE       joniernst 1.000000000 0.89079199
## 19    0.4295459 0.7858627 FALSE   lisamurkowski 0.429545890 0.78586274
## 20    0.7064591 0.6245778 FALSE      marcorubio 0.706459139 0.62457777
## 21    0.4781925 0.5287547 FALSE      MarkWarner 0.478192462 0.52875474
## 22    0.2768264 0.5411446 FALSE  MartinHeinrich 0.276826397 0.54114463
## 23    0.4421794 0.6590007 FALSE     maziehirono 0.442179396 0.65900071
## 24    0.5507811 0.6800289 FALSE       MikeCrapo 0.550781098 0.68002886
## 25    0.2749538 0.4744784 FALSE     PattyMurray 0.274953782 0.47447841
## 26    0.4101392 0.8554135 FALSE        RandPaul 0.410139162 0.85541347
## 27    0.4175093 0.5254365 FALSE        RonWyden 0.417509337 0.52543650
## 28    0.8483218 0.6124618 FALSE        RoyBlunt 0.848321817 0.61246184
## 29    0.6669244 0.4590610 FALSE  Sen_JoeManchin 0.666924358 0.45906105
## 30    0.5432123 0.4100616 FALSE    SenAlexander 0.543212265 0.41006164
## 31    0.5446980 0.5210859 FALSE    SenAngusKing 0.544697985 0.52108593
## 32    0.5753059 0.6019949 FALSE    SenateMajLdr 0.575305871 0.60199492
## 33    0.3969839 0.4437440 FALSE  SenatorBaldwin 0.396983880 0.44374404
## 34    0.6968503 0.5688294 FALSE     SenatorBurr 0.696850346 0.56882936
## 35    0.2211182 0.6985012 FALSE SenatorCantwell 0.221118196 0.69850119
## 36    0.5179329 0.5881033 FALSE   SenatorCardin 0.517932876 0.58810334
## 37    0.5204533 0.6212819 FALSE   SenatorCarper 0.520453335 0.62128188
## 38    0.4468391 0.4402281 FALSE  SenatorCollins 0.446839108 0.44022809
## 39    0.4116386 0.5554641 FALSE   SenatorDurbin 0.411638621 0.55546407
## 40    0.7076994 0.8051583 FALSE     SenatorEnzi 0.707699360 0.80515829
## 41    0.7540678 0.7762385 FALSE  SenatorFischer 0.754067844 0.77623846
## 42    0.3029414 0.4629216 FALSE   SenatorHassan 0.302941422 0.46292162
## 43    0.5275960 0.4716775 FALSE SenatorHeitkamp 0.527596019 0.47167745
## 44    0.6333832 0.7144322 FALSE  SenatorIsakson 0.633383190 0.71443221
## 45    0.7093614 0.7288584 FALSE SenatorLankford 0.709361407 0.72885837
## 46    0.3486256 0.8011896 FALSE    SenatorLeahy 0.348625610 0.80118965
## 47    0.2075508 0.5296549 FALSE SenatorMenendez 0.207550807 0.52965494
## 48    0.6534665 0.8499123 FALSE    SenatorRisch 0.653466460 0.84991229
## 49    0.8162746 0.5491912 FALSE   SenatorRounds 0.816274595 0.54919118
## 50    0.3566078 0.5366487 FALSE  SenatorShaheen 0.356607831 0.53664875
## 51    0.2779848 0.7356608 FALSE   SenatorTester 0.277984793 0.73566076
## 52    0.5603286 0.7702285 FALSE SenatorTimScott 0.560328650 0.77022846
## 53    0.3625031 0.7686541 FALSE SenatorTomUdall 0.362503117 0.76865410
## 54    0.6669221 0.6067823 FALSE   SenatorWicker 0.666922052 0.60678226
## 55    0.4631773 0.7920311 FALSE     SenBennetCO 0.463177319 0.79203111
## 56    0.5820865 0.3752292 FALSE   SenBillNelson 0.582086473 0.37522915
## 57    0.4831484 0.4291480 FALSE   SenBlumenthal 0.483148392 0.42914804
## 58    0.2734336 0.5840768 FALSE     SenBobCasey 0.273433640 0.58407679
## 59    0.5745898 0.4858137 FALSE       SenCapito 0.574589817 0.48581371
## 60    0.3676334 0.4429741 FALSE  SenCortezMasto 0.367633420 0.44297408
## 61    0.6902237 0.5142908 FALSE  SenCoryGardner 0.690223731 0.51429083
## 62    0.7931346 0.6770077 FALSE  SenDanSullivan 0.793134597 0.67700772
## 63    0.7439614 0.7213531 FALSE  sendavidperdue 0.743961405 0.72135305
## 64    0.5721325 0.5644972 FALSE   SenDeanHeller 0.572132530 0.56449722
## 65    0.6482835 0.4161830 FALSE     SenDonnelly 0.648283503 0.41618298
## 66    0.3711870 0.2901037 FALSE    SenDougJones 0.371187040 0.29010370
## 67    0.1922032 0.6294974 FALSE    SenDuckworth 0.192203241 0.62949745
## 68    0.3910750 0.7431671 FALSE    SenFeinstein 0.391074953 0.74316715
## 69    0.4914565 0.7536239 FALSE   SenGaryPeters 0.491456545 0.75362393
## 70    0.2608729 0.6322347 FALSE   SenGillibrand 0.260872895 0.63223470
## 71    0.4844430 0.8259203 FALSE     SenJackReed 0.484442977 0.82592033
## 72    0.3049818 0.6219774 FALSE  SenJeffMerkley 0.304981788 0.62197740
## 73    0.6473721 0.7777920 FALSE SenJohnBarrasso 0.647372060 0.77779195
## 74    0.8709787 0.4849846 FALSE   SenJohnHoeven 0.870978745 0.48498460
## 75    0.8611981 0.3936291 FALSE  SenJohnKennedy 0.861198084 0.39362913
## 76    0.5732753 0.6630101 FALSE   SenJohnMcCain 0.573275280 0.66301012
## 77    0.7554063 0.6844109 FALSE    SenJohnThune 0.755406323 0.68441093
## 78    0.2512095 0.4986862 FALSE SenKamalaHarris 0.251209508 0.49868615
## 79    0.2756657 0.6607710 FALSE       SenMarkey 0.275665692 0.66077102
## 80    0.6211460 0.8225987 FALSE      SenMikeLee 0.621146046 0.82259873
## 81    0.6991421 0.4840451 FALSE   senorrinhatch 0.699142087 0.48404506
## 82    0.8565873 0.7046136 FALSE   SenPatRoberts 0.856587305 0.70461364
## 83    0.5175387 0.7087600 FALSE   senrobportman 0.517538662 0.70876003
## 84    0.8601440 0.5612904 FALSE   SenRonJohnson 0.860143957 0.56129035
## 85    0.2669452 0.7757767 FALSE      SenSanders 0.266945238 0.77577670
## 86    0.4146370 0.6542285 FALSE      SenSchumer 0.414636999 0.65422853
## 87    0.3577542 0.9963846 FALSE       SenShelby 0.357754190 0.99638464
## 88    0.3859586 0.4806775 FALSE SenSherrodBrown 0.385958593 0.48067754
## 89    0.4780429 0.3756421 FALSE     SenStabenow 0.478042930 0.37564207
## 90    0.6637223 0.8039531 FALSE      SenTedCruz 0.663722292 0.80395315
## 91    0.5579357 0.8658910 FALSE  SenThadCochran 0.557935712 0.86589097
## 92    0.6274394 0.5746844 FALSE   SenThomTillis 0.627439406 0.57468438
## 93    0.0913645 0.5089397 FALSE    SenTinaSmith 0.091364502 0.50893974
## 94    0.3819211 0.3784532 FALSE    SenToddYoung 0.381921098 0.37845318
## 95    0.7895131 0.3804535 FALSE    SenTomCotton 0.789513080 0.38045350
## 96    0.7350978 0.5583015 FALSE       SenToomey 0.735097762 0.55830147
## 97    0.4247477 0.5748868 FALSE       SenWarren 0.424747742 0.57488680
## 98    0.5062503 0.5090000 FALSE   SenWhitehouse 0.506250274 0.50900003
## 99    0.5814023 0.7200733 FALSE     SteveDaines 0.581402288 0.72007326
## 100   0.4289608 0.6871611 FALSE        timkaine 0.428960776 0.68716113
## 153   0.0000000 0.0000000 FALSE       BobCorker 0.523259128 0.39499936
## 261   0.0913645 0.5089397 FALSE    SenTinaSmith 0.410203382 0.77002395
## 3123  0.0913645 0.5089397 FALSE    SenTinaSmith 0.437075181 0.58939313
## 423   0.0913645 0.5089397 FALSE    SenTinaSmith 0.502686727 0.47380517
## 589   0.0913645 0.5089397 FALSE    SenTinaSmith 0.344449194 0.75136086
## 6106  0.0913645 0.5089397 FALSE    SenTinaSmith 0.176163551 0.61032120
## 851   0.0913645 0.5089397 FALSE    SenTinaSmith 0.250382907 0.47909058
## 1095  0.0913645 0.5089397 FALSE    SenTinaSmith 0.406852389 0.67548983
## 12174 0.0913645 0.5089397 FALSE    SenTinaSmith 0.351494104 0.30550462
## 1337  0.0913645 0.5089397 FALSE    SenTinaSmith 0.278512568 0.46823491
## 1468  0.0913645 0.5089397 FALSE    SenTinaSmith 0.372534009 0.44895976
## 15110 0.0913645 0.5089397 FALSE    SenTinaSmith 0.331743140 0.53405122
## 16100 0.0913645 0.5089397 FALSE    SenTinaSmith 0.361072852 0.48306498
## 17105 0.0913645 0.5089397 FALSE    SenTinaSmith 0.499141872 0.64383736
## 20100 0.0913645 0.5089397 FALSE    SenTinaSmith 0.558442659 0.71018107
## 21100 0.0913645 0.5089397 FALSE    SenTinaSmith 0.359115315 0.38869507
## 24100 0.0913645 0.5089397 FALSE    SenTinaSmith 0.454407858 0.38378967
## 27100 0.0913645 0.5089397 FALSE    SenTinaSmith 0.518790175 0.41540594
## 28100 0.0913645 0.5089397 FALSE    SenTinaSmith 0.282884787 0.61028456
## 31100 0.0913645 0.5089397 FALSE    SenTinaSmith 0.256370202 0.64487498
## 3274  0.0913645 0.5089397 FALSE    SenTinaSmith 0.400222958 0.57003551
## 3311  0.0913645 0.5089397 FALSE    SenTinaSmith 0.262096736 0.71635869
## 36100 0.1922032 0.6294974 FALSE    SenDuckworth 0.387950462 0.56345601
## 37100 0.1922032 0.6294974 FALSE    SenDuckworth 0.493132334 0.59125503
## 41100 0.1922032 0.6294974 FALSE    SenDuckworth 0.252255172 0.65199904
## 42100 0.1922032 0.6294974 FALSE    SenDuckworth 0.251613165 0.59627789
## 433   0.1922032 0.6294974 FALSE    SenDuckworth 0.561146126 0.38888572
## 452   0.1922032 0.6294974 FALSE    SenDuckworth 0.400409849 0.58060230
## 4723  0.1922032 0.6294974 FALSE    SenDuckworth 0.350505580 0.46118499
## 491   0.1922032 0.6294974 FALSE    SenDuckworth 0.389790104 0.65146595
## 5037  0.1922032 0.6294974 FALSE    SenDuckworth 0.273474544 0.68336541
## 5225  0.1922032 0.6294974 FALSE    SenDuckworth 0.482909422 0.51795574
## 5410  0.1922032 0.6294974 FALSE    SenDuckworth 0.280037180 0.62364070
## 5613  0.1922032 0.6294974 FALSE    SenDuckworth 0.378466900 0.46054051
## 57100 0.1922032 0.6294974 FALSE    SenDuckworth 0.369370225 0.73076131
## 5815  0.1922032 0.6294974 FALSE    SenDuckworth 0.729879176 0.76992116
## 6033  0.1922032 0.6294974 FALSE    SenDuckworth 0.548371398 0.66081999
## 64100 0.1922032 0.6294974 FALSE    SenDuckworth 0.107404191 0.52811598
## 66100 0.1922032 0.6294974 FALSE    SenDuckworth 0.463694179 0.81197445
## 70100 0.1922032 0.6294974 FALSE    SenDuckworth 0.354593652 0.64887512
## 72100 0.1922032 0.6294974 FALSE    SenDuckworth 0.343144181 0.75283539
## 7489  0.1922032 0.6294974 FALSE    SenDuckworth 0.334839472 0.54894259
## 77100 0.1922032 0.6294974 FALSE    SenDuckworth 0.462558075 0.44332685
## 8310  0.1922032 0.6294974 FALSE    SenDuckworth 0.240929941 0.52147498
## 8949  0.1922032 0.6294974 FALSE    SenDuckworth 0.366131987 0.49590599
## 9294  0.1922032 0.6294974 FALSE    SenDuckworth 0.368388058 0.69716076
## 9350  0.1922032 0.6294974 FALSE    SenDuckworth 0.263180911 0.49653288
## 9473  0.1922032 0.6294974 FALSE    SenDuckworth 0.495461162 0.62190739
## 9719  0.1922032 0.6294974 FALSE    SenDuckworth 0.259533908 0.55919926
## 9911  0.1922032 0.6294974 FALSE    SenDuckworth 0.417351719 0.65607044
## 101   0.1922032 0.6294974 FALSE    SenDuckworth 0.255570232 0.75351442
## 104   0.1922032 0.6294974 FALSE    SenDuckworth 0.493249150 0.70284229
## 111   0.1922032 0.6294974 FALSE    SenDuckworth 0.504975243 0.48232172
## 119   0.1922032 0.6294974 FALSE    SenDuckworth 0.262272568 0.71621530
## 158   0.1922032 0.6294974 FALSE    SenDuckworth 0.331788848 0.78270929
## 172   0.2075508 0.5296549 FALSE SenatorMenendez 0.386836159 0.55232753
## 173   0.2075508 0.5296549 FALSE SenatorMenendez 0.558973982 0.38475872
## 174   0.2075508 0.5296549 FALSE SenatorMenendez 0.393214380 0.64134166
## 175   0.2075508 0.5296549 FALSE SenatorMenendez 0.373674835 0.68882772
## 176   0.2075508 0.5296549 FALSE SenatorMenendez 0.196001571 0.60478768
## 177   0.2075508 0.5296549 FALSE SenatorMenendez 0.254159151 0.56815529
## 179   0.2075508 0.5296549 FALSE SenatorMenendez 0.264140595 0.63858606
## 180   0.2075508 0.5296549 FALSE SenatorMenendez 0.459661493 0.43771341
## 182   0.2075508 0.5296549 FALSE SenatorMenendez 0.374778890 0.72420829
## 183   0.2075508 0.5296549 FALSE SenatorMenendez 0.467372576 0.80765557
## 184   0.2075508 0.5296549 FALSE SenatorMenendez 0.286834778 0.60478188
## 185   0.2075508 0.5296549 FALSE SenatorMenendez 0.361850533 0.48729581
## 188   0.2075508 0.5296549 FALSE SenatorMenendez 0.550775943 0.59716972
## 192   0.2075508 0.5296549 FALSE SenatorMenendez 0.400272838 0.56978983
## 199   0.2075508 0.5296549 FALSE SenatorMenendez 0.359041757 0.63735188
## 200   0.2075508 0.5296549 FALSE SenatorMenendez 0.009121207 0.02327667
## 201   0.2075508 0.5296549 FALSE SenatorMenendez 0.681899704 0.61990506
## 203   0.2075508 0.5296549 FALSE SenatorMenendez 0.665236387 0.51508621
## 204   0.2075508 0.5296549 FALSE SenatorMenendez 0.554738097 0.47067972
## 206   0.2075508 0.5296549 FALSE SenatorMenendez 0.345649363 0.45487792
## 207   0.2075508 0.5296549 FALSE SenatorMenendez 0.282815430 0.67497796
## 210   0.2075508 0.5296549 FALSE SenatorMenendez 0.437215024 0.58882266
## 214   0.2075508 0.5296549 FALSE SenatorMenendez 0.249342377 0.61005257
## 216   0.2075508 0.5296549 FALSE SenatorMenendez 0.221569734 0.84522476
## 221   0.2075508 0.5296549 FALSE SenatorMenendez 0.252163310 0.53705414
## 229   0.2075508 0.5296549 FALSE SenatorMenendez 0.499561961 0.64266608
## 232   0.2075508 0.5296549 FALSE SenatorMenendez 0.493364689 0.58347688
## 233   0.2075508 0.5296549 FALSE SenatorMenendez 0.420285831 0.64693125
## 243   0.2075508 0.5296549 FALSE SenatorMenendez 0.255608878 0.49031428
## 248   0.2075508 0.5296549 FALSE SenatorMenendez 0.624073045 0.42241625
## 253   0.2075508 0.5296549 FALSE SenatorMenendez 0.219115812 0.67358151
## 268   0.2075508 0.5296549 FALSE SenatorMenendez 0.549787971 0.65444587
## 272   0.2075508 0.5296549 FALSE SenatorMenendez 0.481309831 0.51072465
## 273   0.2075508 0.5296549 FALSE SenatorMenendez 0.261080557 0.75147433
## 284   0.2075508 0.5296549 FALSE SenatorMenendez 0.337099718 0.77900511
## 311   0.2075508 0.5296549 FALSE SenatorMenendez 0.348902921 0.74767709
## 315   0.2075508 0.5296549 FALSE SenatorMenendez 0.230818565 0.51315023
## 336   0.2075508 0.5296549 FALSE SenatorMenendez 0.269896887 0.71200519
## 355   0.2075508 0.5296549 FALSE SenatorMenendez 0.408589461 0.67266943
## 356   0.2075508 0.5296549 FALSE SenatorMenendez 0.115976376 0.51332786
## 407   0.2075508 0.5296549 FALSE SenatorMenendez 0.282456578 0.47725240
## 422   0.2211182 0.6985012 FALSE SenatorCantwell 0.676332375 0.49427819
## 424   0.2211182 0.6985012 FALSE SenatorCantwell 0.398752894 0.54196514
## 425   0.2211182 0.6985012 FALSE SenatorCantwell 0.269112264 0.49878637
## 426   0.2211182 0.6985012 FALSE SenatorCantwell 0.730415012 0.68507000
## 427   0.2211182 0.6985012 FALSE SenatorCantwell 0.485429257 0.52283785
## 428   0.2211182 0.6985012 FALSE SenatorCantwell 0.355437725 0.65890613
## 431   0.2211182 0.6985012 FALSE SenatorCantwell 0.254193148 0.75427358
## 436   0.2211182 0.6985012 FALSE SenatorCantwell 0.340108356 0.75754218
## 438   0.2211182 0.6985012 FALSE SenatorCantwell 0.347331135 0.97366109
## 439   0.2211182 0.6985012 FALSE SenatorCantwell 0.255104982 0.67499273
## 442   0.2211182 0.6985012 FALSE SenatorCantwell 0.269316845 0.69762942
## 444   0.2211182 0.6985012 FALSE SenatorCantwell 0.406489311 0.77619868
## 445   0.2211182 0.6985012 FALSE SenatorCantwell 0.507500917 0.48654981
## 449   0.2211182 0.6985012 FALSE SenatorCantwell 0.430387561 0.45905219
## 453   0.2211182 0.6985012 FALSE SenatorCantwell 0.496245845 0.62752667
## 456   0.2211182 0.6985012 FALSE SenatorCantwell 0.366896007 0.73681273
## 457   0.2211182 0.6985012 FALSE SenatorCantwell 0.417569193 0.66339820
## 461   0.2211182 0.6985012 FALSE SenatorCantwell 0.548401283 0.66551696
## 462   0.2211182 0.6985012 FALSE SenatorCantwell 0.370697286 0.40079207
## 464   0.2211182 0.6985012 FALSE SenatorCantwell 0.286514462 0.63882845
## 465   0.2211182 0.6985012 FALSE SenatorCantwell 0.768152227 0.67794643
## 468   0.2211182 0.6985012 FALSE SenatorCantwell 0.201865129 0.65255494
## 473   0.2211182 0.6985012 FALSE SenatorCantwell 0.666953575 0.52342865
## 474   0.2211182 0.6985012 FALSE SenatorCantwell 0.403377224 0.58785988
## 480   0.2211182 0.6985012 FALSE SenatorCantwell 0.247486591 0.52340740
## 483   0.2211182 0.6985012 FALSE SenatorCantwell 0.105485584 0.52956969
## 484   0.2211182 0.6985012 FALSE SenatorCantwell 0.522776768 0.53310510
## 493   0.2226792 0.8702001 FALSE     brianschatz 0.297113820 0.64570703
## 494   0.2226792 0.8702001 FALSE     brianschatz 0.490812446 0.52866405
## 495   0.2226792 0.8702001 FALSE     brianschatz 0.364839522 0.67214694
## 496   0.2226792 0.8702001 FALSE     brianschatz 0.373786700 0.72353565
## 499   0.2226792 0.8702001 FALSE     brianschatz 0.463213887 0.54877079
## 501   0.2226792 0.8702001 FALSE     brianschatz 0.410630073 0.59551909
## 502   0.2226792 0.8702001 FALSE     brianschatz 0.677995059 0.98874262
## 509   0.2226792 0.8702001 FALSE     brianschatz 0.385216575 0.85737934
## 510   0.2226792 0.8702001 FALSE     brianschatz 0.256911079 0.65691879
## 511   0.2226792 0.8702001 FALSE     brianschatz 0.649758296 0.68232940
## 513   0.2226792 0.8702001 FALSE     brianschatz 0.405209643 0.54720156
## 519   0.2512095 0.4986862 FALSE SenKamalaHarris 0.202482808 0.60670862
## 520   0.2512095 0.4986862 FALSE SenKamalaHarris 0.345082408 0.45376537
## 521   0.2512095 0.4986862 FALSE SenKamalaHarris 0.378660640 0.72146727
## 522   0.2512095 0.4986862 FALSE SenKamalaHarris 0.265527794 0.75081692
## 524   0.2512095 0.4986862 FALSE SenKamalaHarris 0.422885687 0.44738586
## 528   0.2512095 0.4986862 FALSE SenKamalaHarris 0.358721934 0.31177445
## 530   0.2512095 0.4986862 FALSE SenKamalaHarris 0.541546279 0.75372930
## 531   0.2512095 0.4986862 FALSE SenKamalaHarris 0.282377345 0.47713847
## 535   0.2512095 0.4986862 FALSE SenKamalaHarris 0.501093976 0.63942106
## 538   0.2512095 0.4986862 FALSE SenKamalaHarris 0.377705314 0.68542546
## 539   0.2512095 0.4986862 FALSE SenKamalaHarris 0.259068647 0.60729989
## 541   0.2512095 0.4986862 FALSE SenKamalaHarris 0.116313224 0.50733935
## 542   0.2512095 0.4986862 FALSE SenKamalaHarris 0.401857279 0.56483560
## 544   0.2512095 0.4986862 FALSE SenKamalaHarris 0.263911475 0.51973891
## 545   0.2512095 0.4986862 FALSE SenKamalaHarris 0.423031830 0.64292680
## 546   0.2512095 0.4986862 FALSE SenKamalaHarris 0.481270691 0.50798986
## 547   0.2512095 0.4986862 FALSE SenKamalaHarris 0.558663803 0.38396863
## 555   0.2512095 0.4986862 FALSE SenKamalaHarris 0.399965168 0.83257732
## 559   0.2512095 0.4986862 FALSE SenKamalaHarris 0.361178910 0.48398924
## 560   0.2512095 0.4986862 FALSE SenKamalaHarris 0.497700932 0.61092195
## 565   0.2512095 0.4986862 FALSE SenKamalaHarris 0.551006364 0.65164810
## 568   0.2512095 0.4986862 FALSE SenKamalaHarris 0.257447789 0.49232611
## 575   0.2512095 0.4986862 FALSE SenKamalaHarris 0.388071058 0.54712321
## 577   0.2512095 0.4986862 FALSE SenKamalaHarris 0.502714538 0.47410889
## 578   0.2512095 0.4986862 FALSE SenKamalaHarris 0.769387190 0.66919359
## 587   0.2512095 0.4986862 FALSE SenKamalaHarris 0.396527860 0.63699313
## 588   0.2512095 0.4986862 FALSE SenKamalaHarris 0.294987476 0.59906204
## 592   0.2512095 0.4986862 FALSE SenKamalaHarris 0.438698920 0.58464108
## 596   0.2512095 0.4986862 FALSE SenKamalaHarris 0.529394326 0.66708258
## 600   0.2512095 0.4986862 FALSE SenKamalaHarris 0.798195434 0.75031076
## 612   0.2608729 0.6322347 FALSE   SenGillibrand 0.483909513 0.52022012
## 613   0.2608729 0.6322347 FALSE   SenGillibrand 0.389888882 0.65068866
## 614   0.2608729 0.6322347 FALSE   SenGillibrand 0.401150911 0.58314450
## 615   0.2608729 0.6322347 FALSE   SenGillibrand 0.436843254 0.59961616
## 616   0.2608729 0.6322347 FALSE   SenGillibrand 0.548395712 0.66055918
## 617   0.2608729 0.6322347 FALSE   SenGillibrand 0.405197333 0.67939590
## 620   0.2608729 0.6322347 FALSE   SenGillibrand 0.264160099 0.63857594
## 621   0.2608729 0.6322347 FALSE   SenGillibrand 0.681462829 0.62500730
## 622   0.2608729 0.6322347 FALSE   SenGillibrand 0.369956969 0.69383128
## 623   0.2608729 0.6322347 FALSE   SenGillibrand 0.267124122 0.60826749
## 630   0.2608729 0.6322347 FALSE   SenGillibrand 0.354752490 0.64775868
## 631   0.2608729 0.6322347 FALSE   SenGillibrand 0.464692023 0.44601109
## 633   0.2608729 0.6322347 FALSE   SenGillibrand 0.396240704 0.83463288
## 638   0.2608729 0.6322347 FALSE   SenGillibrand 0.296913069 0.48718391
## 640   0.2608729 0.6322347 FALSE   SenGillibrand 0.497945481 0.64983795
## 642   0.2608729 0.6322347 FALSE   SenGillibrand 0.217183404 0.63049319
## 643   0.2608729 0.6322347 FALSE   SenGillibrand 0.551935786 0.49638702
## 644   0.2608729 0.6322347 FALSE   SenGillibrand 0.506177261 0.48457074
## 649   0.2608729 0.6322347 FALSE   SenGillibrand 0.372045285 0.72695383
## 650   0.2608729 0.6322347 FALSE   SenGillibrand 0.272731188 0.49937942
## 652   0.2608729 0.6322347 FALSE   SenGillibrand 0.465547608 0.80955069
## 653   0.2608729 0.6322347 FALSE   SenGillibrand 0.521419331 0.53020209
## 655   0.2608729 0.6322347 FALSE   SenGillibrand 0.219081325 0.55183707
## 656   0.2608729 0.6322347 FALSE   SenGillibrand 0.417447449 0.65534957
## 657   0.2608729 0.6322347 FALSE   SenGillibrand 0.493580851 0.70161697
## 669   0.2608729 0.6322347 FALSE   SenGillibrand 0.280631516 0.62763994
## 677   0.2608729 0.6322347 FALSE   SenGillibrand 0.338916402 0.55431265
## 678   0.2608729 0.6322347 FALSE   SenGillibrand 0.265888593 0.75079904
## 679   0.2608729 0.6322347 FALSE   SenGillibrand 0.370045156 0.49995869
## 680   0.2608729 0.6322347 FALSE   SenGillibrand 0.355350554 0.46474864
## 682   0.2608729 0.6322347 FALSE   SenGillibrand 0.272513547 0.56576981
## 684   0.2608729 0.6322347 FALSE   SenGillibrand 0.389360569 0.56680816
## 689   0.2608729 0.6322347 FALSE   SenGillibrand 0.429446167 0.45818597
## 694   0.2608729 0.6322347 FALSE   SenGillibrand 0.273904006 0.71099606
## 708   0.2608729 0.6322347 FALSE   SenGillibrand 0.443559136 0.77653508
## 710   0.2608729 0.6322347 FALSE   SenGillibrand 0.602741931 0.57856184
## 711   0.2608729 0.6322347 FALSE   SenGillibrand 0.493293341 0.59233339
## 712   0.2608729 0.6322347 FALSE   SenGillibrand 0.382348112 0.46401210
## 714   0.2608729 0.6322347 FALSE   SenGillibrand 0.111581988 0.52364529
## 724   0.2669452 0.7757767 FALSE      SenSanders 0.404624299 0.54686022
## 725   0.2669452 0.7757767 FALSE      SenSanders 0.553526416 0.61426910
## 726   0.2669452 0.7757767 FALSE      SenSanders 0.298979793 0.64624623
## 727   0.2669452 0.7757767 FALSE      SenSanders 0.596361654 0.81932246
## 728   0.2669452 0.7757767 FALSE      SenSanders 0.493387396 0.71521885
## 729   0.2669452 0.7757767 FALSE      SenSanders 0.362617076 0.67035066
## 730   0.2669452 0.7757767 FALSE      SenSanders 0.409304597 0.59454664
## 731   0.2669452 0.7757767 FALSE      SenSanders 0.395333591 0.67011496
## 735   0.2669452 0.7757767 FALSE      SenSanders 0.397914705 0.57636034
## 736   0.2669452 0.7757767 FALSE      SenSanders 0.369896780 0.71830889
## 737   0.2669452 0.7757767 FALSE      SenSanders 0.638785091 0.80218227
## 739   0.2669452 0.7757767 FALSE      SenSanders 0.347830637 0.56005731
## 741   0.2669452 0.7757767 FALSE      SenSanders 0.754055005 0.62366076
## 743   0.2669452 0.7757767 FALSE      SenSanders 0.233870287 0.72000431
## 744   0.2669452 0.7757767 FALSE      SenSanders 0.272587958 0.60906248
## 751   0.2669452 0.7757767 FALSE      SenSanders 0.489556778 0.52760990
## 755   0.2669452 0.7757767 FALSE      SenSanders 0.388290711 0.84326253
## 756   0.2669452 0.7757767 FALSE      SenSanders 0.387867038 0.46702242
## 761   0.2669452 0.7757767 FALSE      SenSanders 0.261929540 0.65721236
## 769   0.2669452 0.7757767 FALSE      SenSanders 0.535333119 0.77070116
## 774   0.2669452 0.7757767 FALSE      SenSanders 0.682754724 0.80349543
## 775   0.2669452 0.7757767 FALSE      SenSanders 0.469917776 0.45036008
## 777   0.2669452 0.7757767 FALSE      SenSanders 0.421375575 0.67286437
## 778   0.2669452 0.7757767 FALSE      SenSanders 0.337572276 0.77051237
## 780   0.2734336 0.5840768 FALSE     SenBobCasey 0.367550237 0.49759299
## 781   0.2734336 0.5840768 FALSE     SenBobCasey 0.522203587 0.42361285
## 782   0.2734336 0.5840768 FALSE     SenBobCasey 0.394353816 0.53486104
## 784   0.2734336 0.5840768 FALSE     SenBobCasey 0.609877147 0.70591952
## 785   0.2734336 0.5840768 FALSE     SenBobCasey 0.214023716 0.61729635
## 786   0.2734336 0.5840768 FALSE     SenBobCasey 0.550349795 0.60051361
## 787   0.2734336 0.5840768 FALSE     SenBobCasey 0.549098965 0.65664570
## 788   0.2734336 0.5840768 FALSE     SenBobCasey 0.399793723 0.57640237
## 789   0.2734336 0.5840768 FALSE     SenBobCasey 0.274607063 0.49947601
## 790   0.2734336 0.5840768 FALSE     SenBobCasey 0.436467074 0.59360161
## 791   0.2734336 0.5840768 FALSE     SenBobCasey 0.274938418 0.63578160
## 792   0.2734336 0.5840768 FALSE     SenBobCasey 0.561381041 0.38923933
## 798   0.2734336 0.5840768 FALSE     SenBobCasey 0.288987861 0.60276297
## 800   0.2734336 0.5840768 FALSE     SenBobCasey 0.427597833 0.45618971
## 802   0.2734336 0.5840768 FALSE     SenBobCasey 0.231513365 0.67576486
## 803   0.2734336 0.5840768 FALSE     SenBobCasey 0.374326737 0.68817206
## 807   0.2734336 0.5840768 FALSE     SenBobCasey 0.289774276 0.67259272
## 809   0.2734336 0.5840768 FALSE     SenBobCasey 0.358354517 0.63837071
## 811   0.2734336 0.5840768 FALSE     SenBobCasey 0.460529734 0.39348270
## 812   0.2734336 0.5840768 FALSE     SenBobCasey 0.380463856 0.46250807
## 816   0.2734336 0.5840768 FALSE     SenBobCasey 0.387157764 0.56053236
## 818   0.2734336 0.5840768 FALSE     SenBobCasey 0.454057838 0.53527547
## 823   0.2734336 0.5840768 FALSE     SenBobCasey 0.482456800 0.51667275
## 826   0.2734336 0.5840768 FALSE     SenBobCasey 0.414248248 0.76608947
## 829   0.2734336 0.5840768 FALSE     SenBobCasey 0.267790921 0.75079101
## 850   0.2734336 0.5840768 FALSE     SenBobCasey 0.398890600 0.83308703
## 857   0.2734336 0.5840768 FALSE     SenBobCasey 0.334890506 0.54903252
## 858   0.2734336 0.5840768 FALSE     SenBobCasey 0.226825296 0.54557644
## 864   0.2734336 0.5840768 FALSE     SenBobCasey 0.297025515 0.48721158
## 876   0.2734336 0.5840768 FALSE     SenBobCasey 0.351638032 0.74613855
## 877   0.2734336 0.5840768 FALSE     SenBobCasey 0.710136635 0.55969509
## 879   0.2749538 0.4744784 FALSE     PattyMurray 0.226959714 0.67419323
## 880   0.2749538 0.4744784 FALSE     PattyMurray 0.518903297 0.41589894
## 883   0.2749538 0.4744784 FALSE     PattyMurray 0.552141836 0.64965431
## 885   0.2749538 0.4744784 FALSE     PattyMurray 0.450430046 0.77052513
## 886   0.2749538 0.4744784 FALSE     PattyMurray 0.403981470 0.56096696
## 887   0.2749538 0.4744784 FALSE     PattyMurray 0.360997486 0.47928357
## 888   0.2749538 0.4744784 FALSE     PattyMurray 0.399296862 0.63448821
## 891   0.2749538 0.4744784 FALSE     PattyMurray 0.263095489 0.60733370
## 894   0.2749538 0.4744784 FALSE     PattyMurray 0.372740959 0.44984984
## 896   0.2749538 0.4744784 FALSE     PattyMurray 0.650257555 0.66113031
## 897   0.2749538 0.4744784 FALSE     PattyMurray 0.299994560 0.59747990
## 898   0.2749538 0.4744784 FALSE     PattyMurray 0.380472636 0.68379974
## 901   0.2749538 0.4744784 FALSE     PattyMurray 0.687835029 0.78997909
## 903   0.2749538 0.4744784 FALSE     PattyMurray 0.458720706 0.43446670
## 905   0.2749538 0.4744784 FALSE     PattyMurray 0.454038938 0.52230437
## 911   0.2749538 0.4744784 FALSE     PattyMurray 0.203976112 0.60744298
## 914   0.2749538 0.4744784 FALSE     PattyMurray 0.422321121 0.44511361
## 916   0.2749538 0.4744784 FALSE     PattyMurray 0.115935376 0.50432757
## 922   0.2749538 0.4744784 FALSE     PattyMurray 0.267609507 0.75078553
## 928   0.2749538 0.4744784 FALSE     PattyMurray 0.390130452 0.54272049
## 936   0.2749538 0.4744784 FALSE     PattyMurray 0.393968165 0.51702145
## 937   0.2749538 0.4744784 FALSE     PattyMurray 0.481524161 0.50530959
## 939   0.2749538 0.4744784 FALSE     PattyMurray 0.277694689 0.71066244
## 940   0.2749538 0.4744784 FALSE     PattyMurray 0.530735144 0.66509033
## 943   0.2749538 0.4744784 FALSE     PattyMurray 0.268715501 0.48083846
## 948   0.2749538 0.4744784 FALSE     PattyMurray 0.495286687 0.57751325
## 953   0.2756657 0.6607710 FALSE       SenMarkey 0.457249056 0.54240662
## 954   0.2756657 0.6607710 FALSE       SenMarkey 0.403085266 0.58736627
## 955   0.2756657 0.6607710 FALSE       SenMarkey 0.274160913 0.60906621
## 956   0.2756657 0.6607710 FALSE       SenMarkey 0.437855714 0.60339704
## 957   0.2756657 0.6607710 FALSE       SenMarkey 0.598506647 0.81199412
## 958   0.2756657 0.6607710 FALSE       SenMarkey 0.368440829 0.69702454
## 959   0.2756657 0.6607710 FALSE       SenMarkey 0.282915891 0.67492622
## 960   0.2756657 0.6607710 FALSE       SenMarkey 0.466467759 0.44776944
## 961   0.2756657 0.6607710 FALSE       SenMarkey 0.272378488 0.65442978
## 962   0.2756657 0.6607710 FALSE       SenMarkey 0.399421520 0.54269428
## 964   0.2756657 0.6607710 FALSE       SenMarkey 0.276583840 0.56614345
## 966   0.2756657 0.6607710 FALSE       SenMarkey 0.373114956 0.40185087
## 968   0.2756657 0.6607710 FALSE       SenMarkey 0.431510686 0.45997751
## 970   0.2756657 0.6607710 FALSE       SenMarkey 0.372902027 0.50199716
## 971   0.2756657 0.6607710 FALSE       SenMarkey 0.389664657 0.65540418
## 973   0.2756657 0.6607710 FALSE       SenMarkey 0.354509709 0.65398203
## 976   0.2756657 0.6607710 FALSE       SenMarkey 0.710697294 0.56374364
## 977   0.2756657 0.6607710 FALSE       SenMarkey 0.241678906 0.68427947
## 982   0.2756657 0.6607710 FALSE       SenMarkey 0.268835469 0.75084827
## 983   0.2756657 0.6607710 FALSE       SenMarkey 0.548275987 0.66282203
## 984   0.2756657 0.6607710 FALSE       SenMarkey 0.681546907 0.62667078
## 985   0.2756657 0.6607710 FALSE       SenMarkey 0.346827366 0.74917922
## 988   0.2756657 0.6607710 FALSE       SenMarkey 0.337098976 0.77900549
## 995   0.2756657 0.6607710 FALSE       SenMarkey 0.485367783 0.52274491
## 996   0.2756657 0.6607710 FALSE       SenMarkey 0.553013742 0.49844197
## 1003  0.2756657 0.6607710 FALSE       SenMarkey 0.254939415 0.52340634
## 1004  0.2756657 0.6607710 FALSE       SenMarkey 0.299527187 0.48768738
## 1006  0.2756657 0.6607710 FALSE       SenMarkey 0.442696621 0.77769441
## 1011  0.2756657 0.6607710 FALSE       SenMarkey 0.550773384 0.60680711
## 1013  0.2756657 0.6607710 FALSE       SenMarkey 0.215613762 0.63826942
## 1014  0.2756657 0.6607710 FALSE       SenMarkey 0.289909204 0.64192276
## 1019  0.2756657 0.6607710 FALSE       SenMarkey 0.730436620 0.68318051
## 1020  0.2756657 0.6607710 FALSE       SenMarkey 0.563796674 0.39227272
## 1022  0.2756657 0.6607710 FALSE       SenMarkey 0.666651930 0.52261970
## 1028  0.2756657 0.6607710 FALSE       SenMarkey 0.384785404 0.46556598
## 1042  0.2756657 0.6607710 FALSE       SenMarkey 0.275049318 0.49947823
## 1044  0.2756657 0.6607710 FALSE       SenMarkey 0.417180809 0.65926649
## 1053  0.2756657 0.6607710 FALSE       SenMarkey 0.342951985 0.55758957
## 1056  0.2756657 0.6607710 FALSE       SenMarkey 0.495772422 0.62526340
## 1061  0.2756657 0.6607710 FALSE       SenMarkey 0.644703521 0.47051681
## 1066  0.2756657 0.6607710 FALSE       SenMarkey 0.370728360 0.72864075
## 1071  0.2756657 0.6607710 FALSE       SenMarkey 0.493986880 0.59528591
## 1084  0.2756657 0.6607710 FALSE       SenMarkey 0.468492243 0.74374259
## 1091  0.2756657 0.6607710 FALSE       SenMarkey 0.558325031 0.47996435
## 1114  0.2756657 0.6607710 FALSE       SenMarkey 0.219075904 0.55183989
## 1130  0.2756657 0.6607710 FALSE       SenMarkey 0.410146993 0.77009306
## 1136  0.2768264 0.5411446 FALSE  MartinHeinrich 0.267997141 0.75079884
## 1137  0.2768264 0.5411446 FALSE  MartinHeinrich 0.686427973 0.71890560
## 1138  0.2768264 0.5411446 FALSE  MartinHeinrich 0.500072634 0.64144379
## 1139  0.2768264 0.5411446 FALSE  MartinHeinrich 0.425341157 0.45298889
## 1141  0.2768264 0.5411446 FALSE  MartinHeinrich 0.377563058 0.45948671
## 1142  0.2768264 0.5411446 FALSE  MartinHeinrich 0.264124430 0.52009187
## 1145  0.2768264 0.5411446 FALSE  MartinHeinrich 0.547210334 0.56252639
## 1147  0.2768264 0.5411446 FALSE  MartinHeinrich 0.665276311 0.51591138
## 1149  0.2768264 0.5411446 FALSE  MartinHeinrich 0.392663734 0.52821067
## 1152  0.2768264 0.5411446 FALSE  MartinHeinrich 0.275908249 0.63577219
## 1153  0.2768264 0.5411446 FALSE  MartinHeinrich 0.265185745 0.60760953
## 1157  0.2768264 0.5411446 FALSE  MartinHeinrich 0.448270347 0.77196167
## 1158  0.2768264 0.5411446 FALSE  MartinHeinrich 0.353692522 0.74525808
## 1161  0.2768264 0.5411446 FALSE  MartinHeinrich 0.296758444 0.59836858
## 1167  0.2768264 0.5411446 FALSE  MartinHeinrich 0.717812417 0.48627986
## 1169  0.2768264 0.5411446 FALSE  MartinHeinrich 0.378768467 0.72140593
## 1170  0.2768264 0.5411446 FALSE  MartinHeinrich 0.400373833 0.56932689
## 1178  0.2768264 0.5411446 FALSE  MartinHeinrich 0.453239651 0.53029007
## 1180  0.2768264 0.5411446 FALSE  MartinHeinrich 0.481492104 0.51246890
## 1181  0.2768264 0.5411446 FALSE  MartinHeinrich 0.550010573 0.49038107
## 1182  0.2768264 0.5411446 FALSE  MartinHeinrich 0.648910246 0.66440149
## 1183  0.2768264 0.5411446 FALSE  MartinHeinrich 0.350657444 0.46132664
## 1186  0.2768264 0.5411446 FALSE  MartinHeinrich 0.503503344 0.47835151
## 1189  0.2768264 0.5411446 FALSE  MartinHeinrich 0.362423609 0.63350137
## 1190  0.2768264 0.5411446 FALSE  MartinHeinrich 0.671904475 0.56718512
## 1191  0.2768264 0.5411446 FALSE  MartinHeinrich 0.377438490 0.68560874
## 1200  0.2768264 0.5411446 FALSE  MartinHeinrich 0.550152792 0.65350482
## 1205  0.2768264 0.5411446 FALSE  MartinHeinrich 0.275403150 0.55915449
## 1207  0.2768264 0.5411446 FALSE  MartinHeinrich 0.386778466 0.55282348
## 1214  0.2768264 0.5411446 FALSE  MartinHeinrich 0.555112941 0.47248181
## 1216  0.2768264 0.5411446 FALSE  MartinHeinrich 0.550809742 0.59700096
## 1218  0.2768264 0.5411446 FALSE  MartinHeinrich 0.559846709 0.70741004
## 1219  0.2768264 0.5411446 FALSE  MartinHeinrich 0.416310212 0.76465385
## 1220  0.2768264 0.5411446 FALSE  MartinHeinrich 0.528482847 0.66872452
## 1222  0.2768264 0.5411446 FALSE  MartinHeinrich 0.331647432 0.53805533
## 1226  0.2768264 0.5411446 FALSE  MartinHeinrich 0.400376248 0.83239857
## 1233  0.2768264 0.5411446 FALSE  MartinHeinrich 0.520780969 0.42109962
## 1237  0.2768264 0.5411446 FALSE  MartinHeinrich 0.493393959 0.58332406
## 1248  0.2768264 0.5411446 FALSE  MartinHeinrich 0.473690079 0.73603550
## 1251  0.2768264 0.5411446 FALSE  MartinHeinrich 0.469715172 0.80571905
## 1257  0.2768264 0.5411446 FALSE  MartinHeinrich 0.232213894 0.53374543
## 1265  0.2768264 0.5411446 FALSE  MartinHeinrich 0.410924168 0.66984984
## 1268  0.2768264 0.5411446 FALSE  MartinHeinrich 0.275655741 0.49946856
## 1302  0.2768264 0.5411446 FALSE  MartinHeinrich 0.496705098 0.61347027
## 1314  0.2768264 0.5411446 FALSE  MartinHeinrich 0.519767782 0.52295275
## 1321  0.2779848 0.7356608 FALSE   SenatorTester 0.460808001 0.54672083
## 1322  0.2779848 0.7356608 FALSE   SenatorTester 0.276975276 0.56614419
## 1323  0.2779848 0.7356608 FALSE   SenatorTester 0.510419753 0.48984269
## 1324  0.2779848 0.7356608 FALSE   SenatorTester 0.232184319 0.84707755
## 1325  0.2779848 0.7356608 FALSE   SenatorTester 0.300663348 0.48781761
## 1326  0.2779848 0.7356608 FALSE   SenatorTester 0.556435213 0.72135590
## 1327  0.2779848 0.7356608 FALSE   SenatorTester 0.391613638 0.83862642
## 1328  0.2779848 0.7356608 FALSE   SenatorTester 0.746034146 0.52941604
## 1331  0.2779848 0.7356608 FALSE   SenatorTester 0.525219241 0.53675688
## 1334  0.2779848 0.7356608 FALSE   SenatorTester 0.608427669 0.71592285
## 1342  0.2779848 0.7356608 FALSE   SenatorTester 0.548999216 0.66898278
## 1343  0.2779848 0.7356608 FALSE   SenatorTester 0.439260738 0.78475119
## 1346  0.2779848 0.7356608 FALSE   SenatorTester 0.469242349 0.44992355
## 1349  0.2779848 0.7356608 FALSE   SenatorTester 0.537871193 0.81619476
## 1351  0.2779848 0.7356608 FALSE   SenatorTester 0.299205544 0.64630095
## 1355  0.2779848 0.7356608 FALSE   SenatorTester 0.367528727 0.71240742
## 1356  0.2779848 0.7356608 FALSE   SenatorTester 0.376210221 0.50369860
## 1357  0.2779848 0.7356608 FALSE   SenatorTester 0.526285279 0.68502434
## 1366  0.2779848 0.7356608 FALSE   SenatorTester 0.242046217 0.71217664
## 1368  0.2779848 0.7356608 FALSE   SenatorTester 0.396745358 0.57554368
## 1374  0.2779848 0.7356608 FALSE   SenatorTester 0.339214663 0.75956300
## 1381  0.2779848 0.7356608 FALSE   SenatorTester 0.629354695 0.43251394
## 1382  0.2779848 0.7356608 FALSE   SenatorTester 0.330297216 0.78418759
## 1386  0.2779848 0.7356608 FALSE   SenatorTester 0.465899642 0.39749477
## 1389  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.495463308 0.59906320
## 1390  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.551627443 0.61001565
## 1391  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.287062558 0.68302212
## 1395  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.711253239 0.56581404
## 1396  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.368495529 0.73243544
## 1403  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.548460712 0.66604939
## 1410  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.366830905 0.70383733
## 1411  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.301470065 0.64672953
## 1413  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.213041454 0.64330936
## 1417  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.683510945 0.79884002
## 1419  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.246114108 0.69804909
## 1422  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.360705031 0.46699485
## 1423  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.468749716 0.44958521
## 1425  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.673031784 0.57642384
## 1426  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.277972121 0.60866138
## 1428  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.337053832 0.77902901
## 1429  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.345246186 0.75056547
## 1432  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.387596914 0.46691482
## 1433  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.302021186 0.48790468
## 1435  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.622999328 0.77222688
## 1436  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.395695724 0.57472086
## 1437  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.357353424 0.66359207
## 1440  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.554629397 0.50086634
## 1443  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.496752503 0.62923616
## 1448  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.232242355 0.84710149
## 1453  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.402937327 0.54575045
## 1454  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.404029660 0.68901571
## 1461  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.395372771 0.83524038
## 1464  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.109693990 0.52594062
## 1466  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.487555775 0.52559870
## 1469  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.639705435 0.79701120
## 1476  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.287749379 0.71264657
## 1484  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.681838200 0.62891474
## 1485  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.277118840 0.49938449
## 1489  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.628703036 0.43172664
## 1495  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.434077944 0.46172583
## 1503  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.831589491 0.70428303
## 1504  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.539148859 0.81291071
## 1505  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.459756927 0.54564056
## 1507  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.823609071 0.61624075
## 1508  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.718997462 0.72001084
## 1522  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.603975396 0.58331225
## 1529  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.768155009 0.67801776
## 1530  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.417973155 0.66525035
## 1532  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.376213104 0.50369982
## 1534  0.2943128 0.6971773 FALSE  ChrisVanHollen 0.406509851 0.59198590
## 1540  0.3029414 0.4629216 FALSE   SenatorHassan 0.501098247 0.68992621
## 1541  0.3029414 0.4629216 FALSE   SenatorHassan 0.341895170 0.51643644
## 1544  0.3029414 0.4629216 FALSE   SenatorHassan 0.643672190 0.59759293
## 1548  0.3029414 0.4629216 FALSE   SenatorHassan 0.641925764 0.45932620
## 1550  0.3029414 0.4629216 FALSE   SenatorHassan 0.279079926 0.63600525
## 1552  0.3029414 0.4629216 FALSE   SenatorHassan 0.563035846 0.70311231
## 1554  0.3029414 0.4629216 FALSE   SenatorHassan 0.383152922 0.68263965
## 1555  0.3029414 0.4629216 FALSE   SenatorHassan 0.544276308 0.75106281
## 1559  0.3029414 0.4629216 FALSE   SenatorHassan 0.717647679 0.48204189
## 1563  0.3029414 0.4629216 FALSE   SenatorHassan 0.206043706 0.60867819
## 1565  0.3029414 0.4629216 FALSE   SenatorHassan 0.481868635 0.50347412
## 1568  0.3029414 0.4629216 FALSE   SenatorHassan 0.227513012 0.84567188
## 1569  0.3029414 0.4629216 FALSE   SenatorHassan 0.416712764 0.66536695
## 1576  0.3029414 0.4629216 FALSE   SenatorHassan 0.613490011 0.69929083
## 1577  0.3029414 0.4629216 FALSE   SenatorHassan 0.553180705 0.64813705
## 1583  0.3029414 0.4629216 FALSE   SenatorHassan 0.665440789 0.51100361
## 1585  0.3029414 0.4629216 FALSE   SenatorHassan 0.406342193 0.55796830
## 1588  0.3029414 0.4629216 FALSE   SenatorHassan 0.804919741 0.64891850
## 1594  0.3029414 0.4629216 FALSE   SenatorHassan 0.364846688 0.39671419
## 1595  0.3029414 0.4629216 FALSE   SenatorHassan 0.357722581 0.74411542
## 1596  0.3029414 0.4629216 FALSE   SenatorHassan 0.744092784 0.51633044
## 1602  0.3029414 0.4629216 FALSE   SenatorHassan 0.674177542 0.48271407
## 1607  0.3029414 0.4629216 FALSE   SenatorHassan 0.442223087 0.57905015
## 1634  0.3029414 0.4629216 FALSE   SenatorHassan 0.271773585 0.48446931
## 1641  0.3029414 0.4629216 FALSE   SenatorHassan 0.420421145 0.76258745
## 1643  0.3029414 0.4629216 FALSE   SenatorHassan 0.422144316 0.44412261
## 1644  0.3029414 0.4629216 FALSE   SenatorHassan 0.553040527 0.59062591
## 1648  0.3029414 0.4629216 FALSE   SenatorHassan 0.298061284 0.46493675
## 1649  0.3029414 0.4629216 FALSE   SenatorHassan 0.279349548 0.55978683
## 1650  0.3029414 0.4629216 FALSE   SenatorHassan 0.370036643 0.62866407
## 1653  0.3029414 0.4629216 FALSE   SenatorHassan 0.266901248 0.60797241
## 1685  0.3029414 0.4629216 FALSE   SenatorHassan 0.343743340 0.45034050
## 1687  0.3029414 0.4629216 FALSE   SenatorHassan 0.458576212 0.43375325
## 1688  0.3029414 0.4629216 FALSE   SenatorHassan 0.295232993 0.67219426
## 1689  0.3029414 0.4629216 FALSE   SenatorHassan 0.228035651 0.51532416
## 1699  0.3029414 0.4629216 FALSE   SenatorHassan 0.455668406 0.38679468
## 1701  0.3029414 0.4629216 FALSE   SenatorHassan 0.383574923 0.71931868
## 1710  0.3029414 0.4629216 FALSE   SenatorHassan 0.503906154 0.63540689
## 1714  0.3029414 0.4629216 FALSE   SenatorHassan 0.280262868 0.71076477
## 1726  0.3029414 0.4629216 FALSE   SenatorHassan 0.518796162 0.41543322
## 1732  0.3029414 0.4629216 FALSE   SenatorHassan 0.454789241 0.51996331
## 1738  0.3029414 0.4629216 FALSE   SenatorHassan 0.269802812 0.75094055
## 1740  0.3029414 0.4629216 FALSE   SenatorHassan 0.229320704 0.67488511
## 1747  0.3029414 0.4629216 FALSE   SenatorHassan 0.372488026 0.44873935
## 1767  0.3029414 0.4629216 FALSE   SenatorHassan 0.623509366 0.41953591
## 1770  0.3029414 0.4629216 FALSE   SenatorHassan 0.362004614 0.31335630
## 1775  0.3029414 0.4629216 FALSE   SenatorHassan 0.395563824 0.51346176
## 1776  0.3029414 0.4629216 FALSE   SenatorHassan 0.304661114 0.59697946
## 1779  0.3029414 0.4629216 FALSE   SenatorHassan 0.427704840 0.63861721
## 1788  0.3029414 0.4629216 FALSE   SenatorHassan 0.549678118 0.48371437
## 1798  0.3029414 0.4629216 FALSE   SenatorHassan 0.115793356 0.50362645
## 1800  0.3029414 0.4629216 FALSE   SenatorHassan 0.603802095 0.56654327
## 1803  0.3029414 0.4629216 FALSE   SenatorHassan 0.650887558 0.65994453
## 1807  0.3029414 0.4629216 FALSE   SenatorHassan 0.345279656 0.77641457
## 1827  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.290738275 0.64082566
## 1828  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.390425517 0.56869298
## 1829  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.353375783 0.74537982
## 1830  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.239585522 0.68165014
## 1831  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.390652837 0.64717446
## 1832  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.484449964 0.52123713
## 1833  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.471033240 0.73920558
## 1834  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.495453466 0.62136258
## 1835  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.401481580 0.58403479
## 1838  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.418042781 0.65248735
## 1840  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.398535316 0.54171491
## 1841  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.225697817 0.54685046
## 1842  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.279941010 0.49897591
## 1843  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.272947233 0.75150787
## 1846  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.373781785 0.68871678
## 1847  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.466182638 0.44751004
## 1849  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.399871021 0.83261949
## 1852  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.493243287 0.59203071
## 1856  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.550373895 0.60383791
## 1859  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.446149372 0.77372676
## 1862  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.285049741 0.56475346
## 1868  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.297824479 0.67242519
## 1881  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.285223167 0.62657217
## 1882  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.261203819 0.52160152
## 1883  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.374389527 0.40229171
## 1888  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.548562630 0.65923057
## 1891  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.835292001 0.56400702
## 1894  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.436786562 0.59929892
## 1897  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.113461502 0.52063258
## 1898  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.376596488 0.72278642
## 1903  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.542088135 0.80815527
## 1904  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.600037164 0.80920413
## 1915  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.414417943 0.76595934
## 1916  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.385516756 0.46595901
## 1920  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.803419980 0.65590675
## 1924  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.406832793 0.67552703
## 1932  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.463671481 0.39609840
## 1933  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.359374604 0.46657052
## 1943  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.431457054 0.45993576
## 1954  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.283761037 0.71133721
## 1964  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.354264949 0.97162934
## 1976  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.823325650 0.61289960
## 1986  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.506876328 0.48566654
## 1998  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.366296176 0.31462062
## 2018  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.356214837 0.64252954
## 2026  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.602704025 0.57831217
## 2027  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.557634117 0.47881771
## 2029  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.672077156 0.57218928
## 2043  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.373528048 0.50236813
## 2045  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.289427566 0.60329122
## 2055  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.552274335 0.49708399
## 2064  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.217147849 0.62783415
## 2079  0.3049818 0.6219774 FALSE  SenJeffMerkley 0.647872642 0.66887516
## 2082  0.3486256 0.8011896 FALSE    SenatorLeahy 0.551981012 0.67610798
## 2083  0.3486256 0.8011896 FALSE    SenatorLeahy 0.290816552 0.78320371
## 2084  0.3486256 0.8011896 FALSE    SenatorLeahy 0.638723254 0.80373390
## 2085  0.3486256 0.8011896 FALSE    SenatorLeahy 0.287192326 0.68295517
## 2086  0.3486256 0.8011896 FALSE    SenatorLeahy 0.503903088 0.66796788
## 2088  0.3486256 0.8011896 FALSE    SenatorLeahy 0.534050822 0.85850774
## 2089  0.3486256 0.8011896 FALSE    SenatorLeahy 0.272395977 0.65442070
## 2090  0.3486256 0.8011896 FALSE    SenatorLeahy 0.596222835 0.82064077
## 2091  0.3486256 0.8011896 FALSE    SenatorLeahy 0.557794611 0.72829990
## 2093  0.3486256 0.8011896 FALSE    SenatorLeahy 0.376313595 0.76334392
## 2095  0.3486256 0.8011896 FALSE    SenatorLeahy 0.440275516 0.46435109
## 2097  0.3486256 0.8011896 FALSE    SenatorLeahy 0.804411991 0.66477291
## 2099  0.3486256 0.8011896 FALSE    SenatorLeahy 0.416777257 0.59858219
## 2102  0.3486256 0.8011896 FALSE    SenatorLeahy 0.383066169 0.50550966
## 2104  0.3486256 0.8011896 FALSE    SenatorLeahy 0.391385226 0.83888199
## 2108  0.3486256 0.8011896 FALSE    SenatorLeahy 0.414562309 0.70759846
## 2113  0.3486256 0.8011896 FALSE    SenatorLeahy 0.381403155 0.72889288
## 2121  0.3486256 0.8011896 FALSE    SenatorLeahy 0.405428641 0.57968051
## 2123  0.3486256 0.8011896 FALSE    SenatorLeahy 0.556526373 0.61849736
## 2126  0.3486256 0.8011896 FALSE    SenatorLeahy 0.649413356 0.68150565
## 2137  0.3486256 0.8011896 FALSE    SenatorLeahy 0.632928339 0.43591161
## 2138  0.3486256 0.8011896 FALSE    SenatorLeahy 0.310897187 0.64626749
## 2141  0.3486256 0.8011896 FALSE    SenatorLeahy 0.404393513 0.67703360
## 2146  0.3486256 0.8011896 FALSE    SenatorLeahy 0.355853828 0.56163738
## 2149  0.3486256 0.8011896 FALSE    SenatorLeahy 0.281615050 0.60770018
## 2154  0.3486256 0.8011896 FALSE    SenatorLeahy 0.411450464 0.54969119
## 2161  0.3486256 0.8011896 FALSE    SenatorLeahy 0.684040998 0.63564244
## 2164  0.3566078 0.5366487 FALSE  SenatorShaheen 0.289321538 0.63983020
## 2165  0.3566078 0.5366487 FALSE  SenatorShaheen 0.317654083 0.48313393
## 2166  0.3566078 0.5366487 FALSE  SenatorShaheen 0.294844538 0.48962298
## 2167  0.3566078 0.5366487 FALSE  SenatorShaheen 0.522523448 0.42409634
## 2172  0.3566078 0.5366487 FALSE  SenatorShaheen 0.429757019 0.45848192
## 2173  0.3566078 0.5366487 FALSE  SenatorShaheen 0.502318056 0.63746468
## 2177  0.3566078 0.5366487 FALSE  SenatorShaheen 0.649667917 0.66240443
## 2178  0.3566078 0.5366487 FALSE  SenatorShaheen 0.453244989 0.53037448
## 2180  0.3566078 0.5366487 FALSE  SenatorShaheen 0.674431777 0.48783987
## 2181  0.3566078 0.5366487 FALSE  SenatorShaheen 0.439586476 0.58289005
## 2183  0.3566078 0.5366487 FALSE  SenatorShaheen 0.551679594 0.65041542
## 2187  0.3566078 0.5366487 FALSE  SenatorShaheen 0.427851245 0.63851403
## 2189  0.3566078 0.5366487 FALSE  SenatorShaheen 0.377971035 0.40313915
## 2190  0.3566078 0.5366487 FALSE  SenatorShaheen 0.231994464 0.84700041
## 2191  0.3566078 0.5366487 FALSE  SenatorShaheen 0.504226238 0.48055739
## 2192  0.3566078 0.5366487 FALSE  SenatorShaheen 0.500464063 0.69049920
## 2193  0.3566078 0.5366487 FALSE  SenatorShaheen 0.237165587 0.67933138
## 2196  0.3566078 0.5366487 FALSE  SenatorShaheen 0.543896273 0.75138763
## 2197  0.3566078 0.5366487 FALSE  SenatorShaheen 0.550243110 0.49149154
## 2198  0.3566078 0.5366487 FALSE  SenatorShaheen 0.671961421 0.56647533
## 2199  0.3566078 0.5366487 FALSE  SenatorShaheen 0.481666377 0.51354228
## 2202  0.3566078 0.5366487 FALSE  SenatorShaheen 0.642537084 0.60127106
## 2205  0.3566078 0.5366487 FALSE  SenatorShaheen 0.710138571 0.55687360
## 2211  0.3566078 0.5366487 FALSE  SenatorShaheen 0.386959473 0.71850822
## 2212  0.3566078 0.5366487 FALSE  SenatorShaheen 0.303357175 0.67387070
## 2213  0.3566078 0.5366487 FALSE  SenatorShaheen 0.561758645 0.38978179
## 2215  0.3566078 0.5366487 FALSE  SenatorShaheen 0.717857468 0.48662260
## 2219  0.3566078 0.5366487 FALSE  SenatorShaheen 0.357691852 0.97138472
## 2220  0.3566078 0.5366487 FALSE  SenatorShaheen 0.636270321 0.83176586
## 2223  0.3566078 0.5366487 FALSE  SenatorShaheen 0.682213198 0.61848397
## 2227  0.3566078 0.5366487 FALSE  SenatorShaheen 0.213971601 0.61720361
## 2233  0.3566078 0.5366487 FALSE  SenatorShaheen 0.295150965 0.57169302
## 2236  0.3566078 0.5366487 FALSE  SenatorShaheen 0.374561322 0.62731349
## 2239  0.3566078 0.5366487 FALSE  SenatorShaheen 0.275722433 0.75236814
## 2246  0.3566078 0.5366487 FALSE  SenatorShaheen 0.464095544 0.44533411
## 2249  0.3566078 0.5366487 FALSE  SenatorShaheen 0.665279684 0.51596250
## 2251  0.3566078 0.5366487 FALSE  SenatorShaheen 0.551352293 0.59483768
## 2254  0.3566078 0.5366487 FALSE  SenatorShaheen 0.278564325 0.61457080
## 2273  0.3566078 0.5366487 FALSE  SenatorShaheen 0.349379613 0.77620102
## 2279  0.3566078 0.5366487 FALSE  SenatorShaheen 0.612348767 0.70092099
## 2296  0.3566078 0.5366487 FALSE  SenatorShaheen 0.846103905 0.48748306
## 2302  0.3566078 0.5366487 FALSE  SenatorShaheen 0.835173839 0.56006839
## 2309  0.3566078 0.5366487 FALSE  SenatorShaheen 0.519783124 0.52314742
## 2310  0.3566078 0.5366487 FALSE  SenatorShaheen 0.287170594 0.71240950
## 2311  0.3566078 0.5366487 FALSE  SenatorShaheen 0.317923140 0.60058765
## 2316  0.3566078 0.5366487 FALSE  SenatorShaheen 0.403572844 0.63181014
## 2317  0.3566078 0.5366487 FALSE  SenatorShaheen 0.602682361 0.57120749
## 2326  0.3566078 0.5366487 FALSE  SenatorShaheen 0.387983073 0.54737611
## 2331  0.3566078 0.5366487 FALSE  SenatorShaheen 0.387019319 0.46667235
## 2335  0.3566078 0.5366487 FALSE  SenatorShaheen 0.402945972 0.56265231
## 2343  0.3566078 0.5366487 FALSE  SenatorShaheen 0.386653444 0.68164374
## 2345  0.3566078 0.5366487 FALSE  SenatorShaheen 0.301786796 0.53973805
## 2350  0.3577542 0.9963846 FALSE       SenShelby 0.642559766 0.81726282
## 2351  0.3577542 0.9963846 FALSE       SenShelby 0.543648528 0.78885032
## 2355  0.3577542 0.9963846 FALSE       SenShelby 0.559702515 0.68400488
## 2356  0.3577542 0.9963846 FALSE       SenShelby 0.512076179 0.67423569
## 2358  0.3577542 0.9963846 FALSE       SenShelby 0.231541250 0.72122474
## 2359  0.3577542 0.9963846 FALSE       SenShelby 0.536992599 0.87954330
## 2361  0.3625031 0.7686541 FALSE SenatorTomUdall 0.406023881 0.57982540
## 2362  0.3625031 0.7686541 FALSE SenatorTomUdall 0.291341442 0.68024589
## 2363  0.3625031 0.7686541 FALSE SenatorTomUdall 0.438825220 0.78637644
## 2364  0.3625031 0.7686541 FALSE SenatorTomUdall 0.285636991 0.56454064
## 2366  0.3625031 0.7686541 FALSE SenatorTomUdall 0.243512956 0.70961310
## 2368  0.3625031 0.7686541 FALSE SenatorTomUdall 0.404271783 0.67697853
## 2372  0.3625031 0.7686541 FALSE SenatorTomUdall 0.515448994 0.49352808
## 2373  0.3625031 0.7686541 FALSE SenatorTomUdall 0.474777339 0.45270489
## 2374  0.3625031 0.7686541 FALSE SenatorTomUdall 0.501622480 0.60704987
## 2377  0.3625031 0.7686541 FALSE SenatorTomUdall 0.675390866 0.58165476
## 2378  0.3625031 0.7686541 FALSE SenatorTomUdall 0.314109121 0.64525168
## 2379  0.3625031 0.7686541 FALSE SenatorTomUdall 0.682837985 0.80252922
## 2380  0.3625031 0.7686541 FALSE SenatorTomUdall 0.357242879 0.56164068
## 2386  0.3625031 0.7686541 FALSE SenatorTomUdall 0.537650355 0.81694917
## 2389  0.3625031 0.7686541 FALSE SenatorTomUdall 0.275808454 0.65228287
## 2390  0.3625031 0.7686541 FALSE SenatorTomUdall 0.494141726 0.53087200
## 2397  0.3625031 0.7686541 FALSE SenatorTomUdall 0.730961895 0.68965211
## 2398  0.3625031 0.7686541 FALSE SenatorTomUdall 0.648768516 0.67968280
## 2404  0.3625031 0.7686541 FALSE SenatorTomUdall 0.417101712 0.59868886
## 2405  0.3625031 0.7686541 FALSE SenatorTomUdall 0.109418424 0.52623297
## 2409  0.3625031 0.7686541 FALSE SenatorTomUdall 0.242907555 0.85550947
## 2410  0.3625031 0.7686541 FALSE SenatorTomUdall 0.372418851 0.75980896
## 2413  0.3625031 0.7686541 FALSE SenatorTomUdall 0.221151002 0.55063194
## 2418  0.3625031 0.7686541 FALSE SenatorTomUdall 0.291876079 0.77391843
## 2420  0.3625031 0.7686541 FALSE SenatorTomUdall 0.502174184 0.63833687
## 2421  0.3625031 0.7686541 FALSE SenatorTomUdall 0.531961607 0.43238702
## 2422  0.3625031 0.7686541 FALSE SenatorTomUdall 0.729072532 0.77575432
## 2426  0.3625031 0.7686541 FALSE SenatorTomUdall 0.502678202 0.66642059
## 2428  0.3625031 0.7686541 FALSE SenatorTomUdall 0.449047031 0.61678059
## 2429  0.3625031 0.7686541 FALSE SenatorTomUdall 0.411994601 0.54982067
## 2432  0.3625031 0.7686541 FALSE SenatorTomUdall 0.467333186 0.55127309
## 2438  0.3625031 0.7686541 FALSE SenatorTomUdall 0.466624648 0.75651821
## 2439  0.3625031 0.7686541 FALSE SenatorTomUdall 0.569902432 0.39705916
## 2441  0.3625031 0.7686541 FALSE SenatorTomUdall 0.644848645 0.61851958
## 2443  0.3625031 0.7686541 FALSE SenatorTomUdall 0.607267781 0.58945277
## 2448  0.3625031 0.7686541 FALSE SenatorTomUdall 0.282084901 0.49843978
## 2453  0.3625031 0.7686541 FALSE SenatorTomUdall 0.405330875 0.77964719
## 2461  0.3625031 0.7686541 FALSE SenatorTomUdall 0.375835040 0.67657928
## 2472  0.3625031 0.7686541 FALSE SenatorTomUdall 0.381141461 0.72877241
## 2475  0.3625031 0.7686541 FALSE SenatorTomUdall 0.358433989 0.77819409
## 2485  0.3625031 0.7686541 FALSE SenatorTomUdall 0.284298725 0.60659234
## 2487  0.3625031 0.7686541 FALSE SenatorTomUdall 0.307721958 0.48746029
## 2495  0.3625031 0.7686541 FALSE SenatorTomUdall 0.383929083 0.50559503
## 2499  0.3625031 0.7686541 FALSE SenatorTomUdall 0.564807275 0.48680287
## 2503  0.3625031 0.7686541 FALSE SenatorTomUdall 0.211562177 0.64531616
## 2507  0.3625031 0.7686541 FALSE SenatorTomUdall 0.301273247 0.74475185
## 2508  0.3625031 0.7686541 FALSE SenatorTomUdall 0.555623539 0.61740939
## 2579  0.3625031 0.7686541 FALSE SenatorTomUdall 0.795911228 0.76118930
## 2591  0.3625031 0.7686541 FALSE SenatorTomUdall 0.311569688 0.71526594
## 2615  0.3625031 0.7686541 FALSE SenatorTomUdall 0.367239654 0.46797098
## 2622  0.3625031 0.7686541 FALSE SenatorTomUdall 0.535553112 0.85475458
## 2624  0.3625031 0.7686541 FALSE SenatorTomUdall 0.550925550 0.67421233
## 2640  0.3625031 0.7686541 FALSE SenatorTomUdall 0.398107022 0.83349937
## 2650  0.3625031 0.7686541 FALSE SenatorTomUdall 0.461814135 0.81529322
## 2670  0.3625031 0.7686541 FALSE SenatorTomUdall 0.394345595 0.46860444
## 2713  0.3625031 0.7686541 FALSE SenatorTomUdall 0.427483746 0.67922540
## 2763  0.3625031 0.7686541 FALSE SenatorTomUdall 0.529879807 0.54122102
## 2831  0.3676334 0.4429741 FALSE  SenCortezMasto 0.555456729 0.58679586
## 2832  0.3676334 0.4429741 FALSE  SenCortezMasto 0.389128673 0.71824302
## 2833  0.3676334 0.4429741 FALSE  SenCortezMasto 0.674331769 0.48097128
## 2837  0.3676334 0.4429741 FALSE  SenCortezMasto 0.326831502 0.45555520
## 2839  0.3676334 0.4429741 FALSE  SenCortezMasto 0.273760520 0.48789487
## 2840  0.3676334 0.4429741 FALSE  SenCortezMasto 0.448301796 0.57378072
## 2844  0.3676334 0.4429741 FALSE  SenCortezMasto 0.550640859 0.55172585
## 2847  0.3676334 0.4429741 FALSE  SenCortezMasto 0.285390802 0.63774012
## 2848  0.3676334 0.4429741 FALSE  SenCortezMasto 0.717763716 0.48052723
## 2854  0.3676334 0.4429741 FALSE  SenCortezMasto 0.209331082 0.61128654
## 2857  0.3676334 0.4429741 FALSE  SenCortezMasto 0.229534863 0.51775110
## 2859  0.3676334 0.4429741 FALSE  SenCortezMasto 0.673498535 0.55990228
## 2861  0.3676334 0.4429741 FALSE  SenCortezMasto 0.434024346 0.63536821
## 2862  0.3676334 0.4429741 FALSE  SenCortezMasto 0.371992478 0.44308843
## 2863  0.3676334 0.4429741 FALSE  SenCortezMasto 0.554537831 0.46373330
## 2864  0.3676334 0.4429741 FALSE  SenCortezMasto 0.378009275 0.62687700
## 2867  0.3676334 0.4429741 FALSE  SenCortezMasto 0.550108801 0.48074618
## 2872  0.3676334 0.4429741 FALSE  SenCortezMasto 0.414814564 0.55194487
## 2878  0.3676334 0.4429741 FALSE  SenCortezMasto 0.458325564 0.43211909
## 2879  0.3676334 0.4429741 FALSE  SenCortezMasto 0.483679894 0.49824931
## 2885  0.3676334 0.4429741 FALSE  SenCortezMasto 0.404571020 0.50404492
## 2886  0.3676334 0.4429741 FALSE  SenCortezMasto 0.293802372 0.52279208
## 2889  0.3676334 0.4429741 FALSE  SenCortezMasto 0.652658796 0.65724060
## 2891  0.3676334 0.4429741 FALSE  SenCortezMasto 0.273155761 0.61046015
## 2897  0.3676334 0.4429741 FALSE  SenCortezMasto 0.641960394 0.45771923
## 2898  0.3676334 0.4429741 FALSE  SenCortezMasto 0.402530902 0.53218211
## 2901  0.3676334 0.4429741 FALSE  SenCortezMasto 0.359530180 0.51182014
## 2902  0.3676334 0.4429741 FALSE  SenCortezMasto 0.556205132 0.64474512
## 2905  0.3676334 0.4429741 FALSE  SenCortezMasto 0.115680936 0.50313362
## 2917  0.3676334 0.4429741 FALSE  SenCortezMasto 0.499948590 0.57073770
## 2918  0.3676334 0.4429741 FALSE  SenCortezMasto 0.502989029 0.46726202
## 2924  0.3676334 0.4429741 FALSE  SenCortezMasto 0.456577106 0.76791810
## 2926  0.3676334 0.4429741 FALSE  SenCortezMasto 0.409207337 0.62982528
## 2932  0.3676334 0.4429741 FALSE  SenCortezMasto 0.313240603 0.59838097
## 2938  0.3676334 0.4429741 FALSE  SenCortezMasto 0.507956491 0.63162968
## 2940  0.3676334 0.4429741 FALSE  SenCortezMasto 0.684424574 0.61276769
## 2946  0.3676334 0.4429741 FALSE  SenCortezMasto 0.233553630 0.67681340
## 2947  0.3676334 0.4429741 FALSE  SenCortezMasto 0.298623626 0.46643239
## 2958  0.3676334 0.4429741 FALSE  SenCortezMasto 0.421854119 0.44109430
## 2959  0.3676334 0.4429741 FALSE  SenCortezMasto 0.407576253 0.83054519
## 2979  0.3676334 0.4429741 FALSE  SenCortezMasto 0.558247638 0.38275975
## 2981  0.3676334 0.4429741 FALSE  SenCortezMasto 0.734183737 0.67119723
## 2987  0.3676334 0.4429741 FALSE  SenCortezMasto 0.370606047 0.31509695
## 2988  0.3676334 0.4429741 FALSE  SenCortezMasto 0.836322097 0.39611614
## 3003  0.3676334 0.4429741 FALSE  SenCortezMasto 0.605141116 0.56338012
## 3025  0.3676334 0.4429741 FALSE  SenCortezMasto 0.505257204 0.68698468
## 3028  0.3676334 0.4429741 FALSE  SenCortezMasto 0.690586957 0.78693288
## 3030  0.3676334 0.4429741 FALSE  SenCortezMasto 0.389446790 0.68122781
## 3044  0.3676334 0.4429741 FALSE  SenCortezMasto 0.285306433 0.71175692
## 3046  0.3676334 0.4429741 FALSE  SenCortezMasto 0.362896882 0.74365720
## 3062  0.3676334 0.4429741 FALSE  SenCortezMasto 0.607262260 0.80180833
## 3064  0.3676334 0.4429741 FALSE  SenCortezMasto 0.287314538 0.56328446
## 3068  0.3676334 0.4429741 FALSE  SenCortezMasto 0.422871166 0.66291414
## 3070  0.3676334 0.4429741 FALSE  SenCortezMasto 0.375030173 0.45819267
## 3075  0.3676334 0.4429741 FALSE  SenCortezMasto 0.835835549 0.55545072
## 3078  0.3711870 0.2901037 FALSE    SenDougJones 0.561615713 0.58107652
## 3080  0.3711870 0.2901037 FALSE    SenDougJones 0.111057437 0.49353882
## 3081  0.3711870 0.2901037 FALSE    SenDougJones 0.454330971 0.57108716
## 3082  0.3711870 0.2901037 FALSE    SenDougJones 0.203865037 0.60738405
## 3083  0.3711870 0.2901037 FALSE    SenDougJones 0.378848866 0.62684377
## 3085  0.3711870 0.2901037 FALSE    SenDougJones 0.425361064 0.66242165
## 3086  0.3711870 0.2901037 FALSE    SenDougJones 0.522705754 0.39576188
## 3087  0.3711870 0.2901037 FALSE    SenDougJones 0.550222838 0.84211048
## 3088  0.3711870 0.2901037 FALSE    SenDougJones 0.657158834 0.65263461
## 3089  0.3711870 0.2901037 FALSE    SenDougJones 0.263674614 0.47701541
## 3090  0.3711870 0.2901037 FALSE    SenDougJones 0.720439358 0.47166213
## 3091  0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.113760181 0.52004980
## 3092  0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.549361914 0.57481706
## 3093  0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.361464051 0.56117256
## 3094  0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.472592203 0.45181005
## 3096  0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.753017805 0.61843490
## 3097  0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.300573525 0.65862629
## 3098  0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.469635712 0.39918604
## 3100  0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.719946253 0.49372180
## 3104  0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.529190403 0.43075916
## 3105  0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.387909615 0.71836834
## 3107  0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.386198089 0.68174253
## 3108  0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.561049672 0.48348759
## 3109  0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.685048030 0.79457920
## 3110  0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.647702476 0.67052566
## 3111  0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.539409873 0.75653888
## 3115  0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.237257264 0.84989049
## 3120  0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.629482836 0.43266130
## 3121  0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.406408659 0.83069337
## 3122  0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.548316698 0.66157165
## 3125  0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.294496665 0.59754337
## 3128  0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.494424617 0.69923423
## 3129  0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.385003889 0.50565931
## 3131  0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.681545533 0.62665436
## 3132  0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.489650957 0.52769395
## 3133  0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.551077847 0.60815957
## 3143  0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.316376875 0.68542254
## 3146  0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.267257076 0.51785581
## 3149  0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.412058682 0.59642717
## 3150  0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.328184493 0.63128517
## 3156  0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.803364321 0.65728347
## 3157  0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.381692166 0.40345213
## 3158  0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.600727006 0.80817434
## 3165  0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.835575963 0.56591784
## 3166  0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.543553372 0.80645263
## 3169  0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.353673683 0.77670461
## 3170  0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.371755716 0.31509723
## 3173  0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.283745705 0.75726335
## 3180  0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.555547458 0.50201212
## 3181  0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.646160009 0.47298376
## 3182  0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.730499617 0.68225315
## 3190  0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.293820330 0.55948056
## 3194  0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.525091318 0.53659653
## 3200  0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.403711526 0.57917401
## 3202  0.3794175 0.6518373 FALSE   ChrisMurphyCT 0.745243614 0.52738678
## 3203  0.3819211 0.3784532 FALSE    SenToddYoung 0.558949587 0.58308799
## 3204  0.3819211 0.3784532 FALSE    SenToddYoung 0.623530594 0.41267677
## 3205  0.3819211 0.3784532 FALSE    SenToddYoung 0.285099568 0.56196557
## 3207  0.3819211 0.3784532 FALSE    SenToddYoung 0.504326516 0.56713034
## 3209  0.3819211 0.3784532 FALSE    SenToddYoung 0.379646474 0.62683835
## 3211  0.3819211 0.3784532 FALSE    SenToddYoung 0.607910570 0.55907590
## 3212  0.3819211 0.3784532 FALSE    SenToddYoung 0.391363929 0.41938391
## 3215  0.3819211 0.3784532 FALSE    SenToddYoung 0.284471834 0.63737333
## 3216  0.3819211 0.3784532 FALSE    SenToddYoung 0.232342008 0.67616229
## 3218  0.3819211 0.3784532 FALSE    SenToddYoung 0.360557894 0.51196278
## 3221  0.3819211 0.3784532 FALSE    SenToddYoung 0.756484907 0.60333997
## 3222  0.3819211 0.3784532 FALSE    SenToddYoung 0.425194845 0.66244640
## 3224  0.3819211 0.3784532 FALSE    SenToddYoung 0.312513359 0.59813888
## 3225  0.3819211 0.3784532 FALSE    SenToddYoung 0.407499416 0.53080911
## 3226  0.3819211 0.3784532 FALSE    SenToddYoung 0.686539591 0.60947109
## 3236  0.3819211 0.3784532 FALSE    SenToddYoung 0.549967798 0.74747648
## 3241  0.3819211 0.3784532 FALSE    SenToddYoung 0.718612864 0.47622178
## 3249  0.3819211 0.3784532 FALSE    SenToddYoung 0.273894524 0.75176197
## 3253  0.3819211 0.3784532 FALSE    SenToddYoung 0.559324628 0.64226453
## 3254  0.3819211 0.3784532 FALSE    SenToddYoung 0.807164984 0.64435034
## 3255  0.3819211 0.3784532 FALSE    SenToddYoung 0.271635718 0.60967009
## 3264  0.3819211 0.3784532 FALSE    SenToddYoung 0.508043337 0.68563345
## 3265  0.3819211 0.3784532 FALSE    SenToddYoung 0.556551702 0.45631274
## 3268  0.3819211 0.3784532 FALSE    SenToddYoung 0.408662686 0.83045711
## 3281  0.3819211 0.3784532 FALSE    SenToddYoung 0.764513381 0.38033081
## 3283  0.3859586 0.4806775 FALSE SenSherrodBrown 0.461060848 0.44085873
## 3284  0.3859586 0.4806775 FALSE SenSherrodBrown 0.275989191 0.49537446
## 3286  0.3859586 0.4806775 FALSE SenSherrodBrown 0.390587754 0.71817189
## 3287  0.3859586 0.4806775 FALSE SenSherrodBrown 0.505046027 0.68710515
## 3288  0.3859586 0.4806775 FALSE SenSherrodBrown 0.403105612 0.50500289
## 3292  0.3859586 0.4806775 FALSE SenSherrodBrown 0.403519499 0.53181920
## 3300  0.3859586 0.4806775 FALSE SenSherrodBrown 0.389832777 0.46769945
## 3301  0.3859586 0.4806775 FALSE SenSherrodBrown 0.824279335 0.60560917
## 3302  0.3859586 0.4806775 FALSE SenSherrodBrown 0.391086776 0.68113186
## 3303  0.3859586 0.4806775 FALSE SenSherrodBrown 0.236204314 0.67856606
## 3304  0.3859586 0.4806775 FALSE SenSherrodBrown 0.498544177 0.57232113
## 3305  0.3859586 0.4806775 FALSE SenSherrodBrown 0.641998022 0.46097879
## 3307  0.3859586 0.4806775 FALSE SenSherrodBrown 0.276295917 0.75259126
## 3308  0.3859586 0.4806775 FALSE SenSherrodBrown 0.410561150 0.62956302
## 3309  0.3859586 0.4806775 FALSE SenSherrodBrown 0.415229605 0.55176960
## 3310  0.3859586 0.4806775 FALSE SenSherrodBrown 0.231658867 0.52303667
## 3315  0.3859586 0.4806775 FALSE SenSherrodBrown 0.291841996 0.56716134
## 3316  0.3859586 0.4806775 FALSE SenSherrodBrown 0.317412333 0.60028682
## 3317  0.3859586 0.4806775 FALSE SenSherrodBrown 0.434662271 0.63515763
## 3318  0.3859586 0.4806775 FALSE SenSherrodBrown 0.288722258 0.63945140
## 3319  0.3859586 0.4806775 FALSE SenSherrodBrown 0.351518034 0.77635753
## 3321  0.3859586 0.4806775 FALSE SenSherrodBrown 0.604142472 0.56561504
## 3326  0.3859586 0.4806775 FALSE SenSherrodBrown 0.380372247 0.62685554
## 3332  0.3859586 0.4806775 FALSE SenSherrodBrown 0.423863650 0.66268626
## 3333  0.3859586 0.4806775 FALSE SenSherrodBrown 0.624006462 0.42215167
## 3337  0.3859586 0.4806775 FALSE SenSherrodBrown 0.477585019 0.80187935
## 3339  0.3859586 0.4806775 FALSE SenSherrodBrown 0.744037970 0.51682428
## 3341  0.3859586 0.4806775 FALSE SenSherrodBrown 0.503172525 0.60321607
## 3342  0.3859586 0.4806775 FALSE SenSherrodBrown 0.554451991 0.46832146
## 3343  0.3859586 0.4806775 FALSE SenSherrodBrown 0.378561840 0.46545896
## 3349  0.3859586 0.4806775 FALSE SenSherrodBrown 0.327388500 0.46815042
## 3350  0.3859586 0.4806775 FALSE SenSherrodBrown 0.481915682 0.50327049
## 3353  0.3859586 0.4806775 FALSE SenSherrodBrown 0.116250243 0.50655230
## 3354  0.3859586 0.4806775 FALSE SenSherrodBrown 0.212029848 0.61426900
## 3355  0.3859586 0.4806775 FALSE SenSherrodBrown 0.461562249 0.39444066
## 3359  0.3859586 0.4806775 FALSE SenSherrodBrown 0.549599080 0.48513325
## 3360  0.3859586 0.4806775 FALSE SenSherrodBrown 0.520470628 0.51491867
## 3362  0.3859586 0.4806775 FALSE SenSherrodBrown 0.482443486 0.73030516
## 3369  0.3859586 0.4806775 FALSE SenSherrodBrown 0.447656803 0.57419224
## 3370  0.3859586 0.4806775 FALSE SenSherrodBrown 0.276786332 0.61295356
## 3373  0.3859586 0.4806775 FALSE SenSherrodBrown 0.299914889 0.47587238
## 3374  0.3859586 0.4806775 FALSE SenSherrodBrown 0.368218096 0.51450825
## 3394  0.3859586 0.4806775 FALSE SenSherrodBrown 0.805149719 0.64832139
## 3397  0.3859586 0.4806775 FALSE SenSherrodBrown 0.502646339 0.47326284
## 3406  0.3859586 0.4806775 FALSE SenSherrodBrown 0.373119018 0.31502894
## 3407  0.3859586 0.4806775 FALSE SenSherrodBrown 0.550346225 0.80134757
## 3415  0.3859586 0.4806775 FALSE SenSherrodBrown 0.232343945 0.84714381
## 3421  0.3859586 0.4806775 FALSE SenSherrodBrown 0.674143532 0.48377626
## 3439  0.3859586 0.4806775 FALSE SenSherrodBrown 0.555360884 0.64557239
## 3449  0.3859586 0.4806775 FALSE SenSherrodBrown 0.457159391 0.76776622
## 3464  0.3859586 0.4806775 FALSE SenSherrodBrown 0.426016161 0.45406301
## 3524  0.3910750 0.7431671 FALSE    SenFeinstein 0.263623821 0.52038604
## 3531  0.3910750 0.7431671 FALSE    SenFeinstein 0.414953194 0.70786832
## 3534  0.3910750 0.7431671 FALSE    SenFeinstein 0.429204313 0.68037002
## 3535  0.3910750 0.7431671 FALSE    SenFeinstein 0.648398897 0.67836782
## 3537  0.3910750 0.7431671 FALSE    SenFeinstein 0.245297142 0.70485560
## 3538  0.3910750 0.7431671 FALSE    SenFeinstein 0.213907970 0.64190329
## 3539  0.3910750 0.7431671 FALSE    SenFeinstein 0.223846870 0.54861380
## 3541  0.3910750 0.7431671 FALSE    SenFeinstein 0.502102863 0.60745304
## 3542  0.3910750 0.7431671 FALSE    SenFeinstein 0.476114285 0.45313806
## 3543  0.3910750 0.7431671 FALSE    SenFeinstein 0.495216421 0.53143336
## 3545  0.3910750 0.7431671 FALSE    SenFeinstein 0.396490617 0.46873917
## 3546  0.3910750 0.7431671 FALSE    SenFeinstein 0.408916055 0.58031538
## 3547  0.3910750 0.7431671 FALSE    SenFeinstein 0.291124792 0.76942460
## 3548  0.3910750 0.7431671 FALSE    SenFeinstein 0.316892181 0.70790903
## 3552  0.3910750 0.7431671 FALSE    SenFeinstein 0.803827509 0.66240523
## 3553  0.3910750 0.7431671 FALSE    SenFeinstein 0.554337919 0.58205717
## 3554  0.3910750 0.7431671 FALSE    SenFeinstein 0.550391890 0.67307741
## 3555  0.3910750 0.7431671 FALSE    SenFeinstein 0.570567669 0.39741737
## 3559  0.3910750 0.7431671 FALSE    SenFeinstein 0.284871628 0.49742697
## 3560  0.3910750 0.7431671 FALSE    SenFeinstein 0.419842497 0.59940085
## 3563  0.3910750 0.7431671 FALSE    SenFeinstein 0.442313219 0.46481500
## 3566  0.3910750 0.7431671 FALSE    SenFeinstein 0.414496246 0.55025426
## 3570  0.3910750 0.7431671 FALSE    SenFeinstein 0.408234740 0.67839485
## 3571  0.3910750 0.7431671 FALSE    SenFeinstein 0.382582880 0.67663610
## 3572  0.3910750 0.7431671 FALSE    SenFeinstein 0.369579700 0.46789821
## 3573  0.3910750 0.7431671 FALSE    SenFeinstein 0.532826148 0.43280210
## 3579  0.3910750 0.7431671 FALSE    SenFeinstein 0.386445792 0.50567279
## 3580  0.3910750 0.7431671 FALSE    SenFeinstein 0.675132314 0.58121190
## 3582  0.3910750 0.7431671 FALSE    SenFeinstein 0.722520446 0.49805809
## 3588  0.3910750 0.7431671 FALSE    SenFeinstein 0.363386968 0.78101287
## 3589  0.3910750 0.7431671 FALSE    SenFeinstein 0.289132883 0.56290584
## 3590  0.3910750 0.7431671 FALSE    SenFeinstein 0.516364622 0.49401253
## 3591  0.3910750 0.7431671 FALSE    SenFeinstein 0.493415552 0.71532323
## 3605  0.3910750 0.7431671 FALSE    SenFeinstein 0.442482074 0.77800589
## 3612  0.3917261 0.7061237 FALSE      CoryBooker 0.548951735 0.66878637
## 3615  0.3917261 0.7061237 FALSE      CoryBooker 0.286207283 0.49680236
## 3616  0.3917261 0.7061237 FALSE      CoryBooker 0.418647431 0.59913110
## 3618  0.3917261 0.7061237 FALSE      CoryBooker 0.319207989 0.69946368
## 3619  0.3917261 0.7061237 FALSE      CoryBooker 0.322926139 0.63938431
## 3621  0.3917261 0.7061237 FALSE      CoryBooker 0.282642063 0.64452711
## 3622  0.3917261 0.7061237 FALSE      CoryBooker 0.598854389 0.81128139
## 3623  0.3917261 0.7061237 FALSE      CoryBooker 0.288774595 0.76359150
## 3624  0.3917261 0.7061237 FALSE      CoryBooker 0.291114043 0.56165958
## 3625  0.3917261 0.7061237 FALSE      CoryBooker 0.407078914 0.83060148
## 3626  0.3917261 0.7061237 FALSE      CoryBooker 0.215541320 0.63846038
## 3627  0.3917261 0.7061237 FALSE      CoryBooker 0.225602109 0.54695091
## 3633  0.3917261 0.7061237 FALSE      CoryBooker 0.386597954 0.50566936
## 3634  0.3917261 0.7061237 FALSE      CoryBooker 0.240618681 0.85278816
## 3635  0.3917261 0.7061237 FALSE      CoryBooker 0.683887273 0.97801020
## 3639  0.3917261 0.7061237 FALSE      CoryBooker 0.396483014 0.46873902
## 3642  0.3917261 0.7061237 FALSE      CoryBooker 0.112263413 0.52265963
## 3644  0.3917261 0.7061237 FALSE      CoryBooker 0.406683328 0.69850641
## 3645  0.3917261 0.7061237 FALSE      CoryBooker 0.499786718 0.66127280
## 3646  0.3917261 0.7061237 FALSE      CoryBooker 0.647821701 0.67525468
## 3648  0.3917261 0.7061237 FALSE      CoryBooker 0.384945590 0.67621846
## 3650  0.3917261 0.7061237 FALSE      CoryBooker 0.298951000 0.66987016
## 3652  0.3917261 0.7061237 FALSE      CoryBooker 0.413977730 0.55018580
## 3653  0.3917261 0.7061237 FALSE      CoryBooker 0.423909024 0.67606511
## 3654  0.3917261 0.7061237 FALSE      CoryBooker 0.468885873 0.74287382
## 3660  0.3917261 0.7061237 FALSE      CoryBooker 0.536960709 0.76134368
## 3662  0.3917261 0.7061237 FALSE      CoryBooker 0.290833039 0.60202841
## 3668  0.3917261 0.7061237 FALSE      CoryBooker 0.569619972 0.39689909
## 3674  0.3917261 0.7061237 FALSE      CoryBooker 0.448135714 0.61623532
## 3675  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.557610349 0.64352653
## 3676  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.509929413 0.63030310
## 3677  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.551802347 0.80065940
## 3678  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.425704585 0.66237409
## 3680  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.299196704 0.46837262
## 3683  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.408387130 0.53067641
## 3684  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.518850201 0.41567322
## 3685  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.419569828 0.55042889
## 3689  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.684873976 0.61196505
## 3690  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.287421980 0.71251039
## 3694  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.276062081 0.75249832
## 3698  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.437042378 0.63453418
## 3701  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.393109697 0.45672213
## 3703  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.210720221 0.61270098
## 3707  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.458901747 0.39172358
## 3708  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.391568216 0.71817201
## 3712  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.458499544 0.43332348
## 3713  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.478866023 0.80155031
## 3716  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.616938423 0.69560219
## 3717  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.556607234 0.58540092
## 3718  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.421901046 0.44198680
## 3722  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.484786664 0.49618155
## 3725  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.690896329 0.71200486
## 3727  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.506179460 0.60075735
## 3730  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.274603105 0.48986915
## 3736  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.674361532 0.48073990
## 3737  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.717782411 0.48035834
## 3740  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.327437277 0.45792631
## 3741  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.451629075 0.57205861
## 3742  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.507186973 0.68600388
## 3756  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.289953664 0.56531276
## 3757  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.623432515 0.41890849
## 3759  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.316448912 0.59976243
## 3760  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.411417363 0.50119010
## 3765  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.287864168 0.63894908
## 3768  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.351977288 0.77641534
## 3769  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.550262967 0.48005139
## 3775  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.427177157 0.76097521
## 3777  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.392227003 0.68112870
## 3790  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.381520455 0.62692590
## 3792  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.640118300 0.82877402
## 3794  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.115814373 0.50372401
## 3795  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.275508663 0.61196664
## 3810  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.503148856 0.46644905
## 3817  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.558641038 0.38390737
## 3823  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.641964507 0.45764477
## 3829  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.522550215 0.50948954
## 3830  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.771591801 0.66432277
## 3840  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.387541049 0.40281332
## 3843  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.755143860 0.60589761
## 3847  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.551550069 0.55030700
## 3852  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.296247219 0.52540196
## 3856  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.605702980 0.56233416
## 3860  0.3969839 0.4437440 FALSE  SenatorBaldwin 0.460923671 0.51067744
## 3862  0.4101392 0.8554135 FALSE        RandPaul 0.596442982 0.82644042
## 3863  0.4101392 0.8554135 FALSE        RandPaul 0.395261070 0.76781418
## 3867  0.4101392 0.8554135 FALSE        RandPaul 0.416951091 0.55043027
## 3874  0.4101392 0.8554135 FALSE        RandPaul 0.383148045 0.67655740
## 3877  0.4101392 0.8554135 FALSE        RandPaul 0.261383502 0.52152231
## 3879  0.4101392 0.8554135 FALSE        RandPaul 0.683788248 0.50377472
## 3885  0.4101392 0.8554135 FALSE        RandPaul 0.639221691 0.80892512
## 3890  0.4101392 0.8554135 FALSE        RandPaul 0.288793689 0.78792764
## 3891  0.4101392 0.8554135 FALSE        RandPaul 0.394786385 0.73093568
## 3893  0.4101392 0.8554135 FALSE        RandPaul 0.561655338 0.62293921
## 3894  0.4101392 0.8554135 FALSE        RandPaul 0.805762612 0.66829366
## 3895  0.4101392 0.8554135 FALSE        RandPaul 0.557107483 0.68207848
## 3898  0.4101392 0.8554135 FALSE        RandPaul 0.756961253 0.62983926
## 3916  0.4116386 0.5554641 FALSE   SenatorDurbin 0.215891401 0.62150551
## 3917  0.4116386 0.5554641 FALSE   SenatorDurbin 0.393797519 0.71831584
## 3918  0.4116386 0.5554641 FALSE   SenatorDurbin 0.376741139 0.46625604
## 3919  0.4116386 0.5554641 FALSE   SenatorDurbin 0.718196623 0.48852646
## 3920  0.4116386 0.5554641 FALSE   SenatorDurbin 0.387344637 0.62812736
## 3922  0.4116386 0.5554641 FALSE   SenatorDurbin 0.410761818 0.55416498
## 3924  0.4116386 0.5554641 FALSE   SenatorDurbin 0.477944538 0.80177969
## 3925  0.4116386 0.5554641 FALSE   SenatorDurbin 0.499062027 0.60834310
## 3930  0.4116386 0.5554641 FALSE   SenatorDurbin 0.837675791 0.40209681
## 3932  0.4116386 0.5554641 FALSE   SenatorDurbin 0.804080981 0.65165317
## 3934  0.4116386 0.5554641 FALSE   SenatorDurbin 0.507332324 0.48631927
## 3935  0.4116386 0.5554641 FALSE   SenatorDurbin 0.368117856 0.74429276
## 3939  0.4116386 0.5554641 FALSE   SenatorDurbin 0.232353269 0.53279148
## 3940  0.4116386 0.5554641 FALSE   SenatorDurbin 0.380263379 0.54473671
## 3943  0.4116386 0.5554641 FALSE   SenatorDurbin 0.394077715 0.50432241
## 3944  0.4116386 0.5554641 FALSE   SenatorDurbin 0.469382603 0.39909412
## 3946  0.4116386 0.5554641 FALSE   SenatorDurbin 0.321976951 0.47912806
## 3947  0.4116386 0.5554641 FALSE   SenatorDurbin 0.274777071 0.50702702
## 3948  0.4116386 0.5554641 FALSE   SenatorDurbin 0.116104833 0.51253362
## 3950  0.4116386 0.5554641 FALSE   SenatorDurbin 0.503329094 0.68819092
## 3951  0.4116386 0.5554641 FALSE   SenatorDurbin 0.551601708 0.49563953
## 3952  0.4116386 0.5554641 FALSE   SenatorDurbin 0.427608658 0.76093791
## 3955  0.4116386 0.5554641 FALSE   SenatorDurbin 0.386060303 0.40310814
## 3963  0.4116386 0.5554641 FALSE   SenatorDurbin 0.412712386 0.54997197
## 3965  0.4116386 0.5554641 FALSE   SenatorDurbin 0.643536397 0.46789300
## 3967  0.4116386 0.5554641 FALSE   SenatorDurbin 0.649822699 0.66205127
## 3969  0.4116386 0.5554641 FALSE   SenatorDurbin 0.483810302 0.52002036
## 3970  0.4116386 0.5554641 FALSE   SenatorDurbin 0.547278034 0.84327651
## 3971  0.4116386 0.5554641 FALSE   SenatorDurbin 0.551258819 0.59515831
## 3974  0.4116386 0.5554641 FALSE   SenatorDurbin 0.470832115 0.45090371
## 3978  0.4116386 0.5554641 FALSE   SenatorDurbin 0.400235371 0.46853169
## 3979  0.4116386 0.5554641 FALSE   SenatorDurbin 0.435106307 0.63502215
## 3983  0.4116386 0.5554641 FALSE   SenatorDurbin 0.552461448 0.64916149
## 3995  0.4116386 0.5554641 FALSE   SenatorDurbin 0.295431140 0.64546327
## 3996  0.4116386 0.5554641 FALSE   SenatorDurbin 0.241110799 0.68349133
## 3998  0.4116386 0.5554641 FALSE   SenatorDurbin 0.494034193 0.58076489
## 3999  0.4116386 0.5554641 FALSE   SenatorDurbin 0.534477689 0.97600279
## 4014  0.4116386 0.5554641 FALSE   SenatorDurbin 0.503969371 0.63533352
## 4016  0.4116386 0.5554641 FALSE   SenatorDurbin 0.395001867 0.68133922
## 4020  0.4116386 0.5554641 FALSE   SenatorDurbin 0.457855626 0.76760408
## 4025  0.4116386 0.5554641 FALSE   SenatorDurbin 0.354835590 0.77697320
## 4030  0.4116386 0.5554641 FALSE   SenatorDurbin 0.520492823 0.52733975
## 4034  0.4116386 0.5554641 FALSE   SenatorDurbin 0.602537977 0.57246653
## 4044  0.4116386 0.5554641 FALSE   SenatorDurbin 0.301686552 0.54378522
## 4056  0.4116386 0.5554641 FALSE   SenatorDurbin 0.425700598 0.66237462
## 4062  0.4116386 0.5554641 FALSE   SenatorDurbin 0.296461952 0.48722199
## 4071  0.4116386 0.5554641 FALSE   SenatorDurbin 0.605718001 0.80292704
## 4072  0.4116386 0.5554641 FALSE   SenatorDurbin 0.439535638 0.46413749
## 4091  0.4116386 0.5554641 FALSE   SenatorDurbin 0.557299282 0.47820711
## 4097  0.4116386 0.5554641 FALSE   SenatorDurbin 0.326194891 0.60874849
## 4113  0.4116386 0.5554641 FALSE   SenatorDurbin 0.297914497 0.57900849
## 4131  0.4116386 0.5554641 FALSE   SenatorDurbin 0.642412376 0.60185522
## 4153  0.4116386 0.5554641 FALSE   SenatorDurbin 0.413878376 0.62924005
## 4165  0.4146370 0.6542285 FALSE      SenSchumer 0.300638034 0.65959537
## 4166  0.4146370 0.6542285 FALSE      SenSchumer 0.328965949 0.62903148
## 4167  0.4146370 0.6542285 FALSE      SenSchumer 0.404360119 0.65353078
## 4169  0.4146370 0.6542285 FALSE      SenSchumer 0.358869096 0.77838458
## 4170  0.4146370 0.6542285 FALSE      SenSchumer 0.492911898 0.53014448
## 4171  0.4146370 0.6542285 FALSE      SenSchumer 0.551530725 0.60972425
## 4172  0.4146370 0.6542285 FALSE      SenSchumer 0.472494846 0.40001867
## 4173  0.4146370 0.6542285 FALSE      SenSchumer 0.295822783 0.59520002
## 4174  0.4146370 0.6542285 FALSE      SenSchumer 0.269318647 0.51592156
## 4175  0.4146370 0.6542285 FALSE      SenSchumer 0.531563895 0.43218212
## 4176  0.4146370 0.6542285 FALSE      SenSchumer 0.401823022 0.68325334
## 4178  0.4146370 0.6542285 FALSE      SenSchumer 0.569221264 0.39666478
## 4179  0.4146370 0.6542285 FALSE      SenSchumer 0.228973426 0.54254181
## 4180  0.4146370 0.6542285 FALSE      SenSchumer 0.412397243 0.58045255
## 4181  0.4146370 0.6542285 FALSE      SenSchumer 0.532876858 0.97652957
## 4182  0.4146370 0.6542285 FALSE      SenSchumer 0.290293919 0.49421873
## 4184  0.4146370 0.6542285 FALSE      SenSchumer 0.399073269 0.46865658
## 4186  0.4146370 0.6542285 FALSE      SenSchumer 0.543920242 0.84518912
## 4188  0.4146370 0.6542285 FALSE      SenSchumer 0.374149202 0.31492759
## 4192  0.4146370 0.6542285 FALSE      SenSchumer 0.548313496 0.66162833
## 4193  0.4146370 0.6542285 FALSE      SenSchumer 0.642295353 0.79107346
## 4196  0.4146370 0.6542285 FALSE      SenSchumer 0.286248646 0.75989028
## 4201  0.4146370 0.6542285 FALSE      SenSchumer 0.416951922 0.55043029
## 4202  0.4146370 0.6542285 FALSE      SenSchumer 0.711147765 0.56547069
## 4204  0.4146370 0.6542285 FALSE      SenSchumer 0.390034442 0.50534305
## 4212  0.4146370 0.6542285 FALSE      SenSchumer 0.317857802 0.68877310
## 4215  0.4146370 0.6542285 FALSE      SenSchumer 0.475868512 0.45306463
## 4217  0.4146370 0.6542285 FALSE      SenSchumer 0.217050136 0.63226003
## 4224  0.4146370 0.6542285 FALSE      SenSchumer 0.604021901 0.58343769
## 4225  0.4146370 0.6542285 FALSE      SenSchumer 0.367671986 0.55906714
## 4226  0.4146370 0.6542285 FALSE      SenSchumer 0.475027046 0.80276131
## 4235  0.4146370 0.6542285 FALSE      SenSchumer 0.397477211 0.71900083
## 4238  0.4146370 0.6542285 FALSE      SenSchumer 0.803363091 0.65740060
## 4241  0.4146370 0.6542285 FALSE      SenSchumer 0.630771458 0.43402474
## 4245  0.4146370 0.6542285 FALSE      SenSchumer 0.372868332 0.74590410
## 4246  0.4146370 0.6542285 FALSE      SenSchumer 0.497883724 0.65227552
## 4250  0.4146370 0.6542285 FALSE      SenSchumer 0.647702893 0.67051968
## 4256  0.4146370 0.6542285 FALSE      SenSchumer 0.299460784 0.72286303
## 4263  0.4146370 0.6542285 FALSE      SenSchumer 0.496877556 0.60158197
## 4271  0.4146370 0.6542285 FALSE      SenSchumer 0.466895878 0.55105692
## 4272  0.4146370 0.6542285 FALSE      SenSchumer 0.647150509 0.47435794
## 4277  0.4146370 0.6542285 FALSE      SenSchumer 0.562895260 0.48528366
## 4284  0.4146370 0.6542285 FALSE      SenSchumer 0.514441262 0.49293663
## 4285  0.4146370 0.6542285 FALSE      SenSchumer 0.239287518 0.85151416
## 4287  0.4146370 0.6542285 FALSE      SenSchumer 0.315546606 0.48451118
## 4292  0.4146370 0.6542285 FALSE      SenSchumer 0.114167389 0.51918807
## 4296  0.4146370 0.6542285 FALSE      SenSchumer 0.285621012 0.63577458
## 4299  0.4146370 0.6542285 FALSE      SenSchumer 0.625291137 0.76606877
## 4301  0.4175093 0.5254365 FALSE        RonWyden 0.823816784 0.60751175
## 4302  0.4175093 0.5254365 FALSE        RonWyden 0.410697408 0.83041970
## 4303  0.4175093 0.5254365 FALSE        RonWyden 0.296589161 0.57465225
## 4305  0.4175093 0.5254365 FALSE        RonWyden 0.301672000 0.53837046
## 4306  0.4175093 0.5254365 FALSE        RonWyden 0.293753509 0.64351324
## 4308  0.4175093 0.5254365 FALSE        RonWyden 0.453229754 0.52738974
## 4311  0.4175093 0.5254365 FALSE        RonWyden 0.607014053 0.80197624
## 4312  0.4175093 0.5254365 FALSE        RonWyden 0.323955809 0.60569900
## 4313  0.4175093 0.5254365 FALSE        RonWyden 0.400362319 0.50111116
## 4314  0.4175093 0.5254365 FALSE        RonWyden 0.395257744 0.68137439
## 4315  0.4175093 0.5254365 FALSE        RonWyden 0.458956272 0.76739003
## 4316  0.4175093 0.5254365 FALSE        RonWyden 0.354684484 0.77693496
## 4317  0.4175093 0.5254365 FALSE        RonWyden 0.496723634 0.57486824
## 4319  0.4175093 0.5254365 FALSE        RonWyden 0.448087147 0.57391460
## 4321  0.4175093 0.5254365 FALSE        RonWyden 0.279830277 0.75435298
## 4326  0.4175093 0.5254365 FALSE        RonWyden 0.239874639 0.68197255
## 4327  0.4175093 0.5254365 FALSE        RonWyden 0.381194625 0.53212221
## 4329  0.4175093 0.5254365 FALSE        RonWyden 0.421126893 0.55015040
## 4335  0.4175093 0.5254365 FALSE        RonWyden 0.534457433 0.66109376
## 4336  0.4175093 0.5254365 FALSE        RonWyden 0.674407791 0.48768025
## 4340  0.4175093 0.5254365 FALSE        RonWyden 0.403075855 0.46799044
## 4348  0.4175093 0.5254365 FALSE        RonWyden 0.563621055 0.39208229
## 4349  0.4175093 0.5254365 FALSE        RonWyden 0.116332582 0.51020265
## 4352  0.4175093 0.5254365 FALSE        RonWyden 0.368017853 0.74426993
## 4355  0.4175093 0.5254365 FALSE        RonWyden 0.437638564 0.63441655
## 4360  0.4175093 0.5254365 FALSE        RonWyden 0.468676074 0.39882098
## 4361  0.4175093 0.5254365 FALSE        RonWyden 0.394088044 0.71834939
## 4363  0.4175093 0.5254365 FALSE        RonWyden 0.483749541 0.72984153
## 4365  0.4175093 0.5254365 FALSE        RonWyden 0.565299566 0.70094991
## 4368  0.4175093 0.5254365 FALSE        RonWyden 0.324886935 0.47489636
## 4370  0.4175093 0.5254365 FALSE        RonWyden 0.438702344 0.46386689
## 4371  0.4175093 0.5254365 FALSE        RonWyden 0.743915326 0.51976918
## 4372  0.4175093 0.5254365 FALSE        RonWyden 0.415194414 0.62923475
## 4377  0.4175093 0.5254365 FALSE        RonWyden 0.214899407 0.61901489
## 4380  0.4175093 0.5254365 FALSE        RonWyden 0.732784929 0.67376798
## 4381  0.4175093 0.5254365 FALSE        RonWyden 0.603100171 0.56897459
## 4383  0.4175093 0.5254365 FALSE        RonWyden 0.275892218 0.50265652
## 4384  0.4175093 0.5254365 FALSE        RonWyden 0.291809399 0.71483096
## 4391  0.4175093 0.5254365 FALSE        RonWyden 0.552813373 0.59108220
## 4392  0.4175093 0.5254365 FALSE        RonWyden 0.710230555 0.55572814
## 4409  0.4175093 0.5254365 FALSE        RonWyden 0.298494955 0.48289347
## 4425  0.4175093 0.5254365 FALSE        RonWyden 0.469066754 0.44980494
## 4426  0.4175093 0.5254365 FALSE        RonWyden 0.672146626 0.56499189
## 4429  0.4175093 0.5254365 FALSE        RonWyden 0.638763198 0.82969314
## 4435  0.4175093 0.5254365 FALSE        RonWyden 0.427194991 0.66222357
## 4439  0.4175093 0.5254365 FALSE        RonWyden 0.386631039 0.62790060
## 4441  0.4175093 0.5254365 FALSE        RonWyden 0.234978938 0.84843507
## 4448  0.4175093 0.5254365 FALSE        RonWyden 0.481668370 0.51355306
## 4450  0.4175093 0.5254365 FALSE        RonWyden 0.232545762 0.52915275
## 4467  0.4175093 0.5254365 FALSE        RonWyden 0.554537278 0.64646058
## 4478  0.4175093 0.5254365 FALSE        RonWyden 0.555893915 0.47504682
## 4485  0.4247477 0.5748868 FALSE       SenWarren 0.242488714 0.68552810
## 4486  0.4247477 0.5748868 FALSE       SenWarren 0.328247950 0.61282942
## 4487  0.4247477 0.5748868 FALSE       SenWarren 0.521887386 0.53131709
## 4488  0.4247477 0.5748868 FALSE       SenWarren 0.551774788 0.65025359
## 4490  0.4247477 0.5748868 FALSE       SenWarren 0.395476730 0.50379474
## 4491  0.4247477 0.5748868 FALSE       SenWarren 0.295720055 0.48839825
## 4492  0.4247477 0.5748868 FALSE       SenWarren 0.392106602 0.63029693
## 4495  0.4247477 0.5748868 FALSE       SenWarren 0.297328168 0.64829155
## 4496  0.4247477 0.5748868 FALSE       SenWarren 0.282388383 0.75611686
## 4497  0.4247477 0.5748868 FALSE       SenWarren 0.425624546 0.57618589
## 4498  0.4247477 0.5748868 FALSE       SenWarren 0.550701506 0.59756489
## 4500  0.4247477 0.5748868 FALSE       SenWarren 0.486808485 0.52471680
## 4501  0.4247477 0.5748868 FALSE       SenWarren 0.459267554 0.54509022
## 4506  0.4247477 0.5748868 FALSE       SenWarren 0.397826447 0.68187938
## 4512  0.4247477 0.5748868 FALSE       SenWarren 0.566612683 0.39486488
## 4513  0.4247477 0.5748868 FALSE       SenWarren 0.437106225 0.63452087
## 4514  0.4247477 0.5748868 FALSE       SenWarren 0.417797263 0.62942908
## 4515  0.4247477 0.5748868 FALSE       SenWarren 0.356596095 0.77749426
## 4517  0.4247477 0.5748868 FALSE       SenWarren 0.473849194 0.45235418
## 4524  0.4247477 0.5748868 FALSE       SenWarren 0.665850590 0.51985410
## 4525  0.4247477 0.5748868 FALSE       SenWarren 0.478659309 0.80159854
## 4531  0.4247477 0.5748868 FALSE       SenWarren 0.361678471 0.97169457
## 4532  0.4247477 0.5748868 FALSE       SenWarren 0.644371081 0.46984760
## 4533  0.4247477 0.5748868 FALSE       SenWarren 0.509949353 0.48938607
## 4535  0.4247477 0.5748868 FALSE       SenWarren 0.687372829 0.71696289
## 4542  0.4247477 0.5748868 FALSE       SenWarren 0.767451540 0.39221311
## 4549  0.4247477 0.5748868 FALSE       SenWarren 0.439519285 0.58301141
## 4552  0.4247477 0.5748868 FALSE       SenWarren 0.442791848 0.46489831
## 4559  0.4247477 0.5748868 FALSE       SenWarren 0.803871297 0.65258536
## 4562  0.4247477 0.5748868 FALSE       SenWarren 0.274099971 0.50873736
## 4566  0.4247477 0.5748868 FALSE       SenWarren 0.370149146 0.74485203
## 4570  0.4247477 0.5748868 FALSE       SenWarren 0.298387659 0.58256122
## 4572  0.4247477 0.5748868 FALSE       SenWarren 0.378409602 0.54888324
## 4573  0.4247477 0.5748868 FALSE       SenWarren 0.471582891 0.39979301
## 4574  0.4247477 0.5748868 FALSE       SenWarren 0.232025711 0.53475190
## 4575  0.4247477 0.5748868 FALSE       SenWarren 0.675408887 0.49190223
## 4585  0.4247477 0.5748868 FALSE       SenWarren 0.528621653 0.43036223
## 4589  0.4247477 0.5748868 FALSE       SenWarren 0.402161794 0.46820195
## 4591  0.4247477 0.5748868 FALSE       SenWarren 0.428977471 0.76086920
## 4592  0.4247477 0.5748868 FALSE       SenWarren 0.558913429 0.48084272
## 4596  0.4247477 0.5748868 FALSE       SenWarren 0.553100022 0.49858824
## 4601  0.4247477 0.5748868 FALSE       SenWarren 0.563066206 0.70307949
## 4605  0.4247477 0.5748868 FALSE       SenWarren 0.216541135 0.62378195
## 4607  0.4247477 0.5748868 FALSE       SenWarren 0.602439418 0.57470934
## 4612  0.4247477 0.5748868 FALSE       SenWarren 0.718600545 0.49012227
## 4613  0.4247477 0.5748868 FALSE       SenWarren 0.115889285 0.51379102
## 4617  0.4247477 0.5748868 FALSE       SenWarren 0.284469726 0.62397701
## 4629  0.4247477 0.5748868 FALSE       SenWarren 0.236796913 0.84956784
## 4631  0.4247477 0.5748868 FALSE       SenWarren 0.411439289 0.83044730
## 4634  0.4247477 0.5748868 FALSE       SenWarren 0.823419608 0.61025278
## 4652  0.4247477 0.5748868 FALSE       SenWarren 0.312550648 0.68007821
## 4657  0.4247477 0.5748868 FALSE       SenWarren 0.301200306 0.54670453
## 4664  0.4247477 0.5748868 FALSE       SenWarren 0.294839607 0.71719687
## 4671  0.4247477 0.5748868 FALSE       SenWarren 0.377566598 0.46591601
## 4672  0.4247477 0.5748868 FALSE       SenWarren 0.493180596 0.58459270
## 4687  0.4247477 0.5748868 FALSE       SenWarren 0.531584128 0.66401398
## 4694  0.4247477 0.5748868 FALSE       SenWarren 0.421130187 0.55017290
## 4697  0.4247477 0.5748868 FALSE       SenWarren 0.832644319 0.69742104
## 4700  0.4247477 0.5748868 FALSE       SenWarren 0.835156138 0.56207067
## 4712  0.4247477 0.5748868 FALSE       SenWarren 0.605614231 0.80300886
## 4713  0.4289608 0.6871611 FALSE        timkaine 0.530404180 0.54159659
## 4714  0.4289608 0.6871611 FALSE        timkaine 0.470772711 0.55262831
## 4715  0.4289608 0.6871611 FALSE        timkaine 0.647688634 0.67378852
## 4717  0.4289608 0.6871611 FALSE        timkaine 0.548618169 0.66713648
## 4719  0.4289608 0.6871611 FALSE        timkaine 0.424608228 0.67715395
## 4720  0.4289608 0.6871611 FALSE        timkaine 0.414898799 0.58025058
## 4721  0.4289608 0.6871611 FALSE        timkaine 0.289616238 0.49472718
## 4722  0.4289608 0.6871611 FALSE        timkaine 0.391055719 0.50515241
## 4725  0.4289608 0.6871611 FALSE        timkaine 0.414003584 0.69477841
## 4726  0.4289608 0.6871611 FALSE        timkaine 0.729956517 0.76963211
## 4727  0.4289608 0.6871611 FALSE        timkaine 0.539198558 0.75686736
## 4728  0.4289608 0.6871611 FALSE        timkaine 0.288878757 0.76378001
## 4731  0.4289608 0.6871611 FALSE        timkaine 0.669374534 0.52808615
## 4732  0.4289608 0.6871611 FALSE        timkaine 0.553147529 0.58076282
## 4733  0.4289608 0.6871611 FALSE        timkaine 0.445033795 0.46516282
## 4734  0.4289608 0.6871611 FALSE        timkaine 0.517190821 0.49440919
## 4736  0.4289608 0.6871611 FALSE        timkaine 0.399773346 0.66635079
## 4737  0.4289608 0.6871611 FALSE        timkaine 0.431556412 0.68163149
## 4738  0.4289608 0.6871611 FALSE        timkaine 0.831608099 0.70359418
## 4739  0.4289608 0.6871611 FALSE        timkaine 0.501227429 0.60670248
## 4741  0.4289608 0.6871611 FALSE        timkaine 0.478010020 0.45361428
## 4745  0.4289608 0.6871611 FALSE        timkaine 0.565303616 0.48715009
## 4746  0.4289608 0.6871611 FALSE        timkaine 0.684633324 0.72518116
## 4749  0.4289608 0.6871611 FALSE        timkaine 0.493250309 0.70283754
## 4750  0.4289608 0.6871611 FALSE        timkaine 0.400240071 0.46853108
## 4751  0.4289608 0.6871611 FALSE        timkaine 0.608602727 0.71112636
## 4757  0.4289608 0.6871611 FALSE        timkaine 0.753419764 0.62118986
## 4758  0.4289608 0.6871611 FALSE        timkaine 0.453113725 0.61863786
## 4759  0.4289608 0.6871611 FALSE        timkaine 0.553698444 0.61456947
## 4760  0.4289608 0.6871611 FALSE        timkaine 0.284636338 0.63999993
## 4761  0.4289608 0.6871611 FALSE        timkaine 0.425685194 0.59986922
## 4766  0.4289608 0.6871611 FALSE        timkaine 0.294271952 0.59788855
## 4771  0.4289608 0.6871611 FALSE        timkaine 0.455422746 0.76826419
## 4775  0.4289608 0.6871611 FALSE        timkaine 0.216493187 0.63541341
## 4778  0.4289608 0.6871611 FALSE        timkaine 0.533682702 0.43317413
## 4779  0.4289608 0.6871611 FALSE        timkaine 0.559938576 0.50607059
## 4788  0.4289608 0.6871611 FALSE        timkaine 0.643236765 0.61478271
## 4790  0.4289608 0.6871611 FALSE        timkaine 0.499489432 0.66052256
## 4793  0.4289608 0.6871611 FALSE        timkaine 0.319243873 0.69532274
## 4798  0.4289608 0.6871611 FALSE        timkaine 0.605689111 0.58701015
## 4802  0.4289608 0.6871611 FALSE        timkaine 0.373723030 0.46722107
## 4803  0.4289608 0.6871611 FALSE        timkaine 0.227922121 0.54414664
## 4807  0.4289608 0.6871611 FALSE        timkaine 0.500165461 0.63589017
## 4819  0.4289608 0.6871611 FALSE        timkaine 0.301786810 0.72801458
## 4820  0.4289608 0.6871611 FALSE        timkaine 0.673981980 0.57893074
## 4825  0.4289608 0.6871611 FALSE        timkaine 0.648876610 0.47636072
## 4829  0.4289608 0.6871611 FALSE        timkaine 0.682071650 0.63007780
## 4831  0.4289608 0.6871611 FALSE        timkaine 0.792724191 0.55758037
## 4838  0.4295459 0.7858627 FALSE   lisamurkowski 0.769184926 0.68417803
## 4839  0.4295459 0.7858627 FALSE   lisamurkowski 0.457304721 0.61971789
## 4842  0.4295459 0.7858627 FALSE   lisamurkowski 0.554271409 0.67925366
## 4846  0.4295459 0.7858627 FALSE   lisamurkowski 0.213079827 0.64325129
## 4849  0.4295459 0.7858627 FALSE   lisamurkowski 0.534727520 0.54401168
## 4850  0.4295459 0.7858627 FALSE   lisamurkowski 0.724647301 0.50056975
## 4851  0.4295459 0.7858627 FALSE   lisamurkowski 0.290062075 0.56235352
## 4853  0.4295459 0.7858627 FALSE   lisamurkowski 0.425316161 0.59988034
## 4854  0.4295459 0.7858627 FALSE   lisamurkowski 0.244174776 0.70816525
## 4856  0.4295459 0.7858627 FALSE   lisamurkowski 0.535505392 0.77319593
## 4858  0.4295459 0.7858627 FALSE   lisamurkowski 0.508600974 0.67221232
## 4859  0.4295459 0.7858627 FALSE   lisamurkowski 0.286070792 0.49687065
## 4870  0.4295459 0.7858627 FALSE   lisamurkowski 0.445589840 0.46519686
## 4877  0.4295459 0.7858627 FALSE   lisamurkowski 0.684856259 0.63716012
## 4878  0.4295459 0.7858627 FALSE   lisamurkowski 0.320109735 0.64188080
## 4880  0.4295459 0.7858627 FALSE   lisamurkowski 0.677442113 0.58458755
## 4884  0.4295459 0.7858627 FALSE   lisamurkowski 0.729078831 0.77697956
## 4891  0.4295459 0.7858627 FALSE   lisamurkowski 0.635552741 0.43769873
## 4892  0.4295459 0.7858627 FALSE   lisamurkowski 0.312066167 0.48619691
## 4893  0.4295459 0.7858627 FALSE   lisamurkowski 0.563709371 0.50832184
## 4895  0.4295459 0.7858627 FALSE   lisamurkowski 0.407809860 0.76173978
## 4896  0.4295459 0.7858627 FALSE   lisamurkowski 0.295064589 0.67654070
## 4898  0.4295459 0.7858627 FALSE   lisamurkowski 0.439702034 0.68387766
## 4907  0.4295459 0.7858627 FALSE   lisamurkowski 0.520148333 0.49554232
## 4913  0.4295459 0.7858627 FALSE   lisamurkowski 0.279355629 0.64906885
## 4914  0.4295459 0.7858627 FALSE   lisamurkowski 0.755442061 0.62715441
## 4916  0.4295459 0.7858627 FALSE   lisamurkowski 0.413575853 0.58038890
## 4918  0.4295459 0.7858627 FALSE   lisamurkowski 0.110707009 0.52477853
## 4919  0.4295459 0.7858627 FALSE   lisamurkowski 0.245829269 0.86076210
## 4933  0.4295459 0.7858627 FALSE   lisamurkowski 0.388175530 0.67525306
## 4939  0.4295459 0.7858627 FALSE   lisamurkowski 0.373188886 0.79653718
## 4949  0.4421794 0.6590007 FALSE     maziehirono 0.409996508 0.68905929
## 4950  0.4421794 0.6590007 FALSE     maziehirono 0.318518998 0.69092769
## 4951  0.4421794 0.6590007 FALSE     maziehirono 0.517223812 0.49442426
## 4952  0.4421794 0.6590007 FALSE     maziehirono 0.730488183 0.68238947
## 4953  0.4421794 0.6590007 FALSE     maziehirono 0.439269975 0.65849661
## 4955  0.4421794 0.6590007 FALSE     maziehirono 0.291741893 0.49300297
## 4958  0.4421794 0.6590007 FALSE     maziehirono 0.317415978 0.48330512
## 4959  0.4421794 0.6590007 FALSE     maziehirono 0.439583760 0.66453035
## 4960  0.4421794 0.6590007 FALSE     maziehirono 0.558508180 0.71003036
## 4961  0.4421794 0.6590007 FALSE     maziehirono 0.404256277 0.65467230
## 4962  0.4421794 0.6590007 FALSE     maziehirono 0.478761526 0.45376014
## 4964  0.4421794 0.6590007 FALSE     maziehirono 0.402120898 0.46821057
## 4965  0.4421794 0.6590007 FALSE     maziehirono 0.393475718 0.50452063
## 4966  0.4421794 0.6590007 FALSE     maziehirono 0.721043880 0.49581953
## 4967  0.4421794 0.6590007 FALSE     maziehirono 0.478306686 0.80168511
## 4968  0.4421794 0.6590007 FALSE     maziehirono 0.429820913 0.59936665
## 4971  0.4421794 0.6590007 FALSE     maziehirono 0.217030918 0.63242772
## 4972  0.4421794 0.6590007 FALSE     maziehirono 0.245728399 0.69410369
## 4973  0.4421794 0.6590007 FALSE     maziehirono 0.418711710 0.57944263
## 4974  0.4421794 0.6590007 FALSE     maziehirono 0.329118402 0.62849076
## 4976  0.4421794 0.6590007 FALSE     maziehirono 0.548286964 0.66224588
## 4978  0.4421794 0.6590007 FALSE     maziehirono 0.285604841 0.63588584
## 4979  0.4421794 0.6590007 FALSE     maziehirono 0.685798199 0.79310262
## 4987  0.4421794 0.6590007 FALSE     maziehirono 0.479909249 0.73145052
## 4989  0.4421794 0.6590007 FALSE     maziehirono 0.270357074 0.51476007
## 4993  0.4421794 0.6590007 FALSE     maziehirono 0.551913545 0.57920071
## 4994  0.4421794 0.6590007 FALSE     maziehirono 0.647683333 0.67082664
## 4995  0.4421794 0.6590007 FALSE     maziehirono 0.564875089 0.48685137
## 4997  0.4421794 0.6590007 FALSE     maziehirono 0.229444371 0.54172440
## 5012  0.4421794 0.6590007 FALSE     maziehirono 0.730666518 0.76744199
## 5014  0.4421794 0.6590007 FALSE     maziehirono 0.499679843 0.60518628
## 5016  0.4421794 0.6590007 FALSE     maziehirono 0.377198766 0.74842941
## 5017  0.4421794 0.6590007 FALSE     maziehirono 0.404050036 0.72179784
## 5018  0.4421794 0.6590007 FALSE     maziehirono 0.370935982 0.55713543
## 5019  0.4421794 0.6590007 FALSE     maziehirono 0.552324220 0.61183584
## 5021  0.4421794 0.6590007 FALSE     maziehirono 0.496430154 0.53199058
## 5024  0.4421794 0.6590007 FALSE     maziehirono 0.678477193 0.49811496
## 5026  0.4421794 0.6590007 FALSE     maziehirono 0.642802268 0.79026547
## 5028  0.4421794 0.6590007 FALSE     maziehirono 0.362366887 0.78030479
## 5042  0.4421794 0.6590007 FALSE     maziehirono 0.297184468 0.55565494
## 5045  0.4421794 0.6590007 FALSE     maziehirono 0.547057337 0.80336847
## 5050  0.4421794 0.6590007 FALSE     maziehirono 0.240694264 0.85286637
## 5052  0.4421794 0.6590007 FALSE     maziehirono 0.529783547 0.54114982
## 5058  0.4421794 0.6590007 FALSE     maziehirono 0.796763814 0.75427176
## 5062  0.4421794 0.6590007 FALSE     maziehirono 0.296282655 0.59422186
## 5063  0.4421794 0.6590007 FALSE     maziehirono 0.609371870 0.70747114
## 5064  0.4421794 0.6590007 FALSE     maziehirono 0.497978833 0.65394504
## 5067  0.4421794 0.6590007 FALSE     maziehirono 0.300637474 0.72508455
## 5076  0.4421794 0.6590007 FALSE     maziehirono 0.432023252 0.76098579
## 5078  0.4421794 0.6590007 FALSE     maziehirono 0.642570722 0.61244024
## 5079  0.4468391 0.4402281 FALSE  SenatorCollins 0.525425672 0.50516180
## 5088  0.4468391 0.4402281 FALSE  SenatorCollins 0.504300183 0.46260530
## 5095  0.4468391 0.4402281 FALSE  SenatorCollins 0.459079854 0.57016987
## 5100  0.4468391 0.4402281 FALSE  SenatorCollins 0.519353781 0.41752975
## 5101  0.4468391 0.4402281 FALSE  SenatorCollins 0.792295834 0.54211877
## 5102  0.4468391 0.4402281 FALSE  SenatorCollins 0.619321044 0.69376204
## 5103  0.4468391 0.4402281 FALSE  SenatorCollins 0.430795158 0.76089397
## 5105  0.4468391 0.4402281 FALSE  SenatorCollins 0.642015389 0.45692956
## 5107  0.4468391 0.4402281 FALSE  SenatorCollins 0.299471770 0.46959289
## 5110  0.4468391 0.4402281 FALSE  SenatorCollins 0.482017052 0.80103831
## 5111  0.4468391 0.4402281 FALSE  SenatorCollins 0.373689920 0.51839492
## 5114  0.4468391 0.4402281 FALSE  SenatorCollins 0.292674915 0.56811517
## 5118  0.4468391 0.4402281 FALSE  SenatorCollins 0.554382455 0.54689226
## 5119  0.4468391 0.4402281 FALSE  SenatorCollins 0.467167318 0.39815253
## 5120  0.4468391 0.4402281 FALSE  SenatorCollins 0.623459718 0.41914603
## 5123  0.4468391 0.4402281 FALSE  SenatorCollins 0.387006972 0.62801713
## 5124  0.4468391 0.4402281 FALSE  SenatorCollins 0.425646101 0.50179770
## 5125  0.4468391 0.4402281 FALSE  SenatorCollins 0.278265836 0.61427682
## 5127  0.4468391 0.4402281 FALSE  SenatorCollins 0.559553612 0.38605829
## 5134  0.4468391 0.4402281 FALSE  SenatorCollins 0.674510775 0.47976738
## 5135  0.4468391 0.4402281 FALSE  SenatorCollins 0.421921942 0.44198533
## 5139  0.4468391 0.4402281 FALSE  SenatorCollins 0.514417552 0.62816386
## 5154  0.4468391 0.4402281 FALSE  SenatorCollins 0.560935709 0.64126765
## 5156  0.4468391 0.4402281 FALSE  SenatorCollins 0.554861872 0.46163360
## 5162  0.4468391 0.4402281 FALSE  SenatorCollins 0.646987107 0.59169591
## 5170  0.4468391 0.4402281 FALSE  SenatorCollins 0.487932884 0.72887349
## 5178  0.4468391 0.4402281 FALSE  SenatorCollins 0.559758456 0.58241744
## 5181  0.4468391 0.4402281 FALSE  SenatorCollins 0.392618409 0.44210787
## 5184  0.4468391 0.4402281 FALSE  SenatorCollins 0.711963292 0.54882538
## 5202  0.4468391 0.4402281 FALSE  SenatorCollins 0.298324348 0.52838383
## 5206  0.4468391 0.4402281 FALSE  SenatorCollins 0.395600841 0.71858023
## 5209  0.4468391 0.4402281 FALSE  SenatorCollins 0.418357038 0.62950686
## 5226  0.4468391 0.4402281 FALSE  SenatorCollins 0.655206608 0.65434628
## 5231  0.4468391 0.4402281 FALSE  SenatorCollins 0.686075286 0.61010370
## 5237  0.4468391 0.4402281 FALSE  SenatorCollins 0.717880803 0.47959953
## 5242  0.4468391 0.4402281 FALSE  SenatorCollins 0.469846244 0.50518907
## 5254  0.4468391 0.4402281 FALSE  SenatorCollins 0.212267639 0.61458370
## 5278  0.4468391 0.4402281 FALSE  SenatorCollins 0.462017535 0.76705802
## 5279  0.4468391 0.4402281 FALSE  SenatorCollins 0.230968876 0.52090313
## 5280  0.4468391 0.4402281 FALSE  SenatorCollins 0.507100528 0.56557203
## 5316  0.4468391 0.4402281 FALSE  SenatorCollins 0.755843202 0.60447481
## 5329  0.4468391 0.4402281 FALSE  SenatorCollins 0.355189202 0.77706664
## 5334  0.4468391 0.4402281 FALSE  SenatorCollins 0.674619021 0.55739397
## 5352  0.4614245 0.5950597 FALSE       clairecmc 0.506474572 0.68634160
## 5353  0.4614245 0.5950597 FALSE       clairecmc 0.676490422 0.49462343
## 5355  0.4614245 0.5950597 FALSE       clairecmc 0.823347068 0.61133851
## 5356  0.4614245 0.5950597 FALSE       clairecmc 0.449183772 0.46511790
## 5360  0.4614245 0.5950597 FALSE       clairecmc 0.551909071 0.65002993
## 5362  0.4614245 0.5950597 FALSE       clairecmc 0.832496004 0.69793463
## 5363  0.4614245 0.5950597 FALSE       clairecmc 0.550352100 0.60047527
## 5367  0.4614245 0.5950597 FALSE       clairecmc 0.504492492 0.63474764
## 5371  0.4614245 0.5950597 FALSE       clairecmc 0.299234495 0.65243366
## 5382  0.4614245 0.5950597 FALSE       clairecmc 0.472063139 0.55299172
## 5385  0.4614245 0.5950597 FALSE       clairecmc 0.364006942 0.97217921
## 5394  0.4614245 0.5950597 FALSE       clairecmc 0.649161408 0.66368038
## 5398  0.4614245 0.5950597 FALSE       clairecmc 0.405014940 0.68494805
## 5407  0.4614245 0.5950597 FALSE       clairecmc 0.285454158 0.62767823
## 5409  0.4614245 0.5950597 FALSE       clairecmc 0.446652975 0.58693507
## 5411  0.4614245 0.5950597 FALSE       clairecmc 0.629540900 0.76026911
## 5412  0.4614245 0.5950597 FALSE       clairecmc 0.646076338 0.47285815
## 5417  0.4614245 0.5950597 FALSE       clairecmc 0.430143469 0.63461860
## 5433  0.4614245 0.5950597 FALSE       clairecmc 0.399971930 0.63760645
## 5454  0.4614245 0.5950597 FALSE       clairecmc 0.297826835 0.72045242
## 5460  0.4614245 0.5950597 FALSE       clairecmc 0.731486725 0.67714094
## 5467  0.4631773 0.7920311 FALSE     SenBennetCO 0.557047294 0.68202728
## 5468  0.4631773 0.7920311 FALSE     SenBennetCO 0.224996653 0.54756143
## 5469  0.4631773 0.7920311 FALSE     SenBennetCO 0.804901279 0.66624071
## 5470  0.4631773 0.7920311 FALSE     SenBennetCO 0.322009735 0.64028175
## 5472  0.4631773 0.7920311 FALSE     SenBennetCO 0.391976521 0.50494242
## 5474  0.4631773 0.7920311 FALSE     SenBennetCO 0.674400955 0.53364644
## 5477  0.4631773 0.7920311 FALSE     SenBennetCO 0.477150967 0.40062615
## 5478  0.4631773 0.7920311 FALSE     SenBennetCO 0.373546089 0.79919723
## 5479  0.4631773 0.7920311 FALSE     SenBennetCO 0.513090805 0.67469424
## 5480  0.4631773 0.7920311 FALSE     SenBennetCO 0.683937706 0.50389013
## 5481  0.4631773 0.7920311 FALSE     SenBennetCO 0.538085295 0.43453028
## 5483  0.4631773 0.7920311 FALSE     SenBennetCO 0.678772263 0.58609734
## 5485  0.4631773 0.7920311 FALSE     SenBennetCO 0.301901374 0.74294067
## 5486  0.4631773 0.7920311 FALSE     SenBennetCO 0.436715349 0.71092805
## 5489  0.4631773 0.7920311 FALSE     SenBennetCO 0.512502729 0.64498394
## 5490  0.4631773 0.7920311 FALSE     SenBennetCO 0.407712473 0.72534443
## 5493  0.4631773 0.7920311 FALSE     SenBennetCO 0.570980214 0.49003788
## 5498  0.4631773 0.7920311 FALSE     SenBennetCO 0.454135716 0.79037278
## 5502  0.4631773 0.7920311 FALSE     SenBennetCO 0.562601544 0.62352630
## 5506  0.4631773 0.7920311 FALSE     SenBennetCO 0.280491078 0.64773073
## 5508  0.4631773 0.7920311 FALSE     SenBennetCO 0.653875738 0.48038553
## 5509  0.4631773 0.7920311 FALSE     SenBennetCO 0.561335284 0.58704538
## 5511  0.4631773 0.7920311 FALSE     SenBennetCO 0.825663524 0.62302601
## 5512  0.4631773 0.7920311 FALSE     SenBennetCO 0.503872423 0.72969407
## 5514  0.4631773 0.7920311 FALSE     SenBennetCO 0.386855215 0.77430877
## 5515  0.4631773 0.7920311 FALSE     SenBennetCO 0.291733369 0.56121406
## 5523  0.4631773 0.7920311 FALSE     SenBennetCO 0.287701055 0.49598439
## 5525  0.4631773 0.7920311 FALSE     SenBennetCO 0.416960314 0.57989109
## 5530  0.4631773 0.7920311 FALSE     SenBennetCO 0.429104468 0.59950425
## 5540  0.4631773 0.7920311 FALSE     SenBennetCO 0.447998892 0.46520117
## 5544  0.4631773 0.7920311 FALSE     SenBennetCO 0.296146390 0.67510772
## 5546  0.4631773 0.7920311 FALSE     SenBennetCO 0.476768979 0.55371418
## 5547  0.4631773 0.7920311 FALSE     SenBennetCO 0.265851655 0.51894961
## 5548  0.4631773 0.7920311 FALSE     SenBennetCO 0.421730384 0.55007758
## 5550  0.4631773 0.7920311 FALSE     SenBennetCO 0.313885165 0.48539904
## 5552  0.4631773 0.7920311 FALSE     SenBennetCO 0.316109451 0.70942085
## 5554  0.4631773 0.7920311 FALSE     SenBennetCO 0.411770198 0.75719237
## 5556  0.4631773 0.7920311 FALSE     SenBennetCO 0.502488967 0.53371547
## 5566  0.4631773 0.7920311 FALSE     SenBennetCO 0.682735309 0.80381810
## 5573  0.4631773 0.7920311 FALSE     SenBennetCO 0.716138929 0.57459757
## 5574  0.4631773 0.7920311 FALSE     SenBennetCO 0.535935377 0.77570279
## 5582  0.4631773 0.7920311 FALSE     SenBennetCO 0.532677459 0.97660369
## 5587  0.4631773 0.7920311 FALSE     SenBennetCO 0.725847684 0.50173113
## 5603  0.4631773 0.7920311 FALSE     SenBennetCO 0.596601347 0.81784922
## 5611  0.4631773 0.7920311 FALSE     SenBennetCO 0.685865882 0.63875231
## 5624  0.4631773 0.7920311 FALSE     SenBennetCO 0.650939270 0.68467217
## 5625  0.4631773 0.7920311 FALSE     SenBennetCO 0.560046909 0.73307125
## 5628  0.4631773 0.7920311 FALSE     SenBennetCO 0.629548461 0.84263704
## 5631  0.4631773 0.7920311 FALSE     SenBennetCO 0.610635752 0.72480304
## 5632  0.4631773 0.7920311 FALSE     SenBennetCO 0.756093055 0.62840085
## 5665  0.4631773 0.7920311 FALSE     SenBennetCO 0.535378861 0.69972076
## 5687  0.4780429 0.3756421 FALSE     SenStabenow 0.490569917 0.72863965
## 5688  0.4780429 0.3756421 FALSE     SenStabenow 0.827260935 0.59899190
## 5690  0.4780429 0.3756421 FALSE     SenStabenow 0.565410995 0.63927927
## 5691  0.4780429 0.3756421 FALSE     SenStabenow 0.398043824 0.68193512
## 5692  0.4780429 0.3756421 FALSE     SenStabenow 0.518869012 0.62701252
## 5693  0.4780429 0.3756421 FALSE     SenStabenow 0.290946836 0.56623616
## 5695  0.4780429 0.3756421 FALSE     SenStabenow 0.445318500 0.63419858
## 5703  0.4780429 0.3756421 FALSE     SenStabenow 0.478168047 0.50375475
## 5704  0.4780429 0.3756421 FALSE     SenStabenow 0.426876193 0.50225759
## 5705  0.4780429 0.3756421 FALSE     SenStabenow 0.560770123 0.44980518
## 5707  0.4780429 0.3756421 FALSE     SenStabenow 0.420185083 0.62985193
## 5708  0.4780429 0.3756421 FALSE     SenStabenow 0.431207782 0.55073586
## 5717  0.4780429 0.3756421 FALSE     SenStabenow 0.623963587 0.41039146
## 5724  0.4780429 0.3756421 FALSE     SenStabenow 0.516199944 0.59664636
## 5726  0.4780429 0.3756421 FALSE     SenStabenow 0.290128082 0.71380806
## 5727  0.4780429 0.3756421 FALSE     SenStabenow 0.644055392 0.44896103
## 5728  0.4780429 0.3756421 FALSE     SenStabenow 0.402439274 0.46187895
## 5731  0.4780429 0.3756421 FALSE     SenStabenow 0.210895757 0.61289655
## 5732  0.4780429 0.3756421 FALSE     SenStabenow 0.513319691 0.56353266
## 5733  0.4780429 0.3756421 FALSE     SenStabenow 0.306716014 0.67547111
## 5735  0.4780429 0.3756421 FALSE     SenStabenow 0.325315946 0.45176901
## 5738  0.4780429 0.3756421 FALSE     SenStabenow 0.388977539 0.42995761
## 5749  0.4780429 0.3756421 FALSE     SenStabenow 0.114999574 0.50079214
## 5750  0.4780429 0.3756421 FALSE     SenStabenow 0.290135682 0.64038427
## 5756  0.4780429 0.3756421 FALSE     SenStabenow 0.463312579 0.57013108
## 5763  0.4780429 0.3756421 FALSE     SenStabenow 0.612432030 0.55468991
## 5764  0.4780429 0.3756421 FALSE     SenStabenow 0.297433082 0.46353853
## 5765  0.4780429 0.3756421 FALSE     SenStabenow 0.277023756 0.61315200
## 5767  0.4780429 0.3756421 FALSE     SenStabenow 0.387824760 0.62829333
## 5768  0.4780429 0.3756421 FALSE     SenStabenow 0.416125063 0.42766253
## 5769  0.4780429 0.3756421 FALSE     SenStabenow 0.396831759 0.71883899
## 5770  0.4780429 0.3756421 FALSE     SenStabenow 0.432480996 0.76103563
## 5772  0.4780429 0.3756421 FALSE     SenStabenow 0.557086670 0.37532837
## 5781  0.4780429 0.3756421 FALSE     SenStabenow 0.837717294 0.68821478
## 5799  0.4780429 0.3756421 FALSE     SenStabenow 0.501076835 0.48454118
## 5801  0.4780429 0.3756421 FALSE     SenStabenow 0.516132424 0.44946066
## 5807  0.4780429 0.3756421 FALSE     SenStabenow 0.514595183 0.68393392
## 5817  0.4780429 0.3756421 FALSE     SenStabenow 0.319353237 0.60152107
## 5826  0.4781925 0.5287547 FALSE      MarkWarner 0.215783033 0.62119122
## 5827  0.4781925 0.5287547 FALSE      MarkWarner 0.395405226 0.39950498
## 5828  0.4781925 0.5287547 FALSE      MarkWarner 0.837622305 0.40194674
## 5829  0.4781925 0.5287547 FALSE      MarkWarner 0.691728836 0.78592441
## 5830  0.4781925 0.5287547 FALSE      MarkWarner 0.436380527 0.66328756
## 5832  0.4781925 0.5287547 FALSE      MarkWarner 0.116331767 0.51021867
## 5834  0.4781925 0.5287547 FALSE      MarkWarner 0.672259929 0.56432254
## 5835  0.4781925 0.5287547 FALSE      MarkWarner 0.395064735 0.63233947
## 5836  0.4781925 0.5287547 FALSE      MarkWarner 0.443672651 0.55855132
## 5838  0.4781925 0.5287547 FALSE      MarkWarner 0.467553841 0.57082270
## 5839  0.4781925 0.5287547 FALSE      MarkWarner 0.237657819 0.85018408
## 5841  0.4781925 0.5287547 FALSE      MarkWarner 0.295369255 0.71769466
## 5842  0.4781925 0.5287547 FALSE      MarkWarner 0.381415341 0.31291558
## 5845  0.4781925 0.5287547 FALSE      MarkWarner 0.402681087 0.68365173
## 5851  0.4781925 0.5287547 FALSE      MarkWarner 0.558826338 0.64260845
## 5853  0.4781925 0.5287547 FALSE      MarkWarner 0.539965600 0.65748945
## 5855  0.4781925 0.5287547 FALSE      MarkWarner 0.511234754 0.49058007
## 5859  0.4781925 0.5287547 FALSE      MarkWarner 0.643472272 0.46772128
## 5860  0.4781925 0.5287547 FALSE      MarkWarner 0.448841913 0.63490485
## 5864  0.4781925 0.5287547 FALSE      MarkWarner 0.674638717 0.48900337
## 5866  0.4781925 0.5287547 FALSE      MarkWarner 0.617350582 0.69525005
## 5867  0.4781925 0.5287547 FALSE      MarkWarner 0.275992997 0.50196924
## 5869  0.4781925 0.5287547 FALSE      MarkWarner 0.514336485 0.62819317
## 5870  0.4781925 0.5287547 FALSE      MarkWarner 0.627435847 0.42998063
## 5874  0.4781925 0.5287547 FALSE      MarkWarner 0.425933583 0.63192635
## 5875  0.4781925 0.5287547 FALSE      MarkWarner 0.717984183 0.48743829
## 5880  0.4781925 0.5287547 FALSE      MarkWarner 0.652526991 0.65741755
## 5881  0.4781925 0.5287547 FALSE      MarkWarner 0.455185326 0.46379376
## 5886  0.4781925 0.5287547 FALSE      MarkWarner 0.823937627 0.60694720
## 5888  0.4781925 0.5287547 FALSE      MarkWarner 0.519862553 0.52394973
## 5892  0.4781925 0.5287547 FALSE      MarkWarner 0.512200119 0.68433668
## 5894  0.4781925 0.5287547 FALSE      MarkWarner 0.835234167 0.55916847
## 5897  0.4781925 0.5287547 FALSE      MarkWarner 0.442472045 0.52680150
## 5898  0.4781925 0.5287547 FALSE      MarkWarner 0.299107306 0.48092878
## 5900  0.4781925 0.5287547 FALSE      MarkWarner 0.283444632 0.62148683
## 5915  0.4781925 0.5287547 FALSE      MarkWarner 0.643818714 0.59723052
## 5916  0.4781925 0.5287547 FALSE      MarkWarner 0.297568264 0.57755606
## 5917  0.4781925 0.5287547 FALSE      MarkWarner 0.531201387 0.43198740
## 5918  0.4781925 0.5287547 FALSE      MarkWarner 0.489984459 0.72866730
## 5932  0.4781925 0.5287547 FALSE      MarkWarner 0.326995932 0.61012931
## 5934  0.4781925 0.5287547 FALSE      MarkWarner 0.326344644 0.47171305
## 5936  0.4781925 0.5287547 FALSE      MarkWarner 0.558092580 0.47959386
## 5939  0.4781925 0.5287547 FALSE      MarkWarner 0.683407821 0.61490116
## 5940  0.4781925 0.5287547 FALSE      MarkWarner 0.569532664 0.69807071
## 5944  0.4781925 0.5287547 FALSE      MarkWarner 0.359362866 0.77861286
## 5948  0.4781925 0.5287547 FALSE      MarkWarner 0.301779208 0.53960930
## 5949  0.4781925 0.5287547 FALSE      MarkWarner 0.690459358 0.71249645
## 5954  0.4781925 0.5287547 FALSE      MarkWarner 0.555345910 0.58694169
## 5976  0.4781925 0.5287547 FALSE      MarkWarner 0.846132536 0.48775334
## 5982  0.4781925 0.5287547 FALSE      MarkWarner 0.400485535 0.72000595
## 5988  0.4781925 0.5287547 FALSE      MarkWarner 0.723718098 0.70668307
## 5989  0.4781925 0.5287547 FALSE      MarkWarner 0.603545268 0.56733113
## 5997  0.4781925 0.5287547 FALSE      MarkWarner 0.478067346 0.40064206
## 6005  0.4781925 0.5287547 FALSE      MarkWarner 0.665281697 0.51599227
## 6009  0.4781925 0.5287547 FALSE      MarkWarner 0.415237975 0.83093895
## 6016  0.4781925 0.5287547 FALSE      MarkWarner 0.434193599 0.76129856
## 6025  0.4781925 0.5287547 FALSE      MarkWarner 0.504023072 0.56733035
## 6026  0.4781925 0.5287547 FALSE      MarkWarner 0.312748292 0.68029149
## 6060  0.4781925 0.5287547 FALSE      MarkWarner 0.743924671 0.52014477
## 6071  0.4781925 0.5287547 FALSE      MarkWarner 0.791320145 0.54768273
## 6075  0.4781925 0.5287547 FALSE      MarkWarner 0.710261481 0.55544505
## 6088  0.4781925 0.5287547 FALSE      MarkWarner 0.464600803 0.76707167
## 6097  0.4781925 0.5287547 FALSE      MarkWarner 0.408127631 0.49223322
## 6104  0.4781925 0.5287547 FALSE      MarkWarner 0.548766681 0.55560694
## 6116  0.4781925 0.5287547 FALSE      MarkWarner 0.551753132 0.49598651
## 6121  0.4831484 0.4291480 FALSE   SenBlumenthal 0.434046940 0.55168066
## 6122  0.4831484 0.4291480 FALSE   SenBlumenthal 0.280175854 0.75456466
## 6123  0.4831484 0.4291480 FALSE   SenBlumenthal 0.499302476 0.48498487
## 6125  0.4831484 0.4291480 FALSE   SenBlumenthal 0.421916879 0.63031194
## 6126  0.4831484 0.4291480 FALSE   SenBlumenthal 0.389973731 0.62917529
## 6127  0.4831484 0.4291480 FALSE   SenBlumenthal 0.514486837 0.68394700
## 6129  0.4831484 0.4291480 FALSE   SenBlumenthal 0.399562141 0.68238349
## 6135  0.4831484 0.4291480 FALSE   SenBlumenthal 0.807514126 0.64380767
## 6136  0.4831484 0.4291480 FALSE   SenBlumenthal 0.431590976 0.50477960
## 6138  0.4831484 0.4291480 FALSE   SenBlumenthal 0.423954898 0.53370839
## 6139  0.4831484 0.4291480 FALSE   SenBlumenthal 0.421632728 0.43956860
## 6141  0.4831484 0.4291480 FALSE   SenBlumenthal 0.479434800 0.50378563
## 6146  0.4831484 0.4291480 FALSE   SenBlumenthal 0.370874170 0.74509724
## 6148  0.4831484 0.4291480 FALSE   SenBlumenthal 0.321947543 0.60361540
## 6152  0.4831484 0.4291480 FALSE   SenBlumenthal 0.279329265 0.61537165
## 6154  0.4831484 0.4291480 FALSE   SenBlumenthal 0.291890837 0.71488524
## 6156  0.4831484 0.4291480 FALSE   SenBlumenthal 0.446566262 0.63438861
## 6158  0.4831484 0.4291480 FALSE   SenBlumenthal 0.398109060 0.71917712
## 6161  0.4831484 0.4291480 FALSE   SenBlumenthal 0.212793559 0.61531863
## 6165  0.4831484 0.4291480 FALSE   SenBlumenthal 0.392456249 0.44000303
## 6166  0.4831484 0.4291480 FALSE   SenBlumenthal 0.327513603 0.45831641
## 6172  0.4831484 0.4291480 FALSE   SenBlumenthal 0.764823006 0.38437782
## 6173  0.4831484 0.4291480 FALSE   SenBlumenthal 0.556865164 0.79907671
## 6178  0.4831484 0.4291480 FALSE   SenBlumenthal 0.652871430 0.78143074
## 6181  0.4831484 0.4291480 FALSE   SenBlumenthal 0.470750555 0.43293133
## 6187  0.4831484 0.4291480 FALSE   SenBlumenthal 0.386866321 0.30957574
## 6190  0.4831484 0.4291480 FALSE   SenBlumenthal 0.292346325 0.64214962
## 6191  0.4831484 0.4291480 FALSE   SenBlumenthal 0.375660680 0.52046268
## 6197  0.4831484 0.4291480 FALSE   SenBlumenthal 0.299381469 0.46915975
## 6210  0.4831484 0.4291480 FALSE   SenBlumenthal 0.745070702 0.51181097
## 6221  0.4831484 0.4291480 FALSE   SenBlumenthal 0.648946676 0.58940740
## 6231  0.4831484 0.4291480 FALSE   SenBlumenthal 0.621734354 0.69231198
## 6235  0.4831484 0.4291480 FALSE   SenBlumenthal 0.712850484 0.54689715
## 6242  0.4831484 0.4291480 FALSE   SenBlumenthal 0.275156402 0.49150658
## 6250  0.4831484 0.4291480 FALSE   SenBlumenthal 0.238550596 0.68058160
## 6253  0.4831484 0.4291480 FALSE   SenBlumenthal 0.560134670 0.38719236
## 6254  0.4831484 0.4291480 FALSE   SenBlumenthal 0.718146422 0.47809550
## 6257  0.4831484 0.4291480 FALSE   SenBlumenthal 0.484361407 0.80092046
## 6269  0.4844430 0.8259203 FALSE     SenJackReed 0.504534081 0.53394106
## 6271  0.4844430 0.8259203 FALSE     SenJackReed 0.392331943 0.67324333
## 6272  0.4844430 0.8259203 FALSE     SenJackReed 0.409784108 0.75974929
## 6276  0.4844430 0.8259203 FALSE     SenJackReed 0.539850164 0.54561140
## 6278  0.4844430 0.8259203 FALSE     SenJackReed 0.561306865 0.68495908
## 6280  0.4844430 0.8259203 FALSE     SenJackReed 0.449265032 0.46511011
## 6283  0.4844430 0.8259203 FALSE     SenJackReed 0.449741093 0.80059888
## 6290  0.4844430 0.8259203 FALSE     SenJackReed 0.212952039 0.64344332
## 6292  0.4844430 0.8259203 FALSE     SenJackReed 0.424052930 0.67738755
## 6294  0.4844430 0.8259203 FALSE     SenJackReed 0.839705143 0.57568670
## 6295  0.4844430 0.8259203 FALSE     SenJackReed 0.613369199 0.72941354
## 6296  0.4844430 0.8259203 FALSE     SenJackReed 0.770617639 0.68786989
## 6298  0.4844430 0.8259203 FALSE     SenJackReed 0.407027480 0.72589409
## 6301  0.4844430 0.8259203 FALSE     SenJackReed 0.418137060 0.57960470
## 6302  0.4844430 0.8259203 FALSE     SenJackReed 0.639001393 0.43939596
## 6303  0.4844430 0.8259203 FALSE     SenJackReed 0.540173893 0.78501987
## 6306  0.4844430 0.8259203 FALSE     SenJackReed 0.540432946 0.70278662
## 6307  0.4844430 0.8259203 FALSE     SenJackReed 0.430531410 0.59920858
## 6308  0.4844430 0.8259203 FALSE     SenJackReed 0.279768264 0.64860434
## 6315  0.4844430 0.8259203 FALSE     SenJackReed 0.568184628 0.50997925
## 6316  0.4844430 0.8259203 FALSE     SenJackReed 0.533702313 0.97624301
## 6322  0.4844430 0.8259203 FALSE     SenJackReed 0.564182091 0.58819934
## 6327  0.4844430 0.8259203 FALSE     SenJackReed 0.463904898 0.61993633
## 6331  0.4844430 0.8259203 FALSE     SenJackReed 0.448315686 0.68323593
## 6333  0.4844430 0.8259203 FALSE     SenJackReed 0.438242445 0.71037429
## 6336  0.4844430 0.8259203 FALSE     SenJackReed 0.573000324 0.49066401
## 6339  0.4844430 0.8259203 FALSE     SenJackReed 0.321497040 0.64074563
## 6351  0.4844430 0.8259203 FALSE     SenJackReed 0.576792964 0.39966230
## 6352  0.4844430 0.8259203 FALSE     SenJackReed 0.797074835 0.56520272
## 6353  0.4844430 0.8259203 FALSE     SenJackReed 0.483229962 0.45414790
## 6357  0.4844430 0.8259203 FALSE     SenJackReed 0.832818217 0.71236157
## 6361  0.4844430 0.8259203 FALSE     SenJackReed 0.392816551 0.50471851
## 6366  0.4844430 0.8259203 FALSE     SenJackReed 0.687940294 0.64137218
## 6367  0.4844430 0.8259203 FALSE     SenJackReed 0.291306200 0.78139307
## 6368  0.4844430 0.8259203 FALSE     SenJackReed 0.422944938 0.54983843
## 6371  0.4844430 0.8259203 FALSE     SenJackReed 0.686407549 0.73876395
## 6373  0.4844430 0.8259203 FALSE     SenJackReed 0.295272901 0.67628093
## 6379  0.4844430 0.8259203 FALSE     SenJackReed 0.516120640 0.64590357
## 6386  0.4844430 0.8259203 FALSE     SenJackReed 0.535973728 0.85394646
## 6403  0.4844430 0.8259203 FALSE     SenJackReed 0.805907222 0.66859420
## 6405  0.4844430 0.8259203 FALSE     SenJackReed 0.373221190 0.80566820
## 6406  0.4844430 0.8259203 FALSE     SenJackReed 0.653276798 0.68807390
## 6429  0.4844430 0.8259203 FALSE     SenJackReed 0.224621207 0.54791970
## 6432  0.4844430 0.8259203 FALSE     SenJackReed 0.291554202 0.56134591
## 6439  0.4914565 0.7536239 FALSE   SenGaryPeters 0.478929558 0.40062634
## 6441  0.4914565 0.7536239 FALSE   SenGaryPeters 0.576263378 0.39954153
## 6442  0.4914565 0.7536239 FALSE   SenGaryPeters 0.567178230 0.50968981
## 6443  0.4914565 0.7536239 FALSE   SenGaryPeters 0.755377752 0.62702136
## 6445  0.4914565 0.7536239 FALSE   SenGaryPeters 0.557978770 0.72881047
## 6446  0.4914565 0.7536239 FALSE   SenGaryPeters 0.725608037 0.50151144
## 6447  0.4914565 0.7536239 FALSE   SenGaryPeters 0.674251253 0.53352309
## 6449  0.4914565 0.7536239 FALSE   SenGaryPeters 0.572064802 0.49039732
## 6450  0.4914565 0.7536239 FALSE   SenGaryPeters 0.556521055 0.68156533
## 6451  0.4914565 0.7536239 FALSE   SenGaryPeters 0.524417561 0.49647458
## 6453  0.4914565 0.7536239 FALSE   SenGaryPeters 0.536512136 0.97550004
## 6456  0.4914565 0.7536239 FALSE   SenGaryPeters 0.562323422 0.58749247
## 6459  0.4914565 0.7536239 FALSE   SenGaryPeters 0.539488176 0.43478271
## 6460  0.4914565 0.7536239 FALSE   SenGaryPeters 0.433489328 0.59830868
## 6463  0.4914565 0.7536239 FALSE   SenGaryPeters 0.749810587 0.53546443
## 6464  0.4914565 0.7536239 FALSE   SenGaryPeters 0.631975624 0.83713951
## 6465  0.4914565 0.7536239 FALSE   SenGaryPeters 0.731223889 0.69075206
## 6467  0.4914565 0.7536239 FALSE   SenGaryPeters 0.387335014 0.76575981
## 6469  0.4914565 0.7536239 FALSE   SenGaryPeters 0.612313114 0.59458903
## 6472  0.4914565 0.7536239 FALSE   SenGaryPeters 0.394971652 0.50399631
## 6475  0.4914565 0.7536239 FALSE   SenGaryPeters 0.302896750 0.73775704
## 6483  0.4914565 0.7536239 FALSE   SenGaryPeters 0.804319652 0.66445683
## 6484  0.4914565 0.7536239 FALSE   SenGaryPeters 0.415940404 0.74575739
## 6487  0.4914565 0.7536239 FALSE   SenGaryPeters 0.446086544 0.70537399
## 6496  0.4914565 0.7536239 FALSE   SenGaryPeters 0.316543735 0.48389725
## 6498  0.4914565 0.7536239 FALSE   SenGaryPeters 0.453726692 0.68117412
## 6501  0.4914565 0.7536239 FALSE   SenGaryPeters 0.504973706 0.73037303
## 6504  0.4914565 0.7536239 FALSE   SenGaryPeters 0.298629994 0.67065235
## 6507  0.4914565 0.7536239 FALSE   SenGaryPeters 0.678265299 0.58555049
## 6508  0.4914565 0.7536239 FALSE   SenGaryPeters 0.450362769 0.46497852
## 6509  0.4914565 0.7536239 FALSE   SenGaryPeters 0.544032513 0.80595782
## 6512  0.4914565 0.7536239 FALSE   SenGaryPeters 0.282994688 0.64388057
## 6513  0.4914565 0.7536239 FALSE   SenGaryPeters 0.466076803 0.61962299
## 6518  0.4914565 0.7536239 FALSE   SenGaryPeters 0.719163108 0.72452235
## 6522  0.4914565 0.7536239 FALSE   SenGaryPeters 0.515102643 0.64570257
## 6523  0.4914565 0.7536239 FALSE   SenGaryPeters 0.268355187 0.51688027
## 6526  0.4914565 0.7536239 FALSE   SenGaryPeters 0.372344913 0.79329062
## 6533  0.4914565 0.7536239 FALSE   SenGaryPeters 0.414296809 0.71687379
## 6538  0.4914565 0.7536239 FALSE   SenGaryPeters 0.795116356 0.56250767
## 6546  0.4914565 0.7536239 FALSE   SenGaryPeters 0.729160027 0.77409355
## 6551  0.4914565 0.7536239 FALSE   SenGaryPeters 0.483788304 0.45413985
## 6557  0.4914565 0.7536239 FALSE   SenGaryPeters 0.609285096 0.72108668
## 6573  0.4914565 0.7536239 FALSE   SenGaryPeters 0.429924933 0.67400931
## 6577  0.4914565 0.7536239 FALSE   SenGaryPeters 0.425743166 0.83588106
## 6578  0.4914565 0.7536239 FALSE   SenGaryPeters 0.535091503 0.69949258
## 6581  0.4914565 0.7536239 FALSE   SenGaryPeters 0.539118412 0.54545535
## 6584  0.4914565 0.7536239 FALSE   SenGaryPeters 0.685748403 0.97620739
## 6586  0.4914565 0.7536239 FALSE   SenGaryPeters 0.684521322 0.73168152
## 6603  0.4914565 0.7536239 FALSE   SenGaryPeters 0.425216341 0.54921889
## 6604  0.4914565 0.7536239 FALSE   SenGaryPeters 0.504741146 0.53395444
## 6605  0.4914565 0.7536239 FALSE   SenGaryPeters 0.290275368 0.49423313
## 6606  0.4914565 0.7536239 FALSE   SenGaryPeters 0.768903819 0.68316153
## 6618  0.4914565 0.7536239 FALSE   SenGaryPeters 0.685023778 0.63744343
## 6626  0.4914565 0.7536239 FALSE   SenGaryPeters 0.825074488 0.62165758
## 6630  0.4914565 0.7536239 FALSE   SenGaryPeters 0.293168596 0.59942382
## 6636  0.4914565 0.7536239 FALSE   SenGaryPeters 0.404274319 0.46765741
## 6638  0.4914565 0.7536239 FALSE   SenGaryPeters 0.486856927 0.80103714
## 6641  0.4914565 0.7536239 FALSE   SenGaryPeters 0.420979248 0.57865356
## 6643  0.4914565 0.7536239 FALSE   SenGaryPeters 0.637746991 0.43885415
## 6653  0.4914565 0.7536239 FALSE   SenGaryPeters 0.397921552 0.66864806
## 6659  0.4914565 0.7536239 FALSE   SenGaryPeters 0.513984126 0.61278952
## 6665  0.5062503 0.5090000 FALSE   SenWhitehouse 0.463182317 0.45914632
## 6666  0.5062503 0.5090000 FALSE   SenWhitehouse 0.438910320 0.66422630
## 6667  0.5062503 0.5090000 FALSE   SenWhitehouse 0.520848556 0.51358895
## 6668  0.5062503 0.5090000 FALSE   SenWhitehouse 0.516128168 0.68379985
## 6669  0.5062503 0.5090000 FALSE   SenWhitehouse 0.563299104 0.64008685
## 6670  0.5062503 0.5090000 FALSE   SenWhitehouse 0.486159169 0.80097930
## 6671  0.5062503 0.5090000 FALSE   SenWhitehouse 0.381191728 0.53210650
## 6672  0.5062503 0.5090000 FALSE   SenWhitehouse 0.517315968 0.59647952
## 6676  0.5062503 0.5090000 FALSE   SenWhitehouse 0.238117071 0.85053611
## 6677  0.5062503 0.5090000 FALSE   SenWhitehouse 0.805661599 0.64713178
## 6678  0.5062503 0.5090000 FALSE   SenWhitehouse 0.560401432 0.58192361
## 6681  0.5062503 0.5090000 FALSE   SenWhitehouse 0.301584566 0.53767576
## 6683  0.5062503 0.5090000 FALSE   SenWhitehouse 0.299679895 0.47816885
## 6686  0.5062503 0.5090000 FALSE   SenWhitehouse 0.402108805 0.72073382
## 6688  0.5062503 0.5090000 FALSE   SenWhitehouse 0.490096190 0.45316320
## 6689  0.5062503 0.5090000 FALSE   SenWhitehouse 0.276189090 0.49969633
## 6690  0.5062503 0.5090000 FALSE   SenWhitehouse 0.404284875 0.68450707
## 6691  0.5062503 0.5090000 FALSE   SenWhitehouse 0.296548183 0.64702614
## 6693  0.5062503 0.5090000 FALSE   SenWhitehouse 0.410293185 0.48640708
## 6694  0.5062503 0.5090000 FALSE   SenWhitehouse 0.442091241 0.52088348
## 6699  0.5062503 0.5090000 FALSE   SenWhitehouse 0.327323060 0.46844753
## 6702  0.5062503 0.5090000 FALSE   SenWhitehouse 0.434078593 0.54444374
## 6709  0.5062503 0.5090000 FALSE   SenWhitehouse 0.837422486 0.40135706
## 6711  0.5062503 0.5090000 FALSE   SenWhitehouse 0.232491250 0.52793032
## 6713  0.5062503 0.5090000 FALSE   SenWhitehouse 0.444189530 0.55917003
## 6714  0.5062503 0.5090000 FALSE   SenWhitehouse 0.326782097 0.60974031
## 6717  0.5062503 0.5090000 FALSE   SenWhitehouse 0.534463219 0.43348074
## 6721  0.5062503 0.5090000 FALSE   SenWhitehouse 0.717768831 0.48590666
## 6724  0.5062503 0.5090000 FALSE   SenWhitehouse 0.725317287 0.70469782
## 6725  0.5062503 0.5090000 FALSE   SenWhitehouse 0.241939212 0.68466336
## 6728  0.5062503 0.5090000 FALSE   SenWhitehouse 0.466938626 0.76731568
## 6736  0.5062503 0.5090000 FALSE   SenWhitehouse 0.710658464 0.55303643
## 6738  0.5062503 0.5090000 FALSE   SenWhitehouse 0.573016769 0.69652155
## 6742  0.5062503 0.5090000 FALSE   SenWhitehouse 0.754663756 0.60703158
## 6744  0.5062503 0.5090000 FALSE   SenWhitehouse 0.665234063 0.51357216
## 6745  0.5062503 0.5090000 FALSE   SenWhitehouse 0.766747586 0.39078463
## 6746  0.5062503 0.5090000 FALSE   SenWhitehouse 0.743930890 0.51835224
## 6747  0.5062503 0.5090000 FALSE   SenWhitehouse 0.654797390 0.65475382
## 6748  0.5062503 0.5090000 FALSE   SenWhitehouse 0.436220673 0.76177027
## 6749  0.5062503 0.5090000 FALSE   SenWhitehouse 0.771561708 0.66437402
## 6751  0.5062503 0.5090000 FALSE   SenWhitehouse 0.427975374 0.63308408
## 6759  0.5062503 0.5090000 FALSE   SenWhitehouse 0.544481861 0.65583548
## 6772  0.5062503 0.5090000 FALSE   SenWhitehouse 0.605460163 0.56277166
## 6774  0.5062503 0.5090000 FALSE   SenWhitehouse 0.557770662 0.47905564
## 6777  0.5062503 0.5090000 FALSE   SenWhitehouse 0.283213656 0.62101461
## 6781  0.5062503 0.5090000 FALSE   SenWhitehouse 0.684807900 0.61207881
## 6788  0.5062503 0.5090000 FALSE   SenWhitehouse 0.215544093 0.62054174
## 6789  0.5062503 0.5090000 FALSE   SenWhitehouse 0.801386797 0.74514016
## 6799  0.5062503 0.5090000 FALSE   SenWhitehouse 0.846032764 0.48662716
## 6800  0.5062503 0.5090000 FALSE   SenWhitehouse 0.550915318 0.49384602
## 6801  0.5062503 0.5090000 FALSE   SenWhitehouse 0.635750823 0.75565721
## 6802  0.5062503 0.5090000 FALSE   SenWhitehouse 0.693638676 0.78448712
## 6810  0.5062503 0.5090000 FALSE   SenWhitehouse 0.672997871 0.56134208
## 6812  0.5062503 0.5090000 FALSE   SenWhitehouse 0.553012238 0.54839088
## 6817  0.5062503 0.5090000 FALSE   SenWhitehouse 0.416822844 0.83132346
## 6819  0.5062503 0.5090000 FALSE   SenWhitehouse 0.399162393 0.39655671
## 6838  0.5062503 0.5090000 FALSE   SenWhitehouse 0.283638734 0.75716683
## 6842  0.5062503 0.5090000 FALSE   SenWhitehouse 0.519983348 0.62685718
## 6896  0.5062503 0.5090000 FALSE   SenWhitehouse 0.835412470 0.55763610
## 6933  0.5062503 0.5090000 FALSE   SenWhitehouse 0.674348713 0.48725265
## 6947  0.5175387 0.7087600 FALSE   senrobportman 0.517851196 0.61310321
## 6948  0.5175387 0.7087600 FALSE   senrobportman 0.568392113 0.51003330
## 6949  0.5175387 0.7087600 FALSE   senrobportman 0.520548607 0.67658052
## 6950  0.5175387 0.7087600 FALSE   senrobportman 0.453249129 0.69308362
## 6951  0.5175387 0.7087600 FALSE   senrobportman 0.507660767 0.53396021
## 6952  0.5175387 0.7087600 FALSE   senrobportman 0.504021501 0.73201092
## 6953  0.5175387 0.7087600 FALSE   senrobportman 0.526536439 0.49665499
## 6956  0.5175387 0.7087600 FALSE   senrobportman 0.415198062 0.73660395
## 6957  0.5175387 0.7087600 FALSE   senrobportman 0.295697520 0.59544867
## 6960  0.5175387 0.7087600 FALSE   senrobportman 0.216492753 0.63541519
## 6962  0.5175387 0.7087600 FALSE   senrobportman 0.398451228 0.50233243
## 6965  0.5175387 0.7087600 FALSE   senrobportman 0.724962486 0.50088996
## 6967  0.5175387 0.7087600 FALSE   senrobportman 0.284830705 0.63937776
## 6968  0.5175387 0.7087600 FALSE   senrobportman 0.638083771 0.43900765
## 6969  0.5175387 0.7087600 FALSE   senrobportman 0.563408967 0.62398273
## 6970  0.5175387 0.7087600 FALSE   senrobportman 0.486200217 0.45396107
## 6971  0.5175387 0.7087600 FALSE   senrobportman 0.472488608 0.61747811
## 6973  0.5175387 0.7087600 FALSE   senrobportman 0.425848189 0.57603317
## 6975  0.5175387 0.7087600 FALSE   senrobportman 0.416720650 0.70664744
## 6976  0.5175387 0.7087600 FALSE   senrobportman 0.244607607 0.85819401
## 6981  0.5175387 0.7087600 FALSE   senrobportman 0.683623623 0.63475319
## 6984  0.5175387 0.7087600 FALSE   senrobportman 0.553951388 0.67887162
## 6986  0.5175387 0.7087600 FALSE   senrobportman 0.373682431 0.55490958
## 6991  0.5175387 0.7087600 FALSE   senrobportman 0.730536282 0.68695673
## 6992  0.5175387 0.7087600 FALSE   senrobportman 0.370556866 0.78918881
## 6995  0.5175387 0.7087600 FALSE   senrobportman 0.648306690 0.67798922
## 6997  0.5175387 0.7087600 FALSE   senrobportman 0.402531587 0.66136310
## 7000  0.5175387 0.7087600 FALSE   senrobportman 0.463041805 0.67277605
## 7006  0.5175387 0.7087600 FALSE   senrobportman 0.754362312 0.62458160
## 7007  0.5175387 0.7087600 FALSE   senrobportman 0.246103237 0.69936590
## 7008  0.5175387 0.7087600 FALSE   senrobportman 0.573201306 0.49071621
## 7009  0.5175387 0.7087600 FALSE   senrobportman 0.541117400 0.54582819
## 7013  0.5175387 0.7087600 FALSE   senrobportman 0.654089374 0.48051479
## 7014  0.5175387 0.7087600 FALSE   senrobportman 0.577336391 0.39977374
## 7017  0.5175387 0.7087600 FALSE   senrobportman 0.480986409 0.40046818
## 7023  0.5175387 0.7087600 FALSE   senrobportman 0.319381837 0.48175544
## 7029  0.5175387 0.7087600 FALSE   senrobportman 0.673624112 0.53298448
## 7033  0.5175387 0.7087600 FALSE   senrobportman 0.684497512 0.72625324
## 7045  0.5175387 0.7087600 FALSE   senrobportman 0.519620825 0.64626801
## 7051  0.5175387 0.7087600 FALSE   senrobportman 0.546045439 0.74971043
## 7053  0.5175387 0.7087600 FALSE   senrobportman 0.552768467 0.80026408
## 7056  0.5175387 0.7087600 FALSE   senrobportman 0.824318305 0.61944979
## 7057  0.5175387 0.7087600 FALSE   senrobportman 0.768298889 0.67986913
## 7058  0.5175387 0.7087600 FALSE   senrobportman 0.642772587 0.79031094
## 7059  0.5175387 0.7087600 FALSE   senrobportman 0.608413104 0.71320958
## 7061  0.5175387 0.7087600 FALSE   senrobportman 0.604318766 0.80410974
## 7065  0.5175387 0.7087600 FALSE   senrobportman 0.677141389 0.58420978
## 7067  0.5175387 0.7087600 FALSE   senrobportman 0.794223224 0.56096985
## 7078  0.5175387 0.7087600 FALSE   senrobportman 0.453204240 0.46440422
## 7079  0.5175387 0.7087600 FALSE   senrobportman 0.688637851 0.97403243
## 7081  0.5175387 0.7087600 FALSE   senrobportman 0.369894858 0.97453049
## 7082  0.5175387 0.7087600 FALSE   senrobportman 0.831589174 0.70491936
## 7092  0.5179329 0.5881033 FALSE   SenatorCardin 0.520938878 0.62676440
## 7093  0.5179329 0.5881033 FALSE   SenatorCardin 0.316782325 0.68621746
## 7094  0.5179329 0.5881033 FALSE   SenatorCardin 0.413039323 0.46290710
## 7095  0.5179329 0.5881033 FALSE   SenatorCardin 0.286966942 0.76080569
## 7096  0.5179329 0.5881033 FALSE   SenatorCardin 0.380425685 0.54424545
## 7098  0.5179329 0.5881033 FALSE   SenatorCardin 0.551007968 0.59611173
## 7100  0.5179329 0.5881033 FALSE   SenatorCardin 0.378813513 0.74970757
## 7101  0.5179329 0.5881033 FALSE   SenatorCardin 0.509902852 0.53373177
## 7102  0.5179329 0.5881033 FALSE   SenatorCardin 0.285512430 0.62800465
## 7106  0.5179329 0.5881033 FALSE   SenatorCardin 0.691244437 0.78633715
## 7108  0.5179329 0.5881033 FALSE   SenatorCardin 0.232118994 0.53428140
## 7109  0.5179329 0.5881033 FALSE   SenatorCardin 0.449500023 0.57839744
## 7110  0.5179329 0.5881033 FALSE   SenatorCardin 0.799186852 0.74838087
## 7111  0.5179329 0.5881033 FALSE   SenatorCardin 0.405347293 0.49645976
## 7113  0.5179329 0.5881033 FALSE   SenatorCardin 0.487929133 0.80116459
## 7114  0.5179329 0.5881033 FALSE   SenatorCardin 0.492102266 0.54952773
## 7118  0.5179329 0.5881033 FALSE   SenatorCardin 0.540059959 0.97504078
## 7120  0.5179329 0.5881033 FALSE   SenatorCardin 0.438718580 0.53867161
## 7121  0.5179329 0.5881033 FALSE   SenatorCardin 0.299611689 0.65358845
## 7122  0.5179329 0.5881033 FALSE   SenatorCardin 0.402128751 0.64138737
## 7124  0.5179329 0.5881033 FALSE   SenatorCardin 0.460432429 0.64191777
## 7127  0.5179329 0.5881033 FALSE   SenatorCardin 0.406904966 0.72381745
## 7129  0.5179329 0.5881033 FALSE   SenatorCardin 0.298430251 0.58448845
## 7130  0.5179329 0.5881033 FALSE   SenatorCardin 0.642116240 0.60367233
## 7131  0.5179329 0.5881033 FALSE   SenatorCardin 0.558419578 0.64290270
## 7132  0.5179329 0.5881033 FALSE   SenatorCardin 0.495405295 0.72893775
## 7138  0.5179329 0.5881033 FALSE   SenatorCardin 0.633179116 0.43610426
## 7139  0.5179329 0.5881033 FALSE   SenatorCardin 0.445666223 0.66856199
## 7141  0.5179329 0.5881033 FALSE   SenatorCardin 0.488492722 0.45357012
## 7142  0.5179329 0.5881033 FALSE   SenatorCardin 0.681914290 0.61982904
## 7144  0.5179329 0.5881033 FALSE   SenatorCardin 0.482656116 0.40021275
## 7146  0.5179329 0.5881033 FALSE   SenatorCardin 0.364177827 0.78161598
## 7148  0.5179329 0.5881033 FALSE   SenatorCardin 0.518559628 0.59635371
## 7153  0.5179329 0.5881033 FALSE   SenatorCardin 0.409986100 0.68904816
## 7156  0.5179329 0.5881033 FALSE   SenatorCardin 0.329671377 0.61805004
## 7158  0.5179329 0.5881033 FALSE   SenatorCardin 0.823389487 0.61062366
## 7162  0.5179329 0.5881033 FALSE   SenatorCardin 0.435537304 0.56280252
## 7165  0.5179329 0.5881033 FALSE   SenatorCardin 0.542368673 0.65648674
## 7166  0.5179329 0.5881033 FALSE   SenatorCardin 0.486237215 0.59200517
## 7168  0.5179329 0.5881033 FALSE   SenatorCardin 0.297599971 0.48506851
## 7169  0.5179329 0.5881033 FALSE   SenatorCardin 0.539697869 0.43481339
## 7175  0.5179329 0.5881033 FALSE   SenatorCardin 0.217003784 0.62634577
## 7189  0.5179329 0.5881033 FALSE   SenatorCardin 0.744800270 0.52594166
## 7224  0.5204533 0.6212819 FALSE   SenatorCarper 0.540534023 0.43491777
## 7225  0.5204533 0.6212819 FALSE   SenatorCarper 0.420794731 0.83279801
## 7226  0.5204533 0.6212819 FALSE   SenatorCarper 0.642043658 0.60924509
## 7227  0.5204533 0.6212819 FALSE   SenatorCarper 0.570489832 0.48985702
## 7230  0.5204533 0.6212819 FALSE   SenatorCarper 0.456255245 0.46338703
## 7231  0.5204533 0.6212819 FALSE   SenatorCarper 0.409271711 0.72602428
## 7232  0.5204533 0.6212819 FALSE   SenatorCarper 0.441633445 0.76397916
## 7233  0.5204533 0.6212819 FALSE   SenatorCarper 0.553658034 0.64751290
## 7234  0.5204533 0.6212819 FALSE   SenatorCarper 0.752923333 0.61670113
## 7235  0.5204533 0.6212819 FALSE   SenatorCarper 0.447243799 0.58579218
## 7236  0.5204533 0.6212819 FALSE   SenatorCarper 0.273961911 0.50904609
## 7238  0.5204533 0.6212819 FALSE   SenatorCarper 0.509387641 0.53380239
## 7239  0.5204533 0.6212819 FALSE   SenatorCarper 0.296410217 0.48730890
## 7241  0.5204533 0.6212819 FALSE   SenatorCarper 0.449248650 0.67255284
## 7250  0.5204533 0.6212819 FALSE   SenatorCarper 0.323152317 0.47763623
## 7251  0.5204533 0.6212819 FALSE   SenatorCarper 0.464700907 0.64814799
## 7252  0.5204533 0.6212819 FALSE   SenatorCarper 0.380782268 0.75159911
## 7255  0.5204533 0.6212819 FALSE   SenatorCarper 0.518371172 0.68377390
## 7256  0.5204533 0.6212819 FALSE   SenatorCarper 0.798231729 0.75023222
## 7258  0.5204533 0.6212819 FALSE   SenatorCarper 0.329981657 0.62189671
## 7261  0.5204533 0.6212819 FALSE   SenatorCarper 0.832321869 0.69859794
## 7263  0.5204533 0.6212819 FALSE   SenatorCarper 0.403850704 0.64654385
## 7264  0.5204533 0.6212819 FALSE   SenatorCarper 0.745782838 0.52882529
## 7266  0.5204533 0.6212819 FALSE   SenatorCarper 0.411257756 0.46426857
## 7267  0.5204533 0.6212819 FALSE   SenatorCarper 0.519826584 0.61303152
## 7268  0.5204533 0.6212819 FALSE   SenatorCarper 0.576011956 0.39947993
## 7272  0.5204533 0.6212819 FALSE   SenatorCarper 0.484271649 0.60520900
## 7284  0.5204533 0.6212819 FALSE   SenatorCarper 0.433029930 0.56840284
## 7290  0.5204533 0.6212819 FALSE   SenatorCarper 0.526403782 0.49664901
## 7291  0.5204533 0.6212819 FALSE   SenatorCarper 0.835524940 0.56563823
## 7293  0.5204533 0.6212819 FALSE   SenatorCarper 0.496807238 0.72920324
## 7296  0.5204533 0.6212819 FALSE   SenatorCarper 0.672887314 0.57595489
## 7299  0.5204533 0.6212819 FALSE   SenatorCarper 0.300346605 0.65678949
## 7302  0.5204533 0.6212819 FALSE   SenatorCarper 0.487913436 0.45368972
## 7304  0.5204533 0.6212819 FALSE   SenatorCarper 0.565312554 0.50902863
## 7308  0.5204533 0.6212819 FALSE   SenatorCarper 0.412600175 0.69236598
## 7312  0.5204533 0.6212819 FALSE   SenatorCarper 0.403239403 0.49874335
## 7316  0.5204533 0.6212819 FALSE   SenatorCarper 0.378819605 0.54812207
## 7322  0.5204533 0.6212819 FALSE   SenatorCarper 0.631625613 0.75837419
## 7323  0.5204533 0.6212819 FALSE   SenatorCarper 0.556750848 0.79909977
## 7324  0.5204533 0.6212819 FALSE   SenatorCarper 0.823330858 0.61313413
## 7333  0.5204533 0.6212819 FALSE   SenatorCarper 0.614097450 0.69852435
## 7350  0.5204533 0.6212819 FALSE   SenatorCarper 0.687636975 0.71648707
## 7354  0.5204533 0.6212819 FALSE   SenatorCarper 0.285850670 0.63118078
## 7359  0.5204533 0.6212819 FALSE   SenatorCarper 0.538818355 0.54538470
## 7366  0.5204533 0.6212819 FALSE   SenatorCarper 0.318013589 0.68922304
## 7378  0.5204533 0.6212819 FALSE   SenatorCarper 0.300574634 0.54895624
## 7380  0.5228768 0.6516892 FALSE    amyklobuchar 0.803366945 0.65712059
## 7381  0.5228768 0.6516892 FALSE    amyklobuchar 0.321912115 0.47920390
## 7382  0.5228768 0.6516892 FALSE    amyklobuchar 0.329752560 0.62535510
## 7383  0.5228768 0.6516892 FALSE    amyklobuchar 0.452348191 0.67832774
## 7384  0.5228768 0.6516892 FALSE    amyklobuchar 0.409931314 0.46513011
## 7385  0.5228768 0.6516892 FALSE    amyklobuchar 0.651952550 0.47908216
## 7386  0.5228768 0.6516892 FALSE    amyklobuchar 0.486732824 0.55225074
## 7387  0.5228768 0.6516892 FALSE    amyklobuchar 0.548883099 0.65753093
## 7389  0.5228768 0.6516892 FALSE    amyklobuchar 0.479808872 0.61200121
## 7391  0.5228768 0.6516892 FALSE    amyklobuchar 0.367599368 0.78491093
## 7398  0.5228768 0.6516892 FALSE    amyklobuchar 0.732058524 0.76438141
## 7399  0.5228768 0.6516892 FALSE    amyklobuchar 0.643055001 0.61422295
## 7400  0.5228768 0.6516892 FALSE    amyklobuchar 0.540578103 0.54574413
## 7401  0.5228768 0.6516892 FALSE    amyklobuchar 0.646746160 0.78560074
## 7403  0.5228768 0.6516892 FALSE    amyklobuchar 0.823501469 0.61545355
## 7405  0.5228768 0.6516892 FALSE    amyklobuchar 0.831895896 0.70069773
## 7406  0.5228768 0.6516892 FALSE    amyklobuchar 0.382701761 0.75392268
## 7407  0.5228768 0.6516892 FALSE    amyklobuchar 0.541115672 0.43497357
## 7408  0.5228768 0.6516892 FALSE    amyklobuchar 0.430546097 0.57181972
## 7409  0.5228768 0.6516892 FALSE    amyklobuchar 0.553897638 0.84121925
## 7410  0.5228768 0.6516892 FALSE    amyklobuchar 0.411612933 0.72891263
## 7411  0.5228768 0.6516892 FALSE    amyklobuchar 0.439630122 0.65364218
## 7412  0.5228768 0.6516892 FALSE    amyklobuchar 0.557161313 0.61919303
## 7413  0.5228768 0.6516892 FALSE    amyklobuchar 0.382553776 0.46303357
## 7414  0.5228768 0.6516892 FALSE    amyklobuchar 0.368554857 0.97383812
## 7415  0.5228768 0.6516892 FALSE    amyklobuchar 0.567149082 0.50968075
## 7425  0.5228768 0.6516892 FALSE    amyklobuchar 0.422243914 0.83353940
## 7426  0.5228768 0.6516892 FALSE    amyklobuchar 0.519866901 0.68386868
## 7431  0.5228768 0.6516892 FALSE    amyklobuchar 0.556236750 0.79921036
## 7432  0.5228768 0.6516892 FALSE    amyklobuchar 0.730650236 0.68092723
## 7435  0.5228768 0.6516892 FALSE    amyklobuchar 0.472963360 0.76902603
## 7436  0.5228768 0.6516892 FALSE    amyklobuchar 0.414816265 0.69654006
## 7437  0.5228768 0.6516892 FALSE    amyklobuchar 0.647874647 0.66886053
## 7438  0.5228768 0.6516892 FALSE    amyklobuchar 0.722782091 0.49840553
## 7446  0.5228768 0.6516892 FALSE    amyklobuchar 0.295292351 0.48901604
## 7451  0.5228768 0.6516892 FALSE    amyklobuchar 0.835996319 0.56776272
## 7458  0.5228768 0.6516892 FALSE    amyklobuchar 0.289440609 0.76486990
## 7461  0.5228768 0.6516892 FALSE    amyklobuchar 0.509143772 0.53383202
## 7467  0.5228768 0.6516892 FALSE    amyklobuchar 0.559836155 0.58626415
## 7471  0.5228768 0.6516892 FALSE    amyklobuchar 0.455298403 0.46375340
## 7474  0.5228768 0.6516892 FALSE    amyklobuchar 0.272992379 0.51095427
## 7476  0.5228768 0.6516892 FALSE    amyklobuchar 0.608684629 0.80092586
## 7480  0.5228768 0.6516892 FALSE    amyklobuchar 0.377166622 0.55087324
## 7485  0.5228768 0.6516892 FALSE    amyklobuchar 0.526940846 0.49666887
## 7486  0.5228768 0.6516892 FALSE    amyklobuchar 0.519870845 0.61302812
## 7487  0.5228768 0.6516892 FALSE    amyklobuchar 0.489828288 0.80150725
## 7494  0.5228768 0.6516892 FALSE    amyklobuchar 0.301633187 0.72755191
## 7495  0.5228768 0.6516892 FALSE    amyklobuchar 0.792673344 0.55743624
## 7500  0.5228768 0.6516892 FALSE    amyklobuchar 0.285804261 0.63408592
## 7503  0.5228768 0.6516892 FALSE    amyklobuchar 0.318831898 0.69229759
## 7504  0.5228768 0.6516892 FALSE    amyklobuchar 0.217147133 0.63117145
## 7506  0.5228768 0.6516892 FALSE    amyklobuchar 0.607309213 0.58950920
## 7508  0.5228768 0.6516892 FALSE    amyklobuchar 0.688465727 0.78918746
## 7511  0.5228768 0.6516892 FALSE    amyklobuchar 0.443821762 0.76533959
## 7512  0.5228768 0.6516892 FALSE    amyklobuchar 0.565146837 0.70107957
## 7513  0.5228768 0.6516892 FALSE    amyklobuchar 0.576850935 0.39967479
## 7519  0.5228768 0.6516892 FALSE    amyklobuchar 0.670901885 0.53015482
## 7520  0.5228768 0.6516892 FALSE    amyklobuchar 0.467077409 0.65674484
## 7545  0.5228768 0.6516892 FALSE    amyklobuchar 0.848430966 0.49578264
## 7547  0.5228768 0.6516892 FALSE    amyklobuchar 0.746895068 0.53117891
## 7552  0.5228768 0.6516892 FALSE    amyklobuchar 0.681727376 0.62823015
## 7561  0.5228768 0.6516892 FALSE    amyklobuchar 0.753156943 0.61965335
## 7592  0.5228768 0.6516892 FALSE    amyklobuchar 0.572088966 0.49040471
## 7593  0.5228768 0.6516892 FALSE    amyklobuchar 0.686261125 0.71929924
## 7594  0.5228768 0.6516892 FALSE    amyklobuchar 0.681026964 0.50127417
## 7595  0.5228768 0.6516892 FALSE    amyklobuchar 0.487541976 0.45375894
## 7615  0.5228768 0.6516892 FALSE    amyklobuchar 0.482050765 0.40031872
## 7621  0.5228768 0.6516892 FALSE    amyklobuchar 0.977649471 0.87959137
## 7644  0.5228768 0.6516892 FALSE    amyklobuchar 0.444434814 0.59029521
## 7647  0.5228768 0.6516892 FALSE    amyklobuchar 0.297562960 0.59061712
## 7656  0.5228768 0.6516892 FALSE    amyklobuchar 0.404417529 0.65181149
## 7678  0.5228768 0.6516892 FALSE    amyklobuchar 0.433528025 0.54463029
## 7702  0.5228768 0.6516892 FALSE    amyklobuchar 0.115099476 0.51679155
## 7711  0.5228768 0.6516892 FALSE    amyklobuchar 0.841320550 0.40879104
## 7723  0.5228768 0.6516892 FALSE    amyklobuchar 0.300648839 0.65985321
## 7727  0.5228768 0.6516892 FALSE    amyklobuchar 0.299630610 0.55139001
## 7750  0.5228768 0.6516892 FALSE    amyklobuchar 0.674279602 0.57957932
## 7752  0.5228768 0.6516892 FALSE    amyklobuchar 0.380858307 0.31315726
## 7760  0.5228768 0.6516892 FALSE    amyklobuchar 0.401583549 0.50019320
## 7768  0.5228768 0.6516892 FALSE    amyklobuchar 0.393382722 0.40067099
## 7796  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.470134945 0.44930025
## 7797  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.642026227 0.46131561
## 7798  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.567469870 0.63869351
## 7799  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.550649511 0.47861220
## 7801  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.694932360 0.70844263
## 7802  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.518598242 0.68378250
## 7803  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.536520635 0.49746114
## 7804  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.518661880 0.48729860
## 7805  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.638262597 0.75451068
## 7807  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.561317604 0.54195754
## 7809  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.773357567 0.66171494
## 7810  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.431902316 0.54082226
## 7816  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.473240302 0.57302818
## 7818  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.845997497 0.48401650
## 7819  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.648974592 0.58937856
## 7822  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.737134364 0.66734824
## 7826  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.436993577 0.76199787
## 7827  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.282291654 0.61934142
## 7829  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.404261568 0.68449355
## 7832  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.521645572 0.59631033
## 7835  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.566711133 0.57851875
## 7837  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.402978387 0.39192874
## 7844  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.765904085 0.38867636
## 7845  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.327922456 0.46389525
## 7846  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.312287747 0.67980206
## 7847  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.657992916 0.65200450
## 7848  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.494635003 0.72882680
## 7852  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.792129851 0.54270802
## 7855  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.295161059 0.71749552
## 7858  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.559335928 0.79871157
## 7888  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.360557663 0.77922089
## 7901  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.439365974 0.66442940
## 7903  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.501211537 0.44643164
## 7926  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.116273793 0.50681202
## 7930  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.717656926 0.48185633
## 7933  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.387503341 0.30904515
## 7935  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.374650142 0.74680347
## 7939  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.744385281 0.51449144
## 7950  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.452551603 0.63625390
## 7956  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.300919072 0.53447058
## 7959  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.392240410 0.44738951
## 7963  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.442394409 0.55717818
## 7964  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.325701479 0.60798832
## 7965  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.439973864 0.51446631
## 7976  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.523532019 0.62669776
## 7983  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.836183788 0.69016731
## 7994  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.576109659 0.69563992
## 7995  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.610039543 0.55673320
## 8007  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.625569686 0.42662724
## 8023  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.548016196 0.65518222
## 8026  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.494553728 0.50985212
## 8035  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.807097361 0.64445910
## 8044  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.276090989 0.49625472
## 8046  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.537070322 0.43429543
## 8068  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.557604019 0.74537738
## 8081  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.489506526 0.39785886
## 8123  0.5275960 0.4716775 FALSE SenatorHeitkamp 0.645572498 0.82619130
## 8135  0.5414873 1.0000000 FALSE      GrahamBlog 0.570927469 0.68789963
## 8142  0.5414873 1.0000000 FALSE      GrahamBlog 0.650495223 0.82516740
## 8144  0.5414873 1.0000000 FALSE      GrahamBlog 0.975680303 0.89658443
## 8146  0.5414873 1.0000000 FALSE      GrahamBlog 0.423247397 0.67769896
## 8147  0.5414873 1.0000000 FALSE      GrahamBlog 0.222023609 0.55003969
## 8150  0.5414873 1.0000000 FALSE      GrahamBlog 0.577873210 0.74482292
## 8153  0.5414873 1.0000000 FALSE      GrahamBlog 0.610905297 0.84540502
## 8155  0.5414873 1.0000000 FALSE      GrahamBlog 0.558285494 0.79514484
## 8166  0.5414873 1.0000000 FALSE      GrahamBlog 0.403708882 0.76473990
## 8194  0.5414873 1.0000000 FALSE      GrahamBlog 0.696401600 0.64746545
## 8196  0.5414873 1.0000000 FALSE      GrahamBlog 0.573189240 0.62690516
## 8197  0.5414873 1.0000000 FALSE      GrahamBlog 0.437422404 0.71068561
## 8198  0.5414873 1.0000000 FALSE      GrahamBlog 0.426949380 0.87391797
## 8210  0.5414873 1.0000000 FALSE      GrahamBlog 0.519360174 0.61306257
## 8216  0.5414873 1.0000000 FALSE      GrahamBlog 0.659324359 0.63059980
## 8237  0.5432123 0.4100616 FALSE    SenAlexander 0.470697592 0.43275998
## 8239  0.5432123 0.4100616 FALSE    SenAlexander 0.837580986 0.55052409
## 8242  0.5432123 0.4100616 FALSE    SenAlexander 0.676555478 0.47332848
## 8247  0.5432123 0.4100616 FALSE    SenAlexander 0.626281919 0.69046198
## 8249  0.5432123 0.4100616 FALSE    SenAlexander 0.656408265 0.78004698
## 8251  0.5432123 0.4100616 FALSE    SenAlexander 0.698090771 0.78207854
## 8252  0.5432123 0.4100616 FALSE    SenAlexander 0.521447272 0.56335160
## 8253  0.5432123 0.4100616 FALSE    SenAlexander 0.616505295 0.79803323
## 8255  0.5432123 0.4100616 FALSE    SenAlexander 0.679465284 0.55086385
## 8256  0.5432123 0.4100616 FALSE    SenAlexander 0.571182806 0.57733726
## 8260  0.5432123 0.4100616 FALSE    SenAlexander 0.740111251 0.66463568
## 8264  0.5432123 0.4100616 FALSE    SenAlexander 0.275131959 0.49142555
## 8268  0.5432123 0.4100616 FALSE    SenAlexander 0.565893706 0.44544587
## 8269  0.5432123 0.4100616 FALSE    SenAlexander 0.514999319 0.48558093
## 8271  0.5432123 0.4100616 FALSE    SenAlexander 0.428412791 0.53692688
## 8272  0.5432123 0.4100616 FALSE    SenAlexander 0.793999344 0.53784159
## 8280  0.5432123 0.4100616 FALSE    SenAlexander 0.559141902 0.74525665
## 8282  0.5432123 0.4100616 FALSE    SenAlexander 0.468304289 0.76756247
## 8283  0.5432123 0.4100616 FALSE    SenAlexander 0.661576131 0.64984538
## 8284  0.5432123 0.4100616 FALSE    SenAlexander 0.640558840 0.75373826
## 8286  0.5432123 0.4100616 FALSE    SenAlexander 0.299262750 0.46864111
## 8288  0.5432123 0.4100616 FALSE    SenAlexander 0.438490339 0.66404864
## 8293  0.5432123 0.4100616 FALSE    SenAlexander 0.327357525 0.45755004
## 8300  0.5432123 0.4100616 FALSE    SenAlexander 0.557128624 0.84090400
## 8309  0.5432123 0.4100616 FALSE    SenAlexander 0.401461069 0.72042669
## 8311  0.5432123 0.4100616 FALSE    SenAlexander 0.669829407 0.49983152
## 8314  0.5432123 0.4100616 FALSE    SenAlexander 0.730412238 0.70034305
## 8322  0.5432123 0.4100616 FALSE    SenAlexander 0.299257693 0.53010665
## 8323  0.5432123 0.4100616 FALSE    SenAlexander 0.373753774 0.74632871
## 8325  0.5432123 0.4100616 FALSE    SenAlexander 0.827488906 0.59864193
## 8330  0.5432123 0.4100616 FALSE    SenAlexander 0.565022727 0.46271672
## 8335  0.5432123 0.4100616 FALSE    SenAlexander 0.719149816 0.47458538
## 8342  0.5432123 0.4100616 FALSE    SenAlexander 0.838371017 0.68749153
## 8346  0.5432123 0.4100616 FALSE    SenAlexander 0.436783667 0.76193337
## 8348  0.5432123 0.4100616 FALSE    SenAlexander 0.578345668 0.69526082
## 8349  0.5432123 0.4100616 FALSE    SenAlexander 0.616052344 0.55242826
## 8351  0.5432123 0.4100616 FALSE    SenAlexander 0.643681125 0.44985496
## 8354  0.5432123 0.4100616 FALSE    SenAlexander 0.544363466 0.49608817
## 8358  0.5446980 0.5210859 FALSE    SenAngusKing 0.301756599 0.53927781
## 8359  0.5446980 0.5210859 FALSE    SenAngusKing 0.824382436 0.60525726
## 8360  0.5446980 0.5210859 FALSE    SenAngusKing 0.560343866 0.79863428
## 8362  0.5446980 0.5210859 FALSE    SenAngusKing 0.466111421 0.45615222
## 8363  0.5446980 0.5210859 FALSE    SenAngusKing 0.558426738 0.50488607
## 8364  0.5446980 0.5210859 FALSE    SenAngusKing 0.791407404 0.54661770
## 8366  0.5446980 0.5210859 FALSE    SenAngusKing 0.439516355 0.76293699
## 8367  0.5446980 0.5210859 FALSE    SenAngusKing 0.503027894 0.52589095
## 8370  0.5446980 0.5210859 FALSE    SenAngusKing 0.672997102 0.56134453
## 8374  0.5446980 0.5210859 FALSE    SenAngusKing 0.497056178 0.44992238
## 8379  0.5446980 0.5210859 FALSE    SenAngusKing 0.738209298 0.75691215
## 8381  0.5446980 0.5210859 FALSE    SenAngusKing 0.381522692 0.53458726
## 8384  0.5446980 0.5210859 FALSE    SenAngusKing 0.407655263 0.68685550
## 8387  0.5446980 0.5210859 FALSE    SenAngusKing 0.443254581 0.66665047
## 8389  0.5446980 0.5210859 FALSE    SenAngusKing 0.638087942 0.75457977
## 8399  0.5446980 0.5210859 FALSE    SenAngusKing 0.644630591 0.47037422
## 8404  0.5446980 0.5210859 FALSE    SenAngusKing 0.377321294 0.74851901
## 8407  0.5446980 0.5210859 FALSE    SenAngusKing 0.646452235 0.59243003
## 8408  0.5446980 0.5210859 FALSE    SenAngusKing 0.243039413 0.68648202
## 8409  0.5446980 0.5210859 FALSE    SenAngusKing 0.419131650 0.45534043
## 8410  0.5446980 0.5210859 FALSE    SenAngusKing 0.432106499 0.63634512
## 8411  0.5446980 0.5210859 FALSE    SenAngusKing 0.645615376 0.82617707
## 8417  0.5446980 0.5210859 FALSE    SenAngusKing 0.565977977 0.48759401
## 8422  0.5446980 0.5210859 FALSE    SenAngusKing 0.535773369 0.49530225
## 8426  0.5446980 0.5210859 FALSE    SenAngusKing 0.558763281 0.74527752
## 8435  0.5446980 0.5210859 FALSE    SenAngusKing 0.362962158 0.78070884
## 8442  0.5446980 0.5210859 FALSE    SenAngusKing 0.543546783 0.43505940
## 8443  0.5446980 0.5210859 FALSE    SenAngusKing 0.489290797 0.80139486
## 8449  0.5446980 0.5210859 FALSE    SenAngusKing 0.297463537 0.71998981
## 8454  0.5446980 0.5210859 FALSE    SenAngusKing 0.328024104 0.61227938
## 8462  0.5446980 0.5210859 FALSE    SenAngusKing 0.299588756 0.47873495
## 8469  0.5446980 0.5210859 FALSE    SenAngusKing 0.276137010 0.50058868
## 8473  0.5446980 0.5210859 FALSE    SenAngusKing 0.297853254 0.64925095
## 8475  0.5446980 0.5210859 FALSE    SenAngusKing 0.685400267 0.61110469
## 8483  0.5446980 0.5210859 FALSE    SenAngusKing 0.568340424 0.63850201
## 8496  0.5446980 0.5210859 FALSE    SenAngusKing 0.435843782 0.54921026
## 8501  0.5446980 0.5210859 FALSE    SenAngusKing 0.654026242 0.78091000
## 8504  0.5446980 0.5210859 FALSE    SenAngusKing 0.530099702 0.51649702
## 8515  0.5446980 0.5210859 FALSE    SenAngusKing 0.718067097 0.48789421
## 8518  0.5446980 0.5210859 FALSE    SenAngusKing 0.767835211 0.39290621
## 8520  0.5446980 0.5210859 FALSE    SenAngusKing 0.521119247 0.68401777
## 8522  0.5446980 0.5210859 FALSE    SenAngusKing 0.771959534 0.66371800
## 8524  0.5446980 0.5210859 FALSE    SenAngusKing 0.656491641 0.65317982
## 8534  0.5446980 0.5210859 FALSE    SenAngusKing 0.405297347 0.72260691
## 8544  0.5446980 0.5210859 FALSE    SenAngusKing 0.284151549 0.62311855
## 8587  0.5446980 0.5210859 FALSE    SenAngusKing 0.630717889 0.43397200
## 8590  0.5446980 0.5210859 FALSE    SenAngusKing 0.470380235 0.76809122
## 8597  0.5446980 0.5210859 FALSE    SenAngusKing 0.419473412 0.83222141
## 8622  0.5446980 0.5210859 FALSE    SenAngusKing 0.526332965 0.59698312
## 8632  0.5446980 0.5210859 FALSE    SenAngusKing 0.693833580 0.70926535
## 8633  0.5446980 0.5210859 FALSE    SenAngusKing 0.743912321 0.51953038
## 8634  0.5446980 0.5210859 FALSE    SenAngusKing 0.606457080 0.56109239
## 8641  0.5446980 0.5210859 FALSE    SenAngusKing 0.285371313 0.75888056
## 8675  0.5446980 0.5210859 FALSE    SenAngusKing 0.576867403 0.69548800
## 8710  0.5446980 0.5210859 FALSE    SenAngusKing 0.846130383 0.48773395
## 8717  0.5446980 0.5210859 FALSE    SenAngusKing 0.575878755 0.39944617
## 8736  0.5446980 0.5210859 FALSE    SenAngusKing 0.457093833 0.63893683
## 8741  0.5446980 0.5210859 FALSE    SenAngusKing 0.497036118 0.72925451
## 8746  0.5446980 0.5210859 FALSE    SenAngusKing 0.327247844 0.46876951
## 8751  0.5446980 0.5210859 FALSE    SenAngusKing 0.527205142 0.56488643
## 8757  0.5446980 0.5210859 FALSE    SenAngusKing 0.447558341 0.56465564
## 8763  0.5446980 0.5210859 FALSE    SenAngusKing 0.549824991 0.65504715
## 8784  0.5446980 0.5210859 FALSE    SenAngusKing 0.480115000 0.57845649
## 8790  0.5446980 0.5210859 FALSE    SenAngusKing 0.622960202 0.69170863
## 8803  0.5507811 0.6800289 FALSE       MikeCrapo 0.640534191 0.82851705
## 8806  0.5507811 0.6800289 FALSE       MikeCrapo 0.433833003 0.54437160
## 8808  0.5507811 0.6800289 FALSE       MikeCrapo 0.542213097 0.97501054
## 8811  0.5507811 0.6800289 FALSE       MikeCrapo 0.793849788 0.56024234
## 8812  0.5507811 0.6800289 FALSE       MikeCrapo 0.329312435 0.62772368
## 8817  0.5507811 0.6800289 FALSE       MikeCrapo 0.456781575 0.46316599
## 8822  0.5507811 0.6800289 FALSE       MikeCrapo 0.676958556 0.58397257
## 8824  0.5507811 0.6800289 FALSE       MikeCrapo 0.466723535 0.66375310
## 8825  0.5507811 0.6800289 FALSE       MikeCrapo 0.530360921 0.49652409
## 8826  0.5507811 0.6800289 FALSE       MikeCrapo 0.512549511 0.53319341
## 8828  0.5507811 0.6800289 FALSE       MikeCrapo 0.478579556 0.77233920
## 8835  0.5507811 0.6800289 FALSE       MikeCrapo 0.719514491 0.71612349
## 8836  0.5507811 0.6800289 FALSE       MikeCrapo 0.285539933 0.63630130
## 8838  0.5507811 0.6800289 FALSE       MikeCrapo 0.556974069 0.84090947
## 8842  0.5507811 0.6800289 FALSE       MikeCrapo 0.571547855 0.51062795
## 8843  0.5507811 0.6800289 FALSE       MikeCrapo 0.439199826 0.65888337
## 8845  0.5507811 0.6800289 FALSE       MikeCrapo 0.842823897 0.41058168
## 8846  0.5507811 0.6800289 FALSE       MikeCrapo 0.430265071 0.57213906
## 8850  0.5507811 0.6800289 FALSE       MikeCrapo 0.401888727 0.49994489
## 8856  0.5507811 0.6800289 FALSE       MikeCrapo 0.479541359 0.61228699
## 8860  0.5507811 0.6800289 FALSE       MikeCrapo 0.796954360 0.75360014
## 8862  0.5507811 0.6800289 FALSE       MikeCrapo 0.557697122 0.74536735
## 8863  0.5507811 0.6800289 FALSE       MikeCrapo 0.416396327 0.70207625
## 8866  0.5507811 0.6800289 FALSE       MikeCrapo 0.448379305 0.76942186
## 8867  0.5507811 0.6800289 FALSE       MikeCrapo 0.629801322 0.76000799
## 8868  0.5507811 0.6800289 FALSE       MikeCrapo 0.443944712 0.59090168
## 8872  0.5507811 0.6800289 FALSE       MikeCrapo 0.489007961 0.55129415
## 8877  0.5507811 0.6800289 FALSE       MikeCrapo 0.494791129 0.80316256
## 8878  0.5507811 0.6800289 FALSE       MikeCrapo 0.536453100 0.69241243
## 8880  0.5507811 0.6800289 FALSE       MikeCrapo 0.731470782 0.76554394
## 8883  0.5507811 0.6800289 FALSE       MikeCrapo 0.294999737 0.48941694
## 8884  0.5507811 0.6800289 FALSE       MikeCrapo 0.543912893 0.43505182
## 8892  0.5507811 0.6800289 FALSE       MikeCrapo 0.566216348 0.70021407
## 8895  0.5507811 0.6800289 FALSE       MikeCrapo 0.299124649 0.55244896
## 8900  0.5507811 0.6800289 FALSE       MikeCrapo 0.725172126 0.50109670
## 8902  0.5507811 0.6800289 FALSE       MikeCrapo 0.714236606 0.57207871
## 8903  0.5507811 0.6800289 FALSE       MikeCrapo 0.768136539 0.67731934
## 8905  0.5507811 0.6800289 FALSE       MikeCrapo 0.748783664 0.53416311
## 8910  0.5507811 0.6800289 FALSE       MikeCrapo 0.507146140 0.73416020
## 8915  0.5507811 0.6800289 FALSE       MikeCrapo 0.646882403 0.78547564
## 8920  0.5507811 0.6800289 FALSE       MikeCrapo 0.674128864 0.53342078
## 8924  0.5507811 0.6800289 FALSE       MikeCrapo 0.526345302 0.61164546
## 8925  0.5507811 0.6800289 FALSE       MikeCrapo 0.639617707 0.43963301
## 8930  0.5507811 0.6800289 FALSE       MikeCrapo 0.612729574 0.59489875
## 8931  0.5507811 0.6800289 FALSE       MikeCrapo 0.553338464 0.67809399
## 8934  0.5507811 0.6800289 FALSE       MikeCrapo 0.655292901 0.48119042
## 8940  0.5507811 0.6800289 FALSE       MikeCrapo 0.559846234 0.79866732
## 8942  0.5507811 0.6800289 FALSE       MikeCrapo 0.290633727 0.76778574
## 8966  0.5507811 0.6800289 FALSE       MikeCrapo 0.579532210 0.40009832
## 8967  0.5507811 0.6800289 FALSE       MikeCrapo 0.576065841 0.49127173
## 8970  0.5507811 0.6800289 FALSE       MikeCrapo 0.803443438 0.65961766
## 9000  0.5507811 0.6800289 FALSE       MikeCrapo 0.849627968 0.49799015
## 9001  0.5507811 0.6800289 FALSE       MikeCrapo 0.216958642 0.63298602
## 9002  0.5507811 0.6800289 FALSE       MikeCrapo 0.300604668 0.66251672
## 9003  0.5507811 0.6800289 FALSE       MikeCrapo 0.684052856 0.50397782
## 9007  0.5507811 0.6800289 FALSE       MikeCrapo 0.753850492 0.62297115
## 9012  0.5507811 0.6800289 FALSE       MikeCrapo 0.823942507 0.61799801
## 9022  0.5507811 0.6800289 FALSE       MikeCrapo 0.647692653 0.67385288
## 9023  0.5507811 0.6800289 FALSE       MikeCrapo 0.610304860 0.70482021
## 9030  0.5507811 0.6800289 FALSE       MikeCrapo 0.394134950 0.40026652
## 9035  0.5507811 0.6800289 FALSE       MikeCrapo 0.302480613 0.73066528
## 9037  0.5507811 0.6800289 FALSE       MikeCrapo 0.404085951 0.65589558
## 9043  0.5507811 0.6800289 FALSE       MikeCrapo 0.567589210 0.58908092
## 9044  0.5507811 0.6800289 FALSE       MikeCrapo 0.685468439 0.72150133
## 9047  0.5507811 0.6800289 FALSE       MikeCrapo 0.272596281 0.51163243
## 9054  0.5507811 0.6800289 FALSE       MikeCrapo 0.682908498 0.63296629
## 9055  0.5507811 0.6800289 FALSE       MikeCrapo 0.540417132 0.66950316
## 9063  0.5507811 0.6800289 FALSE       MikeCrapo 0.483853484 0.39995744
## 9074  0.5507811 0.6800289 FALSE       MikeCrapo 0.831667704 0.70261027
## 9076  0.5507811 0.6800289 FALSE       MikeCrapo 0.370069131 0.78833759
## 9080  0.5507811 0.6800289 FALSE       MikeCrapo 0.382917987 0.46275746
## 9083  0.5507811 0.6800289 FALSE       MikeCrapo 0.385122481 0.75800683
## 9086  0.5507811 0.6800289 FALSE       MikeCrapo 0.321746529 0.47939487
## 9087  0.5507811 0.6800289 FALSE       MikeCrapo 0.453918038 0.68569995
## 9092  0.5507811 0.6800289 FALSE       MikeCrapo 0.246079041 0.69710253
## 9100  0.5507811 0.6800289 FALSE       MikeCrapo 0.414324038 0.73397584
## 9101  0.5507811 0.6800289 FALSE       MikeCrapo 0.773932674 0.40000474
## 9105  0.5507811 0.6800289 FALSE       MikeCrapo 0.370775622 0.97504355
## 9108  0.5507811 0.6800289 FALSE       MikeCrapo 0.610081586 0.80018048
## 9110  0.5507811 0.6800289 FALSE       MikeCrapo 0.531921422 0.64349636
## 9116  0.5507811 0.6800289 FALSE       MikeCrapo 0.545654093 0.54606765
## 9128  0.5507811 0.6800289 FALSE       MikeCrapo 0.376719127 0.55149920
## 9130  0.5507811 0.6800289 FALSE       MikeCrapo 0.230449576 0.53968720
## 9133  0.5507811 0.6800289 FALSE       MikeCrapo 0.425779192 0.83590989
## 9139  0.5507811 0.6800289 FALSE       MikeCrapo 0.645776155 0.62011834
## 9141  0.5507811 0.6800289 FALSE       MikeCrapo 0.319257059 0.69550944
## 9143  0.5507811 0.6800289 FALSE       MikeCrapo 0.730412053 0.68387568
## 9146  0.5579357 0.8658910 FALSE  SenThadCochran 0.829486241 0.62890024
## 9147  0.5579357 0.8658910 FALSE  SenThadCochran 0.855105547 0.50429888
## 9148  0.5579357 0.8658910 FALSE  SenThadCochran 0.657229094 0.62982671
## 9149  0.5579357 0.8658910 FALSE  SenThadCochran 0.755911589 0.54068935
## 9150  0.5579357 0.8658910 FALSE  SenThadCochran 0.450761830 0.79908711
## 9151  0.5579357 0.8658910 FALSE  SenThadCochran 0.378697302 0.98273232
## 9153  0.5579357 0.8658910 FALSE  SenThadCochran 0.808556076 0.67285960
## 9157  0.5579357 0.8658910 FALSE  SenThadCochran 0.629561734 0.79533596
## 9158  0.5579357 0.8658910 FALSE  SenThadCochran 0.544530698 0.97518594
## 9167  0.5579357 0.8658910 FALSE  SenThadCochran 0.761391681 0.63496667
## 9170  0.5579357 0.8658910 FALSE  SenThadCochran 0.544019353 0.43504861
## 9173  0.5579357 0.8658910 FALSE  SenThadCochran 0.573663874 0.62694094
## 9175  0.5579357 0.8658910 FALSE  SenThadCochran 0.453152535 0.46441777
## 9178  0.5579357 0.8658910 FALSE  SenThadCochran 0.559504993 0.84849356
## 9179  0.5579357 0.8658910 FALSE  SenThadCochran 0.600519916 0.83672540
## 9181  0.5579357 0.8658910 FALSE  SenThadCochran 0.506404960 0.83786484
## 9211  0.5579357 0.8658910 FALSE  SenThadCochran 0.295889795 0.67546747
## 9273  0.5579357 0.8658910 FALSE  SenThadCochran 0.573495425 0.51078974
## 9275  0.5579357 0.8658910 FALSE  SenThadCochran 0.521496246 0.61284809
## 9292  0.5579357 0.8658910 FALSE  SenThadCochran 0.642148173 0.81658475
## 9305  0.5579357 0.8658910 FALSE  SenThadCochran 0.300652187 0.74620539
## 9307  0.5579357 0.8658910 FALSE  SenThadCochran 0.693355232 0.64586832
## 9321  0.5579357 0.8658910 FALSE  SenThadCochran 0.736999090 0.70132760
## 9342  0.5579357 0.8658910 FALSE  SenThadCochran 0.443590008 0.70743391
## 9368  0.5579357 0.8658910 FALSE  SenThadCochran 0.213199219 0.64306833
## 9381  0.5579357 0.8658910 FALSE  SenThadCochran 0.690824742 0.74563312
## 9412  0.5579357 0.8658910 FALSE  SenThadCochran 0.409749651 0.72344861
## 9414  0.5579357 0.8658910 FALSE  SenThadCochran 0.372510500 0.80857287
## 9420  0.5579357 0.8658910 FALSE  SenThadCochran 0.280522825 0.64769046
## 9445  0.5579357 0.8658910 FALSE  SenThadCochran 0.322978119 0.63933056
## 9490  0.5579357 0.8658910 FALSE  SenThadCochran 0.571390441 0.68793896
## 9506  0.5579357 0.8658910 FALSE  SenThadCochran 0.577430124 0.74475568
## 9553  0.5603286 0.7702285 FALSE SenatorTimScott 0.729218434 0.50429703
## 9554  0.5603286 0.7702285 FALSE SenatorTimScott 0.570700504 0.58945617
## 9555  0.5603286 0.7702285 FALSE SenatorTimScott 0.487570592 0.78655678
## 9556  0.5603286 0.7702285 FALSE SenatorTimScott 0.570278296 0.68782983
## 9557  0.5603286 0.7702285 FALSE SenatorTimScott 0.486243116 0.55242301
## 9558  0.5603286 0.7702285 FALSE SenatorTimScott 0.573337837 0.51078234
## 9559  0.5603286 0.7702285 FALSE SenatorTimScott 0.454969428 0.46386911
## 9560  0.5603286 0.7702285 FALSE SenatorTimScott 0.415094077 0.71500847
## 9561  0.5603286 0.7702285 FALSE SenatorTimScott 0.678909365 0.53658399
## 9563  0.5603286 0.7702285 FALSE SenatorTimScott 0.688847277 0.97389882
## 9564  0.5603286 0.7702285 FALSE SenatorTimScott 0.805314234 0.66728905
## 9566  0.5603286 0.7702285 FALSE SenatorTimScott 0.685272297 0.73554528
## 9567  0.5603286 0.7702285 FALSE SenatorTimScott 0.688752425 0.64222634
## 9569  0.5603286 0.7702285 FALSE SenatorTimScott 0.302799506 0.73869885
## 9570  0.5603286 0.7702285 FALSE SenatorTimScott 0.398855750 0.50209397
## 9571  0.5603286 0.7702285 FALSE SenatorTimScott 0.653801988 0.68869618
## 9572  0.5603286 0.7702285 FALSE SenatorTimScott 0.688231568 0.50653862
## 9575  0.5603286 0.7702285 FALSE SenatorTimScott 0.832178788 0.71001960
## 9576  0.5603286 0.7702285 FALSE SenatorTimScott 0.269991879 0.51518532
## 9577  0.5603286 0.7702285 FALSE SenatorTimScott 0.653265533 0.62772264
## 9581  0.5603286 0.7702285 FALSE SenatorTimScott 0.682822801 0.58952303
## 9583  0.5603286 0.7702285 FALSE SenatorTimScott 0.544399013 0.43503346
## 9584  0.5603286 0.7702285 FALSE SenatorTimScott 0.504597734 0.81112892
## 9585  0.5603286 0.7702285 FALSE SenatorTimScott 0.228205314 0.54374008
## 9586  0.5603286 0.7702285 FALSE SenatorTimScott 0.776841478 0.40200415
## 9595  0.5603286 0.7702285 FALSE SenatorTimScott 0.602201784 0.80628570
## 9596  0.5603286 0.7702285 FALSE SenatorTimScott 0.826396210 0.62447299
## 9598  0.5603286 0.7702285 FALSE SenatorTimScott 0.543530413 0.97508363
## 9599  0.5603286 0.7702285 FALSE SenatorTimScott 0.729079864 0.77546331
## 9603  0.5603286 0.7702285 FALSE SenatorTimScott 0.719802478 0.72778316
## 9610  0.5603286 0.7702285 FALSE SenatorTimScott 0.573088979 0.62689644
## 9614  0.5603286 0.7702285 FALSE SenatorTimScott 0.619324041 0.59833054
## 9615  0.5603286 0.7702285 FALSE SenatorTimScott 0.460382208 0.67613716
## 9622  0.5603286 0.7702285 FALSE SenatorTimScott 0.531821873 0.72927807
## 9625  0.5603286 0.7702285 FALSE SenatorTimScott 0.752929336 0.53855934
## 9626  0.5603286 0.7702285 FALSE SenatorTimScott 0.558560876 0.84089879
## 9627  0.5603286 0.7702285 FALSE SenatorTimScott 0.415761407 0.74711417
## 9636  0.5603286 0.7702285 FALSE SenatorTimScott 0.839633230 0.57558406
## 9642  0.5603286 0.7702285 FALSE SenatorTimScott 0.113203529 0.52110760
## 9643  0.5603286 0.7702285 FALSE SenatorTimScott 0.769926063 0.68630094
## 9659  0.5603286 0.7702285 FALSE SenatorTimScott 0.732522727 0.69447776
## 9669  0.5603286 0.7702285 FALSE SenatorTimScott 0.553412626 0.70488997
## 9675  0.5603286 0.7702285 FALSE SenatorTimScott 0.719192015 0.57758896
## 9676  0.5603286 0.7702285 FALSE SenatorTimScott 0.622465909 0.77562777
## 9678  0.5603286 0.7702285 FALSE SenatorTimScott 0.561084881 0.79860353
## 9679  0.5603286 0.7702285 FALSE SenatorTimScott 0.473716123 0.61682930
## 9680  0.5603286 0.7702285 FALSE SenatorTimScott 0.515760190 0.75948335
## 9681  0.5603286 0.7702285 FALSE SenatorTimScott 0.374434311 0.97776279
## 9682  0.5603286 0.7702285 FALSE SenatorTimScott 0.373040208 0.55548958
## 9683  0.5603286 0.7702285 FALSE SenatorTimScott 0.797353766 0.56553138
## 9685  0.5603286 0.7702285 FALSE SenatorTimScott 0.757513169 0.63065241
## 9689  0.5603286 0.7702285 FALSE SenatorTimScott 0.613515195 0.72960663
## 9690  0.5603286 0.7702285 FALSE SenatorTimScott 0.577819615 0.49144399
## 9699  0.5603286 0.7702285 FALSE SenatorTimScott 0.245577359 0.70367316
## 9700  0.5603286 0.7702285 FALSE SenatorTimScott 0.291940769 0.77530401
## 9704  0.5603286 0.7702285 FALSE SenatorTimScott 0.795924013 0.76166670
## 9707  0.5603286 0.7702285 FALSE SenatorTimScott 0.373362467 0.79757193
## 9715  0.5603286 0.7702285 FALSE SenatorTimScott 0.571718174 0.74312142
## 9729  0.5603286 0.7702285 FALSE SenatorTimScott 0.431884894 0.84307965
## 9732  0.5603286 0.7702285 FALSE SenatorTimScott 0.639954681 0.79620069
## 9738  0.5603286 0.7702285 FALSE SenatorTimScott 0.683373325 0.79939253
## 9740  0.5603286 0.7702285 FALSE SenatorTimScott 0.658822389 0.48271180
## 9745  0.5603286 0.7702285 FALSE SenatorTimScott 0.634470079 0.83365998
## 9746  0.5617509 0.8235947 FALSE      JerryMoran 0.833403870 0.71396930
## 9747  0.5617509 0.8235947 FALSE      JerryMoran 0.454017180 0.46417543
## 9748  0.5617509 0.8235947 FALSE      JerryMoran 0.546105044 0.54604631
## 9752  0.5617509 0.8235947 FALSE      JerryMoran 0.471473764 0.61795100
## 9753  0.5617509 0.8235947 FALSE      JerryMoran 0.625328017 0.78958433
## 9754  0.5617509 0.8235947 FALSE      JerryMoran 0.412294435 0.72033443
## 9759  0.5617509 0.8235947 FALSE      JerryMoran 0.619667527 0.73533390
## 9760  0.5617509 0.8235947 FALSE      JerryMoran 0.659994155 0.48308130
## 9761  0.5617509 0.8235947 FALSE      JerryMoran 0.571485756 0.68794599
## 9762  0.5617509 0.8235947 FALSE      JerryMoran 0.301864526 0.74306065
## 9766  0.5617509 0.8235947 FALSE      JerryMoran 0.530011017 0.49656054
## 9767  0.5617509 0.8235947 FALSE      JerryMoran 0.735105636 0.69900141
## 9771  0.5617509 0.8235947 FALSE      JerryMoran 0.578152012 0.49146262
## 9772  0.5617509 0.8235947 FALSE      JerryMoran 0.485009074 0.55280747
## 9776  0.5617509 0.8235947 FALSE      JerryMoran 0.796611483 0.76664967
## 9777  0.5617509 0.8235947 FALSE      JerryMoran 0.691756929 0.64479768
## 9779  0.5617509 0.8235947 FALSE      JerryMoran 0.560994695 0.79521959
## 9781  0.5617509 0.8235947 FALSE      JerryMoran 0.722159155 0.73358669
## 9784  0.5617509 0.8235947 FALSE      JerryMoran 0.629436211 0.84301685
## 9788  0.5617509 0.8235947 FALSE      JerryMoran 0.488034154 0.45366598
## 9796  0.5617509 0.8235947 FALSE      JerryMoran 0.486986521 0.79965488
## 9797  0.5617509 0.8235947 FALSE      JerryMoran 0.828194594 0.62729069
## 9799  0.5617509 0.8235947 FALSE      JerryMoran 0.386603687 0.77529958
## 9805  0.5617509 0.8235947 FALSE      JerryMoran 0.544331889 0.43503656
## 9811  0.5617509 0.8235947 FALSE      JerryMoran 0.397363294 0.50292463
## 9817  0.5617509 0.8235947 FALSE      JerryMoran 0.292663637 0.60005199
## 9820  0.5617509 0.8235947 FALSE      JerryMoran 0.376830666 0.98022643
## 9825  0.5617509 0.8235947 FALSE      JerryMoran 0.528391022 0.67607347
## 9828  0.5617509 0.8235947 FALSE      JerryMoran 0.596149559 0.82301786
## 9829  0.5617509 0.8235947 FALSE      JerryMoran 0.434606137 0.85027858
## 9837  0.5617509 0.8235947 FALSE      JerryMoran 0.428396619 0.54794133
## 9850  0.5617509 0.8235947 FALSE      JerryMoran 0.324644579 0.63741679
## 9855  0.5617509 0.8235947 FALSE      JerryMoran 0.759878178 0.63351605
## 9874  0.5617509 0.8235947 FALSE      JerryMoran 0.423851177 0.57727813
## 9877  0.5617509 0.8235947 FALSE      JerryMoran 0.573779509 0.62694828
## 9885  0.5721325 0.5644972 FALSE   SenDeanHeller 0.408869564 0.72560720
## 9886  0.5721325 0.5644972 FALSE   SenDeanHeller 0.422301643 0.83357144
## 9887  0.5721325 0.5644972 FALSE   SenDeanHeller 0.244474092 0.68958479
## 9890  0.5721325 0.5644972 FALSE   SenDeanHeller 0.301748593 0.54311546
## 9891  0.5721325 0.5644972 FALSE   SenDeanHeller 0.667216695 0.52407225
## 9892  0.5721325 0.5644972 FALSE   SenDeanHeller 0.443083651 0.76484539
## 9895  0.5721325 0.5644972 FALSE   SenDeanHeller 0.285301125 0.62691854
## 9896  0.5721325 0.5644972 FALSE   SenDeanHeller 0.801280090 0.74527446
## 9898  0.5721325 0.5644972 FALSE   SenDeanHeller 0.298380103 0.58244156
## 9899  0.5721325 0.5644972 FALSE   SenDeanHeller 0.639055583 0.75421577
## 9900  0.5721325 0.5644972 FALSE   SenDeanHeller 0.299443380 0.65304952
## 9903  0.5721325 0.5644972 FALSE   SenDeanHeller 0.654791041 0.78060293
## 9904  0.5721325 0.5644972 FALSE   SenDeanHeller 0.683637863 0.61437045
## 9906  0.5721325 0.5644972 FALSE   SenDeanHeller 0.579915339 0.69511752
## 9907  0.5721325 0.5644972 FALSE   SenDeanHeller 0.441747891 0.53155962
## 9908  0.5721325 0.5644972 FALSE   SenDeanHeller 0.462398381 0.64429722
## 9909  0.5721325 0.5644972 FALSE   SenDeanHeller 0.399793468 0.39593398
## 9912  0.5721325 0.5644972 FALSE   SenDeanHeller 0.650210055 0.47765223
## 9920  0.5721325 0.5644972 FALSE   SenDeanHeller 0.720055747 0.49395447
## 9922  0.5721325 0.5644972 FALSE   SenDeanHeller 0.411390523 0.69068633
## 9924  0.5721325 0.5644972 FALSE   SenDeanHeller 0.580773494 0.40019465
## 9925  0.5721325 0.5644972 FALSE   SenDeanHeller 0.501265653 0.73062868
## 9926  0.5721325 0.5644972 FALSE   SenDeanHeller 0.298880182 0.48172599
## 9930  0.5721325 0.5644972 FALSE   SenDeanHeller 0.561760676 0.74526951
## 9932  0.5721325 0.5644972 FALSE   SenDeanHeller 0.389125091 0.45574546
## 9934  0.5721325 0.5644972 FALSE   SenDeanHeller 0.496882193 0.45003781
## 9937  0.5721325 0.5644972 FALSE   SenDeanHeller 0.473974565 0.76948295
## 9938  0.5721325 0.5644972 FALSE   SenDeanHeller 0.804865449 0.64906646
## 9947  0.5721325 0.5644972 FALSE   SenDeanHeller 0.616481896 0.79803767
## 9957  0.5721325 0.5644972 FALSE   SenDeanHeller 0.572985299 0.63801180
## 9958  0.5721325 0.5644972 FALSE   SenDeanHeller 0.538410945 0.49421714
## 9961  0.5721325 0.5644972 FALSE   SenDeanHeller 0.636864619 0.43842279
## 9965  0.5721325 0.5644972 FALSE   SenDeanHeller 0.535173222 0.62992224
## 9967  0.5721325 0.5644972 FALSE   SenDeanHeller 0.644090763 0.59659736
## 9980  0.5721325 0.5644972 FALSE   SenDeanHeller 0.501558312 0.53764502
## 9983  0.5721325 0.5644972 FALSE   SenDeanHeller 0.678022571 0.49742288
## 9985  0.5721325 0.5644972 FALSE   SenDeanHeller 0.464589183 0.45783306
## 9988  0.5721325 0.5644972 FALSE   SenDeanHeller 0.299592766 0.72308715
## 9998  0.5721325 0.5644972 FALSE   SenDeanHeller 0.734486310 0.67072323
## 10001 0.5721325 0.5644972 FALSE   SenDeanHeller 0.485523087 0.58840694
## 10010 0.5721325 0.5644972 FALSE   SenDeanHeller 0.655590596 0.65398046
## 10017 0.5721325 0.5644972 FALSE   SenDeanHeller 0.646615040 0.82586945
## 10021 0.5721325 0.5644972 FALSE   SenDeanHeller 0.365789740 0.78301294
## 10029 0.5721325 0.5644972 FALSE   SenDeanHeller 0.623928792 0.69128887
## 10033 0.5721325 0.5644972 FALSE   SenDeanHeller 0.770855534 0.66566562
## 10038 0.5721325 0.5644972 FALSE   SenDeanHeller 0.241495495 0.85373961
## 10057 0.5721325 0.5644972 FALSE   SenDeanHeller 0.540853275 0.57812059
## 10062 0.5721325 0.5644972 FALSE   SenDeanHeller 0.835145507 0.56156870
## 10065 0.5721325 0.5644972 FALSE   SenDeanHeller 0.492393415 0.80221821
## 10069 0.5721325 0.5644972 FALSE   SenDeanHeller 0.744545055 0.52492744
## 10075 0.5721325 0.5644972 FALSE   SenDeanHeller 0.577540034 0.49142488
## 10077 0.5721325 0.5644972 FALSE   SenDeanHeller 0.753672280 0.61011585
## 10086 0.5721325 0.5644972 FALSE   SenDeanHeller 0.562751831 0.79861470
## 10092 0.5721325 0.5644972 FALSE   SenDeanHeller 0.602853001 0.57015573
## 10102 0.5721325 0.5644972 FALSE   SenDeanHeller 0.555324418 0.65544516
## 10106 0.5721325 0.5644972 FALSE   SenDeanHeller 0.725497588 0.70449816
## 10107 0.5721325 0.5644972 FALSE   SenDeanHeller 0.846819259 0.49141260
## 10135 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.532788578 0.60821076
## 10136 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.496411392 0.80397136
## 10137 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.614386328 0.69818047
## 10138 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.633813205 0.75678820
## 10139 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.682439218 0.63150910
## 10140 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.453617887 0.68303477
## 10141 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.686857434 0.71796933
## 10142 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.768185111 0.67541928
## 10143 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.647749728 0.66994500
## 10146 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.482790727 0.60803986
## 10148 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.614370269 0.59599629
## 10149 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.546162758 0.43488692
## 10151 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.578608160 0.49148098
## 10153 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.302260857 0.72968809
## 10154 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.836581391 0.56964532
## 10155 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.574474329 0.62698109
## 10156 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.536862553 0.69289853
## 10157 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.432452453 0.56931269
## 10163 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.378203517 0.54924344
## 10167 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.645488748 0.61965135
## 10168 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.563540449 0.79865878
## 10169 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.296087226 0.48783422
## 10170 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.684760221 0.50449406
## 10172 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.753547689 0.62178216
## 10176 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.676966750 0.58398333
## 10177 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.285752463 0.63468564
## 10178 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.563325633 0.74540875
## 10180 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.533401429 0.49599406
## 10196 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.774319509 0.40030685
## 10198 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.459178679 0.46197056
## 10203 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.516226450 0.53192329
## 10211 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.492138503 0.45247566
## 10225 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.725481316 0.50139293
## 10228 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.793635008 0.55979538
## 10239 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.540070581 0.63677910
## 10242 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.613963149 0.79865283
## 10245 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.549632840 0.54559404
## 10249 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.547269027 0.65716836
## 10255 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.217107124 0.63168757
## 10264 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.730577141 0.68149345
## 10265 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.690522089 0.78699400
## 10273 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.674770221 0.53394252
## 10277 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.413958343 0.73309985
## 10290 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.412648811 0.46322762
## 10294 0.5732753 0.6630101 FALSE   SenJohnMcCain 0.492641404 0.54915641
## 10295 0.5745898 0.4858137 FALSE       SenCapito 0.641325272 0.75353424
## 10296 0.5745898 0.4858137 FALSE       SenCapito 0.392114436 0.44804161
## 10297 0.5745898 0.4858137 FALSE       SenCapito 0.494519720 0.39444407
## 10298 0.5745898 0.4858137 FALSE       SenCapito 0.575151792 0.57699540
## 10300 0.5745898 0.4858137 FALSE       SenCapito 0.674144607 0.48440002
## 10301 0.5745898 0.4858137 FALSE       SenCapito 0.367526711 0.97337383
## 10302 0.5745898 0.4858137 FALSE       SenCapito 0.642911936 0.46601832
## 10303 0.5745898 0.4858137 FALSE       SenCapito 0.410949331 0.48135800
## 10304 0.5745898 0.4858137 FALSE       SenCapito 0.297105210 0.71955456
## 10305 0.5745898 0.4858137 FALSE       SenCapito 0.807659767 0.64359008
## 10307 0.5745898 0.4858137 FALSE       SenCapito 0.551536326 0.47887896
## 10309 0.5745898 0.4858137 FALSE       SenCapito 0.560861064 0.50201358
## 10310 0.5745898 0.4858137 FALSE       SenCapito 0.530317582 0.62782213
## 10311 0.5745898 0.4858137 FALSE       SenCapito 0.561580631 0.74525983
## 10312 0.5745898 0.4858137 FALSE       SenCapito 0.661005717 0.65013610
## 10313 0.5745898 0.4858137 FALSE       SenCapito 0.717624189 0.48358313
## 10319 0.5745898 0.4858137 FALSE       SenCapito 0.630112007 0.43335263
## 10320 0.5745898 0.4858137 FALSE       SenCapito 0.553823061 0.65521462
## 10321 0.5745898 0.4858137 FALSE       SenCapito 0.490848167 0.80175478
## 10322 0.5745898 0.4858137 FALSE       SenCapito 0.405589719 0.72281226
## 10324 0.5745898 0.4858137 FALSE       SenCapito 0.552779355 0.43315863
## 10326 0.5745898 0.4858137 FALSE       SenCapito 0.559030104 0.84091494
## 10330 0.5745898 0.4858137 FALSE       SenCapito 0.825632628 0.60196419
## 10332 0.5745898 0.4858137 FALSE       SenCapito 0.804225834 0.74213832
## 10335 0.5745898 0.4858137 FALSE       SenCapito 0.774318786 0.66054670
## 10336 0.5745898 0.4858137 FALSE       SenCapito 0.421310731 0.44950636
## 10338 0.5745898 0.4858137 FALSE       SenCapito 0.665949012 0.50831270
## 10339 0.5745898 0.4858137 FALSE       SenCapito 0.744275432 0.51508532
## 10343 0.5745898 0.4858137 FALSE       SenCapito 0.457363716 0.63914029
## 10344 0.5745898 0.4858137 FALSE       SenCapito 0.380954539 0.53097091
## 10345 0.5745898 0.4858137 FALSE       SenCapito 0.756874137 0.60271285
## 10347 0.5745898 0.4858137 FALSE       SenCapito 0.440426337 0.76335461
## 10348 0.5745898 0.4858137 FALSE       SenCapito 0.627156592 0.69022004
## 10349 0.5745898 0.4858137 FALSE       SenCapito 0.689237419 0.60645562
## 10350 0.5745898 0.4858137 FALSE       SenCapito 0.530046063 0.56623394
## 10352 0.5745898 0.4858137 FALSE       SenCapito 0.792092230 0.54284979
## 10354 0.5745898 0.4858137 FALSE       SenCapito 0.398459902 0.63563889
## 10356 0.5745898 0.4858137 FALSE       SenCapito 0.523736367 0.68454044
## 10357 0.5745898 0.4858137 FALSE       SenCapito 0.504398875 0.44231683
## 10359 0.5745898 0.4858137 FALSE       SenCapito 0.529924773 0.50096772
## 10360 0.5745898 0.4858137 FALSE       SenCapito 0.470384969 0.44863002
## 10362 0.5745898 0.4858137 FALSE       SenCapito 0.327853122 0.46502095
## 10375 0.5745898 0.4858137 FALSE       SenCapito 0.283526927 0.62166139
## 10379 0.5745898 0.4858137 FALSE       SenCapito 0.573362959 0.49075690
## 10382 0.5745898 0.4858137 FALSE       SenCapito 0.299935912 0.47542349
## 10392 0.5745898 0.4858137 FALSE       SenCapito 0.573460738 0.63801080
## 10409 0.5745898 0.4858137 FALSE       SenCapito 0.580395575 0.40017190
## 10421 0.5745898 0.4858137 FALSE       SenCapito 0.446237538 0.56211227
## 10425 0.5745898 0.4858137 FALSE       SenCapito 0.443612018 0.66690425
## 10436 0.5745898 0.4858137 FALSE       SenCapito 0.498868132 0.72974782
## 10437 0.5745898 0.4858137 FALSE       SenCapito 0.697237744 0.70699478
## 10438 0.5753059 0.6019949 FALSE    SenateMajLdr 0.438412144 0.64649921
## 10439 0.5753059 0.6019949 FALSE    SenateMajLdr 0.694508351 0.97125038
## 10440 0.5753059 0.6019949 FALSE    SenateMajLdr 0.736165369 0.75878850
## 10442 0.5753059 0.6019949 FALSE    SenateMajLdr 0.559577708 0.84094495
## 10443 0.5753059 0.6019949 FALSE    SenateMajLdr 0.681039413 0.50128725
## 10444 0.5753059 0.6019949 FALSE    SenateMajLdr 0.803944878 0.65223883
## 10445 0.5753059 0.6019949 FALSE    SenateMajLdr 0.799912231 0.74718683
## 10448 0.5753059 0.6019949 FALSE    SenateMajLdr 0.691203359 0.71167450
## 10449 0.5753059 0.6019949 FALSE    SenateMajLdr 0.367405108 0.78468721
## 10450 0.5753059 0.6019949 FALSE    SenateMajLdr 0.547335329 0.43471930
## 10451 0.5753059 0.6019949 FALSE    SenateMajLdr 0.445076426 0.76627186
## 10452 0.5753059 0.6019949 FALSE    SenateMajLdr 0.398277382 0.39736011
## 10456 0.5753059 0.6019949 FALSE    SenateMajLdr 0.637889421 0.75466016
## 10458 0.5753059 0.6019949 FALSE    SenateMajLdr 0.574106822 0.63802395
## 10461 0.5753059 0.6019949 FALSE    SenateMajLdr 0.574743896 0.51081323
## 10465 0.5753059 0.6019949 FALSE    SenateMajLdr 0.605294055 0.58628539
## 10466 0.5753059 0.6019949 FALSE    SenateMajLdr 0.462386523 0.45980557
## 10467 0.5753059 0.6019949 FALSE    SenateMajLdr 0.710983033 0.56489540
## 10468 0.5753059 0.6019949 FALSE    SenateMajLdr 0.732673490 0.67400813
## 10470 0.5753059 0.6019949 FALSE    SenateMajLdr 0.752979771 0.61445911
## 10471 0.5753059 0.6019949 FALSE    SenateMajLdr 0.681821700 0.62033553
## 10475 0.5753059 0.6019949 FALSE    SenateMajLdr 0.536190757 0.49515362
## 10476 0.5753059 0.6019949 FALSE    SenateMajLdr 0.558276714 0.65617900
## 10477 0.5753059 0.6019949 FALSE    SenateMajLdr 0.772135410 0.39842616
## 10478 0.5753059 0.6019949 FALSE    SenateMajLdr 0.791854039 0.55454249
## 10480 0.5753059 0.6019949 FALSE    SenateMajLdr 0.641956114 0.60547768
## 10481 0.5753059 0.6019949 FALSE    SenateMajLdr 0.529435566 0.68677222
## 10482 0.5753059 0.6019949 FALSE    SenateMajLdr 0.580113248 0.69510651
## 10487 0.5753059 0.6019949 FALSE    SenateMajLdr 0.653433310 0.48010841
## 10488 0.5753059 0.6019949 FALSE    SenateMajLdr 0.574240685 0.58940818
## 10493 0.5753059 0.6019949 FALSE    SenateMajLdr 0.835395383 0.56482703
## 10496 0.5753059 0.6019949 FALSE    SenateMajLdr 0.670350252 0.52945806
## 10499 0.5753059 0.6019949 FALSE    SenateMajLdr 0.652372061 0.65762980
## 10500 0.5753059 0.6019949 FALSE    SenateMajLdr 0.694050215 0.78421311
## 10516 0.5753059 0.6019949 FALSE    SenateMajLdr 0.297965725 0.48424829
## 10517 0.5753059 0.6019949 FALSE    SenateMajLdr 0.387482561 0.45817315
## 10518 0.5753059 0.6019949 FALSE    SenateMajLdr 0.217139069 0.62770733
## 10520 0.5753059 0.6019949 FALSE    SenateMajLdr 0.325206766 0.47429063
## 10521 0.5753059 0.6019949 FALSE    SenateMajLdr 0.423789695 0.83446919
## 10522 0.5753059 0.6019949 FALSE    SenateMajLdr 0.653696140 0.78105170
## 10526 0.5753059 0.6019949 FALSE    SenateMajLdr 0.672732110 0.57541045
## 10534 0.5753059 0.6019949 FALSE    SenateMajLdr 0.317991184 0.68915659
## 10551 0.5753059 0.6019949 FALSE    SenateMajLdr 0.621910071 0.69222034
## 10558 0.5753059 0.6019949 FALSE    SenateMajLdr 0.833101454 0.69604540
## 10560 0.5753059 0.6019949 FALSE    SenateMajLdr 0.562545541 0.74532695
## 10597 0.5753059 0.6019949 FALSE    SenateMajLdr 0.440001835 0.53634923
## 10598 0.5753059 0.6019949 FALSE    SenateMajLdr 0.722237022 0.49766699
## 10600 0.5753059 0.6019949 FALSE    SenateMajLdr 0.541021405 0.63449106
## 10604 0.5753059 0.6019949 FALSE    SenateMajLdr 0.298389716 0.58555810
## 10609 0.5753059 0.6019949 FALSE    SenateMajLdr 0.382185448 0.75323963
## 10610 0.5753059 0.6019949 FALSE    SenateMajLdr 0.300198178 0.65595883
## 10633 0.5753059 0.6019949 FALSE    SenateMajLdr 0.723554734 0.70691118
## 10637 0.5753059 0.6019949 FALSE    SenateMajLdr 0.486378288 0.59657934
## 10641 0.5753059 0.6019949 FALSE    SenateMajLdr 0.645949467 0.82606916
## 10645 0.5753059 0.6019949 FALSE    SenateMajLdr 0.769496912 0.66886771
## 10675 0.5753059 0.6019949 FALSE    SenateMajLdr 0.578632777 0.49148173
## 10684 0.5753059 0.6019949 FALSE    SenateMajLdr 0.410918829 0.72796121
## 10688 0.5753059 0.6019949 FALSE    SenateMajLdr 0.581339274 0.40021798
## 10694 0.5753059 0.6019949 FALSE    SenateMajLdr 0.301322525 0.54613860
## 10701 0.5793850 0.4664930 FALSE      ChrisCoons 0.529348543 0.59791791
## 10702 0.5793850 0.4664930 FALSE      ChrisCoons 0.836994021 0.39988719
## 10703 0.5793850 0.4664930 FALSE      ChrisCoons 0.558105048 0.49998497
## 10704 0.5793850 0.4664930 FALSE      ChrisCoons 0.523722396 0.68453687
## 10710 0.5793850 0.4664930 FALSE      ChrisCoons 0.574052160 0.63802219
## 10711 0.5793850 0.4664930 FALSE      ChrisCoons 0.662322172 0.64949231
## 10712 0.5793850 0.4664930 FALSE      ChrisCoons 0.496657847 0.39232993
## 10719 0.5793850 0.4664930 FALSE      ChrisCoons 0.690796236 0.60509256
## 10720 0.5793850 0.4664930 FALSE      ChrisCoons 0.506455077 0.43819229
## 10722 0.5793850 0.4664930 FALSE      ChrisCoons 0.299945186 0.47382288
## 10723 0.5793850 0.4664930 FALSE      ChrisCoons 0.471362276 0.44508754
## 10725 0.5793850 0.4664930 FALSE      ChrisCoons 0.713562274 0.54560412
## 10728 0.5793850 0.4664930 FALSE      ChrisCoons 0.556703599 0.43110882
## 10729 0.5793850 0.4664930 FALSE      ChrisCoons 0.808530911 0.64238233
## 10731 0.5793850 0.4664930 FALSE      ChrisCoons 0.397752910 0.63484276
## 10734 0.5793850 0.4664930 FALSE      ChrisCoons 0.405141472 0.72249995
## 10737 0.5793850 0.4664930 FALSE      ChrisCoons 0.443042200 0.66650409
## 10738 0.5793850 0.4664930 FALSE      ChrisCoons 0.667267322 0.50439117
## 10739 0.5793850 0.4664930 FALSE      ChrisCoons 0.628063198 0.69000482
## 10743 0.5793850 0.4664930 FALSE      ChrisCoons 0.527864652 0.49643744
## 10750 0.5793850 0.4664930 FALSE      ChrisCoons 0.581346791 0.40021821
## 10760 0.5793850 0.4664930 FALSE      ChrisCoons 0.805046714 0.74143432
## 10765 0.5793850 0.4664930 FALSE      ChrisCoons 0.653687710 0.58557254
## 10767 0.5793850 0.4664930 FALSE      ChrisCoons 0.846028863 0.48340239
## 10769 0.5793850 0.4664930 FALSE      ChrisCoons 0.561894074 0.74527752
## 10771 0.5793850 0.4664930 FALSE      ChrisCoons 0.698263541 0.70645664
## 10773 0.5793850 0.4664930 FALSE      ChrisCoons 0.407140051 0.68644092
## 10781 0.5793850 0.4664930 FALSE      ChrisCoons 0.232197750 0.52546827
## 10783 0.5793850 0.4664930 FALSE      ChrisCoons 0.674406348 0.48041970
## 10789 0.5793850 0.4664930 FALSE      ChrisCoons 0.617291353 0.55183668
## 10794 0.5793850 0.4664930 FALSE      ChrisCoons 0.826349610 0.60053615
## 10800 0.5793850 0.4664930 FALSE      ChrisCoons 0.392480629 0.44573383
## 10801 0.5793850 0.4664930 FALSE      ChrisCoons 0.296725701 0.64729971
## 10802 0.5793850 0.4664930 FALSE      ChrisCoons 0.421791682 0.44683806
## 10804 0.5793850 0.4664930 FALSE      ChrisCoons 0.296769576 0.57510828
## 10805 0.5793850 0.4664930 FALSE      ChrisCoons 0.326732710 0.60965274
## 10811 0.5793850 0.4664930 FALSE      ChrisCoons 0.433724379 0.54375000
## 10828 0.5793850 0.4664930 FALSE      ChrisCoons 0.837623433 0.68832341
## 10840 0.5793850 0.4664930 FALSE      ChrisCoons 0.440164557 0.76322993
## 10853 0.5793850 0.4664930 FALSE      ChrisCoons 0.581203418 0.69507405
## 10854 0.5793850 0.4664930 FALSE      ChrisCoons 0.283050027 0.62069456
## 10860 0.5793850 0.4664930 FALSE      ChrisCoons 0.766377420 0.38992669
## 10861 0.5793850 0.4664930 FALSE      ChrisCoons 0.530172920 0.62777751
## 10862 0.5793850 0.4664930 FALSE      ChrisCoons 0.404754435 0.38863349
## 10865 0.5793850 0.4664930 FALSE      ChrisCoons 0.741787234 0.75446264
## 10875 0.5793850 0.4664930 FALSE      ChrisCoons 0.380453384 0.52913945
## 10894 0.5793850 0.4664930 FALSE      ChrisCoons 0.456689347 0.63864239
## 10897 0.5793850 0.4664930 FALSE      ChrisCoons 0.490827692 0.80174937
## 10898 0.5793850 0.4664930 FALSE      ChrisCoons 0.580611898 0.46154986
## 10900 0.5793850 0.4664930 FALSE      ChrisCoons 0.362816191 0.78060744
## 10916 0.5793850 0.4664930 FALSE      ChrisCoons 0.471582145 0.76848628
## 10938 0.5793850 0.4664930 FALSE      ChrisCoons 0.562983954 0.79862508
## 10948 0.5793850 0.4664930 FALSE      ChrisCoons 0.498776783 0.72971966
## 10951 0.5793850 0.4664930 FALSE      ChrisCoons 0.730399474 0.70035129
## 10955 0.5793850 0.4664930 FALSE      ChrisCoons 0.642037846 0.75336766
## 10976 0.5793850 0.4664930 FALSE      ChrisCoons 0.499484922 0.51565393
## 10990 0.5793850 0.4664930 FALSE      ChrisCoons 0.410891642 0.47884913
## 10995 0.5793850 0.4664930 FALSE      ChrisCoons 0.576058134 0.57700624
## 11000 0.5793850 0.4664930 FALSE      ChrisCoons 0.744830040 0.51262097
## 11008 0.5793850 0.4664930 FALSE      ChrisCoons 0.441000463 0.51688273
## 11009 0.5793850 0.4664930 FALSE      ChrisCoons 0.529208087 0.56579035
## 11032 0.5814023 0.7200733 FALSE     SteveDaines 0.768636211 0.68199060
## 11033 0.5814023 0.7200733 FALSE     SteveDaines 0.628556956 0.76133012
## 11035 0.5814023 0.7200733 FALSE     SteveDaines 0.728555018 0.50386077
## 11036 0.5814023 0.7200733 FALSE     SteveDaines 0.302951869 0.73437812
## 11037 0.5814023 0.7200733 FALSE     SteveDaines 0.825135256 0.62180975
## 11038 0.5814023 0.7200733 FALSE     SteveDaines 0.678536790 0.53639095
## 11039 0.5814023 0.7200733 FALSE     SteveDaines 0.619892515 0.59851806
## 11040 0.5814023 0.7200733 FALSE     SteveDaines 0.570012763 0.74718030
## 11041 0.5814023 0.7200733 FALSE     SteveDaines 0.684420120 0.72714601
## 11042 0.5814023 0.7200733 FALSE     SteveDaines 0.796058889 0.56389918
## 11043 0.5814023 0.7200733 FALSE     SteveDaines 0.687982551 0.50641613
## 11045 0.5814023 0.7200733 FALSE     SteveDaines 0.576594911 0.62696167
## 11046 0.5814023 0.7200733 FALSE     SteveDaines 0.844928751 0.41261093
## 11047 0.5814023 0.7200733 FALSE     SteveDaines 0.831626662 0.70601591
## 11058 0.5814023 0.7200733 FALSE     SteveDaines 0.692134012 0.97215244
## 11059 0.5814023 0.7200733 FALSE     SteveDaines 0.776451002 0.40176974
## 11061 0.5814023 0.7200733 FALSE     SteveDaines 0.532888648 0.49611079
## 11062 0.5814023 0.7200733 FALSE     SteveDaines 0.608529114 0.71712942
## 11063 0.5814023 0.7200733 FALSE     SteveDaines 0.403102500 0.65983873
## 11064 0.5814023 0.7200733 FALSE     SteveDaines 0.576800242 0.68776036
## 11065 0.5814023 0.7200733 FALSE     SteveDaines 0.416658800 0.70795734
## 11068 0.5814023 0.7200733 FALSE     SteveDaines 0.579583910 0.49149226
## 11069 0.5814023 0.7200733 FALSE     SteveDaines 0.681681430 0.58870156
## 11070 0.5814023 0.7200733 FALSE     SteveDaines 0.457672923 0.46275870
## 11071 0.5814023 0.7200733 FALSE     SteveDaines 0.546268884 0.43487408
## 11072 0.5814023 0.7200733 FALSE     SteveDaines 0.284983925 0.63884214
## 11073 0.5814023 0.7200733 FALSE     SteveDaines 0.545016335 0.97525034
## 11075 0.5814023 0.7200733 FALSE     SteveDaines 0.730293972 0.76850522
## 11078 0.5814023 0.7200733 FALSE     SteveDaines 0.433612059 0.54455985
## 11082 0.5814023 0.7200733 FALSE     SteveDaines 0.328542182 0.63033850
## 11090 0.5814023 0.7200733 FALSE     SteveDaines 0.410861747 0.46453839
## 11092 0.5814023 0.7200733 FALSE     SteveDaines 0.429753850 0.83991302
## 11096 0.5814023 0.7200733 FALSE     SteveDaines 0.375977787 0.55245396
## 11099 0.5814023 0.7200733 FALSE     SteveDaines 0.549232870 0.54567119
## 11100 0.5814023 0.7200733 FALSE     SteveDaines 0.491147729 0.45283370
## 11102 0.5814023 0.7200733 FALSE     SteveDaines 0.851569592 0.50074165
## 11106 0.5814023 0.7200733 FALSE     SteveDaines 0.539132297 0.67068286
## 11109 0.5814023 0.7200733 FALSE     SteveDaines 0.646211309 0.78611035
## 11112 0.5814023 0.7200733 FALSE     SteveDaines 0.372233287 0.79296301
## 11113 0.5814023 0.7200733 FALSE     SteveDaines 0.271974163 0.51260841
## 11114 0.5814023 0.7200733 FALSE     SteveDaines 0.216552553 0.63516411
## 11119 0.5814023 0.7200733 FALSE     SteveDaines 0.730915402 0.68943037
## 11120 0.5814023 0.7200733 FALSE     SteveDaines 0.291561995 0.77141605
## 11122 0.5820865 0.3752292 FALSE   SenBillNelson 0.518579612 0.48725176
## 11123 0.5820865 0.3752292 FALSE   SenBillNelson 0.489736486 0.80148718
## 11125 0.5820865 0.3752292 FALSE   SenBillNelson 0.391472255 0.43544348
## 11127 0.5820865 0.3752292 FALSE   SenBillNelson 0.811930000 0.63876366
## 11128 0.5820865 0.3752292 FALSE   SenBillNelson 0.503042733 0.37554285
## 11129 0.5820865 0.3752292 FALSE   SenBillNelson 0.695300455 0.60220627
## 11132 0.5820865 0.3752292 FALSE   SenBillNelson 0.574040364 0.63802183
## 11133 0.5820865 0.3752292 FALSE   SenBillNelson 0.469371969 0.42939895
## 11134 0.5820865 0.3752292 FALSE   SenBillNelson 0.213143589 0.61584088
## 11135 0.6211460 0.8225987 FALSE      SenMikeLee 0.640822009 0.81398196
## 11137 0.6211460 0.8225987 FALSE      SenMikeLee 0.664540650 0.69591386
## 11138 0.6211460 0.8225987 FALSE      SenMikeLee 0.561845559 0.70244710
## 11140 0.6211460 0.8225987 FALSE      SenMikeLee 0.725199795 0.58125861
## 11141 0.6211460 0.8225987 FALSE      SenMikeLee 0.843271367 0.57973800
## 11146 0.6211460 0.8225987 FALSE      SenMikeLee 0.730462408 0.78447153
## 11147 0.6211460 0.8225987 FALSE      SenMikeLee 0.579272912 0.78654149
## 11150 0.6211460 0.8225987 FALSE      SenMikeLee 0.547853015 0.43462713
## 11152 0.6211460 0.8225987 FALSE      SenMikeLee 0.487722017 0.79678061
## 11154 0.6211460 0.8225987 FALSE      SenMikeLee 0.634371762 0.83377561
## 11158 0.6211460 0.8225987 FALSE      SenMikeLee 0.427066666 0.57513576
## 11165 0.6211460 0.8225987 FALSE      SenMikeLee 0.834236676 0.71581406
## 11166 0.6211460 0.8225987 FALSE      SenMikeLee 0.797025678 0.76815185
## 11168 0.6211460 0.8225987 FALSE      SenMikeLee 0.734196819 0.50673116
## 11184 0.6211460 0.8225987 FALSE      SenMikeLee 0.634743420 0.79936780
## 11188 0.6211460 0.8225987 FALSE      SenMikeLee 0.514850674 0.53247413
## 11189 0.6211460 0.8225987 FALSE      SenMikeLee 0.434842226 0.85157177
## 11202 0.6211460 0.8225987 FALSE      SenMikeLee 0.829969303 0.62943786
## 11218 0.6211460 0.8225987 FALSE      SenMikeLee 0.475787873 0.61552170
## 11222 0.6211460 0.8225987 FALSE      SenMikeLee 0.586747412 0.82317552
## 11226 0.6211460 0.8225987 FALSE      SenMikeLee 0.737985299 0.70234157
## 11229 0.6211460 0.8225987 FALSE      SenMikeLee 0.491422637 0.45273907
## 11230 0.6211460 0.8225987 FALSE      SenMikeLee 0.663800912 0.48386516
## 11237 0.6211460 0.8225987 FALSE      SenMikeLee 0.414017794 0.71744102
## 11251 0.6211460 0.8225987 FALSE      SenMikeLee 0.693529599 0.50840691
## 11260 0.6211460 0.8225987 FALSE      SenMikeLee 0.373548820 0.80314760
## 11283 0.6211460 0.8225987 FALSE      SenMikeLee 0.551728005 0.97719371
## 11295 0.6211460 0.8225987 FALSE      SenMikeLee 0.696567392 0.64753759
## 11297 0.6211460 0.8225987 FALSE      SenMikeLee 0.534365942 0.72724902
## 11301 0.6211460 0.8225987 FALSE      SenMikeLee 0.291729630 0.77905297
## 11340 0.6211460 0.8225987 FALSE      SenMikeLee 0.808804732 0.67317711
## 11345 0.6211460 0.8225987 FALSE      SenMikeLee 0.691555116 0.97241557
## 11349 0.6211460 0.8225987 FALSE      SenMikeLee 0.292582294 0.49220510
## 11355 0.6211460 0.8225987 FALSE      SenMikeLee 0.684757889 0.53868600
## 11390 0.6211460 0.8225987 FALSE      SenMikeLee 0.780616392 0.40381691
## 11419 0.6211460 0.8225987 FALSE      SenMikeLee 0.689703596 0.59278607
## 11422 0.6211460 0.8225987 FALSE      SenMikeLee 0.386976480 0.77375846
## 11438 0.6211460 0.8225987 FALSE      SenMikeLee 0.326090669 0.63537200
## 11452 0.6211460 0.8225987 FALSE      SenMikeLee 0.399836582 0.66626168
## 11455 0.6211460 0.8225987 FALSE      SenMikeLee 0.683191935 0.81009652
## 11469 0.6211460 0.8225987 FALSE      SenMikeLee 0.531636722 0.64364104
## 11494 0.6211460 0.8225987 FALSE      SenMikeLee 0.294049356 0.59821865
## 11510 0.6211460 0.8225987 FALSE      SenMikeLee 0.630572806 0.73927375
## 11530 0.6274394 0.5746844 FALSE   SenThomTillis 0.671938816 0.57093072
## 11533 0.6274394 0.5746844 FALSE   SenThomTillis 0.502086600 0.53610798
## 11534 0.6274394 0.5746844 FALSE   SenThomTillis 0.543373672 0.61129899
## 11535 0.6274394 0.5746844 FALSE   SenThomTillis 0.647523588 0.59101205
## 11536 0.6274394 0.5746844 FALSE   SenThomTillis 0.409255527 0.48974688
## 11538 0.6274394 0.5746844 FALSE   SenThomTillis 0.672206347 0.53162212
## 11541 0.6274394 0.5746844 FALSE   SenThomTillis 0.450711070 0.67483536
## 11543 0.6274394 0.5746844 FALSE   SenThomTillis 0.753819189 0.60955849
## 11548 0.6274394 0.5746844 FALSE   SenThomTillis 0.411432496 0.72865610
## 11549 0.6274394 0.5746844 FALSE   SenThomTillis 0.805252898 0.64806744
## 11550 0.6274394 0.5746844 FALSE   SenThomTillis 0.568444015 0.74658231
## 11553 0.6274394 0.5746844 FALSE   SenThomTillis 0.745629961 0.52844380
## 11554 0.6274394 0.5746844 FALSE   SenThomTillis 0.728410275 0.70177852
## 11555 0.6274394 0.5746844 FALSE   SenThomTillis 0.381364876 0.54012563
## 11556 0.6274394 0.5746844 FALSE   SenThomTillis 0.285570370 0.62835724
## 11559 0.6274394 0.5746844 FALSE   SenThomTillis 0.773495957 0.39964860
## 11560 0.6274394 0.5746844 FALSE   SenThomTillis 0.506582837 0.73371927
## 11561 0.6274394 0.5746844 FALSE   SenThomTillis 0.466891957 0.45515737
## 11562 0.6274394 0.5746844 FALSE   SenThomTillis 0.710382295 0.56206255
## 11563 0.6274394 0.5746844 FALSE   SenThomTillis 0.771863625 0.66387205
## 11564 0.6274394 0.5746844 FALSE   SenThomTillis 0.500749942 0.44690150
## 11566 0.6274394 0.5746844 FALSE   SenThomTillis 0.586344417 0.64169820
## 11567 0.6274394 0.5746844 FALSE   SenThomTillis 0.232408276 0.53232069
## 11569 0.6274394 0.5746844 FALSE   SenThomTillis 0.723045300 0.49874278
## 11570 0.6274394 0.5746844 FALSE   SenThomTillis 0.449747730 0.57486183
## 11571 0.6274394 0.5746844 FALSE   SenThomTillis 0.697630621 0.70678149
## 11575 0.6274394 0.5746844 FALSE   SenThomTillis 0.589533093 0.48934074
## 11578 0.6274394 0.5746844 FALSE   SenThomTillis 0.658845063 0.48271956
## 11584 0.6274394 0.5746844 FALSE   SenThomTillis 0.587368055 0.50730130
## 11587 0.6274394 0.5746844 FALSE   SenThomTillis 0.543007039 0.63686435
## 11588 0.6274394 0.5746844 FALSE   SenThomTillis 0.632320845 0.68945480
## 11590 0.6274394 0.5746844 FALSE   SenThomTillis 0.533387091 0.68942542
## 11592 0.6274394 0.5746844 FALSE   SenThomTillis 0.659814553 0.77926044
## 11593 0.6274394 0.5746844 FALSE   SenThomTillis 0.418720307 0.45609425
## 11594 0.6274394 0.5746844 FALSE   SenThomTillis 0.288778111 0.76359780
## 11596 0.6274394 0.5746844 FALSE   SenThomTillis 0.588949179 0.69623958
## 11598 0.6274394 0.5746844 FALSE   SenThomTillis 0.563739563 0.84157400
## 11601 0.6274394 0.5746844 FALSE   SenThomTillis 0.802888898 0.74342877
## 11602 0.6274394 0.5746844 FALSE   SenThomTillis 0.568130129 0.79942224
## 11607 0.6274394 0.5746844 FALSE   SenThomTillis 0.329717169 0.61834961
## 11608 0.6274394 0.5746844 FALSE   SenThomTillis 0.791499348 0.55253590
## 11610 0.6274394 0.5746844 FALSE   SenThomTillis 0.685320249 0.61123058
## 11611 0.6274394 0.5746844 FALSE   SenThomTillis 0.596718935 0.56902587
## 11615 0.6274394 0.5746844 FALSE   SenThomTillis 0.651112824 0.82502333
## 11616 0.6274394 0.5746844 FALSE   SenThomTillis 0.441848572 0.53114629
## 11617 0.6274394 0.5746844 FALSE   SenThomTillis 0.403289219 0.64441146
## 11626 0.6274394 0.5746844 FALSE   SenThomTillis 0.464933593 0.64864472
## 11633 0.6274394 0.5746844 FALSE   SenThomTillis 0.597451222 0.59039391
## 11635 0.6274394 0.5746844 FALSE   SenThomTillis 0.554599327 0.43231776
## 11643 0.6274394 0.5746844 FALSE   SenThomTillis 0.835185266 0.56272693
## 11659 0.6274394 0.5746844 FALSE   SenThomTillis 0.565490930 0.65981448
## 11661 0.6274394 0.5746844 FALSE   SenThomTillis 0.368029514 0.78542613
## 11664 0.6274394 0.5746844 FALSE   SenThomTillis 0.528229516 0.52091275
## 11666 0.6274394 0.5746844 FALSE   SenThomTillis 0.834839958 0.69228267
## 11675 0.6274394 0.5746844 FALSE   SenThomTillis 0.740768276 0.75506958
## 11676 0.6274394 0.5746844 FALSE   SenThomTillis 0.486238330 0.59201424
## 11679 0.6274394 0.5746844 FALSE   SenThomTillis 0.565680311 0.53467792
## 11680 0.6274394 0.5746844 FALSE   SenThomTillis 0.413560871 0.69394812
## 11683 0.6274394 0.5746844 FALSE   SenThomTillis 0.683631621 0.50365183
## 11686 0.6274394 0.5746844 FALSE   SenThomTillis 0.697811634 0.97042757
## 11699 0.6274394 0.5746844 FALSE   SenThomTillis 0.662133710 0.64957867
## 11708 0.6274394 0.5746844 FALSE   SenThomTillis 0.736427885 0.66813768
## 11714 0.6274394 0.5746844 FALSE   SenThomTillis 0.438054504 0.64547522
## 11724 0.6274394 0.5746844 FALSE   SenThomTillis 0.217007312 0.62637366
## 11727 0.6274394 0.5746844 FALSE   SenThomTillis 0.587629581 0.39960689
## 11735 0.6274394 0.5746844 FALSE   SenThomTillis 0.478250749 0.77208639
## 11754 0.6274394 0.5746844 FALSE   SenThomTillis 0.298424845 0.58341373
## 11776 0.6274394 0.5746844 FALSE   SenThomTillis 0.644930330 0.75291148
## 11785 0.6274394 0.5746844 FALSE   SenThomTillis 0.699477653 0.78154890
## 11790 0.6274394 0.5746844 FALSE   SenThomTillis 0.301712790 0.54352527
## 11801 0.6274394 0.5746844 FALSE   SenThomTillis 0.645023885 0.44096957
## 11806 0.6274394 0.5746844 FALSE   SenThomTillis 0.436540050 0.55768191
## 11836 0.6274394 0.5746844 FALSE   SenThomTillis 0.542747263 0.58506258
## 11840 0.6333832 0.7144322 FALSE  SenatorIsakson 0.580196645 0.75505405
## 11841 0.6333832 0.7144322 FALSE  SenatorIsakson 0.494797228 0.45126827
## 11842 0.6333832 0.7144322 FALSE  SenatorIsakson 0.719010227 0.71979141
## 11845 0.6333832 0.7144322 FALSE  SenatorIsakson 0.453741239 0.69046698
## 11846 0.6333832 0.7144322 FALSE  SenatorIsakson 0.494225070 0.54793690
## 11848 0.6333832 0.7144322 FALSE  SenatorIsakson 0.731130250 0.69038356
## 11850 0.6333832 0.7144322 FALSE  SenatorIsakson 0.296939683 0.59258948
## 11851 0.6333832 0.7144322 FALSE  SenatorIsakson 0.586778989 0.62420680
## 11852 0.6333832 0.7144322 FALSE  SenatorIsakson 0.431277025 0.84206466
## 11855 0.6333832 0.7144322 FALSE  SenatorIsakson 0.584705031 0.49092045
## 11856 0.6333832 0.7144322 FALSE  SenatorIsakson 0.585824649 0.39994809
## 11865 0.6333832 0.7144322 FALSE  SenatorIsakson 0.302940314 0.73417012
## 11870 0.6333832 0.7144322 FALSE  SenatorIsakson 0.731816173 0.76484271
## 11872 0.6333832 0.7144322 FALSE  SenatorIsakson 0.431956828 0.57003014
## 11873 0.6333832 0.7144322 FALSE  SenatorIsakson 0.542508747 0.70998266
## 11874 0.6333832 0.7144322 FALSE  SenatorIsakson 0.691857469 0.78581832
## 11876 0.6333832 0.7144322 FALSE  SenatorIsakson 0.550313536 0.43403187
## 11877 0.6333832 0.7144322 FALSE  SenatorIsakson 0.377642254 0.55015997
## 11878 0.6333832 0.7144322 FALSE  SenatorIsakson 0.453139191 0.77759496
## 11882 0.6333832 0.7144322 FALSE  SenatorIsakson 0.403691132 0.65782001
## 11884 0.6333832 0.7144322 FALSE  SenatorIsakson 0.436319173 0.54190435
## 11890 0.6333832 0.7144322 FALSE  SenatorIsakson 0.295728227 0.48838605
## 11896 0.6333832 0.7144322 FALSE  SenatorIsakson 0.592272142 0.67926186
## 11899 0.6333832 0.7144322 FALSE  SenatorIsakson 0.731942650 0.50579795
## 11903 0.6333832 0.7144322 FALSE  SenatorIsakson 0.721451407 0.57924848
## 11912 0.6333832 0.7144322 FALSE  SenatorIsakson 0.300389064 0.66447977
## 11914 0.6333832 0.7144322 FALSE  SenatorIsakson 0.460901254 0.46089826
## 11919 0.6333832 0.7144322 FALSE  SenatorIsakson 0.804360893 0.66459992
## 11922 0.6333832 0.7144322 FALSE  SenatorIsakson 0.569082720 0.84351366
## 11925 0.6333832 0.7144322 FALSE  SenatorIsakson 0.683393784 0.53833977
## 11932 0.6333832 0.7144322 FALSE  SenatorIsakson 0.831611458 0.70571231
## 11935 0.6333832 0.7144322 FALSE  SenatorIsakson 0.485924756 0.78166028
## 11939 0.6333832 0.7144322 FALSE  SenatorIsakson 0.686860810 0.59174681
## 11940 0.6333832 0.7144322 FALSE  SenatorIsakson 0.628501751 0.59966179
## 11943 0.6333832 0.7144322 FALSE  SenatorIsakson 0.539739075 0.63718975
## 11948 0.6333832 0.7144322 FALSE  SenatorIsakson 0.692280426 0.50808497
## 11956 0.6333832 0.7144322 FALSE  SenatorIsakson 0.575466589 0.80269296
## 11959 0.6333832 0.7144322 FALSE  SenatorIsakson 0.555120972 0.54380952
## 11960 0.6333832 0.7144322 FALSE  SenatorIsakson 0.690685326 0.64397330
## 11968 0.6333832 0.7144322 FALSE  SenatorIsakson 0.544617033 0.66403276
## 11973 0.6333832 0.7144322 FALSE  SenatorIsakson 0.641982241 0.75337987
## 11978 0.6333832 0.7144322 FALSE  SenatorIsakson 0.754648518 0.53986723
## 11979 0.6333832 0.7144322 FALSE  SenatorIsakson 0.581586928 0.58764057
## 11983 0.6333832 0.7144322 FALSE  SenatorIsakson 0.375230256 0.97850765
## 11987 0.6333832 0.7144322 FALSE  SenatorIsakson 0.404131140 0.49784608
## 11990 0.6333832 0.7144322 FALSE  SenatorIsakson 0.647036077 0.44115184
## 11998 0.6333832 0.7144322 FALSE  SenatorIsakson 0.839426060 0.57528209
## 11999 0.6333832 0.7144322 FALSE  SenatorIsakson 0.372540302 0.79390353
## 12010 0.6333832 0.7144322 FALSE  SenatorIsakson 0.413428647 0.46257406
## 12024 0.6333832 0.7144322 FALSE  SenatorIsakson 0.649800574 0.82518253
## 12030 0.6333832 0.7144322 FALSE  SenatorIsakson 0.549145514 0.97620187
## 12031 0.6333832 0.7144322 FALSE  SenatorIsakson 0.466190715 0.66596179
## 12053 0.6333832 0.7144322 FALSE  SenatorIsakson 0.659485733 0.63065068
## 12056 0.6333832 0.7144322 FALSE  SenatorIsakson 0.396901182 0.39846810
## 12060 0.6333832 0.7144322 FALSE  SenatorIsakson 0.481961184 0.60931609
## 12068 0.6333832 0.7144322 FALSE  SenatorIsakson 0.684800221 0.72419488
## 12069 0.6333832 0.7144322 FALSE  SenatorIsakson 0.655607095 0.69056074
## 12077 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.600217392 0.73653509
## 12078 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.552241511 0.97743130
## 12079 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.684932363 0.79483047
## 12086 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.734930192 0.50698089
## 12088 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.532024177 0.60875365
## 12090 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.585234801 0.77239264
## 12094 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.694802953 0.50866562
## 12095 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.247107653 0.86488479
## 12096 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.729070493 0.77660242
## 12100 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.842620540 0.57912094
## 12105 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.736492565 0.70075932
## 12106 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.302823751 0.73849381
## 12108 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.583794969 0.81180228
## 12109 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.434344141 0.54391864
## 12111 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.387490264 0.76945562
## 12114 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.299511882 0.66827830
## 12115 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.580636605 0.51007142
## 12116 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.648220490 0.44118290
## 12118 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.651361382 0.82500108
## 12120 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.829016118 0.62834548
## 12123 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.553982102 0.54429811
## 12125 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.665393630 0.48401414
## 12126 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.758272884 0.54195858
## 12128 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.550025485 0.43411533
## 12129 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.586834134 0.68401386
## 12131 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.629881136 0.59956485
## 12133 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.492240802 0.54943430
## 12134 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.536705482 0.49495873
## 12135 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.297896130 0.55460072
## 12143 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.402042614 0.66247244
## 12147 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.855805548 0.50485352
## 12148 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.650472619 0.78275300
## 12151 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.633774685 0.80102287
## 12152 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.580449007 0.58807340
## 12163 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.796027085 0.76320089
## 12166 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.832989192 0.71286768
## 12168 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.584788509 0.62512671
## 12170 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.375851076 0.55260799
## 12172 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.725819422 0.58151596
## 12188 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.402472610 0.49944686
## 12191 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.454528748 0.78493709
## 12192 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.291944888 0.77590913
## 12198 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.429825856 0.57261704
## 12199 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.540440754 0.66947987
## 12214 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.517871510 0.53113478
## 12220 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.697463639 0.64790331
## 12238 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.295631840 0.59557635
## 12240 0.6473721 0.7777920 FALSE SenJohnBarrasso 0.666820133 0.69657557
## 12268 0.6482835 0.4161830 FALSE     SenDonnelly 0.813414768 0.63756533
## 12269 0.6482835 0.4161830 FALSE     SenDonnelly 0.749911222 0.50308749
## 12270 0.6482835 0.4161830 FALSE     SenDonnelly 0.458357343 0.63994096
## 12271 0.6482835 0.4161830 FALSE     SenDonnelly 0.406674007 0.38195939
## 12272 0.6482835 0.4161830 FALSE     SenDonnelly 0.796674315 0.53367251
## 12274 0.6482835 0.4161830 FALSE     SenDonnelly 0.722065439 0.53696703
## 12277 0.6482835 0.4161830 FALSE     SenDonnelly 0.662727723 0.77897294
## 12279 0.6482835 0.4161830 FALSE     SenDonnelly 0.747020696 0.75225227
## 12284 0.6482835 0.4161830 FALSE     SenDonnelly 0.847092727 0.47760502
## 12285 0.6482835 0.4161830 FALSE     SenDonnelly 0.647435072 0.75279203
## 12291 0.6482835 0.4161830 FALSE     SenDonnelly 0.583551414 0.54225741
## 12292 0.6482835 0.4161830 FALSE     SenDonnelly 0.839518093 0.54716329
## 12294 0.6482835 0.4161830 FALSE     SenDonnelly 0.471662893 0.43726504
## 12298 0.6482835 0.4161830 FALSE     SenDonnelly 0.479483707 0.57777195
## 12299 0.6482835 0.4161830 FALSE     SenDonnelly 0.689270603 0.54500611
## 12300 0.6482835 0.4161830 FALSE     SenDonnelly 0.410235634 0.47470885
## 12309 0.6482835 0.4161830 FALSE     SenDonnelly 0.559446894 0.65657882
## 12310 0.6482835 0.4161830 FALSE     SenDonnelly 0.442276652 0.76434698
## 12315 0.6482835 0.4161830 FALSE     SenDonnelly 0.841950375 0.68434642
## 12317 0.6482835 0.4161830 FALSE     SenDonnelly 0.566944959 0.79914016
## 12324 0.6482835 0.4161830 FALSE     SenDonnelly 0.550309836 0.46123320
## 12339 0.6482835 0.4161830 FALSE     SenDonnelly 0.568169946 0.41151565
## 12340 0.6482835 0.4161830 FALSE     SenDonnelly 0.809709741 0.73840683
## 12344 0.6482835 0.4161830 FALSE     SenDonnelly 0.780996824 0.65515196
## 12348 0.6482835 0.4161830 FALSE     SenDonnelly 0.527738394 0.68593536
## 12349 0.6482835 0.4161830 FALSE     SenDonnelly 0.580544290 0.63909022
## 12356 0.6482835 0.4161830 FALSE     SenDonnelly 0.592761313 0.46864406
## 12357 0.6482835 0.4161830 FALSE     SenDonnelly 0.670274706 0.64737525
## 12359 0.6482835 0.4161830 FALSE     SenDonnelly 0.684149252 0.46403968
## 12364 0.6482835 0.4161830 FALSE     SenDonnelly 0.501993057 0.73095276
## 12368 0.6482835 0.4161830 FALSE     SenDonnelly 0.656957007 0.43613395
## 12374 0.6482835 0.4161830 FALSE     SenDonnelly 0.445132600 0.56041415
## 12383 0.6482835 0.4161830 FALSE     SenDonnelly 0.327715558 0.45956868
## 12386 0.6482835 0.4161830 FALSE     SenDonnelly 0.584445019 0.57872529
## 12387 0.6482835 0.4161830 FALSE     SenDonnelly 0.680396734 0.49130322
## 12395 0.6482835 0.4161830 FALSE     SenDonnelly 0.299654460 0.47062140
## 12398 0.6482835 0.4161830 FALSE     SenDonnelly 0.296913601 0.71932980
## 12403 0.6482835 0.4161830 FALSE     SenDonnelly 0.499040118 0.51495709
## 12410 0.6534665 0.8499123 FALSE    SenatorRisch 0.435132775 0.85484841
## 12411 0.6534665 0.8499123 FALSE    SenatorRisch 0.698881258 0.75155565
## 12412 0.6534665 0.8499123 FALSE    SenatorRisch 0.563713367 0.70142410
## 12413 0.6534665 0.8499123 FALSE    SenatorRisch 0.850839367 0.41638208
## 12414 0.6534665 0.8499123 FALSE    SenatorRisch 0.700735940 0.64891385
## 12415 0.6534665 0.8499123 FALSE    SenatorRisch 0.804372178 0.57117601
## 12416 0.6534665 0.8499123 FALSE    SenatorRisch 0.579325031 0.78648077
## 12417 0.6534665 0.8499123 FALSE    SenatorRisch 0.394398632 0.40011677
## 12421 0.6534665 0.8499123 FALSE    SenatorRisch 0.584127642 0.49103908
## 12422 0.6534665 0.8499123 FALSE    SenatorRisch 0.529426153 0.61030480
## 12423 0.6534665 0.8499123 FALSE    SenatorRisch 0.582822863 0.62583805
## 12424 0.6534665 0.8499123 FALSE    SenatorRisch 0.453581939 0.79273794
## 12425 0.6534665 0.8499123 FALSE    SenatorRisch 0.552549068 0.54482115
## 12429 0.6534665 0.8499123 FALSE    SenatorRisch 0.460728833 0.67576133
## 12431 0.6534665 0.8499123 FALSE    SenatorRisch 0.583132634 0.68598472
## 12432 0.6534665 0.8499123 FALSE    SenatorRisch 0.640240744 0.83873541
## 12434 0.6534665 0.8499123 FALSE    SenatorRisch 0.298020667 0.67196276
## 12435 0.6534665 0.8499123 FALSE    SenatorRisch 0.244713137 0.70676428
## 12436 0.6534665 0.8499123 FALSE    SenatorRisch 0.666064061 0.48404624
## 12439 0.6534665 0.8499123 FALSE    SenatorRisch 0.798840582 0.77250826
## 12444 0.6534665 0.8499123 FALSE    SenatorRisch 0.214760199 0.64027630
## 12447 0.6534665 0.8499123 FALSE    SenatorRisch 0.578983950 0.58854006
## 12450 0.6534665 0.8499123 FALSE    SenatorRisch 0.373803970 0.55479518
## 12456 0.6534665 0.8499123 FALSE    SenatorRisch 0.658277436 0.82835301
## 12457 0.6534665 0.8499123 FALSE    SenatorRisch 0.247651566 0.86902406
## 12459 0.6534665 0.8499123 FALSE    SenatorRisch 0.832462724 0.63178771
## 12461 0.6534665 0.8499123 FALSE    SenatorRisch 0.629793042 0.59957334
## 12463 0.6534665 0.8499123 FALSE    SenatorRisch 0.736715314 0.50748572
## 12464 0.6534665 0.8499123 FALSE    SenatorRisch 0.649477137 0.80270317
## 12465 0.6534665 0.8499123 FALSE    SenatorRisch 0.688417098 0.82107037
## 12472 0.6534665 0.8499123 FALSE    SenatorRisch 0.733898112 0.79100945
## 12474 0.6637223 0.8039531 FALSE      SenTedCruz 0.736649771 0.50746969
## 12475 0.6637223 0.8039531 FALSE      SenTedCruz 0.644046329 0.81256992
## 12476 0.6637223 0.8039531 FALSE      SenTedCruz 0.687945974 0.53918685
## 12477 0.6637223 0.8039531 FALSE      SenTedCruz 0.584096261 0.77798093
## 12480 0.6637223 0.8039531 FALSE      SenTedCruz 0.760244989 0.54278492
## 12482 0.6637223 0.8039531 FALSE      SenTedCruz 0.697254988 0.97053308
## 12487 0.6637223 0.8039531 FALSE      SenTedCruz 0.383488164 0.46230352
## 12488 0.6637223 0.8039531 FALSE      SenTedCruz 0.728122348 0.58230863
## 12489 0.6637223 0.8039531 FALSE      SenTedCruz 0.531925684 0.60882052
## 12492 0.6637223 0.8039531 FALSE      SenTedCruz 0.429442774 0.57301434
## 12494 0.6637223 0.8039531 FALSE      SenTedCruz 0.434639764 0.85044149
## 12495 0.6637223 0.8039531 FALSE      SenTedCruz 0.567620988 0.69850636
## 12496 0.6637223 0.8039531 FALSE      SenTedCruz 0.442795596 0.59218637
## 12497 0.6637223 0.8039531 FALSE      SenTedCruz 0.291882440 0.77754758
## 12498 0.6637223 0.8039531 FALSE      SenTedCruz 0.782394778 0.40441868
## 12499 0.6637223 0.8039531 FALSE      SenTedCruz 0.586762845 0.39978789
## 12502 0.6637223 0.8039531 FALSE      SenTedCruz 0.373624649 0.80140890
## 12503 0.6637223 0.8039531 FALSE      SenTedCruz 0.538488367 0.72240224
## 12505 0.6637223 0.8039531 FALSE      SenTedCruz 0.844414952 0.58072224
## 12508 0.6637223 0.8039531 FALSE      SenTedCruz 0.726541873 0.73928514
## 12514 0.6637223 0.8039531 FALSE      SenTedCruz 0.245437484 0.70429534
## 12515 0.6637223 0.8039531 FALSE      SenTedCruz 0.271305380 0.51355747
## 12521 0.6637223 0.8039531 FALSE      SenTedCruz 0.796801148 0.76738319
## 12522 0.6637223 0.8039531 FALSE      SenTedCruz 0.598913271 0.73791606
## 12535 0.6637223 0.8039531 FALSE      SenTedCruz 0.493999255 0.45167044
## 12537 0.6637223 0.8039531 FALSE      SenTedCruz 0.830971000 0.63046043
## 12539 0.6637223 0.8039531 FALSE      SenTedCruz 0.696377423 0.75022227
## 12544 0.6637223 0.8039531 FALSE      SenTedCruz 0.283870727 0.64203775
## 12545 0.6637223 0.8039531 FALSE      SenTedCruz 0.299120073 0.66942503
## 12549 0.6637223 0.8039531 FALSE      SenTedCruz 0.581063781 0.58784744
## 12554 0.6637223 0.8039531 FALSE      SenTedCruz 0.809677133 0.67421382
## 12562 0.6669221 0.6067823 FALSE   SenatorWicker 0.567628670 0.84284652
## 12563 0.6669221 0.6067823 FALSE   SenatorWicker 0.466774053 0.45531444
## 12564 0.6669221 0.6067823 FALSE   SenatorWicker 0.771288560 0.66485245
## 12565 0.6669221 0.6067823 FALSE   SenatorWicker 0.546743898 0.64424848
## 12566 0.6669221 0.6067823 FALSE   SenatorWicker 0.823334062 0.61167948
## 12569 0.6669221 0.6067823 FALSE   SenatorWicker 0.501123769 0.44652290
## 12570 0.6669221 0.6067823 FALSE   SenatorWicker 0.436148298 0.56039111
## 12571 0.6669221 0.6067823 FALSE   SenatorWicker 0.380992800 0.54215995
## 12572 0.6669221 0.6067823 FALSE   SenatorWicker 0.449503503 0.77080639
## 12573 0.6669221 0.6067823 FALSE   SenatorWicker 0.415240936 0.69763521
## 12575 0.6669221 0.6067823 FALSE   SenatorWicker 0.300431028 0.65735369
## 12577 0.6669221 0.6067823 FALSE   SenatorWicker 0.729563239 0.50451210
## 12579 0.6669221 0.6067823 FALSE   SenatorWicker 0.600271809 0.60329950
## 12581 0.6669221 0.6067823 FALSE   SenatorWicker 0.439206280 0.64960788
## 12584 0.6669221 0.6067823 FALSE   SenatorWicker 0.501295800 0.53830648
## 12585 0.6669221 0.6067823 FALSE   SenatorWicker 0.573985169 0.74928809
## 12588 0.6669221 0.6067823 FALSE   SenatorWicker 0.849511944 0.49779773
## 12589 0.6669221 0.6067823 FALSE   SenatorWicker 0.654847926 0.82495049
## 12591 0.6669221 0.6067823 FALSE   SenatorWicker 0.804515967 0.65009765
## 12592 0.6669221 0.6067823 FALSE   SenatorWicker 0.384576523 0.75691677
## 12593 0.6669221 0.6067823 FALSE   SenatorWicker 0.556520935 0.43122480
## 12594 0.6669221 0.6067823 FALSE   SenatorWicker 0.835809312 0.56701967
## 12599 0.6669221 0.6067823 FALSE   SenatorWicker 0.413485411 0.73208692
## 12600 0.6669221 0.6067823 FALSE   SenatorWicker 0.285823925 0.63067069
## 12603 0.6669221 0.6067823 FALSE   SenatorWicker 0.329959785 0.62092877
## 12605 0.6669221 0.6067823 FALSE   SenatorWicker 0.301715883 0.72779721
## 12609 0.6669221 0.6067823 FALSE   SenatorWicker 0.466530726 0.65334274
## 12610 0.6669221 0.6067823 FALSE   SenatorWicker 0.803229337 0.74308162
## 12613 0.6669221 0.6067823 FALSE   SenatorWicker 0.714723919 0.57278962
## 12615 0.6669221 0.6067823 FALSE   SenatorWicker 0.650211589 0.75295373
## 12618 0.6669221 0.6067823 FALSE   SenatorWicker 0.545331729 0.61881905
## 12621 0.6669221 0.6067823 FALSE   SenatorWicker 0.650716626 0.44106430
## 12622 0.6669221 0.6067823 FALSE   SenatorWicker 0.594708584 0.65014103
## 12629 0.6669221 0.6067823 FALSE   SenatorWicker 0.753008149 0.61409311
## 12630 0.6669221 0.6067823 FALSE   SenatorWicker 0.666923968 0.48406105
## 12635 0.6669221 0.6067823 FALSE   SenatorWicker 0.683661925 0.61431682
## 12639 0.6669221 0.6067823 FALSE   SenatorWicker 0.589758117 0.50568637
## 12645 0.6669221 0.6067823 FALSE   SenatorWicker 0.542738689 0.59121327
## 12650 0.6669221 0.6067823 FALSE   SenatorWicker 0.681370241 0.58846011
## 12651 0.6669221 0.6067823 FALSE   SenatorWicker 0.408766610 0.49091445
## 12668 0.6669221 0.6067823 FALSE   SenatorWicker 0.692794354 0.50822576
## 12669 0.6669221 0.6067823 FALSE   SenatorWicker 0.545543479 0.48908115
## 12672 0.6669221 0.6067823 FALSE   SenatorWicker 0.730011419 0.70060702
## 12675 0.6669221 0.6067823 FALSE   SenatorWicker 0.565167802 0.53543817
## 12676 0.6669221 0.6067823 FALSE   SenatorWicker 0.369960850 0.78815862
## 12683 0.6669221 0.6067823 FALSE   SenatorWicker 0.326191285 0.47211095
## 12702 0.6669221 0.6067823 FALSE   SenatorWicker 0.298392123 0.58551697
## 12703 0.6669221 0.6067823 FALSE   SenatorWicker 0.500440550 0.80670893
## 12709 0.6669221 0.6067823 FALSE   SenatorWicker 0.571926996 0.66669278
## 12712 0.6669221 0.6067823 FALSE   SenatorWicker 0.590686832 0.39870326
## 12713 0.6669221 0.6067823 FALSE   SenatorWicker 0.646837870 0.59045459
## 12714 0.6669221 0.6067823 FALSE   SenatorWicker 0.592619382 0.48770277
## 12720 0.6669221 0.6067823 FALSE   SenatorWicker 0.549084950 0.97618246
## 12724 0.6669221 0.6067823 FALSE   SenatorWicker 0.749932674 0.53560791
## 12727 0.6669221 0.6067823 FALSE   SenatorWicker 0.742634466 0.75400610
## 12729 0.6669221 0.6067823 FALSE   SenatorWicker 0.777606281 0.40243595
## 12734 0.6669221 0.6067823 FALSE   SenatorWicker 0.596464382 0.70011998
## 12738 0.6669221 0.6067823 FALSE   SenatorWicker 0.834368907 0.69315315
## 12739 0.6669221 0.6067823 FALSE   SenatorWicker 0.510628697 0.73757934
## 12759 0.6669221 0.6067823 FALSE   SenatorWicker 0.684116238 0.53853332
## 12767 0.6669221 0.6067823 FALSE   SenatorWicker 0.736613492 0.66792368
## 12774 0.6669221 0.6067823 FALSE   SenatorWicker 0.389563542 0.45497699
## 12791 0.6669221 0.6067823 FALSE   SenatorWicker 0.792948697 0.55818576
## 12804 0.6669221 0.6067823 FALSE   SenatorWicker 0.232205716 0.53379444
## 12826 0.6669221 0.6067823 FALSE   SenatorWicker 0.664127948 0.77895644
## 12828 0.6669221 0.6067823 FALSE   SenatorWicker 0.626333299 0.79814280
## 12859 0.6669221 0.6067823 FALSE   SenatorWicker 0.640819509 0.69056380
## 12879 0.6669221 0.6067823 FALSE   SenatorWicker 0.594963819 0.57468212
## 12880 0.6669221 0.6067823 FALSE   SenatorWicker 0.493862053 0.39500067
## 12899 0.6669244 0.4590610 FALSE  Sen_JoeManchin 0.598602240 0.47885644
## 12900 0.6669244 0.4590610 FALSE  Sen_JoeManchin 0.654326757 0.82492710
## 12903 0.6669244 0.4590610 FALSE  Sen_JoeManchin 0.846178083 0.48183386
## 12906 0.6669244 0.4590610 FALSE  Sen_JoeManchin 0.720942211 0.53769515
## 12907 0.6669244 0.4590610 FALSE  Sen_JoeManchin 0.690274625 0.54470966
## 12910 0.6669244 0.4590610 FALSE  Sen_JoeManchin 0.447301020 0.56410025
## 12911 0.6669244 0.4590610 FALSE  Sen_JoeManchin 0.566455498 0.41926773
## 12912 0.6669244 0.4590610 FALSE  Sen_JoeManchin 0.530373646 0.68730629
## 12914 0.6669244 0.4590610 FALSE  Sen_JoeManchin 0.530123736 0.50157994
## 12915 0.6669244 0.4590610 FALSE  Sen_JoeManchin 0.471748077 0.44235958
## 12917 0.6669244 0.4590610 FALSE  Sen_JoeManchin 0.588796919 0.58094756
## 12918 0.6669244 0.4590610 FALSE  Sen_JoeManchin 0.297260034 0.57650692
## 12920 0.6669244 0.4590610 FALSE  Sen_JoeManchin 0.829232524 0.59631877
## 12923 0.6669244 0.4590610 FALSE  Sen_JoeManchin 0.566991752 0.50977276
## 12924 0.6669244 0.4590610 FALSE  Sen_JoeManchin 0.679386244 0.46872492
## 12925 0.6669244 0.4590610 FALSE  Sen_JoeManchin 0.794870220 0.53627403
## 12927 0.6669244 0.4590610 FALSE  Sen_JoeManchin 0.392597384 0.44431590
## 12928 0.6669244 0.4590610 FALSE  Sen_JoeManchin 0.666922443 0.58178226
## 12938 0.6669244 0.4590610 FALSE  Sen_JoeManchin 0.747444606 0.75213177
## 12939 0.6669244 0.4590610 FALSE  Sen_JoeManchin 0.447008524 0.66986146
## 12941 0.6669244 0.4590610 FALSE  Sen_JoeManchin 0.537207278 0.60272641
## 12943 0.6669244 0.4590610 FALSE  Sen_JoeManchin 0.700651105 0.60026179
## 12947 0.6669244 0.4590610 FALSE  Sen_JoeManchin 0.635518702 0.55102587
## 12958 0.6669244 0.4590610 FALSE  Sen_JoeManchin 0.410329173 0.68942258
## 12959 0.6669244 0.4590610 FALSE  Sen_JoeManchin 0.507823659 0.43316440
## 12961 0.6669244 0.4590610 FALSE  Sen_JoeManchin 0.501644548 0.52009450
## 12962 0.6669244 0.4590610 FALSE  Sen_JoeManchin 0.552494150 0.46942289
## 12964 0.6669244 0.4590610 FALSE  Sen_JoeManchin 0.482272538 0.58126258
## 12967 0.6669244 0.4590610 FALSE  Sen_JoeManchin 0.624269492 0.79779461
## 12972 0.6669244 0.4590610 FALSE  Sen_JoeManchin 0.283868882 0.62242733
## 12975 0.6669244 0.4590610 FALSE  Sen_JoeManchin 0.658250854 0.43911008
## 12976 0.6669244 0.4590610 FALSE  Sen_JoeManchin 0.297886529 0.64931526
## 12981 0.6669244 0.4590610 FALSE  Sen_JoeManchin 0.286544821 0.76025716
## 12987 0.6669244 0.4590610 FALSE  Sen_JoeManchin 0.648902788 0.75283886
## 12989 0.6669244 0.4590610 FALSE  Sen_JoeManchin 0.537848655 0.63166806
## 12993 0.6669244 0.4590610 FALSE  Sen_JoeManchin 0.423732452 0.83443199
## 12995 0.6669244 0.4590610 FALSE  Sen_JoeManchin 0.327778831 0.61171608
## 13006 0.6669244 0.4590610 FALSE  Sen_JoeManchin 0.583707518 0.64029078
## 13011 0.6669244 0.4590610 FALSE  Sen_JoeManchin 0.588846833 0.54590604
## 13014 0.6669244 0.4590610 FALSE  Sen_JoeManchin 0.327940016 0.46265647
## 13018 0.6669244 0.4590610 FALSE  Sen_JoeManchin 0.400181892 0.63791459
## 13019 0.6669244 0.4590610 FALSE  Sen_JoeManchin 0.736916309 0.69736625
## 13020 0.6669244 0.4590610 FALSE  Sen_JoeManchin 0.705476859 0.70416201
## 13033 0.6726420 0.6722629 FALSE       JeffFlake 0.598167594 0.66532803
## 13038 0.6726420 0.6722629 FALSE       JeffFlake 0.589704909 0.48926365
## 13041 0.6726420 0.6722629 FALSE       JeffFlake 0.434457964 0.56567571
## 13045 0.6726420 0.6722629 FALSE       JeffFlake 0.699909623 0.97014553
## 13053 0.6726420 0.6722629 FALSE       JeffFlake 0.430642006 0.84110846
## 13054 0.6726420 0.6722629 FALSE       JeffFlake 0.653193969 0.75347929
## 13056 0.6726420 0.6722629 FALSE       JeffFlake 0.595575852 0.61662803
## 13066 0.6726420 0.6722629 FALSE       JeffFlake 0.300558192 0.54900605
## 13069 0.6726420 0.6722629 FALSE       JeffFlake 0.803472289 0.65994755
## 13071 0.6726420 0.6722629 FALSE       JeffFlake 0.542245145 0.49193586
## 13073 0.6726420 0.6722629 FALSE       JeffFlake 0.453914183 0.68563553
## 13076 0.6726420 0.6722629 FALSE       JeffFlake 0.979207261 0.87691172
## 13077 0.6726420 0.6722629 FALSE       JeffFlake 0.579168704 0.75379519
## 13079 0.6726420 0.6722629 FALSE       JeffFlake 0.464274542 0.45814472
## 13083 0.6726420 0.6722629 FALSE       JeffFlake 0.838629642 0.57402355
## 13085 0.6726420 0.6722629 FALSE       JeffFlake 0.586226142 0.50794052
## 13092 0.6726420 0.6722629 FALSE       JeffFlake 0.484905152 0.60364221
## 13094 0.6726420 0.6722629 FALSE       JeffFlake 0.629247437 0.79894778
## 13095 0.6726420 0.6722629 FALSE       JeffFlake 0.667594572 0.48405206
## 13098 0.6726420 0.6722629 FALSE       JeffFlake 0.415318098 0.73706224
## 13103 0.6726420 0.6722629 FALSE       JeffFlake 0.691153133 0.59317154
## 13105 0.6726420 0.6722629 FALSE       JeffFlake 0.695656600 0.50880089
## 13106 0.6726420 0.6722629 FALSE       JeffFlake 0.439177402 0.53790626
## 13117 0.6726420 0.6722629 FALSE       JeffFlake 0.439576148 0.65597176
## 13121 0.6726420 0.6722629 FALSE       JeffFlake 0.484880091 0.77962185
## 13122 0.6726420 0.6722629 FALSE       JeffFlake 0.554278175 0.43247917
## 13127 0.6726420 0.6722629 FALSE       JeffFlake 0.831965195 0.70028331
## 13133 0.6726420 0.6722629 FALSE       JeffFlake 0.730671347 0.68078037
## 13135 0.6726420 0.6722629 FALSE       JeffFlake 0.498517331 0.44886594
## 13136 0.6726420 0.6722629 FALSE       JeffFlake 0.416546477 0.70313192
## 13149 0.6726420 0.6722629 FALSE       JeffFlake 0.691997283 0.64497028
## 13151 0.6726420 0.6722629 FALSE       JeffFlake 0.723082754 0.58022497
## 13160 0.6726420 0.6722629 FALSE       JeffFlake 0.576527608 0.80342910
## 13161 0.6726420 0.6722629 FALSE       JeffFlake 0.371854295 0.79194691
## 13167 0.6726420 0.6722629 FALSE       JeffFlake 0.386376642 0.76123420
## 13181 0.6726420 0.6722629 FALSE       JeffFlake 0.544158641 0.62922282
## 13191 0.6726420 0.6722629 FALSE       JeffFlake 0.589183975 0.58277968
## 13192 0.6726420 0.6722629 FALSE       JeffFlake 0.687458401 0.53913742
## 13193 0.6726420 0.6722629 FALSE       JeffFlake 0.379581956 0.54650723
## 13197 0.6726420 0.6722629 FALSE       JeffFlake 0.733943380 0.50663891
## 13229 0.6726420 0.6722629 FALSE       JeffFlake 0.302668339 0.73169559
## 13248 0.6726420 0.6722629 FALSE       JeffFlake 0.498307513 0.54360010
## 13267 0.6726420 0.6722629 FALSE       JeffFlake 0.738653842 0.75655577
## 13276 0.6726420 0.6722629 FALSE       JeffFlake 0.386670247 0.30973201
## 13279 0.6726420 0.6722629 FALSE       JeffFlake 0.541874014 0.70303372
## 13284 0.6902237 0.5142908 FALSE  SenCoryGardner 0.479000096 0.77267549
## 13285 0.6902237 0.5142908 FALSE  SenCoryGardner 0.540912778 0.57825834
## 13286 0.6902237 0.5142908 FALSE  SenCoryGardner 0.276193730 0.49957421
## 13288 0.6902237 0.5142908 FALSE  SenCoryGardner 0.593089229 0.69797314
## 13289 0.6902237 0.5142908 FALSE  SenCoryGardner 0.327724364 0.46620883
## 13291 0.6902237 0.5142908 FALSE  SenCoryGardner 0.746461526 0.66106590
## 13292 0.6902237 0.5142908 FALSE  SenCoryGardner 0.381551879 0.53497707
## 13293 0.6902237 0.5142908 FALSE  SenCoryGardner 0.436927870 0.64290965
## 13295 0.6902237 0.5142908 FALSE  SenCoryGardner 0.299237494 0.65244214
## 13297 0.6902237 0.5142908 FALSE  SenCoryGardner 0.446858108 0.76782702
## 13298 0.6902237 0.5142908 FALSE  SenCoryGardner 0.571643016 0.74793530
## 13301 0.6902237 0.5142908 FALSE  SenCoryGardner 0.534138281 0.69006638
## 13302 0.6902237 0.5142908 FALSE  SenCoryGardner 0.563606589 0.42452095
## 13305 0.6902237 0.5142908 FALSE  SenCoryGardner 0.675407372 0.64741632
## 13306 0.6902237 0.5142908 FALSE  SenCoryGardner 0.426003477 0.83609189
## 13307 0.6902237 0.5142908 FALSE  SenCoryGardner 0.717249222 0.54079634
## 13308 0.6902237 0.5142908 FALSE  SenCoryGardner 0.645456790 0.55735308
## 13309 0.6902237 0.5142908 FALSE  SenCoryGardner 0.840134152 0.68579095
## 13310 0.6902237 0.5142908 FALSE  SenCoryGardner 0.507429023 0.73439166
## 13313 0.6902237 0.5142908 FALSE  SenCoryGardner 0.702818115 0.59984433
## 13314 0.6902237 0.5142908 FALSE  SenCoryGardner 0.412757472 0.69260766
## 13315 0.6902237 0.5142908 FALSE  SenCoryGardner 0.503134497 0.52705329
## 13316 0.6902237 0.5142908 FALSE  SenCoryGardner 0.693834939 0.54401188
## 13322 0.6902237 0.5142908 FALSE  SenCoryGardner 0.284979861 0.62561245
## 13323 0.6902237 0.5142908 FALSE  SenCoryGardner 0.506270198 0.43865499
## 13324 0.6902237 0.5142908 FALSE  SenCoryGardner 0.811012573 0.63960492
## 13328 0.6902237 0.5142908 FALSE  SenCoryGardner 0.836048690 0.55462566
## 13330 0.6902237 0.5142908 FALSE  SenCoryGardner 0.737681343 0.69715469
## 13336 0.6902237 0.5142908 FALSE  SenCoryGardner 0.748147926 0.75194948
## 13337 0.6902237 0.5142908 FALSE  SenCoryGardner 0.809196278 0.73867152
## 13339 0.6902237 0.5142908 FALSE  SenCoryGardner 0.721122359 0.49595270
## 13341 0.6902237 0.5142908 FALSE  SenCoryGardner 0.566875966 0.66089890
## 13350 0.6902237 0.5142908 FALSE  SenCoryGardner 0.666000049 0.77905713
## 13352 0.6902237 0.5142908 FALSE  SenCoryGardner 0.640213137 0.69038327
## 13354 0.6902237 0.5142908 FALSE  SenCoryGardner 0.779771566 0.65587885
## 13357 0.6902237 0.5142908 FALSE  SenCoryGardner 0.651384948 0.75311612
## 13361 0.6902237 0.5142908 FALSE  SenCoryGardner 0.658110500 0.43917059
## 13368 0.6902237 0.5142908 FALSE  SenCoryGardner 0.595139566 0.55471580
## 13371 0.6902237 0.5142908 FALSE  SenCoryGardner 0.595179350 0.58682769
## 13374 0.6902237 0.5142908 FALSE  SenCoryGardner 0.442488485 0.52441562
## 13382 0.6902237 0.5142908 FALSE  SenCoryGardner 0.602341449 0.47639271
## 13385 0.6902237 0.5142908 FALSE  SenCoryGardner 0.626611888 0.79820355
## 13391 0.6902237 0.5142908 FALSE  SenCoryGardner 0.792181048 0.54252028
## 13402 0.6902237 0.5142908 FALSE  SenCoryGardner 0.597433059 0.39496446
## 13413 0.6902237 0.5142908 FALSE  SenCoryGardner 0.656188186 0.82506089
## 13423 0.6902237 0.5142908 FALSE  SenCoryGardner 0.692071483 0.50802435
## 13427 0.6902237 0.5142908 FALSE  SenCoryGardner 0.588728790 0.64335842
## 13430 0.6902237 0.5142908 FALSE  SenCoryGardner 0.673029546 0.58253977
## 13435 0.6968503 0.5688294 FALSE     SenatorBurr 0.560597327 0.42802715
## 13436 0.6968503 0.5688294 FALSE     SenatorBurr 0.412792985 0.73078462
## 13437 0.6968503 0.5688294 FALSE     SenatorBurr 0.628292795 0.79864202
## 13438 0.6968503 0.5688294 FALSE     SenatorBurr 0.449741550 0.57433040
## 13439 0.6968503 0.5688294 FALSE     SenatorBurr 0.673500079 0.48318075
## 13440 0.6968503 0.5688294 FALSE     SenatorBurr 0.652350935 0.57258302
## 13442 0.6968503 0.5688294 FALSE     SenatorBurr 0.298417446 0.58317711
## 13446 0.6968503 0.5688294 FALSE     SenatorBurr 0.403604259 0.64551253
## 13447 0.6968503 0.5688294 FALSE     SenatorBurr 0.502782879 0.53326156
## 13450 0.6968503 0.5688294 FALSE     SenatorBurr 0.593158876 0.64785615
## 13451 0.6968503 0.5688294 FALSE     SenatorBurr 0.702212754 0.59994104
## 13453 0.6968503 0.5688294 FALSE     SenatorBurr 0.568551228 0.52857077
## 13458 0.6968503 0.5688294 FALSE     SenatorBurr 0.448954122 0.77010455
## 13460 0.6968503 0.5688294 FALSE     SenatorBurr 0.481255402 0.77476313
## 13462 0.6968503 0.5688294 FALSE     SenatorBurr 0.667210247 0.77919766
## 13463 0.6968503 0.5688294 FALSE     SenatorBurr 0.438565432 0.64698767
## 13470 0.6968503 0.5688294 FALSE     SenatorBurr 0.698466577 0.50903593
## 13476 0.6968503 0.5688294 FALSE     SenatorBurr 0.599424106 0.59541384
## 13479 0.6968503 0.5688294 FALSE     SenatorBurr 0.285612633 0.62863674
## 13483 0.6968503 0.5688294 FALSE     SenatorBurr 0.706552907 0.78018459
## 13486 0.6968503 0.5688294 FALSE     SenatorBurr 0.598234868 0.48291511
## 13488 0.6968503 0.5688294 FALSE     SenatorBurr 0.778478280 0.40288636
## 13489 0.6968503 0.5688294 FALSE     SenatorBurr 0.776513382 0.65833327
## 13493 0.6968503 0.5688294 FALSE     SenatorBurr 0.597117462 0.56536509
## 13494 0.6968503 0.5688294 FALSE     SenatorBurr 0.594834642 0.39673460
## 13497 0.6968503 0.5688294 FALSE     SenatorBurr 0.275905467 0.50257326
## 13502 0.6968503 0.5688294 FALSE     SenatorBurr 0.245238235 0.69192671
## 13512 0.6968503 0.5688294 FALSE     SenatorBurr 0.574356195 0.74953479
## 13517 0.6968503 0.5688294 FALSE     SenatorBurr 0.595272533 0.49985740
## 13518 0.6968503 0.5688294 FALSE     SenatorBurr 0.710994215 0.56493616
## 13525 0.6968503 0.5688294 FALSE     SenatorBurr 0.730836756 0.50524162
## 13528 0.6968503 0.5688294 FALSE     SenatorBurr 0.329754978 0.61861749
## 13531 0.6968503 0.5688294 FALSE     SenatorBurr 0.486270777 0.59229140
## 13553 0.6968503 0.5688294 FALSE     SenatorBurr 0.707412854 0.70393442
## 13554 0.6968503 0.5688294 FALSE     SenatorBurr 0.469070433 0.45166348
## 13555 0.6968503 0.5688294 FALSE     SenatorBurr 0.301772268 0.54278887
## 13561 0.6968503 0.5688294 FALSE     SenatorBurr 0.643372726 0.69151477
## 13562 0.6968503 0.5688294 FALSE     SenatorBurr 0.299351136 0.47993452
## 13565 0.6968503 0.5688294 FALSE     SenatorBurr 0.545447590 0.64093921
## 13573 0.6968503 0.5688294 FALSE     SenatorBurr 0.369421065 0.78731344
## 13577 0.6968503 0.5688294 FALSE     SenatorBurr 0.982862105 0.87259054
## 13580 0.6968503 0.5688294 FALSE     SenatorBurr 0.747419490 0.75213868
## 13583 0.6968503 0.5688294 FALSE     SenatorBurr 0.465745793 0.65065656
## 13591 0.6968503 0.5688294 FALSE     SenatorBurr 0.682402157 0.58715151
## 13594 0.6968503 0.5688294 FALSE     SenatorBurr 0.678339254 0.64792073
## 13599 0.6968503 0.5688294 FALSE     SenatorBurr 0.837539284 0.68842189
## 13602 0.6968503 0.5688294 FALSE     SenatorBurr 0.824298644 0.60554179
## 13607 0.6968503 0.5688294 FALSE     SenatorBurr 0.744108025 0.66210962
## 13610 0.6991421 0.4840451 FALSE   senorrinhatch 0.604120779 0.47011840
## 13611 0.6991421 0.4840451 FALSE   senorrinhatch 0.593408544 0.58475273
## 13614 0.6991421 0.4840451 FALSE   senorrinhatch 0.813432560 0.63755207
## 13620 0.6991421 0.4840451 FALSE   senorrinhatch 0.565798874 0.42077822
## 13625 0.6991421 0.4840451 FALSE   senorrinhatch 0.717625945 0.48368296
## 13628 0.6991421 0.4840451 FALSE   senorrinhatch 0.702039503 0.97004189
## 13629 0.6991421 0.4840451 FALSE   senorrinhatch 0.593252046 0.55111940
## 13630 0.6991421 0.4840451 FALSE   senorrinhatch 0.599587297 0.48545874
## 13631 0.6991421 0.4840451 FALSE   senorrinhatch 0.705159236 0.59961159
## 13632 0.6991421 0.4840451 FALSE   senorrinhatch 0.436091588 0.64139496
## 13634 0.6991421 0.4840451 FALSE   senorrinhatch 0.531043647 0.50579244
## 13635 0.6991421 0.4840451 FALSE   senorrinhatch 0.401554289 0.64021988
## 13640 0.6991421 0.4840451 FALSE   senorrinhatch 0.507378051 0.43530625
## 13641 0.6991421 0.4840451 FALSE   senorrinhatch 0.381594465 0.75251345
## 13643 0.6991421 0.4840451 FALSE   senorrinhatch 0.640244850 0.69039230
## 13646 0.6991421 0.4840451 FALSE   senorrinhatch 0.435901228 0.54943697
## 13653 0.6991421 0.4840451 FALSE   senorrinhatch 0.587657145 0.64256111
## 13655 0.6991421 0.4840451 FALSE   senorrinhatch 0.600396838 0.39225062
## 13656 0.6991421 0.4840451 FALSE   senorrinhatch 0.500490104 0.38664772
## 13658 0.6991421 0.4840451 FALSE   senorrinhatch 0.484076183 0.58448130
## 13659 0.6991421 0.4840451 FALSE   senorrinhatch 0.392705898 0.30282922
## 13660 0.6991421 0.4840451 FALSE   senorrinhatch 0.642949871 0.55507761
## 13668 0.6991421 0.4840451 FALSE   senorrinhatch 0.410206997 0.72707476
## 13669 0.6991421 0.4840451 FALSE   senorrinhatch 0.571239169 0.74773490
## 13670 0.6991421 0.4840451 FALSE   senorrinhatch 0.381318141 0.53285394
## 13678 0.6991421 0.4840451 FALSE   senorrinhatch 0.708318732 0.70388012
## 13690 0.6991421 0.4840451 FALSE   senorrinhatch 0.405641508 0.38634889
## 13691 0.6991421 0.4840451 FALSE   senorrinhatch 0.626758533 0.79823687
## 13692 0.6991421 0.4840451 FALSE   senorrinhatch 0.442243633 0.52180131
## 13697 0.6991421 0.4840451 FALSE   senorrinhatch 0.533252476 0.68931585
## 13703 0.6991421 0.4840451 FALSE   senorrinhatch 0.569008585 0.51525544
## 13704 0.6991421 0.4840451 FALSE   senorrinhatch 0.478381700 0.77218604
## 13705 0.6991421 0.4840451 FALSE   senorrinhatch 0.697525855 0.54383849
## 13709 0.6991421 0.4840451 FALSE   senorrinhatch 0.739321788 0.69678735
## 13710 0.6991421 0.4840451 FALSE   senorrinhatch 0.425493001 0.83568380
## 13718 0.6991421 0.4840451 FALSE   senorrinhatch 0.540991969 0.63446006
## 13722 0.6991421 0.4840451 FALSE   senorrinhatch 0.276196164 0.49786944
## 13728 0.6991421 0.4840451 FALSE   senorrinhatch 0.471470420 0.44450576
## 13731 0.6991421 0.4840451 FALSE   senorrinhatch 0.592561824 0.69770219
## 13732 0.6991421 0.4840451 FALSE   senorrinhatch 0.782186768 0.65453229
## 13743 0.6991421 0.4840451 FALSE   senorrinhatch 0.666473447 0.77910498
## 13744 0.6991421 0.4840451 FALSE   senorrinhatch 0.448480942 0.56702962
## 13745 0.6991421 0.4840451 FALSE   senorrinhatch 0.765118476 0.59473398
## 13754 0.6991421 0.4840451 FALSE   senorrinhatch 0.410957148 0.48094634
## 13755 0.6991421 0.4840451 FALSE   senorrinhatch 0.676127528 0.64750708
## 13769 0.6991421 0.4840451 FALSE   senorrinhatch 0.773078460 0.39929238
## 13809 0.7021885 0.9950414 FALSE        BenSasse 0.706170966 0.64957611
## 13810 0.7021885 0.9950414 FALSE        BenSasse 0.819606390 0.68102115
## 13811 0.7021885 0.9950414 FALSE        BenSasse 0.382754000 0.99628715
## 13812 0.7021885 0.9950414 FALSE        BenSasse 0.699291129 0.50904461
## 13814 0.7021885 0.9950414 FALSE        BenSasse 0.582323630 0.68631521
## 13817 0.7021885 0.9950414 FALSE        BenSasse 0.430758466 0.67333608
## 13822 0.7021885 0.9950414 FALSE        BenSasse 0.410505450 0.75889787
## 13823 0.7021885 0.9950414 FALSE        BenSasse 0.410027409 0.72315494
## 13826 0.7021885 0.9950414 FALSE        BenSasse 0.573669919 0.79137109
## 13830 0.7064591 0.6245778 FALSE      marcorubio 0.736426300 0.50741363
## 13831 0.7064591 0.6245778 FALSE      marcorubio 0.591811538 0.50393586
## 13832 0.7064591 0.6245778 FALSE      marcorubio 0.593245157 0.39760065
## 13833 0.7064591 0.6245778 FALSE      marcorubio 0.595047943 0.48597826
## 13834 0.7064591 0.6245778 FALSE      marcorubio 0.404331148 0.64976070
## 13835 0.7064591 0.6245778 FALSE      marcorubio 0.753088194 0.61911668
## 13837 0.7064591 0.6245778 FALSE      marcorubio 0.804823077 0.74162006
## 13838 0.7064591 0.6245778 FALSE      marcorubio 0.467222962 0.45470215
## 13839 0.7064591 0.6245778 FALSE      marcorubio 0.388885760 0.30776030
## 13840 0.7064591 0.6245778 FALSE      marcorubio 0.823412499 0.61458925
## 13841 0.7064591 0.6245778 FALSE      marcorubio 0.578035364 0.75257989
## 13844 0.7064591 0.6245778 FALSE      marcorubio 0.318933696 0.69284034
## 13845 0.7064591 0.6245778 FALSE      marcorubio 0.408765278 0.49091742
## 13846 0.7064591 0.6245778 FALSE      marcorubio 0.631037793 0.79963891
## 13847 0.7064591 0.6245778 FALSE      marcorubio 0.439508943 0.65170140
## 13851 0.7064591 0.6245778 FALSE      marcorubio 0.700441990 0.50901124
## 13854 0.7064591 0.6245778 FALSE      marcorubio 0.486245071 0.59804969
## 13855 0.7064591 0.6245778 FALSE      marcorubio 0.494945097 0.39406262
## 13859 0.7064591 0.6245778 FALSE      marcorubio 0.415927021 0.69985335
## 13861 0.7064591 0.6245778 FALSE      marcorubio 0.502961822 0.80912591
## 13867 0.7064591 0.6245778 FALSE      marcorubio 0.672732393 0.48337703
## 13873 0.7064591 0.6245778 FALSE      marcorubio 0.597295201 0.65607878
## 13881 0.7064591 0.6245778 FALSE      marcorubio 0.547608610 0.64803679
## 13882 0.7064591 0.6245778 FALSE      marcorubio 0.599943310 0.60623716
## 13886 0.7064591 0.6245778 FALSE      marcorubio 0.565756856 0.53455902
## 13888 0.7064591 0.6245778 FALSE      marcorubio 0.851916061 0.50115909
## 13889 0.7064591 0.6245778 FALSE      marcorubio 0.594953806 0.57470454
## 13892 0.7064591 0.6245778 FALSE      marcorubio 0.546598990 0.48792206
## 13895 0.7064591 0.6245778 FALSE      marcorubio 0.693864756 0.53902426
## 13896 0.7064591 0.6245778 FALSE      marcorubio 0.574331740 0.67164033
## 13898 0.7064591 0.6245778 FALSE      marcorubio 0.746580207 0.75238610
## 13899 0.7064591 0.6245778 FALSE      marcorubio 0.385561878 0.75899524
## 13901 0.7064591 0.6245778 FALSE      marcorubio 0.401840646 0.39355986
## 13902 0.7064591 0.6245778 FALSE      marcorubio 0.501961439 0.44561222
## 13906 0.7064591 0.6245778 FALSE      marcorubio 0.512891906 0.74075826
## 13909 0.7064591 0.6245778 FALSE      marcorubio 0.804232169 0.65106711
## 13913 0.7064591 0.6245778 FALSE      marcorubio 0.451148770 0.77328039
## 13917 0.7064591 0.6245778 FALSE      marcorubio 0.659189659 0.82557621
## 13919 0.7064591 0.6245778 FALSE      marcorubio 0.771743651 0.66406835
## 13921 0.7064591 0.6245778 FALSE      marcorubio 0.655005508 0.44026232
## 13922 0.7064591 0.6245778 FALSE      marcorubio 0.576453136 0.80337474
## 13923 0.7076994 0.8051583 FALSE     SenatorEnzi 0.833584672 0.63265631
## 13924 0.7076994 0.8051583 FALSE     SenatorEnzi 0.670139057 0.78811977
## 13925 0.7076994 0.8051583 FALSE     SenatorEnzi 0.645653471 0.81766050
## 13926 0.7076994 0.8051583 FALSE     SenatorEnzi 0.746219908 0.70766195
## 13927 0.7076994 0.8051583 FALSE     SenatorEnzi 0.509335569 0.82360541
## 13930 0.7076994 0.8051583 FALSE     SenatorEnzi 0.590452551 0.68117440
## 13934 0.7076994 0.8051583 FALSE     SenatorEnzi 0.584654685 0.77599423
## 13939 0.7076994 0.8051583 FALSE     SenatorEnzi 0.859621101 0.50725574
## 13942 0.7076994 0.8051583 FALSE     SenatorEnzi 0.806512134 0.57220627
## 13943 0.7076994 0.8051583 FALSE     SenatorEnzi 0.699808070 0.50903618
## 13945 0.7076994 0.8051583 FALSE     SenatorEnzi 0.812536252 0.67695757
## 13946 0.7076994 0.8051583 FALSE     SenatorEnzi 0.581103241 0.85649599
## 13947 0.7076994 0.8051583 FALSE     SenatorEnzi 0.835869011 0.71860479
## 13948 0.7076994 0.8051583 FALSE     SenatorEnzi 0.679018807 0.69643597
## 13951 0.7076994 0.8051583 FALSE     SenatorEnzi 0.494162986 0.54798863
## 13953 0.7076994 0.8051583 FALSE     SenatorEnzi 0.552820854 0.43314139
## 13964 0.7076994 0.8051583 FALSE     SenatorEnzi 0.435508677 0.54278654
## 13965 0.7076994 0.8051583 FALSE     SenatorEnzi 0.534387800 0.60692449
## 13966 0.7076994 0.8051583 FALSE     SenatorEnzi 0.732855462 0.78946854
## 13974 0.7076994 0.8051583 FALSE     SenatorEnzi 0.697996799 0.59380306
## 13976 0.7076994 0.8051583 FALSE     SenatorEnzi 0.379692423 0.98439657
## 13979 0.7076994 0.8051583 FALSE     SenatorEnzi 0.706630834 0.64957718
## 13982 0.7076994 0.8051583 FALSE     SenatorEnzi 0.454485954 0.78759284
## 13992 0.7076994 0.8051583 FALSE     SenatorEnzi 0.415609143 0.74797064
## 13993 0.7076994 0.8051583 FALSE     SenatorEnzi 0.976008387 0.88376330
## 13999 0.7076994 0.8051583 FALSE     SenatorEnzi 0.302664115 0.73965213
## 14003 0.7076994 0.8051583 FALSE     SenatorEnzi 0.488141371 0.79337130
## 14004 0.7076994 0.8051583 FALSE     SenatorEnzi 0.649225081 0.73377219
## 14006 0.7076994 0.8051583 FALSE     SenatorEnzi 0.672748723 0.83400021
## 14011 0.7076994 0.8051583 FALSE     SenatorEnzi 0.584208237 0.50888937
## 14015 0.7076994 0.8051583 FALSE     SenatorEnzi 0.570327414 0.69561544
## 14029 0.7076994 0.8051583 FALSE     SenatorEnzi 0.602136095 0.73404141
## 14042 0.7076994 0.8051583 FALSE     SenatorEnzi 0.295712702 0.59541890
## 14043 0.7076994 0.8051583 FALSE     SenatorEnzi 0.542110479 0.66766000
## 14052 0.7076994 0.8051583 FALSE     SenatorEnzi 0.430749313 0.57158180
## 14057 0.7076994 0.8051583 FALSE     SenatorEnzi 0.688712910 0.80463799
## 14068 0.7076994 0.8051583 FALSE     SenatorEnzi 0.691723055 0.53924583
## 14071 0.7076994 0.8051583 FALSE     SenatorEnzi 0.769209697 0.63964271
## 14082 0.7076994 0.8051583 FALSE     SenatorEnzi 0.702913793 0.97005197
## 14088 0.7076994 0.8051583 FALSE     SenatorEnzi 0.739925347 0.50804754
## 14093 0.7076994 0.8051583 FALSE     SenatorEnzi 0.588955016 0.62294011
## 14094 0.7076994 0.8051583 FALSE     SenatorEnzi 0.671955699 0.63127027
## 14103 0.7076994 0.8051583 FALSE     SenatorEnzi 0.669849471 0.48388933
## 14116 0.7076994 0.8051583 FALSE     SenatorEnzi 0.539837184 0.72006383
## 14117 0.7076994 0.8051583 FALSE     SenatorEnzi 0.538290774 0.63879832
## 14136 0.7076994 0.8051583 FALSE     SenatorEnzi 0.539475952 0.49367444
## 14143 0.7076994 0.8051583 FALSE     SenatorEnzi 0.846892287 0.58248925
## 14152 0.7093614 0.7288584 FALSE SenatorLankford 0.446736320 0.58678228
## 14153 0.7093614 0.7288584 FALSE SenatorLankford 0.843421595 0.57987429
## 14154 0.7093614 0.7288584 FALSE SenatorLankford 0.796871135 0.75388476
## 14155 0.7093614 0.7288584 FALSE SenatorLankford 0.739267086 0.50796729
## 14156 0.7093614 0.7288584 FALSE SenatorLankford 0.806913276 0.67044754
## 14157 0.7093614 0.7288584 FALSE SenatorLankford 0.771876952 0.69016495
## 14158 0.7093614 0.7288584 FALSE SenatorLankford 0.542025067 0.49209320
## 14159 0.7093614 0.7288584 FALSE SenatorLankford 0.676706277 0.78258925
## 14162 0.7093614 0.7288584 FALSE SenatorLankford 0.698798899 0.59375331
## 14163 0.7093614 0.7288584 FALSE SenatorLankford 0.686249114 0.69323546
## 14167 0.7093614 0.7288584 FALSE SenatorLankford 0.433164418 0.84567501
## 14168 0.7093614 0.7288584 FALSE SenatorLankford 0.497094511 0.54511666
## 14169 0.7093614 0.7288584 FALSE SenatorLankford 0.438872094 0.66036532
## 14170 0.7093614 0.7288584 FALSE SenatorLankford 0.454042720 0.78087221
## 14174 0.7093614 0.7288584 FALSE SenatorLankford 0.463660528 0.45872241
## 14176 0.7093614 0.7288584 FALSE SenatorLankford 0.554647514 0.97874423
## 14178 0.7093614 0.7288584 FALSE SenatorLankford 0.707154653 0.64956809
## 14179 0.7093614 0.7288584 FALSE SenatorLankford 0.216754217 0.63421440
## 14182 0.7093614 0.7288584 FALSE SenatorLankford 0.736910661 0.75805520
## 14183 0.7093614 0.7288584 FALSE SenatorLankford 0.273548765 0.50990924
## 14187 0.7093614 0.7288584 FALSE SenatorLankford 0.638279045 0.80439267
## 14189 0.7093614 0.7288584 FALSE SenatorLankford 0.545977128 0.66124830
## 14190 0.7093614 0.7288584 FALSE SenatorLankford 0.590482906 0.48889478
## 14195 0.7093614 0.7288584 FALSE SenatorLankford 0.593463919 0.61917879
## 14198 0.7093614 0.7288584 FALSE SenatorLankford 0.582790507 0.81009147
## 14200 0.7093614 0.7288584 FALSE SenatorLankford 0.584417760 0.76354155
## 14208 0.7093614 0.7288584 FALSE SenatorLankford 0.560225812 0.54067896
## 14210 0.7093614 0.7288584 FALSE SenatorLankford 0.719529592 0.72665272
## 14213 0.7093614 0.7288584 FALSE SenatorLankford 0.416662345 0.70790849
## 14214 0.7093614 0.7288584 FALSE SenatorLankford 0.762076816 0.54338275
## 14215 0.7093614 0.7288584 FALSE SenatorLankford 0.595779253 0.67389916
## 14225 0.7093614 0.7288584 FALSE SenatorLankford 0.702861978 0.97005052
## 14230 0.7093614 0.7288584 FALSE SenatorLankford 0.483425388 0.60693241
## 14232 0.7093614 0.7288584 FALSE SenatorLankford 0.542402557 0.71136516
## 14233 0.7093614 0.7288584 FALSE SenatorLankford 0.299759831 0.55109740
## 14236 0.7093614 0.7288584 FALSE SenatorLankford 0.829156785 0.62851492
## 14250 0.7093614 0.7288584 FALSE SenatorLankford 0.831919544 0.70867586
## 14262 0.7093614 0.7288584 FALSE SenatorLankford 0.433241847 0.56804583
## 14275 0.7093614 0.7288584 FALSE SenatorLankford 0.523214715 0.52736325
## 14276 0.7093614 0.7288584 FALSE SenatorLankford 0.737419389 0.70177383
## 14285 0.7093614 0.7288584 FALSE SenatorLankford 0.491739481 0.39655629
## 14286 0.7093614 0.7288584 FALSE SenatorLankford 0.507396835 0.81601475
## 14297 0.7093614 0.7288584 FALSE SenatorLankford 0.783908156 0.40481710
## 14298 0.7093614 0.7288584 FALSE SenatorLankford 0.803490286 0.57067516
## 14299 0.7093614 0.7288584 FALSE SenatorLankford 0.639170193 0.59676126
## 14301 0.7093614 0.7288584 FALSE SenatorLankford 0.663946610 0.82721501
## 14308 0.7093614 0.7288584 FALSE SenatorLankford 0.700184762 0.50902330
## 14310 0.7093614 0.7288584 FALSE SenatorLankford 0.586713480 0.50767730
## 14311 0.7093614 0.7288584 FALSE SenatorLankford 0.285312146 0.63749997
## 14326 0.7093614 0.7288584 FALSE SenatorLankford 0.857168391 0.50582384
## 14337 0.7350978 0.5583015 FALSE       SenToomey 0.298394767 0.58268318
## 14338 0.7350978 0.5583015 FALSE       SenToomey 0.469973578 0.44970418
## 14340 0.7350978 0.5583015 FALSE       SenToomey 0.410362715 0.48610330
## 14342 0.7350978 0.5583015 FALSE       SenToomey 0.562996152 0.42534555
## 14343 0.7350978 0.5583015 FALSE       SenToomey 0.289618266 0.76524419
## 14345 0.7350978 0.5583015 FALSE       SenToomey 0.681079909 0.47966737
## 14357 0.7350978 0.5583015 FALSE       SenToomey 0.594264535 0.64942884
## 14358 0.7350978 0.5583015 FALSE       SenToomey 0.232514030 0.53101048
## 14360 0.7350978 0.5583015 FALSE       SenToomey 0.503028743 0.53161116
## 14363 0.7350978 0.5583015 FALSE       SenToomey 0.486202020 0.59173171
## 14364 0.7350978 0.5583015 FALSE       SenToomey 0.825769216 0.60167388
## 14368 0.7350978 0.5583015 FALSE       SenToomey 0.505395670 0.44055236
## 14380 0.7350978 0.5583015 FALSE       SenToomey 0.449712120 0.57355269
## 14381 0.7350978 0.5583015 FALSE       SenToomey 0.300066160 0.65532885
## 14382 0.7350978 0.5583015 FALSE       SenToomey 0.647029545 0.69348521
## 14383 0.7350978 0.5583015 FALSE       SenToomey 0.216990999 0.62624675
## 14384 0.7350978 0.5583015 FALSE       SenToomey 0.684657050 0.65033942
## 14387 0.7350978 0.5583015 FALSE       SenToomey 0.631044013 0.79964159
## 14388 0.7350978 0.5583015 FALSE       SenToomey 0.538100509 0.69453996
## 14389 0.7350978 0.5583015 FALSE       SenToomey 0.835151095 0.56069297
## 14399 0.7350978 0.5583015 FALSE       SenToomey 0.848977127 0.49685594
## 14400 0.7350978 0.5583015 FALSE       SenToomey 0.442376544 0.52800983
## 14401 0.7350978 0.5583015 FALSE       SenToomey 0.751431554 0.65972893
## 14403 0.7350978 0.5583015 FALSE       SenToomey 0.544442015 0.61424318
## 14406 0.7350978 0.5583015 FALSE       SenToomey 0.438586995 0.64705932
## 14415 0.7350978 0.5583015 FALSE       SenToomey 0.318157280 0.68966475
## 14416 0.7350978 0.5583015 FALSE       SenToomey 0.710457152 0.78031087
## 14425 0.7350978 0.5583015 FALSE       SenToomey 0.811247347 0.63938154
## 14430 0.7350978 0.5583015 FALSE       SenToomey 0.716375676 0.60162864
## 14431 0.7350978 0.5583015 FALSE       SenToomey 0.984421747 0.87123903
## 14432 0.7350978 0.5583015 FALSE       SenToomey 0.751899944 0.75133264
## 14433 0.7350978 0.5583015 FALSE       SenToomey 0.576234397 0.75094098
## 14439 0.7350978 0.5583015 FALSE       SenToomey 0.740129434 0.50806883
## 14452 0.7350978 0.5583015 FALSE       SenToomey 0.720953892 0.56219468
## 14481 0.7350978 0.5583015 FALSE       SenToomey 0.381567022 0.53807662
## 14483 0.7350978 0.5583015 FALSE       SenToomey 0.545759305 0.64161976
## 14485 0.7350978 0.5583015 FALSE       SenToomey 0.840616643 0.68537987
## 14503 0.7350978 0.5583015 FALSE       SenToomey 0.811151601 0.73774091
## 14518 0.7350978 0.5583015 FALSE       SenToomey 0.710037285 0.50654605
## 14524 0.7350978 0.5583015 FALSE       SenToomey 0.413096806 0.73133338
## 14532 0.7350978 0.5583015 FALSE       SenToomey 0.600920528 0.47919040
## 14534 0.7350978 0.5583015 FALSE       SenToomey 0.782198657 0.40435955
## 14541 0.7350978 0.5583015 FALSE       SenToomey 0.599420600 0.59540100
## 14551 0.7350978 0.5583015 FALSE       SenToomey 0.465821349 0.65087310
## 14567 0.7350978 0.5583015 FALSE       SenToomey 0.656650399 0.75457746
## 14568 0.7350978 0.5583015 FALSE       SenToomey 0.597114482 0.56354744
## 14569 0.7350978 0.5583015 FALSE       SenToomey 0.845998569 0.41347792
## 14571 0.7350978 0.5583015 FALSE       SenToomey 0.597374066 0.49610341
## 14578 0.7350978 0.5583015 FALSE       SenToomey 0.414688680 0.69623826
## 14583 0.7350978 0.5583015 FALSE       SenToomey 0.670697706 0.77994599
## 14604 0.7350978 0.5583015 FALSE       SenToomey 0.598118956 0.39441142
## 14622 0.7350978 0.5583015 FALSE       SenToomey 0.570413264 0.84422739
## 14624 0.7426211 0.4831933 FALSE      JohnCornyn 0.573731366 0.74912474
## 14625 0.7426211 0.4831933 FALSE      JohnCornyn 0.447519739 0.76848630
## 14626 0.7426211 0.4831933 FALSE      JohnCornyn 0.681319811 0.64881731
## 14627 0.7426211 0.4831933 FALSE      JohnCornyn 0.368079710 0.78548812
## 14628 0.7426211 0.4831933 FALSE      JohnCornyn 0.569252038 0.51638503
## 14629 0.7426211 0.4831933 FALSE      JohnCornyn 0.595689999 0.58752125
## 14633 0.7426211 0.4831933 FALSE      JohnCornyn 0.531102592 0.50628668
## 14635 0.7426211 0.4831933 FALSE      JohnCornyn 0.669693672 0.77967677
## 14639 0.7426211 0.4831933 FALSE      JohnCornyn 0.724137291 0.48355540
## 14640 0.7426211 0.4831933 FALSE      JohnCornyn 0.594697933 0.55373606
## 14641 0.7439614 0.7213531 FALSE  sendavidperdue 0.658334368 0.71599385
## 14646 0.7439614 0.7213531 FALSE  sendavidperdue 0.852777418 0.41716829
## 14648 0.7439614 0.7213531 FALSE  sendavidperdue 0.416702802 0.70720359
## 14652 0.7439614 0.7213531 FALSE  sendavidperdue 0.786201960 0.40523326
## 14654 0.7439614 0.7213531 FALSE  sendavidperdue 0.583553176 0.81136102
## 14655 0.7439614 0.7213531 FALSE  sendavidperdue 0.575228013 0.68525842
## 14658 0.7439614 0.7213531 FALSE  sendavidperdue 0.584487577 0.76379836
## 14663 0.7439614 0.7213531 FALSE  sendavidperdue 0.681141825 0.78602106
## 14664 0.7439614 0.7213531 FALSE  sendavidperdue 0.749540530 0.75165181
## 14665 0.7439614 0.7213531 FALSE  sendavidperdue 0.297433105 0.59107862
## 14666 0.7439614 0.7213531 FALSE  sendavidperdue 0.808413179 0.67267226
## 14667 0.7439614 0.7213531 FALSE  sendavidperdue 0.439133331 0.65922150
## 14668 0.7439614 0.7213531 FALSE  sendavidperdue 0.668957275 0.76517932
## 14670 0.7439614 0.7213531 FALSE  sendavidperdue 0.696503794 0.53848919
## 14674 0.7439614 0.7213531 FALSE  sendavidperdue 0.765847224 0.54414666
## 14676 0.7439614 0.7213531 FALSE  sendavidperdue 0.516254843 0.75045463
## 14678 0.7439614 0.7213531 FALSE  sendavidperdue 0.403975030 0.65652023
## 14682 0.7439614 0.7213531 FALSE  sendavidperdue 0.231092646 0.53806813
## 14684 0.7439614 0.7213531 FALSE  sendavidperdue 0.733793221 0.72355870
## 14689 0.7439614 0.7213531 FALSE  sendavidperdue 0.642990536 0.59425891
## 14698 0.7439614 0.7213531 FALSE  sendavidperdue 0.640436313 0.80669635
## 14709 0.7439614 0.7213531 FALSE  sendavidperdue 0.703781704 0.50861076
## 14710 0.7439614 0.7213531 FALSE  sendavidperdue 0.742761836 0.50819291
## 14712 0.7439614 0.7213531 FALSE  sendavidperdue 0.300150973 0.55014264
## 14713 0.7439614 0.7213531 FALSE  sendavidperdue 0.387313103 0.76557764
## 14715 0.7439614 0.7213531 FALSE  sendavidperdue 0.539469074 0.60079949
## 14716 0.7439614 0.7213531 FALSE  sendavidperdue 0.806593191 0.57224049
## 14717 0.7439614 0.7213531 FALSE  sendavidperdue 0.606401513 0.72027007
## 14718 0.7439614 0.7213531 FALSE  sendavidperdue 0.592946971 0.48749481
## 14720 0.7439614 0.7213531 FALSE  sendavidperdue 0.715492552 0.64788865
## 14721 0.7439614 0.7213531 FALSE  sendavidperdue 0.770235314 0.63999871
## 14723 0.7439614 0.7213531 FALSE  sendavidperdue 0.438945348 0.53830108
## 14728 0.7439614 0.7213531 FALSE  sendavidperdue 0.845458412 0.58152237
## 14730 0.7439614 0.7213531 FALSE  sendavidperdue 0.667856702 0.82946918
## 14743 0.7439614 0.7213531 FALSE  sendavidperdue 0.590596348 0.58135212
## 14748 0.7439614 0.7213531 FALSE  sendavidperdue 0.717627199 0.78221406
## 14760 0.7439614 0.7213531 FALSE  sendavidperdue 0.524894392 0.52565527
## 14775 0.7439614 0.7213531 FALSE  sendavidperdue 0.453814788 0.68985892
## 14779 0.7439614 0.7213531 FALSE  sendavidperdue 0.433338320 0.84609687
## 14790 0.7439614 0.7213531 FALSE  sendavidperdue 0.693235258 0.68643752
## 14806 0.7439614 0.7213531 FALSE  sendavidperdue 0.379173683 0.54740897
## 14809 0.7439614 0.7213531 FALSE  sendavidperdue 0.487421057 0.78592855
## 14813 0.7439614 0.7213531 FALSE  sendavidperdue 0.589185132 0.50611092
## 14816 0.7439614 0.7213531 FALSE  sendavidperdue 0.302973017 0.73489350
## 14818 0.7439614 0.7213531 FALSE  sendavidperdue 0.543968413 0.49057044
## 14819 0.7439614 0.7213531 FALSE  sendavidperdue 0.673969454 0.48304785
## 14824 0.7439614 0.7213531 FALSE  sendavidperdue 0.704228338 0.59271587
## 14837 0.7439614 0.7213531 FALSE  sendavidperdue 0.466662279 0.66405922
## 14839 0.7439614 0.7213531 FALSE  sendavidperdue 0.498435770 0.54342473
## 14840 0.7439614 0.7213531 FALSE  sendavidperdue 0.774569047 0.69375049
## 14855 0.7439614 0.7213531 FALSE  sendavidperdue 0.484248126 0.60526178
## 14856 0.7439614 0.7213531 FALSE  sendavidperdue 0.596931488 0.67109614
## 14859 0.7439614 0.7213531 FALSE  sendavidperdue 0.595712542 0.61643680
## 14862 0.7540678 0.7762385 FALSE  SenatorFischer 0.673547596 0.48316774
## 14863 0.7540678 0.7762385 FALSE  SenatorFischer 0.416270834 0.71087320
## 14864 0.7540678 0.7762385 FALSE  SenatorFischer 0.849080412 0.58370905
## 14866 0.7540678 0.7762385 FALSE  SenatorFischer 0.809668517 0.57330258
## 14867 0.7540678 0.7762385 FALSE  SenatorFischer 0.605176160 0.72780650
## 14868 0.7540678 0.7762385 FALSE  SenatorFischer 0.580672920 0.85549773
## 14869 0.7540678 0.7762385 FALSE  SenatorFischer 0.836093480 0.71893157
## 14870 0.7540678 0.7762385 FALSE  SenatorFischer 0.655330650 0.44016918
## 14872 0.7540678 0.7762385 FALSE  SenatorFischer 0.588425163 0.58345903
## 14874 0.7540678 0.7762385 FALSE  SenatorFischer 0.544886166 0.66354623
## 14875 0.7540678 0.7762385 FALSE  SenatorFischer 0.516364362 0.75576884
## 14876 0.7540678 0.7762385 FALSE  SenatorFischer 0.541286905 0.63510079
## 14877 0.7540678 0.7762385 FALSE  SenatorFischer 0.743596936 0.50817426
## 14878 0.7540678 0.7762385 FALSE  SenatorFischer 0.594462956 0.67627972
## 14879 0.7540678 0.7762385 FALSE  SenatorFischer 0.560556531 0.54041225
## 14882 0.7540678 0.7762385 FALSE  SenatorFischer 0.774233219 0.64093549
## 14883 0.7540678 0.7762385 FALSE  SenatorFischer 0.542513732 0.49173891
## 14887 0.7540678 0.7762385 FALSE  SenatorFischer 0.591940276 0.39820528
## 14889 0.7540678 0.7762385 FALSE  SenatorFischer 0.482680541 0.60821952
## 14890 0.7540678 0.7762385 FALSE  SenatorFischer 0.696143649 0.53857981
## 14891 0.7540678 0.7762385 FALSE  SenatorFischer 0.587732335 0.50708045
## 14892 0.7540678 0.7762385 FALSE  SenatorFischer 0.247297347 0.86584708
## 14893 0.7540678 0.7762385 FALSE  SenatorFischer 0.591665649 0.48826888
## 14898 0.7540678 0.7762385 FALSE  SenatorFischer 0.755041961 0.70940828
## 14899 0.7540678 0.7762385 FALSE  SenatorFischer 0.726518590 0.74704163
## 14900 0.7540678 0.7762385 FALSE  SenatorFischer 0.585316630 0.77100362
## 14903 0.7540678 0.7762385 FALSE  SenatorFischer 0.672369410 0.77742799
## 14905 0.7540678 0.7762385 FALSE  SenatorFischer 0.454534903 0.78512165
## 14910 0.7540678 0.7762385 FALSE  SenatorFischer 0.703760631 0.50861473
## 14918 0.7540678 0.7762385 FALSE  SenatorFischer 0.688056043 0.69194561
## 14927 0.7540678 0.7762385 FALSE  SenatorFischer 0.415971837 0.74543544
## 14935 0.7540678 0.7762385 FALSE  SenatorFischer 0.678355430 0.62901462
## 14936 0.7540678 0.7762385 FALSE  SenatorFischer 0.216391910 0.63581475
## 14941 0.7540678 0.7762385 FALSE  SenatorFischer 0.284870864 0.63924166
## 14946 0.7540678 0.7762385 FALSE  SenatorFischer 0.405452551 0.49632956
## 14948 0.7540678 0.7762385 FALSE  SenatorFischer 0.463708914 0.45867828
## 14953 0.7540678 0.7762385 FALSE  SenatorFischer 0.815093251 0.67879265
## 14957 0.7540678 0.7762385 FALSE  SenatorFischer 0.737265662 0.58320730
## 14959 0.7540678 0.7762385 FALSE  SenatorFischer 0.453072103 0.69376749
## 14962 0.7540678 0.7762385 FALSE  SenatorFischer 0.687623001 0.79662129
## 14963 0.7540678 0.7762385 FALSE  SenatorFischer 0.465580721 0.66779719
## 14968 0.7540678 0.7762385 FALSE  SenatorFischer 0.593208346 0.61944489
## 14971 0.7540678 0.7762385 FALSE  SenatorFischer 0.640738974 0.59585326
## 14985 0.7540678 0.7762385 FALSE  SenatorFischer 0.387498428 0.76913824
## 14988 0.7540678 0.7762385 FALSE  SenatorFischer 0.537485775 0.60368167
## 14993 0.7540678 0.7762385 FALSE  SenatorFischer 0.655634860 0.72582797
## 14996 0.7540678 0.7762385 FALSE  SenatorFischer 0.446076892 0.58792779
## 15001 0.7540678 0.7762385 FALSE  SenatorFischer 0.796552151 0.76639934
## 15003 0.7540678 0.7762385 FALSE  SenatorFischer 0.541579479 0.71561854
## 15005 0.7540678 0.7762385 FALSE  SenatorFischer 0.767469479 0.54429354
## 15009 0.7540678 0.7762385 FALSE  SenatorFischer 0.835851846 0.63412978
## 15010 0.7540678 0.7762385 FALSE  SenatorFischer 0.586025810 0.81761720
## 15011 0.7554063 0.6844109 FALSE    SenJohnThune 0.598104462 0.66592760
## 15012 0.7554063 0.6844109 FALSE    SenJohnThune 0.705900855 0.50811410
## 15013 0.7554063 0.6844109 FALSE    SenJohnThune 0.440130731 0.53607946
## 15014 0.7554063 0.6844109 FALSE    SenJohnThune 0.806012760 0.57198800
## 15015 0.7554063 0.6844109 FALSE    SenJohnThune 0.804896160 0.66622679
## 15016 0.7554063 0.6844109 FALSE    SenJohnThune 0.754432205 0.75124112
## 15017 0.7554063 0.6844109 FALSE    SenJohnThune 0.832071229 0.69971854
## 15018 0.7554063 0.6844109 FALSE    SenJohnThune 0.858443492 0.50661484
## 15019 0.7554063 0.6844109 FALSE    SenJohnThune 0.547632933 0.65517287
## 15020 0.7554063 0.6844109 FALSE    SenJohnThune 0.697377017 0.67589348
## 15021 0.7554063 0.6844109 FALSE    SenJohnThune 0.246109507 0.69784211
## 15024 0.7554063 0.6844109 FALSE    SenJohnThune 0.439539511 0.65643418
## 15025 0.7554063 0.6844109 FALSE    SenJohnThune 0.766873061 0.54425192
## 15026 0.7554063 0.6844109 FALSE    SenJohnThune 0.722288684 0.64392784
## 15028 0.7554063 0.6844109 FALSE    SenJohnThune 0.828555220 0.62776810
## 15029 0.7554063 0.6844109 FALSE    SenJohnThune 0.407850537 0.49274994
## 15032 0.7554063 0.6844109 FALSE    SenJohnThune 0.297908929 0.58917190
## 15034 0.7554063 0.6844109 FALSE    SenJohnThune 0.515638979 0.74728280
## 15037 0.7554063 0.6844109 FALSE    SenJohnThune 0.302841984 0.73299241
## 15042 0.7554063 0.6844109 FALSE    SenJohnThune 0.494749451 0.39424024
## 15045 0.7554063 0.6844109 FALSE    SenJohnThune 0.605893209 0.71505382
## 15046 0.7554063 0.6844109 FALSE    SenJohnThune 0.598038704 0.61239773
## 15047 0.7554063 0.6844109 FALSE    SenJohnThune 0.699168528 0.53763586
## 15052 0.7554063 0.6844109 FALSE    SenJohnThune 0.499991138 0.54099474
## 15053 0.7554063 0.6844109 FALSE    SenJohnThune 0.545867978 0.48874015
## 15057 0.7554063 0.6844109 FALSE    SenJohnThune 0.300776288 0.54831420
## 15058 0.7554063 0.6844109 FALSE    SenJohnThune 0.582051613 0.80900417
## 15061 0.7554063 0.6844109 FALSE    SenJohnThune 0.594311117 0.39703645
## 15079 0.7554063 0.6844109 FALSE    SenJohnThune 0.744206429 0.50814300
## 15081 0.7554063 0.6844109 FALSE    SenJohnThune 0.804629115 0.74178476
## 15087 0.7554063 0.6844109 FALSE    SenJohnThune 0.583212245 0.76016164
## 15096 0.7554063 0.6844109 FALSE    SenJohnThune 0.416681700 0.70463377
## 15103 0.7554063 0.6844109 FALSE    SenJohnThune 0.666285818 0.76144356
## 15105 0.7554063 0.6844109 FALSE    SenJohnThune 0.843945103 0.58033234
## 15120 0.7689116 0.5193352 FALSE     JohnBoozman 0.688446694 0.47178068
## 15121 0.7689116 0.5193352 FALSE     JohnBoozman 0.403085487 0.64378569
## 15122 0.7689116 0.5193352 FALSE     JohnBoozman 0.436511767 0.55294879
## 15123 0.7689116 0.5193352 FALSE     JohnBoozman 0.717457785 0.55467544
## 15124 0.7689116 0.5193352 FALSE     JohnBoozman 0.785844754 0.40518290
## 15126 0.7689116 0.5193352 FALSE     JohnBoozman 0.832100673 0.59343884
## 15128 0.7689116 0.5193352 FALSE     JohnBoozman 0.837430588 0.55084512
## 15129 0.7689116 0.5193352 FALSE     JohnBoozman 0.300862206 0.72557989
## 15130 0.7689116 0.5193352 FALSE     JohnBoozman 0.603466559 0.47320725
## 15132 0.7689116 0.5193352 FALSE     JohnBoozman 0.369388852 0.78726529
## 15136 0.7689116 0.5193352 FALSE     JohnBoozman 0.598467720 0.70180386
## 15137 0.7689116 0.5193352 FALSE     JohnBoozman 0.575820943 0.80292984
## 15139 0.7689116 0.5193352 FALSE     JohnBoozman 0.442505570 0.52500250
## 15143 0.7689116 0.5193352 FALSE     JohnBoozman 0.543582056 0.61179177
## 15144 0.7689116 0.5193352 FALSE     JohnBoozman 0.667283840 0.43243066
## 15146 0.7689116 0.5193352 FALSE     JohnBoozman 0.570908993 0.66520092
## 15150 0.7689116 0.5193352 FALSE     JohnBoozman 0.818487288 0.63463756
## 15155 0.7689116 0.5193352 FALSE     JohnBoozman 0.544893337 0.63984543
## 15158 0.7689116 0.5193352 FALSE     JohnBoozman 0.465167246 0.64917428
## 15161 0.7689116 0.5193352 FALSE     JohnBoozman 0.719217265 0.60307823
## 15163 0.7689116 0.5193352 FALSE     JohnBoozman 0.672388862 0.78050340
## 15168 0.7689116 0.5193352 FALSE     JohnBoozman 0.593425167 0.64821207
## 15170 0.7689116 0.5193352 FALSE     JohnBoozman 0.500457023 0.38671494
## 15172 0.7689116 0.5193352 FALSE     JohnBoozman 0.815643906 0.73631917
## 15175 0.7689116 0.5193352 FALSE     JohnBoozman 0.449207138 0.77042139
## 15177 0.7689116 0.5193352 FALSE     JohnBoozman 0.650721004 0.56557574
## 15179 0.7689116 0.5193352 FALSE     JohnBoozman 0.716196150 0.70481079
## 15181 0.7689116 0.5193352 FALSE     JohnBoozman 0.569697223 0.52089073
## 15183 0.7689116 0.5193352 FALSE     JohnBoozman 0.552122297 0.47652118
## 15184 0.7689116 0.5193352 FALSE     JohnBoozman 0.542044165 0.58149685
## 15190 0.7689116 0.5193352 FALSE     JohnBoozman 0.658010735 0.75516854
## 15191 0.7689116 0.5193352 FALSE     JohnBoozman 0.327760197 0.46592635
## 15192 0.7689116 0.5193352 FALSE     JohnBoozman 0.847284617 0.49295882
## 15193 0.7689116 0.5193352 FALSE     JohnBoozman 0.601881859 0.39049816
## 15195 0.7689116 0.5193352 FALSE     JohnBoozman 0.503179350 0.52794514
## 15200 0.7689116 0.5193352 FALSE     JohnBoozman 0.757327425 0.50341027
## 15207 0.7689116 0.5193352 FALSE     JohnBoozman 0.599225944 0.49006356
## 15210 0.7689116 0.5193352 FALSE     JohnBoozman 0.596499034 0.55890495
## 15214 0.7689116 0.5193352 FALSE     JohnBoozman 0.410832182 0.48318843
## 15218 0.7689116 0.5193352 FALSE     JohnBoozman 0.775604058 0.59131597
## 15220 0.7689116 0.5193352 FALSE     JohnBoozman 0.298222918 0.58083770
## 15226 0.7689116 0.5193352 FALSE     JohnBoozman 0.412584018 0.73042509
## 15228 0.7689116 0.5193352 FALSE     JohnBoozman 0.570935682 0.84453680
## 15243 0.7689116 0.5193352 FALSE     JohnBoozman 0.506989249 0.43667224
## 15244 0.7689116 0.5193352 FALSE     JohnBoozman 0.845893887 0.68201606
## 15248 0.7689116 0.5193352 FALSE     JohnBoozman 0.647646231 0.69390015
## 15254 0.7689116 0.5193352 FALSE     JohnBoozman 0.721450679 0.49532897
## 15256 0.7779185 0.6162086 FALSE     BillCassidy 0.731289426 0.62166970
## 15258 0.7779185 0.6162086 FALSE     BillCassidy 0.984278610 0.87135394
## 15265 0.7779185 0.6162086 FALSE     BillCassidy 0.597853044 0.39463057
## 15276 0.7779185 0.6162086 FALSE     BillCassidy 0.468914387 0.45196189
## 15277 0.7779185 0.6162086 FALSE     BillCassidy 0.574462512 0.84713291
## 15278 0.7779185 0.6162086 FALSE     BillCassidy 0.595634162 0.49930947
## 15279 0.7779185 0.6162086 FALSE     BillCassidy 0.452022309 0.77491694
## 15280 0.7779185 0.6162086 FALSE     BillCassidy 0.502191735 0.53575723
## 15281 0.7779185 0.6162086 FALSE     BillCassidy 0.453459493 0.68217988
## 15282 0.7779185 0.6162086 FALSE     BillCassidy 0.580733962 0.75578467
## 15283 0.7779185 0.6162086 FALSE     BillCassidy 0.404318218 0.64961101
## 15284 0.7779185 0.6162086 FALSE     BillCassidy 0.561995752 0.42655954
## 15285 0.7779185 0.6162086 FALSE     BillCassidy 0.654060369 0.70038037
## 15286 0.7779185 0.6162086 FALSE     BillCassidy 0.414828425 0.73537147
## 15287 0.7779185 0.6162086 FALSE     BillCassidy 0.547638384 0.64824443
## 15288 0.7779185 0.6162086 FALSE     BillCassidy 0.691832384 0.60889777
## 15290 0.7779185 0.6162086 FALSE     BillCassidy 0.431096390 0.84178282
## 15292 0.7779185 0.6162086 FALSE     BillCassidy 0.513997273 0.74281118
## 15294 0.7779185 0.6162086 FALSE     BillCassidy 0.549246377 0.48417794
## 15297 0.7779185 0.6162086 FALSE     BillCassidy 0.603505042 0.70839130
## 15298 0.7779185 0.6162086 FALSE     BillCassidy 0.722358373 0.70750237
## 15300 0.7779185 0.6162086 FALSE     BillCassidy 0.711942092 0.50551969
## 15308 0.7779185 0.6162086 FALSE     BillCassidy 0.651538697 0.58133450
## 15309 0.7779185 0.6162086 FALSE     BillCassidy 0.409585989 0.48884737
## 15311 0.7779185 0.6162086 FALSE     BillCassidy 0.809036594 0.64174526
## 15319 0.7779185 0.6162086 FALSE     BillCassidy 0.600244580 0.60374442
## 15323 0.7779185 0.6162086 FALSE     BillCassidy 0.749033300 0.50735701
## 15327 0.7779185 0.6162086 FALSE     BillCassidy 0.529504998 0.51817706
## 15333 0.7779185 0.6162086 FALSE     BillCassidy 0.823357145 0.61379043
## 15334 0.7779185 0.6162086 FALSE     BillCassidy 0.596378731 0.57058998
## 15336 0.7779185 0.6162086 FALSE     BillCassidy 0.597646072 0.65743656
## 15337 0.7779185 0.6162086 FALSE     BillCassidy 0.545448483 0.62078936
## 15339 0.7779185 0.6162086 FALSE     BillCassidy 0.439501200 0.65162632
## 15340 0.7779185 0.6162086 FALSE     BillCassidy 0.285860895 0.63146019
## 15345 0.7779185 0.6162086 FALSE     BillCassidy 0.852437312 0.41704384
## 15352 0.7779185 0.6162086 FALSE     BillCassidy 0.788285047 0.40542332
## 15354 0.7779185 0.6162086 FALSE     BillCassidy 0.803856373 0.57088883
## 15360 0.7779185 0.6162086 FALSE     BillCassidy 0.299016669 0.48125908
## 15362 0.7779185 0.6162086 FALSE     BillCassidy 0.813781030 0.73679603
## 15364 0.7779185 0.6162086 FALSE     BillCassidy 0.839967994 0.68593749
## 15373 0.7779185 0.6162086 FALSE     BillCassidy 0.663083167 0.75834559
## 15395 0.7895131 0.3804535 FALSE    SenTomCotton 0.705119003 0.49421271
## 15396 0.7895131 0.3804535 FALSE    SenTomCotton 0.714511199 0.60090998
## 15398 0.7895131 0.3804535 FALSE    SenTomCotton 0.747272526 0.69657329
## 15399 0.7895131 0.3804535 FALSE    SenTomCotton 0.753001439 0.46045019
## 15400 0.7895131 0.3804535 FALSE    SenTomCotton 0.772579886 0.49460577
## 15401 0.7895131 0.3804535 FALSE    SenTomCotton 0.507838466 0.42522372
## 15402 0.7895131 0.3804535 FALSE    SenTomCotton 0.395623588 0.29538149
## 15405 0.7895131 0.3804535 FALSE    SenTomCotton 0.630042734 0.79923532
## 15406 0.7895131 0.3804535 FALSE    SenTomCotton 0.602520700 0.45701986
## 15407 0.7895131 0.3804535 FALSE    SenTomCotton 0.670840595 0.77998797
## 15408 0.7895131 0.3804535 FALSE    SenTomCotton 0.440802788 0.51635822
## 15415 0.7895131 0.3804535 FALSE    SenTomCotton 0.824891855 0.63284665
## 15419 0.7895131 0.3804535 FALSE    SenTomCotton 0.592683541 0.58402226
## 15425 0.7895131 0.3804535 FALSE    SenTomCotton 0.851048646 0.53800355
## 15433 0.7895131 0.3804535 FALSE    SenTomCotton 0.678828852 0.58479981
## 15441 0.7895131 0.3804535 FALSE    SenTomCotton 0.672519926 0.41005144
## 15444 0.7895131 0.3804535 FALSE    SenTomCotton 0.715576707 0.46520618
## 15468 0.7895131 0.3804535 FALSE    SenTomCotton 0.411070108 0.69028668
## 15469 0.7895131 0.3804535 FALSE    SenTomCotton 0.434323721 0.54495758
## 15470 0.7931346 0.6770077 FALSE  SenDanSullivan 0.703586762 0.53541970
## 15471 0.7931346 0.6770077 FALSE  SenDanSullivan 0.453495561 0.77869243
## 15472 0.7931346 0.6770077 FALSE  SenDanSullivan 0.763226057 0.75297632
## 15474 0.7931346 0.6770077 FALSE  SenDanSullivan 0.679452539 0.48069539
## 15475 0.7931346 0.6770077 FALSE  SenDanSullivan 0.657724181 0.70872992
## 15476 0.7931346 0.6770077 FALSE  SenDanSullivan 0.727850085 0.63751714
## 15477 0.7931346 0.6770077 FALSE  SenDanSullivan 0.648710378 0.58782004
## 15478 0.7931346 0.6770077 FALSE  SenDanSullivan 0.502674084 0.44476045
## 15481 0.7931346 0.6770077 FALSE  SenDanSullivan 0.789818357 0.40545164
## 15483 0.7931346 0.6770077 FALSE  SenDanSullivan 0.527823163 0.52163374
## 15486 0.7931346 0.6770077 FALSE  SenDanSullivan 0.515687323 0.74747011
## 15487 0.7931346 0.6770077 FALSE  SenDanSullivan 0.640227298 0.80644615
## 15493 0.7931346 0.6770077 FALSE  SenDanSullivan 0.246100566 0.69756248
## 15494 0.7931346 0.6770077 FALSE  SenDanSullivan 0.605900674 0.71509038
## 15498 0.7931346 0.6770077 FALSE  SenDanSullivan 0.416660629 0.70431508
## 15499 0.7931346 0.6770077 FALSE  SenDanSullivan 0.302824311 0.73283262
## 15500 0.7931346 0.6770077 FALSE  SenDanSullivan 0.730619052 0.71570115
## 15505 0.7931346 0.6770077 FALSE  SenDanSullivan 0.710089916 0.50652049
## 15506 0.7931346 0.6770077 FALSE  SenDanSullivan 0.681569244 0.78644640
## 15508 0.7931346 0.6770077 FALSE  SenDanSullivan 0.598224766 0.66459855
## 15515 0.7931346 0.6770077 FALSE  SenDanSullivan 0.713471561 0.58750382
## 15516 0.7931346 0.6770077 FALSE  SenDanSullivan 0.596414061 0.39571623
## 15519 0.7931346 0.6770077 FALSE  SenDanSullivan 0.762526955 0.70461028
## 15520 0.7931346 0.6770077 FALSE  SenDanSullivan 0.772707741 0.54404527
## 15523 0.7931346 0.6770077 FALSE  SenDanSullivan 0.298043286 0.58847739
## 15526 0.7931346 0.6770077 FALSE  SenDanSullivan 0.583537184 0.76093525
## 15527 0.7931346 0.6770077 FALSE  SenDanSullivan 0.541722316 0.59578856
## 15531 0.7931346 0.6770077 FALSE  SenDanSullivan 0.565873047 0.53437566
## 15533 0.7931346 0.6770077 FALSE  SenDanSullivan 0.485694849 0.60105560
## 15534 0.7931346 0.6770077 FALSE  SenDanSullivan 0.811820974 0.57379129
## 15536 0.7931346 0.6770077 FALSE  SenDanSullivan 0.833662903 0.69464009
## 15538 0.7931346 0.6770077 FALSE  SenDanSullivan 0.594411593 0.57583933
## 15543 0.7931346 0.6770077 FALSE  SenDanSullivan 0.748926224 0.50738517
## 15546 0.7931346 0.6770077 FALSE  SenDanSullivan 0.709062113 0.97100493
## 15551 0.7931346 0.6770077 FALSE  SenDanSullivan 0.547373049 0.48697023
## 15553 0.7931346 0.6770077 FALSE  SenDanSullivan 0.300653394 0.66155506
## 15554 0.7931346 0.6770077 FALSE  SenDanSullivan 0.982615445 0.87282599
## 15560 0.7931346 0.6770077 FALSE  SenDanSullivan 0.598943556 0.61013494
## 15565 0.7931346 0.6770077 FALSE  SenDanSullivan 0.688768089 0.61893754
## 15568 0.7931346 0.6770077 FALSE  SenDanSullivan 0.439591846 0.65573039
## 15596 0.7931346 0.6770077 FALSE  SenDanSullivan 0.813038580 0.73702948
## 15597 0.8162746 0.5491912 FALSE   SenatorRounds 0.761239940 0.49987685
## 15598 0.8162746 0.5491912 FALSE   SenatorRounds 0.662228238 0.75768489
## 15599 0.8162746 0.5491912 FALSE   SenatorRounds 0.820360200 0.73576477
## 15600 0.8162746 0.5491912 FALSE   SenatorRounds 0.652214653 0.57133966
## 15601 0.8162746 0.5491912 FALSE   SenatorRounds 0.573205906 0.66897770
## 15602 0.8162746 0.5491912 FALSE   SenatorRounds 0.765668157 0.66161412
## 15603 0.8162746 0.5491912 FALSE   SenatorRounds 0.512614784 0.74030743
## 15604 0.8162746 0.5491912 FALSE   SenatorRounds 0.602988115 0.47473288
## 15605 0.8162746 0.5491912 FALSE   SenatorRounds 0.503146912 0.53026319
## 15606 0.8162746 0.5491912 FALSE   SenatorRounds 0.500285777 0.38705503
## 15607 0.8162746 0.5491912 FALSE   SenatorRounds 0.651933294 0.69767233
## 15609 0.8162746 0.5491912 FALSE   SenatorRounds 0.790060363 0.53266664
## 15610 0.8162746 0.5491912 FALSE   SenatorRounds 0.565487516 0.42141123
## 15611 0.8162746 0.5491912 FALSE   SenatorRounds 0.329732254 0.61845400
## 15612 0.8162746 0.5491912 FALSE   SenatorRounds 0.598772183 0.49215510
## 15615 0.8162746 0.5491912 FALSE   SenatorRounds 0.551740763 0.47816061
## 15617 0.8162746 0.5491912 FALSE   SenatorRounds 0.714317279 0.52096173
## 15620 0.8162746 0.5491912 FALSE   SenatorRounds 0.667883782 0.43170165
## 15623 0.8162746 0.5491912 FALSE   SenatorRounds 0.569565175 0.52365942
## 15624 0.8162746 0.5491912 FALSE   SenatorRounds 0.793429094 0.40514489
## 15626 0.8162746 0.5491912 FALSE   SenatorRounds 0.717461821 0.78214320
## 15627 0.8162746 0.5491912 FALSE   SenatorRounds 0.753642810 0.69830375
## 15633 0.8162746 0.5491912 FALSE   SenatorRounds 0.578752173 0.80526551
## 15642 0.8162746 0.5491912 FALSE   SenatorRounds 0.546478098 0.64344411
## 15644 0.8162746 0.5491912 FALSE   SenatorRounds 0.837025478 0.59015954
## 15646 0.8162746 0.5491912 FALSE   SenatorRounds 0.688328734 0.47197820
## 15648 0.8162746 0.5491912 FALSE   SenatorRounds 0.381598530 0.53733064
## 15653 0.8162746 0.5491912 FALSE   SenatorRounds 0.720990275 0.49619646
## 15657 0.8162746 0.5491912 FALSE   SenatorRounds 0.449694075 0.57324959
## 15660 0.8162746 0.5491912 FALSE   SenatorRounds 0.599726427 0.59664362
## 15661 0.8162746 0.5491912 FALSE   SenatorRounds 0.385009118 0.75776925
## 15666 0.8162746 0.5491912 FALSE   SenatorRounds 0.436635618 0.55507655
## 15676 0.8162746 0.5491912 FALSE   SenatorRounds 0.836043758 0.55464352
## 15680 0.8162746 0.5491912 FALSE   SenatorRounds 0.850310625 0.68041440
## 15693 0.8162746 0.5491912 FALSE   SenatorRounds 0.595914867 0.65240591
## 15706 0.8162746 0.5491912 FALSE   SenatorRounds 0.665368877 0.82792747
## 15708 0.8162746 0.5491912 FALSE   SenatorRounds 0.854765426 0.50401427
## 15712 0.8162746 0.5491912 FALSE   SenatorRounds 0.722145716 0.70737439
## 15719 0.8162746 0.5491912 FALSE   SenatorRounds 0.285598043 0.62853781
## 15721 0.8162746 0.5491912 FALSE   SenatorRounds 0.466167322 0.65195945
## 15722 0.8162746 0.5491912 FALSE   SenatorRounds 0.442465097 0.52692313
## 15728 0.8162746 0.5491912 FALSE   SenatorRounds 0.470817869 0.44730050
## 15734 0.8162746 0.5491912 FALSE   SenatorRounds 0.542722908 0.58487003
## 15740 0.8162746 0.5491912 FALSE   SenatorRounds 0.790336703 0.59451097
## 15741 0.8162746 0.5491912 FALSE   SenatorRounds 0.760673922 0.75212706
## 15744 0.8162746 0.5491912 FALSE   SenatorRounds 0.601617994 0.70536526
## 15747 0.8209075 0.7607588 FALSE       JimInhofe 0.672252496 0.77534984
## 15748 0.8209075 0.7607588 FALSE       JimInhofe 0.525770997 0.52461866
## 15749 0.8209075 0.7607588 FALSE       JimInhofe 0.563580354 0.53747057
## 15750 0.8209075 0.7607588 FALSE       JimInhofe 0.979770005 0.87610365
## 15751 0.8209075 0.7607588 FALSE       JimInhofe 0.416525998 0.70928073
## 15752 0.8209075 0.7607588 FALSE       JimInhofe 0.543129127 0.63180844
## 15753 0.8209075 0.7607588 FALSE       JimInhofe 0.596529189 0.67218921
## 15755 0.8209075 0.7607588 FALSE       JimInhofe 0.733397792 0.73573239
## 15759 0.8209075 0.7607588 FALSE       JimInhofe 0.816821916 0.57418519
## 15761 0.8209075 0.7607588 FALSE       JimInhofe 0.678289382 0.48132843
## 15762 0.8209075 0.7607588 FALSE       JimInhofe 0.574734260 0.68718749
## 15764 0.8209075 0.7607588 FALSE       JimInhofe 0.866512613 0.50958244
## 15766 0.8209075 0.7607588 FALSE       JimInhofe 0.453531357 0.69177486
## 15768 0.8209075 0.7607588 FALSE       JimInhofe 0.466323102 0.66548773
## 15771 0.8209075 0.7607588 FALSE       JimInhofe 0.508986937 0.82116701
## 15772 0.8209075 0.7607588 FALSE       JimInhofe 0.843178510 0.72571351
## 15774 0.8209075 0.7607588 FALSE       JimInhofe 0.701934974 0.53637808
## 15775 0.8209075 0.7607588 FALSE       JimInhofe 0.539653545 0.60048125
## 15776 0.8209075 0.7607588 FALSE       JimInhofe 0.488082337 0.78985394
## 15778 0.8209075 0.7607588 FALSE       JimInhofe 0.744853682 0.58131934
## 15787 0.8209075 0.7607588 FALSE       JimInhofe 0.387499409 0.76822357
## 15789 0.8209075 0.7607588 FALSE       JimInhofe 0.245984597 0.70108230
## 15790 0.8209075 0.7607588 FALSE       JimInhofe 0.687828664 0.79732874
## 15791 0.8209075 0.7607588 FALSE       JimInhofe 0.500987685 0.44666259
## 15792 0.8209075 0.7607588 FALSE       JimInhofe 0.771684728 0.70338495
## 15794 0.8209075 0.7607588 FALSE       JimInhofe 0.586046963 0.81770376
## 15795 0.8209075 0.7607588 FALSE       JimInhofe 0.591271504 0.50443416
## 15796 0.8209075 0.7607588 FALSE       JimInhofe 0.645458028 0.59201439
## 15798 0.8209075 0.7607588 FALSE       JimInhofe 0.484128693 0.60552488
## 15801 0.8209075 0.7607588 FALSE       JimInhofe 0.749407548 0.50725458
## 15806 0.8209075 0.7607588 FALSE       JimInhofe 0.843777295 0.63704532
## 15807 0.8209075 0.7607588 FALSE       JimInhofe 0.774175174 0.54377478
## 15812 0.8283623 0.6576046 FALSE   ChuckGrassley 0.583376675 0.76054402
## 15813 0.8283623 0.6576046 FALSE   ChuckGrassley 0.778872422 0.67578874
## 15816 0.8283623 0.6576046 FALSE   ChuckGrassley 0.595292309 0.49982823
## 15817 0.8483218 0.6124618 FALSE        RoyBlunt 0.758446310 0.50254697
## 15819 0.8483218 0.6124618 FALSE        RoyBlunt 0.547697195 0.64869746
## 15820 0.8483218 0.6124618 FALSE        RoyBlunt 0.718089041 0.50035496
## 15821 0.8483218 0.6124618 FALSE        RoyBlunt 0.597279006 0.49631136
## 15822 0.8483218 0.6124618 FALSE        RoyBlunt 0.681073109 0.78595456
## 15823 0.8483218 0.6124618 FALSE        RoyBlunt 0.691909808 0.60756462
## 15825 0.8483218 0.6124618 FALSE        RoyBlunt 0.720873519 0.57574942
## 15826 0.8483218 0.6124618 FALSE        RoyBlunt 0.731368457 0.62245036
## 15827 0.8483218 0.6124618 FALSE        RoyBlunt 0.499103813 0.38911201
## 15829 0.8483218 0.6124618 FALSE        RoyBlunt 0.596763851 0.56877484
## 15831 0.8483218 0.6124618 FALSE        RoyBlunt 0.802883153 0.61488002
## 15833 0.8483218 0.6124618 FALSE        RoyBlunt 0.597863490 0.65849127
## 15836 0.8483218 0.6124618 FALSE        RoyBlunt 0.757650363 0.56908944
## 15837 0.8483218 0.6124618 FALSE        RoyBlunt 0.601357248 0.47841874
## 15838 0.8483218 0.6124618 FALSE        RoyBlunt 0.827570935 0.57149348
## 15844 0.8483218 0.6124618 FALSE        RoyBlunt 0.652081597 0.57889892
## 15845 0.8483218 0.6124618 FALSE        RoyBlunt 0.299259852 0.48032777
## 15846 0.8483218 0.6124618 FALSE        RoyBlunt 0.486399267 0.59618301
## 15847 0.8483218 0.6124618 FALSE        RoyBlunt 0.541542174 0.70177209
## 15848 0.8483218 0.6124618 FALSE        RoyBlunt 0.576771288 0.84945257
## 15854 0.8483218 0.6124618 FALSE        RoyBlunt 0.329977955 0.62153964
## 15855 0.8483218 0.6124618 FALSE        RoyBlunt 0.564045176 0.42388155
## 15859 0.8483218 0.6124618 FALSE        RoyBlunt 0.655970235 0.70371655
## 15860 0.8565873 0.7046136 FALSE   SenPatRoberts 0.658359037 0.71333355
## 15861 0.8565873 0.7046136 FALSE   SenPatRoberts 0.584934361 0.81423899
## 15862 0.8565873 0.7046136 FALSE   SenPatRoberts 0.606362930 0.71867100
## 15863 0.8565873 0.7046136 FALSE   SenPatRoberts 0.500863667 0.53929118
## 15864 0.8565873 0.7046136 FALSE   SenPatRoberts 0.706676884 0.53311352
## 15865 0.8565873 0.7046136 FALSE   SenPatRoberts 0.779922399 0.68930603
## 15866 0.8565873 0.7046136 FALSE   SenPatRoberts 0.584737166 0.76482251
## 15868 0.8565873 0.7046136 FALSE   SenPatRoberts 0.598791722 0.61056317
## 15869 0.8565873 0.7046136 FALSE   SenPatRoberts 0.561428552 0.42718376
## 15870 0.8565873 0.7046136 FALSE   SenPatRoberts 0.598348912 0.48278328
## 15872 0.8565873 0.7046136 FALSE   SenPatRoberts 0.754062220 0.50542171
## 15876 0.8565873 0.7046136 FALSE   SenPatRoberts 0.774561669 0.76192053
## 15880 0.8565873 0.7046136 FALSE   SenPatRoberts 0.291765132 0.77278123
## 15886 0.8565873 0.7046136 FALSE   SenPatRoberts 0.319310571 0.69750792
## 15890 0.8565873 0.7046136 FALSE   SenPatRoberts 0.869344098 0.50993110
## 15891 0.8565873 0.7046136 FALSE   SenPatRoberts 0.779604977 0.54193275
## 15894 0.8565873 0.7046136 FALSE   SenPatRoberts 0.547568255 0.65560508
## 15900 0.8565873 0.7046136 FALSE   SenPatRoberts 0.485515819 0.60173869
## 15902 0.8565873 0.7046136 FALSE   SenPatRoberts 0.713666702 0.50439292
## 15904 0.8565873 0.7046136 FALSE   SenPatRoberts 0.841231230 0.67903798
## 15906 0.8565873 0.7046136 FALSE   SenPatRoberts 0.547999536 0.48612379
## 15907 0.8565873 0.7046136 FALSE   SenPatRoberts 0.816058998 0.68698128
## 15909 0.8565873 0.7046136 FALSE   SenPatRoberts 0.859523758 0.58628266
## 15914 0.8565873 0.7046136 FALSE   SenPatRoberts 0.496912941 0.39204093
## 15921 0.8565873 0.7046136 FALSE   SenPatRoberts 0.598010012 0.66664234
## 15922 0.8565873 0.7046136 FALSE   SenPatRoberts 0.649186753 0.58701535
## 15924 0.8565873 0.7046136 FALSE   SenPatRoberts 0.453939981 0.68818060
## 15926 0.8565873 0.7046136 FALSE   SenPatRoberts 0.834316315 0.73965891
## 15930 0.8565873 0.7046136 FALSE   SenPatRoberts 0.689140450 0.61824275
## 15934 0.8565873 0.7046136 FALSE   SenPatRoberts 0.544718770 0.62729758
## 15935 0.8565873 0.7046136 FALSE   SenPatRoberts 0.670970172 0.76953792
## 15936 0.8565873 0.7046136 FALSE   SenPatRoberts 0.541572941 0.59623645
## 15938 0.8565873 0.7046136 FALSE   SenPatRoberts 0.579933178 0.85401194
## 15939 0.8565873 0.7046136 FALSE   SenPatRoberts 0.850555208 0.63736188
## 15949 0.8565873 0.7046136 FALSE   SenPatRoberts 0.439476098 0.65706035
## 15955 0.8565873 0.7046136 FALSE   SenPatRoberts 0.575700699 0.68203223
## 15960 0.8565873 0.7046136 FALSE   SenPatRoberts 0.467029322 0.66173589
## 15970 0.8601440 0.5612904 FALSE   SenRonJohnson 0.664895477 0.75996136
## 16036 0.8601440 0.5612904 FALSE   SenRonJohnson 0.596837846 0.65465515
## 16078 0.8601440 0.5612904 FALSE   SenRonJohnson 0.421215668 0.44989387
## 16101 0.8601440 0.5612904 FALSE   SenRonJohnson 0.758646951 0.70112103
## 16153 0.8601440 0.5612904 FALSE   SenRonJohnson 0.600054445 0.59845825
## 16207 0.8601440 0.5612904 FALSE   SenRonJohnson 0.449735561 0.57410649
## 16211 0.8601440 0.5612904 FALSE   SenRonJohnson 0.486335333 0.59294987
## 16264 0.8601440 0.5612904 FALSE   SenRonJohnson 0.765131389 0.75381977
## 16265 0.8601440 0.5612904 FALSE   SenRonJohnson 0.791624928 0.52978040
## 16266 0.8601440 0.5612904 FALSE   SenRonJohnson 0.867464204 0.50973633
## 16272 0.8601440 0.5612904 FALSE   SenRonJohnson 0.545072352 0.61693401
## 16275 0.8601440 0.5612904 FALSE   SenRonJohnson 0.403985536 0.64720981
## 16341 0.8601440 0.5612904 FALSE   SenRonJohnson 0.720951030 0.78395939
## 16372 0.8601440 0.5612904 FALSE   SenRonJohnson 0.638018636 0.80415108
## 16373 0.8601440 0.5612904 FALSE   SenRonJohnson 0.679451298 0.78452125
## 16385 0.8601440 0.5612904 FALSE   SenRonJohnson 0.466523036 0.65330974
## 16386 0.8601440 0.5612904 FALSE   SenRonJohnson 0.652398097 0.57324780
## 16398 0.8601440 0.5612904 FALSE   SenRonJohnson 0.603125067 0.70769906
## 16508 0.8601440 0.5612904 FALSE   SenRonJohnson 0.726083769 0.71027444
## 16517 0.8601440 0.5612904 FALSE   SenRonJohnson 0.503102252 0.53087662
## 16597 0.8601440 0.5612904 FALSE   SenRonJohnson 0.836196250 0.63386374
## 16700 0.8601440 0.5612904 FALSE   SenRonJohnson 0.760090623 0.55889886
## 16775 0.8601440 0.5612904 FALSE   SenRonJohnson 0.415610488 0.69873872
## 16823 0.8601440 0.5612904 FALSE   SenRonJohnson 0.714318998 0.52095552
## 16897 0.8601440 0.5612904 FALSE   SenRonJohnson 0.540501756 0.69887589
## 16994 0.8601440 0.5612904 FALSE   SenRonJohnson 0.694156356 0.65952972
## 17006 0.8601440 0.5612904 FALSE   SenRonJohnson 0.439110146 0.64912314
## 17025 0.8611981 0.3936291 FALSE  SenJohnKennedy 0.814101211 0.38497277
## 17026 0.8611981 0.3936291 FALSE  SenJohnKennedy 0.542754381 0.63652726
## 17027 0.8611981 0.3936291 FALSE  SenJohnKennedy 0.435160914 0.54699638
## 17028 0.8611981 0.3936291 FALSE  SenJohnKennedy 0.831448218 0.63279579
## 17031 0.8611981 0.3936291 FALSE  SenJohnKennedy 0.786679253 0.59279390
## 17034 0.8611981 0.3936291 FALSE  SenJohnKennedy 0.483750605 0.58381042
## 17035 0.8611981 0.3936291 FALSE  SenJohnKennedy 0.752382071 0.69781389
## 17037 0.8611981 0.3936291 FALSE  SenJohnKennedy 0.392509407 0.44048706
## 17038 0.8611981 0.3936291 FALSE  SenJohnKennedy 0.598389068 0.47815893
## 17039 0.8611981 0.3936291 FALSE  SenJohnKennedy 0.647204192 0.55937578
## 17040 0.8611981 0.3936291 FALSE  SenJohnKennedy 0.541240797 0.60739370
## 17041 0.8611981 0.3936291 FALSE  SenJohnKennedy 0.798973189 0.65269906
## 17043 0.8611981 0.3936291 FALSE  SenJohnKennedy 0.463300030 0.64562466
## 17045 0.8611981 0.3936291 FALSE  SenJohnKennedy 0.471682500 0.43743419
## 17046 0.8611981 0.3936291 FALSE  SenJohnKennedy 0.720973974 0.47186439
## 17050 0.8611981 0.3936291 FALSE  SenJohnKennedy 0.591530994 0.64593004
## 17053 0.8611981 0.3936291 FALSE  SenJohnKennedy 0.849790295 0.58750501
## 17055 0.8611981 0.3936291 FALSE  SenJohnKennedy 0.595509331 0.58727011
## 17057 0.8709787 0.4849846 FALSE   SenJohnHoeven 0.442410457 0.52321519
## 17058 0.8709787 0.4849846 FALSE   SenJohnHoeven 0.545424626 0.64089113
## 17059 0.8709787 0.4849846 FALSE   SenJohnHoeven 0.832487914 0.53016151
## 17060 0.8709787 0.4849846 FALSE   SenJohnHoeven 0.450203726 0.77178248
## 17061 0.8709787 0.4849846 FALSE   SenJohnHoeven 0.552577268 0.47264555
## 17062 0.8709787 0.4849846 FALSE   SenJohnHoeven 0.569546347 0.51833659
## 17065 0.8709787 0.4849846 FALSE   SenJohnHoeven 0.503038671 0.52598600
## 17066 0.8709787 0.4849846 FALSE   SenJohnHoeven 0.662545257 0.75792303
## 17074 0.8709787 0.4849846 FALSE   SenJohnHoeven 0.594731548 0.65017935
## 17075 0.8709787 0.4849846 FALSE   SenJohnHoeven 0.691725021 0.46221178
## 17076 0.8709787 0.4849846 FALSE   SenJohnHoeven 0.719057004 0.78288715
## 17077 0.8709787 0.4849846 FALSE   SenJohnHoeven 0.858221952 0.67966714
## 17079 0.8709787 0.4849846 FALSE   SenJohnHoeven 0.834354359 0.63333333
## 17084 0.8709787 0.4849846 FALSE   SenJohnHoeven 0.381482671 0.53415028
## 17086 0.8709787 0.4849846 FALSE   SenJohnHoeven 0.755795264 0.69933125
## 17090 0.8709787 0.4849846 FALSE   SenJohnHoeven 0.767941576 0.66278069
## 17093 0.8709787 0.4849846 FALSE   SenJohnHoeven 0.507893254 0.43271060
## 17101 0.8709787 0.4849846 FALSE   SenJohnHoeven 0.604334922 0.46807526
## 17112 0.8709787 0.4849846 FALSE   SenJohnHoeven 0.600811442 0.70431620
## 17120 0.8709787 0.4849846 FALSE   SenJohnHoeven 0.763380715 0.75303781
## 17123 0.8709787 0.4849846 FALSE   SenJohnHoeven 0.792605687 0.51136094
## 17124 0.8709787 0.4849846 FALSE   SenJohnHoeven 0.301715461 0.53879208
## 17129 0.8709787 0.4849846 FALSE   SenJohnHoeven 0.502127814 0.38234419
## 17132 0.8709787 0.4849846 FALSE   SenJohnHoeven 0.421889788 0.44591101
## 17137 0.8709787 0.4849846 FALSE   SenJohnHoeven 0.767618716 0.48354216
## 17140 0.8709787 0.4849846 FALSE   SenJohnHoeven 0.719375126 0.55798343
## 17145 0.8709787 0.4849846 FALSE   SenJohnHoeven 0.650898773 0.56604388
## 17152 1.0000000 0.8907920 FALSE       joniernst 0.724977759 0.64137243
## 17153 1.0000000 0.8907920 FALSE       joniernst 0.843181906 0.67773860
## 17156 1.0000000 0.8907920 FALSE       joniernst 0.774513412 0.70053294
## 17161 1.0000000 0.8907920 FALSE       joniernst 0.452508392 0.69555814
## 17165 1.0000000 0.8907920 FALSE       joniernst 0.793639870 0.63564666
## 17174 1.0000000 0.8907920 FALSE       joniernst 0.595329910 0.67478268
## 17179 1.0000000 0.8907920 FALSE       joniernst 0.776730002 0.78679434
##        na.y
## 1        NA
## 2        NA
## 3        NA
## 4        NA
## 5        NA
## 6        NA
## 7        NA
## 8        NA
## 9        NA
## 10       NA
## 11       NA
## 12       NA
## 13       NA
## 14       NA
## 15       NA
## 16       NA
## 17       NA
## 18       NA
## 19       NA
## 20       NA
## 21       NA
## 22       NA
## 23       NA
## 24       NA
## 25       NA
## 26       NA
## 27       NA
## 28       NA
## 29       NA
## 30       NA
## 31       NA
## 32       NA
## 33       NA
## 34       NA
## 35       NA
## 36       NA
## 37       NA
## 38       NA
## 39       NA
## 40       NA
## 41       NA
## 42       NA
## 43       NA
## 44       NA
## 45       NA
## 46       NA
## 47       NA
## 48       NA
## 49       NA
## 50       NA
## 51       NA
## 52       NA
## 53       NA
## 54       NA
## 55       NA
## 56       NA
## 57       NA
## 58       NA
## 59       NA
## 60       NA
## 61       NA
## 62       NA
## 63       NA
## 64       NA
## 65       NA
## 66       NA
## 67       NA
## 68       NA
## 69       NA
## 70       NA
## 71       NA
## 72       NA
## 73       NA
## 74       NA
## 75       NA
## 76       NA
## 77       NA
## 78       NA
## 79       NA
## 80       NA
## 81       NA
## 82       NA
## 83       NA
## 84       NA
## 85       NA
## 86       NA
## 87       NA
## 88       NA
## 89       NA
## 90       NA
## 91       NA
## 92       NA
## 93       NA
## 94       NA
## 95       NA
## 96       NA
## 97       NA
## 98       NA
## 99       NA
## 100      NA
## 153   FALSE
## 261   FALSE
## 3123  FALSE
## 423   FALSE
## 589   FALSE
## 6106  FALSE
## 851   FALSE
## 1095  FALSE
## 12174 FALSE
## 1337  FALSE
## 1468  FALSE
## 15110 FALSE
## 16100 FALSE
## 17105 FALSE
## 20100 FALSE
## 21100 FALSE
## 24100 FALSE
## 27100 FALSE
## 28100 FALSE
## 31100 FALSE
## 3274  FALSE
## 3311  FALSE
## 36100 FALSE
## 37100 FALSE
## 41100 FALSE
## 42100 FALSE
## 433   FALSE
## 452   FALSE
## 4723  FALSE
## 491   FALSE
## 5037  FALSE
## 5225  FALSE
## 5410  FALSE
## 5613  FALSE
## 57100 FALSE
## 5815  FALSE
## 6033  FALSE
## 64100 FALSE
## 66100 FALSE
## 70100 FALSE
## 72100 FALSE
## 7489  FALSE
## 77100 FALSE
## 8310  FALSE
## 8949  FALSE
## 9294  FALSE
## 9350  FALSE
## 9473  FALSE
## 9719  FALSE
## 9911  FALSE
## 101   FALSE
## 104   FALSE
## 111   FALSE
## 119   FALSE
## 158   FALSE
## 172   FALSE
## 173   FALSE
## 174   FALSE
## 175   FALSE
## 176   FALSE
## 177   FALSE
## 179   FALSE
## 180   FALSE
## 182   FALSE
## 183   FALSE
## 184   FALSE
## 185   FALSE
## 188   FALSE
## 192   FALSE
## 199   FALSE
## 200   FALSE
## 201   FALSE
## 203   FALSE
## 204   FALSE
## 206   FALSE
## 207   FALSE
## 210   FALSE
## 214   FALSE
## 216   FALSE
## 221   FALSE
## 229   FALSE
## 232   FALSE
## 233   FALSE
## 243   FALSE
## 248   FALSE
## 253   FALSE
## 268   FALSE
## 272   FALSE
## 273   FALSE
## 284   FALSE
## 311   FALSE
## 315   FALSE
## 336   FALSE
## 355   FALSE
## 356   FALSE
## 407   FALSE
## 422   FALSE
## 424   FALSE
## 425   FALSE
## 426   FALSE
## 427   FALSE
## 428   FALSE
## 431   FALSE
## 436   FALSE
## 438   FALSE
## 439   FALSE
## 442   FALSE
## 444   FALSE
## 445   FALSE
## 449   FALSE
## 453   FALSE
## 456   FALSE
## 457   FALSE
## 461   FALSE
## 462   FALSE
## 464   FALSE
## 465   FALSE
## 468   FALSE
## 473   FALSE
## 474   FALSE
## 480   FALSE
## 483   FALSE
## 484   FALSE
## 493   FALSE
## 494   FALSE
## 495   FALSE
## 496   FALSE
## 499   FALSE
## 501   FALSE
## 502   FALSE
## 509   FALSE
## 510   FALSE
## 511   FALSE
## 513   FALSE
## 519   FALSE
## 520   FALSE
## 521   FALSE
## 522   FALSE
## 524   FALSE
## 528   FALSE
## 530   FALSE
## 531   FALSE
## 535   FALSE
## 538   FALSE
## 539   FALSE
## 541   FALSE
## 542   FALSE
## 544   FALSE
## 545   FALSE
## 546   FALSE
## 547   FALSE
## 555   FALSE
## 559   FALSE
## 560   FALSE
## 565   FALSE
## 568   FALSE
## 575   FALSE
## 577   FALSE
## 578   FALSE
## 587   FALSE
## 588   FALSE
## 592   FALSE
## 596   FALSE
## 600   FALSE
## 612   FALSE
## 613   FALSE
## 614   FALSE
## 615   FALSE
## 616   FALSE
## 617   FALSE
## 620   FALSE
## 621   FALSE
## 622   FALSE
## 623   FALSE
## 630   FALSE
## 631   FALSE
## 633   FALSE
## 638   FALSE
## 640   FALSE
## 642   FALSE
## 643   FALSE
## 644   FALSE
## 649   FALSE
## 650   FALSE
## 652   FALSE
## 653   FALSE
## 655   FALSE
## 656   FALSE
## 657   FALSE
## 669   FALSE
## 677   FALSE
## 678   FALSE
## 679   FALSE
## 680   FALSE
## 682   FALSE
## 684   FALSE
## 689   FALSE
## 694   FALSE
## 708   FALSE
## 710   FALSE
## 711   FALSE
## 712   FALSE
## 714   FALSE
## 724   FALSE
## 725   FALSE
## 726   FALSE
## 727   FALSE
## 728   FALSE
## 729   FALSE
## 730   FALSE
## 731   FALSE
## 735   FALSE
## 736   FALSE
## 737   FALSE
## 739   FALSE
## 741   FALSE
## 743   FALSE
## 744   FALSE
## 751   FALSE
## 755   FALSE
## 756   FALSE
## 761   FALSE
## 769   FALSE
## 774   FALSE
## 775   FALSE
## 777   FALSE
## 778   FALSE
## 780   FALSE
## 781   FALSE
## 782   FALSE
## 784   FALSE
## 785   FALSE
## 786   FALSE
## 787   FALSE
## 788   FALSE
## 789   FALSE
## 790   FALSE
## 791   FALSE
## 792   FALSE
## 798   FALSE
## 800   FALSE
## 802   FALSE
## 803   FALSE
## 807   FALSE
## 809   FALSE
## 811   FALSE
## 812   FALSE
## 816   FALSE
## 818   FALSE
## 823   FALSE
## 826   FALSE
## 829   FALSE
## 850   FALSE
## 857   FALSE
## 858   FALSE
## 864   FALSE
## 876   FALSE
## 877   FALSE
## 879   FALSE
## 880   FALSE
## 883   FALSE
## 885   FALSE
## 886   FALSE
## 887   FALSE
## 888   FALSE
## 891   FALSE
## 894   FALSE
## 896   FALSE
## 897   FALSE
## 898   FALSE
## 901   FALSE
## 903   FALSE
## 905   FALSE
## 911   FALSE
## 914   FALSE
## 916   FALSE
## 922   FALSE
## 928   FALSE
## 936   FALSE
## 937   FALSE
## 939   FALSE
## 940   FALSE
## 943   FALSE
## 948   FALSE
## 953   FALSE
## 954   FALSE
## 955   FALSE
## 956   FALSE
## 957   FALSE
## 958   FALSE
## 959   FALSE
## 960   FALSE
## 961   FALSE
## 962   FALSE
## 964   FALSE
## 966   FALSE
## 968   FALSE
## 970   FALSE
## 971   FALSE
## 973   FALSE
## 976   FALSE
## 977   FALSE
## 982   FALSE
## 983   FALSE
## 984   FALSE
## 985   FALSE
## 988   FALSE
## 995   FALSE
## 996   FALSE
## 1003  FALSE
## 1004  FALSE
## 1006  FALSE
## 1011  FALSE
## 1013  FALSE
## 1014  FALSE
## 1019  FALSE
## 1020  FALSE
## 1022  FALSE
## 1028  FALSE
## 1042  FALSE
## 1044  FALSE
## 1053  FALSE
## 1056  FALSE
## 1061  FALSE
## 1066  FALSE
## 1071  FALSE
## 1084  FALSE
## 1091  FALSE
## 1114  FALSE
## 1130  FALSE
## 1136  FALSE
## 1137  FALSE
## 1138  FALSE
## 1139  FALSE
## 1141  FALSE
## 1142  FALSE
## 1145  FALSE
## 1147  FALSE
## 1149  FALSE
## 1152  FALSE
## 1153  FALSE
## 1157  FALSE
## 1158  FALSE
## 1161  FALSE
## 1167  FALSE
## 1169  FALSE
## 1170  FALSE
## 1178  FALSE
## 1180  FALSE
## 1181  FALSE
## 1182  FALSE
## 1183  FALSE
## 1186  FALSE
## 1189  FALSE
## 1190  FALSE
## 1191  FALSE
## 1200  FALSE
## 1205  FALSE
## 1207  FALSE
## 1214  FALSE
## 1216  FALSE
## 1218  FALSE
## 1219  FALSE
## 1220  FALSE
## 1222  FALSE
## 1226  FALSE
## 1233  FALSE
## 1237  FALSE
## 1248  FALSE
## 1251  FALSE
## 1257  FALSE
## 1265  FALSE
## 1268  FALSE
## 1302  FALSE
## 1314  FALSE
## 1321  FALSE
## 1322  FALSE
## 1323  FALSE
## 1324  FALSE
## 1325  FALSE
## 1326  FALSE
## 1327  FALSE
## 1328  FALSE
## 1331  FALSE
## 1334  FALSE
## 1342  FALSE
## 1343  FALSE
## 1346  FALSE
## 1349  FALSE
## 1351  FALSE
## 1355  FALSE
## 1356  FALSE
## 1357  FALSE
## 1366  FALSE
## 1368  FALSE
## 1374  FALSE
## 1381  FALSE
## 1382  FALSE
## 1386  FALSE
## 1389  FALSE
## 1390  FALSE
## 1391  FALSE
## 1395  FALSE
## 1396  FALSE
## 1403  FALSE
## 1410  FALSE
## 1411  FALSE
## 1413  FALSE
## 1417  FALSE
## 1419  FALSE
## 1422  FALSE
## 1423  FALSE
## 1425  FALSE
## 1426  FALSE
## 1428  FALSE
## 1429  FALSE
## 1432  FALSE
## 1433  FALSE
## 1435  FALSE
## 1436  FALSE
## 1437  FALSE
## 1440  FALSE
## 1443  FALSE
## 1448  FALSE
## 1453  FALSE
## 1454  FALSE
## 1461  FALSE
## 1464  FALSE
## 1466  FALSE
## 1469  FALSE
## 1476  FALSE
## 1484  FALSE
## 1485  FALSE
## 1489  FALSE
## 1495  FALSE
## 1503  FALSE
## 1504  FALSE
## 1505  FALSE
## 1507  FALSE
## 1508  FALSE
## 1522  FALSE
## 1529  FALSE
## 1530  FALSE
## 1532  FALSE
## 1534  FALSE
## 1540  FALSE
## 1541  FALSE
## 1544  FALSE
## 1548  FALSE
## 1550  FALSE
## 1552  FALSE
## 1554  FALSE
## 1555  FALSE
## 1559  FALSE
## 1563  FALSE
## 1565  FALSE
## 1568  FALSE
## 1569  FALSE
## 1576  FALSE
## 1577  FALSE
## 1583  FALSE
## 1585  FALSE
## 1588  FALSE
## 1594  FALSE
## 1595  FALSE
## 1596  FALSE
## 1602  FALSE
## 1607  FALSE
## 1634  FALSE
## 1641  FALSE
## 1643  FALSE
## 1644  FALSE
## 1648  FALSE
## 1649  FALSE
## 1650  FALSE
## 1653  FALSE
## 1685  FALSE
## 1687  FALSE
## 1688  FALSE
## 1689  FALSE
## 1699  FALSE
## 1701  FALSE
## 1710  FALSE
## 1714  FALSE
## 1726  FALSE
## 1732  FALSE
## 1738  FALSE
## 1740  FALSE
## 1747  FALSE
## 1767  FALSE
## 1770  FALSE
## 1775  FALSE
## 1776  FALSE
## 1779  FALSE
## 1788  FALSE
## 1798  FALSE
## 1800  FALSE
## 1803  FALSE
## 1807  FALSE
## 1827  FALSE
## 1828  FALSE
## 1829  FALSE
## 1830  FALSE
## 1831  FALSE
## 1832  FALSE
## 1833  FALSE
## 1834  FALSE
## 1835  FALSE
## 1838  FALSE
## 1840  FALSE
## 1841  FALSE
## 1842  FALSE
## 1843  FALSE
## 1846  FALSE
## 1847  FALSE
## 1849  FALSE
## 1852  FALSE
## 1856  FALSE
## 1859  FALSE
## 1862  FALSE
## 1868  FALSE
## 1881  FALSE
## 1882  FALSE
## 1883  FALSE
## 1888  FALSE
## 1891  FALSE
## 1894  FALSE
## 1897  FALSE
## 1898  FALSE
## 1903  FALSE
## 1904  FALSE
## 1915  FALSE
## 1916  FALSE
## 1920  FALSE
## 1924  FALSE
## 1932  FALSE
## 1933  FALSE
## 1943  FALSE
## 1954  FALSE
## 1964  FALSE
## 1976  FALSE
## 1986  FALSE
## 1998  FALSE
## 2018  FALSE
## 2026  FALSE
## 2027  FALSE
## 2029  FALSE
## 2043  FALSE
## 2045  FALSE
## 2055  FALSE
## 2064  FALSE
## 2079  FALSE
## 2082  FALSE
## 2083  FALSE
## 2084  FALSE
## 2085  FALSE
## 2086  FALSE
## 2088  FALSE
## 2089  FALSE
## 2090  FALSE
## 2091  FALSE
## 2093  FALSE
## 2095  FALSE
## 2097  FALSE
## 2099  FALSE
## 2102  FALSE
## 2104  FALSE
## 2108  FALSE
## 2113  FALSE
## 2121  FALSE
## 2123  FALSE
## 2126  FALSE
## 2137  FALSE
## 2138  FALSE
## 2141  FALSE
## 2146  FALSE
## 2149  FALSE
## 2154  FALSE
## 2161  FALSE
## 2164  FALSE
## 2165  FALSE
## 2166  FALSE
## 2167  FALSE
## 2172  FALSE
## 2173  FALSE
## 2177  FALSE
## 2178  FALSE
## 2180  FALSE
## 2181  FALSE
## 2183  FALSE
## 2187  FALSE
## 2189  FALSE
## 2190  FALSE
## 2191  FALSE
## 2192  FALSE
## 2193  FALSE
## 2196  FALSE
## 2197  FALSE
## 2198  FALSE
## 2199  FALSE
## 2202  FALSE
## 2205  FALSE
## 2211  FALSE
## 2212  FALSE
## 2213  FALSE
## 2215  FALSE
## 2219  FALSE
## 2220  FALSE
## 2223  FALSE
## 2227  FALSE
## 2233  FALSE
## 2236  FALSE
## 2239  FALSE
## 2246  FALSE
## 2249  FALSE
## 2251  FALSE
## 2254  FALSE
## 2273  FALSE
## 2279  FALSE
## 2296  FALSE
## 2302  FALSE
## 2309  FALSE
## 2310  FALSE
## 2311  FALSE
## 2316  FALSE
## 2317  FALSE
## 2326  FALSE
## 2331  FALSE
## 2335  FALSE
## 2343  FALSE
## 2345  FALSE
## 2350  FALSE
## 2351  FALSE
## 2355  FALSE
## 2356  FALSE
## 2358  FALSE
## 2359  FALSE
## 2361  FALSE
## 2362  FALSE
## 2363  FALSE
## 2364  FALSE
## 2366  FALSE
## 2368  FALSE
## 2372  FALSE
## 2373  FALSE
## 2374  FALSE
## 2377  FALSE
## 2378  FALSE
## 2379  FALSE
## 2380  FALSE
## 2386  FALSE
## 2389  FALSE
## 2390  FALSE
## 2397  FALSE
## 2398  FALSE
## 2404  FALSE
## 2405  FALSE
## 2409  FALSE
## 2410  FALSE
## 2413  FALSE
## 2418  FALSE
## 2420  FALSE
## 2421  FALSE
## 2422  FALSE
## 2426  FALSE
## 2428  FALSE
## 2429  FALSE
## 2432  FALSE
## 2438  FALSE
## 2439  FALSE
## 2441  FALSE
## 2443  FALSE
## 2448  FALSE
## 2453  FALSE
## 2461  FALSE
## 2472  FALSE
## 2475  FALSE
## 2485  FALSE
## 2487  FALSE
## 2495  FALSE
## 2499  FALSE
## 2503  FALSE
## 2507  FALSE
## 2508  FALSE
## 2579  FALSE
## 2591  FALSE
## 2615  FALSE
## 2622  FALSE
## 2624  FALSE
## 2640  FALSE
## 2650  FALSE
## 2670  FALSE
## 2713  FALSE
## 2763  FALSE
## 2831  FALSE
## 2832  FALSE
## 2833  FALSE
## 2837  FALSE
## 2839  FALSE
## 2840  FALSE
## 2844  FALSE
## 2847  FALSE
## 2848  FALSE
## 2854  FALSE
## 2857  FALSE
## 2859  FALSE
## 2861  FALSE
## 2862  FALSE
## 2863  FALSE
## 2864  FALSE
## 2867  FALSE
## 2872  FALSE
## 2878  FALSE
## 2879  FALSE
## 2885  FALSE
## 2886  FALSE
## 2889  FALSE
## 2891  FALSE
## 2897  FALSE
## 2898  FALSE
## 2901  FALSE
## 2902  FALSE
## 2905  FALSE
## 2917  FALSE
## 2918  FALSE
## 2924  FALSE
## 2926  FALSE
## 2932  FALSE
## 2938  FALSE
## 2940  FALSE
## 2946  FALSE
## 2947  FALSE
## 2958  FALSE
## 2959  FALSE
## 2979  FALSE
## 2981  FALSE
## 2987  FALSE
## 2988  FALSE
## 3003  FALSE
## 3025  FALSE
## 3028  FALSE
## 3030  FALSE
## 3044  FALSE
## 3046  FALSE
## 3062  FALSE
## 3064  FALSE
## 3068  FALSE
## 3070  FALSE
## 3075  FALSE
## 3078  FALSE
## 3080  FALSE
## 3081  FALSE
## 3082  FALSE
## 3083  FALSE
## 3085  FALSE
## 3086  FALSE
## 3087  FALSE
## 3088  FALSE
## 3089  FALSE
## 3090  FALSE
## 3091  FALSE
## 3092  FALSE
## 3093  FALSE
## 3094  FALSE
## 3096  FALSE
## 3097  FALSE
## 3098  FALSE
## 3100  FALSE
## 3104  FALSE
## 3105  FALSE
## 3107  FALSE
## 3108  FALSE
## 3109  FALSE
## 3110  FALSE
## 3111  FALSE
## 3115  FALSE
## 3120  FALSE
## 3121  FALSE
## 3122  FALSE
## 3125  FALSE
## 3128  FALSE
## 3129  FALSE
## 3131  FALSE
## 3132  FALSE
## 3133  FALSE
## 3143  FALSE
## 3146  FALSE
## 3149  FALSE
## 3150  FALSE
## 3156  FALSE
## 3157  FALSE
## 3158  FALSE
## 3165  FALSE
## 3166  FALSE
## 3169  FALSE
## 3170  FALSE
## 3173  FALSE
## 3180  FALSE
## 3181  FALSE
## 3182  FALSE
## 3190  FALSE
## 3194  FALSE
## 3200  FALSE
## 3202  FALSE
## 3203  FALSE
## 3204  FALSE
## 3205  FALSE
## 3207  FALSE
## 3209  FALSE
## 3211  FALSE
## 3212  FALSE
## 3215  FALSE
## 3216  FALSE
## 3218  FALSE
## 3221  FALSE
## 3222  FALSE
## 3224  FALSE
## 3225  FALSE
## 3226  FALSE
## 3236  FALSE
## 3241  FALSE
## 3249  FALSE
## 3253  FALSE
## 3254  FALSE
## 3255  FALSE
## 3264  FALSE
## 3265  FALSE
## 3268  FALSE
## 3281  FALSE
## 3283  FALSE
## 3284  FALSE
## 3286  FALSE
## 3287  FALSE
## 3288  FALSE
## 3292  FALSE
## 3300  FALSE
## 3301  FALSE
## 3302  FALSE
## 3303  FALSE
## 3304  FALSE
## 3305  FALSE
## 3307  FALSE
## 3308  FALSE
## 3309  FALSE
## 3310  FALSE
## 3315  FALSE
## 3316  FALSE
## 3317  FALSE
## 3318  FALSE
## 3319  FALSE
## 3321  FALSE
## 3326  FALSE
## 3332  FALSE
## 3333  FALSE
## 3337  FALSE
## 3339  FALSE
## 3341  FALSE
## 3342  FALSE
## 3343  FALSE
## 3349  FALSE
## 3350  FALSE
## 3353  FALSE
## 3354  FALSE
## 3355  FALSE
## 3359  FALSE
## 3360  FALSE
## 3362  FALSE
## 3369  FALSE
## 3370  FALSE
## 3373  FALSE
## 3374  FALSE
## 3394  FALSE
## 3397  FALSE
## 3406  FALSE
## 3407  FALSE
## 3415  FALSE
## 3421  FALSE
## 3439  FALSE
## 3449  FALSE
## 3464  FALSE
## 3524  FALSE
## 3531  FALSE
## 3534  FALSE
## 3535  FALSE
## 3537  FALSE
## 3538  FALSE
## 3539  FALSE
## 3541  FALSE
## 3542  FALSE
## 3543  FALSE
## 3545  FALSE
## 3546  FALSE
## 3547  FALSE
## 3548  FALSE
## 3552  FALSE
## 3553  FALSE
## 3554  FALSE
## 3555  FALSE
## 3559  FALSE
## 3560  FALSE
## 3563  FALSE
## 3566  FALSE
## 3570  FALSE
## 3571  FALSE
## 3572  FALSE
## 3573  FALSE
## 3579  FALSE
## 3580  FALSE
## 3582  FALSE
## 3588  FALSE
## 3589  FALSE
## 3590  FALSE
## 3591  FALSE
## 3605  FALSE
## 3612  FALSE
## 3615  FALSE
## 3616  FALSE
## 3618  FALSE
## 3619  FALSE
## 3621  FALSE
## 3622  FALSE
## 3623  FALSE
## 3624  FALSE
## 3625  FALSE
## 3626  FALSE
## 3627  FALSE
## 3633  FALSE
## 3634  FALSE
## 3635  FALSE
## 3639  FALSE
## 3642  FALSE
## 3644  FALSE
## 3645  FALSE
## 3646  FALSE
## 3648  FALSE
## 3650  FALSE
## 3652  FALSE
## 3653  FALSE
## 3654  FALSE
## 3660  FALSE
## 3662  FALSE
## 3668  FALSE
## 3674  FALSE
## 3675  FALSE
## 3676  FALSE
## 3677  FALSE
## 3678  FALSE
## 3680  FALSE
## 3683  FALSE
## 3684  FALSE
## 3685  FALSE
## 3689  FALSE
## 3690  FALSE
## 3694  FALSE
## 3698  FALSE
## 3701  FALSE
## 3703  FALSE
## 3707  FALSE
## 3708  FALSE
## 3712  FALSE
## 3713  FALSE
## 3716  FALSE
## 3717  FALSE
## 3718  FALSE
## 3722  FALSE
## 3725  FALSE
## 3727  FALSE
## 3730  FALSE
## 3736  FALSE
## 3737  FALSE
## 3740  FALSE
## 3741  FALSE
## 3742  FALSE
## 3756  FALSE
## 3757  FALSE
## 3759  FALSE
## 3760  FALSE
## 3765  FALSE
## 3768  FALSE
## 3769  FALSE
## 3775  FALSE
## 3777  FALSE
## 3790  FALSE
## 3792  FALSE
## 3794  FALSE
## 3795  FALSE
## 3810  FALSE
## 3817  FALSE
## 3823  FALSE
## 3829  FALSE
## 3830  FALSE
## 3840  FALSE
## 3843  FALSE
## 3847  FALSE
## 3852  FALSE
## 3856  FALSE
## 3860  FALSE
## 3862  FALSE
## 3863  FALSE
## 3867  FALSE
## 3874  FALSE
## 3877  FALSE
## 3879  FALSE
## 3885  FALSE
## 3890  FALSE
## 3891  FALSE
## 3893  FALSE
## 3894  FALSE
## 3895  FALSE
## 3898  FALSE
## 3916  FALSE
## 3917  FALSE
## 3918  FALSE
## 3919  FALSE
## 3920  FALSE
## 3922  FALSE
## 3924  FALSE
## 3925  FALSE
## 3930  FALSE
## 3932  FALSE
## 3934  FALSE
## 3935  FALSE
## 3939  FALSE
## 3940  FALSE
## 3943  FALSE
## 3944  FALSE
## 3946  FALSE
## 3947  FALSE
## 3948  FALSE
## 3950  FALSE
## 3951  FALSE
## 3952  FALSE
## 3955  FALSE
## 3963  FALSE
## 3965  FALSE
## 3967  FALSE
## 3969  FALSE
## 3970  FALSE
## 3971  FALSE
## 3974  FALSE
## 3978  FALSE
## 3979  FALSE
## 3983  FALSE
## 3995  FALSE
## 3996  FALSE
## 3998  FALSE
## 3999  FALSE
## 4014  FALSE
## 4016  FALSE
## 4020  FALSE
## 4025  FALSE
## 4030  FALSE
## 4034  FALSE
## 4044  FALSE
## 4056  FALSE
## 4062  FALSE
## 4071  FALSE
## 4072  FALSE
## 4091  FALSE
## 4097  FALSE
## 4113  FALSE
## 4131  FALSE
## 4153  FALSE
## 4165  FALSE
## 4166  FALSE
## 4167  FALSE
## 4169  FALSE
## 4170  FALSE
## 4171  FALSE
## 4172  FALSE
## 4173  FALSE
## 4174  FALSE
## 4175  FALSE
## 4176  FALSE
## 4178  FALSE
## 4179  FALSE
## 4180  FALSE
## 4181  FALSE
## 4182  FALSE
## 4184  FALSE
## 4186  FALSE
## 4188  FALSE
## 4192  FALSE
## 4193  FALSE
## 4196  FALSE
## 4201  FALSE
## 4202  FALSE
## 4204  FALSE
## 4212  FALSE
## 4215  FALSE
## 4217  FALSE
## 4224  FALSE
## 4225  FALSE
## 4226  FALSE
## 4235  FALSE
## 4238  FALSE
## 4241  FALSE
## 4245  FALSE
## 4246  FALSE
## 4250  FALSE
## 4256  FALSE
## 4263  FALSE
## 4271  FALSE
## 4272  FALSE
## 4277  FALSE
## 4284  FALSE
## 4285  FALSE
## 4287  FALSE
## 4292  FALSE
## 4296  FALSE
## 4299  FALSE
## 4301  FALSE
## 4302  FALSE
## 4303  FALSE
## 4305  FALSE
## 4306  FALSE
## 4308  FALSE
## 4311  FALSE
## 4312  FALSE
## 4313  FALSE
## 4314  FALSE
## 4315  FALSE
## 4316  FALSE
## 4317  FALSE
## 4319  FALSE
## 4321  FALSE
## 4326  FALSE
## 4327  FALSE
## 4329  FALSE
## 4335  FALSE
## 4336  FALSE
## 4340  FALSE
## 4348  FALSE
## 4349  FALSE
## 4352  FALSE
## 4355  FALSE
## 4360  FALSE
## 4361  FALSE
## 4363  FALSE
## 4365  FALSE
## 4368  FALSE
## 4370  FALSE
## 4371  FALSE
## 4372  FALSE
## 4377  FALSE
## 4380  FALSE
## 4381  FALSE
## 4383  FALSE
## 4384  FALSE
## 4391  FALSE
## 4392  FALSE
## 4409  FALSE
## 4425  FALSE
## 4426  FALSE
## 4429  FALSE
## 4435  FALSE
## 4439  FALSE
## 4441  FALSE
## 4448  FALSE
## 4450  FALSE
## 4467  FALSE
## 4478  FALSE
## 4485  FALSE
## 4486  FALSE
## 4487  FALSE
## 4488  FALSE
## 4490  FALSE
## 4491  FALSE
## 4492  FALSE
## 4495  FALSE
## 4496  FALSE
## 4497  FALSE
## 4498  FALSE
## 4500  FALSE
## 4501  FALSE
## 4506  FALSE
## 4512  FALSE
## 4513  FALSE
## 4514  FALSE
## 4515  FALSE
## 4517  FALSE
## 4524  FALSE
## 4525  FALSE
## 4531  FALSE
## 4532  FALSE
## 4533  FALSE
## 4535  FALSE
## 4542  FALSE
## 4549  FALSE
## 4552  FALSE
## 4559  FALSE
## 4562  FALSE
## 4566  FALSE
## 4570  FALSE
## 4572  FALSE
## 4573  FALSE
## 4574  FALSE
## 4575  FALSE
## 4585  FALSE
## 4589  FALSE
## 4591  FALSE
## 4592  FALSE
## 4596  FALSE
## 4601  FALSE
## 4605  FALSE
## 4607  FALSE
## 4612  FALSE
## 4613  FALSE
## 4617  FALSE
## 4629  FALSE
## 4631  FALSE
## 4634  FALSE
## 4652  FALSE
## 4657  FALSE
## 4664  FALSE
## 4671  FALSE
## 4672  FALSE
## 4687  FALSE
## 4694  FALSE
## 4697  FALSE
## 4700  FALSE
## 4712  FALSE
## 4713  FALSE
## 4714  FALSE
## 4715  FALSE
## 4717  FALSE
## 4719  FALSE
## 4720  FALSE
## 4721  FALSE
## 4722  FALSE
## 4725  FALSE
## 4726  FALSE
## 4727  FALSE
## 4728  FALSE
## 4731  FALSE
## 4732  FALSE
## 4733  FALSE
## 4734  FALSE
## 4736  FALSE
## 4737  FALSE
## 4738  FALSE
## 4739  FALSE
## 4741  FALSE
## 4745  FALSE
## 4746  FALSE
## 4749  FALSE
## 4750  FALSE
## 4751  FALSE
## 4757  FALSE
## 4758  FALSE
## 4759  FALSE
## 4760  FALSE
## 4761  FALSE
## 4766  FALSE
## 4771  FALSE
## 4775  FALSE
## 4778  FALSE
## 4779  FALSE
## 4788  FALSE
## 4790  FALSE
## 4793  FALSE
## 4798  FALSE
## 4802  FALSE
## 4803  FALSE
## 4807  FALSE
## 4819  FALSE
## 4820  FALSE
## 4825  FALSE
## 4829  FALSE
## 4831  FALSE
## 4838  FALSE
## 4839  FALSE
## 4842  FALSE
## 4846  FALSE
## 4849  FALSE
## 4850  FALSE
## 4851  FALSE
## 4853  FALSE
## 4854  FALSE
## 4856  FALSE
## 4858  FALSE
## 4859  FALSE
## 4870  FALSE
## 4877  FALSE
## 4878  FALSE
## 4880  FALSE
## 4884  FALSE
## 4891  FALSE
## 4892  FALSE
## 4893  FALSE
## 4895  FALSE
## 4896  FALSE
## 4898  FALSE
## 4907  FALSE
## 4913  FALSE
## 4914  FALSE
## 4916  FALSE
## 4918  FALSE
## 4919  FALSE
## 4933  FALSE
## 4939  FALSE
## 4949  FALSE
## 4950  FALSE
## 4951  FALSE
## 4952  FALSE
## 4953  FALSE
## 4955  FALSE
## 4958  FALSE
## 4959  FALSE
## 4960  FALSE
## 4961  FALSE
## 4962  FALSE
## 4964  FALSE
## 4965  FALSE
## 4966  FALSE
## 4967  FALSE
## 4968  FALSE
## 4971  FALSE
## 4972  FALSE
## 4973  FALSE
## 4974  FALSE
## 4976  FALSE
## 4978  FALSE
## 4979  FALSE
## 4987  FALSE
## 4989  FALSE
## 4993  FALSE
## 4994  FALSE
## 4995  FALSE
## 4997  FALSE
## 5012  FALSE
## 5014  FALSE
## 5016  FALSE
## 5017  FALSE
## 5018  FALSE
## 5019  FALSE
## 5021  FALSE
## 5024  FALSE
## 5026  FALSE
## 5028  FALSE
## 5042  FALSE
## 5045  FALSE
## 5050  FALSE
## 5052  FALSE
## 5058  FALSE
## 5062  FALSE
## 5063  FALSE
## 5064  FALSE
## 5067  FALSE
## 5076  FALSE
## 5078  FALSE
## 5079  FALSE
## 5088  FALSE
## 5095  FALSE
## 5100  FALSE
## 5101  FALSE
## 5102  FALSE
## 5103  FALSE
## 5105  FALSE
## 5107  FALSE
## 5110  FALSE
## 5111  FALSE
## 5114  FALSE
## 5118  FALSE
## 5119  FALSE
## 5120  FALSE
## 5123  FALSE
## 5124  FALSE
## 5125  FALSE
## 5127  FALSE
## 5134  FALSE
## 5135  FALSE
## 5139  FALSE
## 5154  FALSE
## 5156  FALSE
## 5162  FALSE
## 5170  FALSE
## 5178  FALSE
## 5181  FALSE
## 5184  FALSE
## 5202  FALSE
## 5206  FALSE
## 5209  FALSE
## 5226  FALSE
## 5231  FALSE
## 5237  FALSE
## 5242  FALSE
## 5254  FALSE
## 5278  FALSE
## 5279  FALSE
## 5280  FALSE
## 5316  FALSE
## 5329  FALSE
## 5334  FALSE
## 5352  FALSE
## 5353  FALSE
## 5355  FALSE
## 5356  FALSE
## 5360  FALSE
## 5362  FALSE
## 5363  FALSE
## 5367  FALSE
## 5371  FALSE
## 5382  FALSE
## 5385  FALSE
## 5394  FALSE
## 5398  FALSE
## 5407  FALSE
## 5409  FALSE
## 5411  FALSE
## 5412  FALSE
## 5417  FALSE
## 5433  FALSE
## 5454  FALSE
## 5460  FALSE
## 5467  FALSE
## 5468  FALSE
## 5469  FALSE
## 5470  FALSE
## 5472  FALSE
## 5474  FALSE
## 5477  FALSE
## 5478  FALSE
## 5479  FALSE
## 5480  FALSE
## 5481  FALSE
## 5483  FALSE
## 5485  FALSE
## 5486  FALSE
## 5489  FALSE
## 5490  FALSE
## 5493  FALSE
## 5498  FALSE
## 5502  FALSE
## 5506  FALSE
## 5508  FALSE
## 5509  FALSE
## 5511  FALSE
## 5512  FALSE
## 5514  FALSE
## 5515  FALSE
## 5523  FALSE
## 5525  FALSE
## 5530  FALSE
## 5540  FALSE
## 5544  FALSE
## 5546  FALSE
## 5547  FALSE
## 5548  FALSE
## 5550  FALSE
## 5552  FALSE
## 5554  FALSE
## 5556  FALSE
## 5566  FALSE
## 5573  FALSE
## 5574  FALSE
## 5582  FALSE
## 5587  FALSE
## 5603  FALSE
## 5611  FALSE
## 5624  FALSE
## 5625  FALSE
## 5628  FALSE
## 5631  FALSE
## 5632  FALSE
## 5665  FALSE
## 5687  FALSE
## 5688  FALSE
## 5690  FALSE
## 5691  FALSE
## 5692  FALSE
## 5693  FALSE
## 5695  FALSE
## 5703  FALSE
## 5704  FALSE
## 5705  FALSE
## 5707  FALSE
## 5708  FALSE
## 5717  FALSE
## 5724  FALSE
## 5726  FALSE
## 5727  FALSE
## 5728  FALSE
## 5731  FALSE
## 5732  FALSE
## 5733  FALSE
## 5735  FALSE
## 5738  FALSE
## 5749  FALSE
## 5750  FALSE
## 5756  FALSE
## 5763  FALSE
## 5764  FALSE
## 5765  FALSE
## 5767  FALSE
## 5768  FALSE
## 5769  FALSE
## 5770  FALSE
## 5772  FALSE
## 5781  FALSE
## 5799  FALSE
## 5801  FALSE
## 5807  FALSE
## 5817  FALSE
## 5826  FALSE
## 5827  FALSE
## 5828  FALSE
## 5829  FALSE
## 5830  FALSE
## 5832  FALSE
## 5834  FALSE
## 5835  FALSE
## 5836  FALSE
## 5838  FALSE
## 5839  FALSE
## 5841  FALSE
## 5842  FALSE
## 5845  FALSE
## 5851  FALSE
## 5853  FALSE
## 5855  FALSE
## 5859  FALSE
## 5860  FALSE
## 5864  FALSE
## 5866  FALSE
## 5867  FALSE
## 5869  FALSE
## 5870  FALSE
## 5874  FALSE
## 5875  FALSE
## 5880  FALSE
## 5881  FALSE
## 5886  FALSE
## 5888  FALSE
## 5892  FALSE
## 5894  FALSE
## 5897  FALSE
## 5898  FALSE
## 5900  FALSE
## 5915  FALSE
## 5916  FALSE
## 5917  FALSE
## 5918  FALSE
## 5932  FALSE
## 5934  FALSE
## 5936  FALSE
## 5939  FALSE
## 5940  FALSE
## 5944  FALSE
## 5948  FALSE
## 5949  FALSE
## 5954  FALSE
## 5976  FALSE
## 5982  FALSE
## 5988  FALSE
## 5989  FALSE
## 5997  FALSE
## 6005  FALSE
## 6009  FALSE
## 6016  FALSE
## 6025  FALSE
## 6026  FALSE
## 6060  FALSE
## 6071  FALSE
## 6075  FALSE
## 6088  FALSE
## 6097  FALSE
## 6104  FALSE
## 6116  FALSE
## 6121  FALSE
## 6122  FALSE
## 6123  FALSE
## 6125  FALSE
## 6126  FALSE
## 6127  FALSE
## 6129  FALSE
## 6135  FALSE
## 6136  FALSE
## 6138  FALSE
## 6139  FALSE
## 6141  FALSE
## 6146  FALSE
## 6148  FALSE
## 6152  FALSE
## 6154  FALSE
## 6156  FALSE
## 6158  FALSE
## 6161  FALSE
## 6165  FALSE
## 6166  FALSE
## 6172  FALSE
## 6173  FALSE
## 6178  FALSE
## 6181  FALSE
## 6187  FALSE
## 6190  FALSE
## 6191  FALSE
## 6197  FALSE
## 6210  FALSE
## 6221  FALSE
## 6231  FALSE
## 6235  FALSE
## 6242  FALSE
## 6250  FALSE
## 6253  FALSE
## 6254  FALSE
## 6257  FALSE
## 6269  FALSE
## 6271  FALSE
## 6272  FALSE
## 6276  FALSE
## 6278  FALSE
## 6280  FALSE
## 6283  FALSE
## 6290  FALSE
## 6292  FALSE
## 6294  FALSE
## 6295  FALSE
## 6296  FALSE
## 6298  FALSE
## 6301  FALSE
## 6302  FALSE
## 6303  FALSE
## 6306  FALSE
## 6307  FALSE
## 6308  FALSE
## 6315  FALSE
## 6316  FALSE
## 6322  FALSE
## 6327  FALSE
## 6331  FALSE
## 6333  FALSE
## 6336  FALSE
## 6339  FALSE
## 6351  FALSE
## 6352  FALSE
## 6353  FALSE
## 6357  FALSE
## 6361  FALSE
## 6366  FALSE
## 6367  FALSE
## 6368  FALSE
## 6371  FALSE
## 6373  FALSE
## 6379  FALSE
## 6386  FALSE
## 6403  FALSE
## 6405  FALSE
## 6406  FALSE
## 6429  FALSE
## 6432  FALSE
## 6439  FALSE
## 6441  FALSE
## 6442  FALSE
## 6443  FALSE
## 6445  FALSE
## 6446  FALSE
## 6447  FALSE
## 6449  FALSE
## 6450  FALSE
## 6451  FALSE
## 6453  FALSE
## 6456  FALSE
## 6459  FALSE
## 6460  FALSE
## 6463  FALSE
## 6464  FALSE
## 6465  FALSE
## 6467  FALSE
## 6469  FALSE
## 6472  FALSE
## 6475  FALSE
## 6483  FALSE
## 6484  FALSE
## 6487  FALSE
## 6496  FALSE
## 6498  FALSE
## 6501  FALSE
## 6504  FALSE
## 6507  FALSE
## 6508  FALSE
## 6509  FALSE
## 6512  FALSE
## 6513  FALSE
## 6518  FALSE
## 6522  FALSE
## 6523  FALSE
## 6526  FALSE
## 6533  FALSE
## 6538  FALSE
## 6546  FALSE
## 6551  FALSE
## 6557  FALSE
## 6573  FALSE
## 6577  FALSE
## 6578  FALSE
## 6581  FALSE
## 6584  FALSE
## 6586  FALSE
## 6603  FALSE
## 6604  FALSE
## 6605  FALSE
## 6606  FALSE
## 6618  FALSE
## 6626  FALSE
## 6630  FALSE
## 6636  FALSE
## 6638  FALSE
## 6641  FALSE
## 6643  FALSE
## 6653  FALSE
## 6659  FALSE
## 6665  FALSE
## 6666  FALSE
## 6667  FALSE
## 6668  FALSE
## 6669  FALSE
## 6670  FALSE
## 6671  FALSE
## 6672  FALSE
## 6676  FALSE
## 6677  FALSE
## 6678  FALSE
## 6681  FALSE
## 6683  FALSE
## 6686  FALSE
## 6688  FALSE
## 6689  FALSE
## 6690  FALSE
## 6691  FALSE
## 6693  FALSE
## 6694  FALSE
## 6699  FALSE
## 6702  FALSE
## 6709  FALSE
## 6711  FALSE
## 6713  FALSE
## 6714  FALSE
## 6717  FALSE
## 6721  FALSE
## 6724  FALSE
## 6725  FALSE
## 6728  FALSE
## 6736  FALSE
## 6738  FALSE
## 6742  FALSE
## 6744  FALSE
## 6745  FALSE
## 6746  FALSE
## 6747  FALSE
## 6748  FALSE
## 6749  FALSE
## 6751  FALSE
## 6759  FALSE
## 6772  FALSE
## 6774  FALSE
## 6777  FALSE
## 6781  FALSE
## 6788  FALSE
## 6789  FALSE
## 6799  FALSE
## 6800  FALSE
## 6801  FALSE
## 6802  FALSE
## 6810  FALSE
## 6812  FALSE
## 6817  FALSE
## 6819  FALSE
## 6838  FALSE
## 6842  FALSE
## 6896  FALSE
## 6933  FALSE
## 6947  FALSE
## 6948  FALSE
## 6949  FALSE
## 6950  FALSE
## 6951  FALSE
## 6952  FALSE
## 6953  FALSE
## 6956  FALSE
## 6957  FALSE
## 6960  FALSE
## 6962  FALSE
## 6965  FALSE
## 6967  FALSE
## 6968  FALSE
## 6969  FALSE
## 6970  FALSE
## 6971  FALSE
## 6973  FALSE
## 6975  FALSE
## 6976  FALSE
## 6981  FALSE
## 6984  FALSE
## 6986  FALSE
## 6991  FALSE
## 6992  FALSE
## 6995  FALSE
## 6997  FALSE
## 7000  FALSE
## 7006  FALSE
## 7007  FALSE
## 7008  FALSE
## 7009  FALSE
## 7013  FALSE
## 7014  FALSE
## 7017  FALSE
## 7023  FALSE
## 7029  FALSE
## 7033  FALSE
## 7045  FALSE
## 7051  FALSE
## 7053  FALSE
## 7056  FALSE
## 7057  FALSE
## 7058  FALSE
## 7059  FALSE
## 7061  FALSE
## 7065  FALSE
## 7067  FALSE
## 7078  FALSE
## 7079  FALSE
## 7081  FALSE
## 7082  FALSE
## 7092  FALSE
## 7093  FALSE
## 7094  FALSE
## 7095  FALSE
## 7096  FALSE
## 7098  FALSE
## 7100  FALSE
## 7101  FALSE
## 7102  FALSE
## 7106  FALSE
## 7108  FALSE
## 7109  FALSE
## 7110  FALSE
## 7111  FALSE
## 7113  FALSE
## 7114  FALSE
## 7118  FALSE
## 7120  FALSE
## 7121  FALSE
## 7122  FALSE
## 7124  FALSE
## 7127  FALSE
## 7129  FALSE
## 7130  FALSE
## 7131  FALSE
## 7132  FALSE
## 7138  FALSE
## 7139  FALSE
## 7141  FALSE
## 7142  FALSE
## 7144  FALSE
## 7146  FALSE
## 7148  FALSE
## 7153  FALSE
## 7156  FALSE
## 7158  FALSE
## 7162  FALSE
## 7165  FALSE
## 7166  FALSE
## 7168  FALSE
## 7169  FALSE
## 7175  FALSE
## 7189  FALSE
## 7224  FALSE
## 7225  FALSE
## 7226  FALSE
## 7227  FALSE
## 7230  FALSE
## 7231  FALSE
## 7232  FALSE
## 7233  FALSE
## 7234  FALSE
## 7235  FALSE
## 7236  FALSE
## 7238  FALSE
## 7239  FALSE
## 7241  FALSE
## 7250  FALSE
## 7251  FALSE
## 7252  FALSE
## 7255  FALSE
## 7256  FALSE
## 7258  FALSE
## 7261  FALSE
## 7263  FALSE
## 7264  FALSE
## 7266  FALSE
## 7267  FALSE
## 7268  FALSE
## 7272  FALSE
## 7284  FALSE
## 7290  FALSE
## 7291  FALSE
## 7293  FALSE
## 7296  FALSE
## 7299  FALSE
## 7302  FALSE
## 7304  FALSE
## 7308  FALSE
## 7312  FALSE
## 7316  FALSE
## 7322  FALSE
## 7323  FALSE
## 7324  FALSE
## 7333  FALSE
## 7350  FALSE
## 7354  FALSE
## 7359  FALSE
## 7366  FALSE
## 7378  FALSE
## 7380  FALSE
## 7381  FALSE
## 7382  FALSE
## 7383  FALSE
## 7384  FALSE
## 7385  FALSE
## 7386  FALSE
## 7387  FALSE
## 7389  FALSE
## 7391  FALSE
## 7398  FALSE
## 7399  FALSE
## 7400  FALSE
## 7401  FALSE
## 7403  FALSE
## 7405  FALSE
## 7406  FALSE
## 7407  FALSE
## 7408  FALSE
## 7409  FALSE
## 7410  FALSE
## 7411  FALSE
## 7412  FALSE
## 7413  FALSE
## 7414  FALSE
## 7415  FALSE
## 7425  FALSE
## 7426  FALSE
## 7431  FALSE
## 7432  FALSE
## 7435  FALSE
## 7436  FALSE
## 7437  FALSE
## 7438  FALSE
## 7446  FALSE
## 7451  FALSE
## 7458  FALSE
## 7461  FALSE
## 7467  FALSE
## 7471  FALSE
## 7474  FALSE
## 7476  FALSE
## 7480  FALSE
## 7485  FALSE
## 7486  FALSE
## 7487  FALSE
## 7494  FALSE
## 7495  FALSE
## 7500  FALSE
## 7503  FALSE
## 7504  FALSE
## 7506  FALSE
## 7508  FALSE
## 7511  FALSE
## 7512  FALSE
## 7513  FALSE
## 7519  FALSE
## 7520  FALSE
## 7545  FALSE
## 7547  FALSE
## 7552  FALSE
## 7561  FALSE
## 7592  FALSE
## 7593  FALSE
## 7594  FALSE
## 7595  FALSE
## 7615  FALSE
## 7621  FALSE
## 7644  FALSE
## 7647  FALSE
## 7656  FALSE
## 7678  FALSE
## 7702  FALSE
## 7711  FALSE
## 7723  FALSE
## 7727  FALSE
## 7750  FALSE
## 7752  FALSE
## 7760  FALSE
## 7768  FALSE
## 7796  FALSE
## 7797  FALSE
## 7798  FALSE
## 7799  FALSE
## 7801  FALSE
## 7802  FALSE
## 7803  FALSE
## 7804  FALSE
## 7805  FALSE
## 7807  FALSE
## 7809  FALSE
## 7810  FALSE
## 7816  FALSE
## 7818  FALSE
## 7819  FALSE
## 7822  FALSE
## 7826  FALSE
## 7827  FALSE
## 7829  FALSE
## 7832  FALSE
## 7835  FALSE
## 7837  FALSE
## 7844  FALSE
## 7845  FALSE
## 7846  FALSE
## 7847  FALSE
## 7848  FALSE
## 7852  FALSE
## 7855  FALSE
## 7858  FALSE
## 7888  FALSE
## 7901  FALSE
## 7903  FALSE
## 7926  FALSE
## 7930  FALSE
## 7933  FALSE
## 7935  FALSE
## 7939  FALSE
## 7950  FALSE
## 7956  FALSE
## 7959  FALSE
## 7963  FALSE
## 7964  FALSE
## 7965  FALSE
## 7976  FALSE
## 7983  FALSE
## 7994  FALSE
## 7995  FALSE
## 8007  FALSE
## 8023  FALSE
## 8026  FALSE
## 8035  FALSE
## 8044  FALSE
## 8046  FALSE
## 8068  FALSE
## 8081  FALSE
## 8123  FALSE
## 8135  FALSE
## 8142  FALSE
## 8144  FALSE
## 8146  FALSE
## 8147  FALSE
## 8150  FALSE
## 8153  FALSE
## 8155  FALSE
## 8166  FALSE
## 8194  FALSE
## 8196  FALSE
## 8197  FALSE
## 8198  FALSE
## 8210  FALSE
## 8216  FALSE
## 8237  FALSE
## 8239  FALSE
## 8242  FALSE
## 8247  FALSE
## 8249  FALSE
## 8251  FALSE
## 8252  FALSE
## 8253  FALSE
## 8255  FALSE
## 8256  FALSE
## 8260  FALSE
## 8264  FALSE
## 8268  FALSE
## 8269  FALSE
## 8271  FALSE
## 8272  FALSE
## 8280  FALSE
## 8282  FALSE
## 8283  FALSE
## 8284  FALSE
## 8286  FALSE
## 8288  FALSE
## 8293  FALSE
## 8300  FALSE
## 8309  FALSE
## 8311  FALSE
## 8314  FALSE
## 8322  FALSE
## 8323  FALSE
## 8325  FALSE
## 8330  FALSE
## 8335  FALSE
## 8342  FALSE
## 8346  FALSE
## 8348  FALSE
## 8349  FALSE
## 8351  FALSE
## 8354  FALSE
## 8358  FALSE
## 8359  FALSE
## 8360  FALSE
## 8362  FALSE
## 8363  FALSE
## 8364  FALSE
## 8366  FALSE
## 8367  FALSE
## 8370  FALSE
## 8374  FALSE
## 8379  FALSE
## 8381  FALSE
## 8384  FALSE
## 8387  FALSE
## 8389  FALSE
## 8399  FALSE
## 8404  FALSE
## 8407  FALSE
## 8408  FALSE
## 8409  FALSE
## 8410  FALSE
## 8411  FALSE
## 8417  FALSE
## 8422  FALSE
## 8426  FALSE
## 8435  FALSE
## 8442  FALSE
## 8443  FALSE
## 8449  FALSE
## 8454  FALSE
## 8462  FALSE
## 8469  FALSE
## 8473  FALSE
## 8475  FALSE
## 8483  FALSE
## 8496  FALSE
## 8501  FALSE
## 8504  FALSE
## 8515  FALSE
## 8518  FALSE
## 8520  FALSE
## 8522  FALSE
## 8524  FALSE
## 8534  FALSE
## 8544  FALSE
## 8587  FALSE
## 8590  FALSE
## 8597  FALSE
## 8622  FALSE
## 8632  FALSE
## 8633  FALSE
## 8634  FALSE
## 8641  FALSE
## 8675  FALSE
## 8710  FALSE
## 8717  FALSE
## 8736  FALSE
## 8741  FALSE
## 8746  FALSE
## 8751  FALSE
## 8757  FALSE
## 8763  FALSE
## 8784  FALSE
## 8790  FALSE
## 8803  FALSE
## 8806  FALSE
## 8808  FALSE
## 8811  FALSE
## 8812  FALSE
## 8817  FALSE
## 8822  FALSE
## 8824  FALSE
## 8825  FALSE
## 8826  FALSE
## 8828  FALSE
## 8835  FALSE
## 8836  FALSE
## 8838  FALSE
## 8842  FALSE
## 8843  FALSE
## 8845  FALSE
## 8846  FALSE
## 8850  FALSE
## 8856  FALSE
## 8860  FALSE
## 8862  FALSE
## 8863  FALSE
## 8866  FALSE
## 8867  FALSE
## 8868  FALSE
## 8872  FALSE
## 8877  FALSE
## 8878  FALSE
## 8880  FALSE
## 8883  FALSE
## 8884  FALSE
## 8892  FALSE
## 8895  FALSE
## 8900  FALSE
## 8902  FALSE
## 8903  FALSE
## 8905  FALSE
## 8910  FALSE
## 8915  FALSE
## 8920  FALSE
## 8924  FALSE
## 8925  FALSE
## 8930  FALSE
## 8931  FALSE
## 8934  FALSE
## 8940  FALSE
## 8942  FALSE
## 8966  FALSE
## 8967  FALSE
## 8970  FALSE
## 9000  FALSE
## 9001  FALSE
## 9002  FALSE
## 9003  FALSE
## 9007  FALSE
## 9012  FALSE
## 9022  FALSE
## 9023  FALSE
## 9030  FALSE
## 9035  FALSE
## 9037  FALSE
## 9043  FALSE
## 9044  FALSE
## 9047  FALSE
## 9054  FALSE
## 9055  FALSE
## 9063  FALSE
## 9074  FALSE
## 9076  FALSE
## 9080  FALSE
## 9083  FALSE
## 9086  FALSE
## 9087  FALSE
## 9092  FALSE
## 9100  FALSE
## 9101  FALSE
## 9105  FALSE
## 9108  FALSE
## 9110  FALSE
## 9116  FALSE
## 9128  FALSE
## 9130  FALSE
## 9133  FALSE
## 9139  FALSE
## 9141  FALSE
## 9143  FALSE
## 9146  FALSE
## 9147  FALSE
## 9148  FALSE
## 9149  FALSE
## 9150  FALSE
## 9151  FALSE
## 9153  FALSE
## 9157  FALSE
## 9158  FALSE
## 9167  FALSE
## 9170  FALSE
## 9173  FALSE
## 9175  FALSE
## 9178  FALSE
## 9179  FALSE
## 9181  FALSE
## 9211  FALSE
## 9273  FALSE
## 9275  FALSE
## 9292  FALSE
## 9305  FALSE
## 9307  FALSE
## 9321  FALSE
## 9342  FALSE
## 9368  FALSE
## 9381  FALSE
## 9412  FALSE
## 9414  FALSE
## 9420  FALSE
## 9445  FALSE
## 9490  FALSE
## 9506  FALSE
## 9553  FALSE
## 9554  FALSE
## 9555  FALSE
## 9556  FALSE
## 9557  FALSE
## 9558  FALSE
## 9559  FALSE
## 9560  FALSE
## 9561  FALSE
## 9563  FALSE
## 9564  FALSE
## 9566  FALSE
## 9567  FALSE
## 9569  FALSE
## 9570  FALSE
## 9571  FALSE
## 9572  FALSE
## 9575  FALSE
## 9576  FALSE
## 9577  FALSE
## 9581  FALSE
## 9583  FALSE
## 9584  FALSE
## 9585  FALSE
## 9586  FALSE
## 9595  FALSE
## 9596  FALSE
## 9598  FALSE
## 9599  FALSE
## 9603  FALSE
## 9610  FALSE
## 9614  FALSE
## 9615  FALSE
## 9622  FALSE
## 9625  FALSE
## 9626  FALSE
## 9627  FALSE
## 9636  FALSE
## 9642  FALSE
## 9643  FALSE
## 9659  FALSE
## 9669  FALSE
## 9675  FALSE
## 9676  FALSE
## 9678  FALSE
## 9679  FALSE
## 9680  FALSE
## 9681  FALSE
## 9682  FALSE
## 9683  FALSE
## 9685  FALSE
## 9689  FALSE
## 9690  FALSE
## 9699  FALSE
## 9700  FALSE
## 9704  FALSE
## 9707  FALSE
## 9715  FALSE
## 9729  FALSE
## 9732  FALSE
## 9738  FALSE
## 9740  FALSE
## 9745  FALSE
## 9746  FALSE
## 9747  FALSE
## 9748  FALSE
## 9752  FALSE
## 9753  FALSE
## 9754  FALSE
## 9759  FALSE
## 9760  FALSE
## 9761  FALSE
## 9762  FALSE
## 9766  FALSE
## 9767  FALSE
## 9771  FALSE
## 9772  FALSE
## 9776  FALSE
## 9777  FALSE
## 9779  FALSE
## 9781  FALSE
## 9784  FALSE
## 9788  FALSE
## 9796  FALSE
## 9797  FALSE
## 9799  FALSE
## 9805  FALSE
## 9811  FALSE
## 9817  FALSE
## 9820  FALSE
## 9825  FALSE
## 9828  FALSE
## 9829  FALSE
## 9837  FALSE
## 9850  FALSE
## 9855  FALSE
## 9874  FALSE
## 9877  FALSE
## 9885  FALSE
## 9886  FALSE
## 9887  FALSE
## 9890  FALSE
## 9891  FALSE
## 9892  FALSE
## 9895  FALSE
## 9896  FALSE
## 9898  FALSE
## 9899  FALSE
## 9900  FALSE
## 9903  FALSE
## 9904  FALSE
## 9906  FALSE
## 9907  FALSE
## 9908  FALSE
## 9909  FALSE
## 9912  FALSE
## 9920  FALSE
## 9922  FALSE
## 9924  FALSE
## 9925  FALSE
## 9926  FALSE
## 9930  FALSE
## 9932  FALSE
## 9934  FALSE
## 9937  FALSE
## 9938  FALSE
## 9947  FALSE
## 9957  FALSE
## 9958  FALSE
## 9961  FALSE
## 9965  FALSE
## 9967  FALSE
## 9980  FALSE
## 9983  FALSE
## 9985  FALSE
## 9988  FALSE
## 9998  FALSE
## 10001 FALSE
## 10010 FALSE
## 10017 FALSE
## 10021 FALSE
## 10029 FALSE
## 10033 FALSE
## 10038 FALSE
## 10057 FALSE
## 10062 FALSE
## 10065 FALSE
## 10069 FALSE
## 10075 FALSE
## 10077 FALSE
## 10086 FALSE
## 10092 FALSE
## 10102 FALSE
## 10106 FALSE
## 10107 FALSE
## 10135 FALSE
## 10136 FALSE
## 10137 FALSE
## 10138 FALSE
## 10139 FALSE
## 10140 FALSE
## 10141 FALSE
## 10142 FALSE
## 10143 FALSE
## 10146 FALSE
## 10148 FALSE
## 10149 FALSE
## 10151 FALSE
## 10153 FALSE
## 10154 FALSE
## 10155 FALSE
## 10156 FALSE
## 10157 FALSE
## 10163 FALSE
## 10167 FALSE
## 10168 FALSE
## 10169 FALSE
## 10170 FALSE
## 10172 FALSE
## 10176 FALSE
## 10177 FALSE
## 10178 FALSE
## 10180 FALSE
## 10196 FALSE
## 10198 FALSE
## 10203 FALSE
## 10211 FALSE
## 10225 FALSE
## 10228 FALSE
## 10239 FALSE
## 10242 FALSE
## 10245 FALSE
## 10249 FALSE
## 10255 FALSE
## 10264 FALSE
## 10265 FALSE
## 10273 FALSE
## 10277 FALSE
## 10290 FALSE
## 10294 FALSE
## 10295 FALSE
## 10296 FALSE
## 10297 FALSE
## 10298 FALSE
## 10300 FALSE
## 10301 FALSE
## 10302 FALSE
## 10303 FALSE
## 10304 FALSE
## 10305 FALSE
## 10307 FALSE
## 10309 FALSE
## 10310 FALSE
## 10311 FALSE
## 10312 FALSE
## 10313 FALSE
## 10319 FALSE
## 10320 FALSE
## 10321 FALSE
## 10322 FALSE
## 10324 FALSE
## 10326 FALSE
## 10330 FALSE
## 10332 FALSE
## 10335 FALSE
## 10336 FALSE
## 10338 FALSE
## 10339 FALSE
## 10343 FALSE
## 10344 FALSE
## 10345 FALSE
## 10347 FALSE
## 10348 FALSE
## 10349 FALSE
## 10350 FALSE
## 10352 FALSE
## 10354 FALSE
## 10356 FALSE
## 10357 FALSE
## 10359 FALSE
## 10360 FALSE
## 10362 FALSE
## 10375 FALSE
## 10379 FALSE
## 10382 FALSE
## 10392 FALSE
## 10409 FALSE
## 10421 FALSE
## 10425 FALSE
## 10436 FALSE
## 10437 FALSE
## 10438 FALSE
## 10439 FALSE
## 10440 FALSE
## 10442 FALSE
## 10443 FALSE
## 10444 FALSE
## 10445 FALSE
## 10448 FALSE
## 10449 FALSE
## 10450 FALSE
## 10451 FALSE
## 10452 FALSE
## 10456 FALSE
## 10458 FALSE
## 10461 FALSE
## 10465 FALSE
## 10466 FALSE
## 10467 FALSE
## 10468 FALSE
## 10470 FALSE
## 10471 FALSE
## 10475 FALSE
## 10476 FALSE
## 10477 FALSE
## 10478 FALSE
## 10480 FALSE
## 10481 FALSE
## 10482 FALSE
## 10487 FALSE
## 10488 FALSE
## 10493 FALSE
## 10496 FALSE
## 10499 FALSE
## 10500 FALSE
## 10516 FALSE
## 10517 FALSE
## 10518 FALSE
## 10520 FALSE
## 10521 FALSE
## 10522 FALSE
## 10526 FALSE
## 10534 FALSE
## 10551 FALSE
## 10558 FALSE
## 10560 FALSE
## 10597 FALSE
## 10598 FALSE
## 10600 FALSE
## 10604 FALSE
## 10609 FALSE
## 10610 FALSE
## 10633 FALSE
## 10637 FALSE
## 10641 FALSE
## 10645 FALSE
## 10675 FALSE
## 10684 FALSE
## 10688 FALSE
## 10694 FALSE
## 10701 FALSE
## 10702 FALSE
## 10703 FALSE
## 10704 FALSE
## 10710 FALSE
## 10711 FALSE
## 10712 FALSE
## 10719 FALSE
## 10720 FALSE
## 10722 FALSE
## 10723 FALSE
## 10725 FALSE
## 10728 FALSE
## 10729 FALSE
## 10731 FALSE
## 10734 FALSE
## 10737 FALSE
## 10738 FALSE
## 10739 FALSE
## 10743 FALSE
## 10750 FALSE
## 10760 FALSE
## 10765 FALSE
## 10767 FALSE
## 10769 FALSE
## 10771 FALSE
## 10773 FALSE
## 10781 FALSE
## 10783 FALSE
## 10789 FALSE
## 10794 FALSE
## 10800 FALSE
## 10801 FALSE
## 10802 FALSE
## 10804 FALSE
## 10805 FALSE
## 10811 FALSE
## 10828 FALSE
## 10840 FALSE
## 10853 FALSE
## 10854 FALSE
## 10860 FALSE
## 10861 FALSE
## 10862 FALSE
## 10865 FALSE
## 10875 FALSE
## 10894 FALSE
## 10897 FALSE
## 10898 FALSE
## 10900 FALSE
## 10916 FALSE
## 10938 FALSE
## 10948 FALSE
## 10951 FALSE
## 10955 FALSE
## 10976 FALSE
## 10990 FALSE
## 10995 FALSE
## 11000 FALSE
## 11008 FALSE
## 11009 FALSE
## 11032 FALSE
## 11033 FALSE
## 11035 FALSE
## 11036 FALSE
## 11037 FALSE
## 11038 FALSE
## 11039 FALSE
## 11040 FALSE
## 11041 FALSE
## 11042 FALSE
## 11043 FALSE
## 11045 FALSE
## 11046 FALSE
## 11047 FALSE
## 11058 FALSE
## 11059 FALSE
## 11061 FALSE
## 11062 FALSE
## 11063 FALSE
## 11064 FALSE
## 11065 FALSE
## 11068 FALSE
## 11069 FALSE
## 11070 FALSE
## 11071 FALSE
## 11072 FALSE
## 11073 FALSE
## 11075 FALSE
## 11078 FALSE
## 11082 FALSE
## 11090 FALSE
## 11092 FALSE
## 11096 FALSE
## 11099 FALSE
## 11100 FALSE
## 11102 FALSE
## 11106 FALSE
## 11109 FALSE
## 11112 FALSE
## 11113 FALSE
## 11114 FALSE
## 11119 FALSE
## 11120 FALSE
## 11122 FALSE
## 11123 FALSE
## 11125 FALSE
## 11127 FALSE
## 11128 FALSE
## 11129 FALSE
## 11132 FALSE
## 11133 FALSE
## 11134 FALSE
## 11135 FALSE
## 11137 FALSE
## 11138 FALSE
## 11140 FALSE
## 11141 FALSE
## 11146 FALSE
## 11147 FALSE
## 11150 FALSE
## 11152 FALSE
## 11154 FALSE
## 11158 FALSE
## 11165 FALSE
## 11166 FALSE
## 11168 FALSE
## 11184 FALSE
## 11188 FALSE
## 11189 FALSE
## 11202 FALSE
## 11218 FALSE
## 11222 FALSE
## 11226 FALSE
## 11229 FALSE
## 11230 FALSE
## 11237 FALSE
## 11251 FALSE
## 11260 FALSE
## 11283 FALSE
## 11295 FALSE
## 11297 FALSE
## 11301 FALSE
## 11340 FALSE
## 11345 FALSE
## 11349 FALSE
## 11355 FALSE
## 11390 FALSE
## 11419 FALSE
## 11422 FALSE
## 11438 FALSE
## 11452 FALSE
## 11455 FALSE
## 11469 FALSE
## 11494 FALSE
## 11510 FALSE
## 11530 FALSE
## 11533 FALSE
## 11534 FALSE
## 11535 FALSE
## 11536 FALSE
## 11538 FALSE
## 11541 FALSE
## 11543 FALSE
## 11548 FALSE
## 11549 FALSE
## 11550 FALSE
## 11553 FALSE
## 11554 FALSE
## 11555 FALSE
## 11556 FALSE
## 11559 FALSE
## 11560 FALSE
## 11561 FALSE
## 11562 FALSE
## 11563 FALSE
## 11564 FALSE
## 11566 FALSE
## 11567 FALSE
## 11569 FALSE
## 11570 FALSE
## 11571 FALSE
## 11575 FALSE
## 11578 FALSE
## 11584 FALSE
## 11587 FALSE
## 11588 FALSE
## 11590 FALSE
## 11592 FALSE
## 11593 FALSE
## 11594 FALSE
## 11596 FALSE
## 11598 FALSE
## 11601 FALSE
## 11602 FALSE
## 11607 FALSE
## 11608 FALSE
## 11610 FALSE
## 11611 FALSE
## 11615 FALSE
## 11616 FALSE
## 11617 FALSE
## 11626 FALSE
## 11633 FALSE
## 11635 FALSE
## 11643 FALSE
## 11659 FALSE
## 11661 FALSE
## 11664 FALSE
## 11666 FALSE
## 11675 FALSE
## 11676 FALSE
## 11679 FALSE
## 11680 FALSE
## 11683 FALSE
## 11686 FALSE
## 11699 FALSE
## 11708 FALSE
## 11714 FALSE
## 11724 FALSE
## 11727 FALSE
## 11735 FALSE
## 11754 FALSE
## 11776 FALSE
## 11785 FALSE
## 11790 FALSE
## 11801 FALSE
## 11806 FALSE
## 11836 FALSE
## 11840 FALSE
## 11841 FALSE
## 11842 FALSE
## 11845 FALSE
## 11846 FALSE
## 11848 FALSE
## 11850 FALSE
## 11851 FALSE
## 11852 FALSE
## 11855 FALSE
## 11856 FALSE
## 11865 FALSE
## 11870 FALSE
## 11872 FALSE
## 11873 FALSE
## 11874 FALSE
## 11876 FALSE
## 11877 FALSE
## 11878 FALSE
## 11882 FALSE
## 11884 FALSE
## 11890 FALSE
## 11896 FALSE
## 11899 FALSE
## 11903 FALSE
## 11912 FALSE
## 11914 FALSE
## 11919 FALSE
## 11922 FALSE
## 11925 FALSE
## 11932 FALSE
## 11935 FALSE
## 11939 FALSE
## 11940 FALSE
## 11943 FALSE
## 11948 FALSE
## 11956 FALSE
## 11959 FALSE
## 11960 FALSE
## 11968 FALSE
## 11973 FALSE
## 11978 FALSE
## 11979 FALSE
## 11983 FALSE
## 11987 FALSE
## 11990 FALSE
## 11998 FALSE
## 11999 FALSE
## 12010 FALSE
## 12024 FALSE
## 12030 FALSE
## 12031 FALSE
## 12053 FALSE
## 12056 FALSE
## 12060 FALSE
## 12068 FALSE
## 12069 FALSE
## 12077 FALSE
## 12078 FALSE
## 12079 FALSE
## 12086 FALSE
## 12088 FALSE
## 12090 FALSE
## 12094 FALSE
## 12095 FALSE
## 12096 FALSE
## 12100 FALSE
## 12105 FALSE
## 12106 FALSE
## 12108 FALSE
## 12109 FALSE
## 12111 FALSE
## 12114 FALSE
## 12115 FALSE
## 12116 FALSE
## 12118 FALSE
## 12120 FALSE
## 12123 FALSE
## 12125 FALSE
## 12126 FALSE
## 12128 FALSE
## 12129 FALSE
## 12131 FALSE
## 12133 FALSE
## 12134 FALSE
## 12135 FALSE
## 12143 FALSE
## 12147 FALSE
## 12148 FALSE
## 12151 FALSE
## 12152 FALSE
## 12163 FALSE
## 12166 FALSE
## 12168 FALSE
## 12170 FALSE
## 12172 FALSE
## 12188 FALSE
## 12191 FALSE
## 12192 FALSE
## 12198 FALSE
## 12199 FALSE
## 12214 FALSE
## 12220 FALSE
## 12238 FALSE
## 12240 FALSE
## 12268 FALSE
## 12269 FALSE
## 12270 FALSE
## 12271 FALSE
## 12272 FALSE
## 12274 FALSE
## 12277 FALSE
## 12279 FALSE
## 12284 FALSE
## 12285 FALSE
## 12291 FALSE
## 12292 FALSE
## 12294 FALSE
## 12298 FALSE
## 12299 FALSE
## 12300 FALSE
## 12309 FALSE
## 12310 FALSE
## 12315 FALSE
## 12317 FALSE
## 12324 FALSE
## 12339 FALSE
## 12340 FALSE
## 12344 FALSE
## 12348 FALSE
## 12349 FALSE
## 12356 FALSE
## 12357 FALSE
## 12359 FALSE
## 12364 FALSE
## 12368 FALSE
## 12374 FALSE
## 12383 FALSE
## 12386 FALSE
## 12387 FALSE
## 12395 FALSE
## 12398 FALSE
## 12403 FALSE
## 12410 FALSE
## 12411 FALSE
## 12412 FALSE
## 12413 FALSE
## 12414 FALSE
## 12415 FALSE
## 12416 FALSE
## 12417 FALSE
## 12421 FALSE
## 12422 FALSE
## 12423 FALSE
## 12424 FALSE
## 12425 FALSE
## 12429 FALSE
## 12431 FALSE
## 12432 FALSE
## 12434 FALSE
## 12435 FALSE
## 12436 FALSE
## 12439 FALSE
## 12444 FALSE
## 12447 FALSE
## 12450 FALSE
## 12456 FALSE
## 12457 FALSE
## 12459 FALSE
## 12461 FALSE
## 12463 FALSE
## 12464 FALSE
## 12465 FALSE
## 12472 FALSE
## 12474 FALSE
## 12475 FALSE
## 12476 FALSE
## 12477 FALSE
## 12480 FALSE
## 12482 FALSE
## 12487 FALSE
## 12488 FALSE
## 12489 FALSE
## 12492 FALSE
## 12494 FALSE
## 12495 FALSE
## 12496 FALSE
## 12497 FALSE
## 12498 FALSE
## 12499 FALSE
## 12502 FALSE
## 12503 FALSE
## 12505 FALSE
## 12508 FALSE
## 12514 FALSE
## 12515 FALSE
## 12521 FALSE
## 12522 FALSE
## 12535 FALSE
## 12537 FALSE
## 12539 FALSE
## 12544 FALSE
## 12545 FALSE
## 12549 FALSE
## 12554 FALSE
## 12562 FALSE
## 12563 FALSE
## 12564 FALSE
## 12565 FALSE
## 12566 FALSE
## 12569 FALSE
## 12570 FALSE
## 12571 FALSE
## 12572 FALSE
## 12573 FALSE
## 12575 FALSE
## 12577 FALSE
## 12579 FALSE
## 12581 FALSE
## 12584 FALSE
## 12585 FALSE
## 12588 FALSE
## 12589 FALSE
## 12591 FALSE
## 12592 FALSE
## 12593 FALSE
## 12594 FALSE
## 12599 FALSE
## 12600 FALSE
## 12603 FALSE
## 12605 FALSE
## 12609 FALSE
## 12610 FALSE
## 12613 FALSE
## 12615 FALSE
## 12618 FALSE
## 12621 FALSE
## 12622 FALSE
## 12629 FALSE
## 12630 FALSE
## 12635 FALSE
## 12639 FALSE
## 12645 FALSE
## 12650 FALSE
## 12651 FALSE
## 12668 FALSE
## 12669 FALSE
## 12672 FALSE
## 12675 FALSE
## 12676 FALSE
## 12683 FALSE
## 12702 FALSE
## 12703 FALSE
## 12709 FALSE
## 12712 FALSE
## 12713 FALSE
## 12714 FALSE
## 12720 FALSE
## 12724 FALSE
## 12727 FALSE
## 12729 FALSE
## 12734 FALSE
## 12738 FALSE
## 12739 FALSE
## 12759 FALSE
## 12767 FALSE
## 12774 FALSE
## 12791 FALSE
## 12804 FALSE
## 12826 FALSE
## 12828 FALSE
## 12859 FALSE
## 12879 FALSE
## 12880 FALSE
## 12899 FALSE
## 12900 FALSE
## 12903 FALSE
## 12906 FALSE
## 12907 FALSE
## 12910 FALSE
## 12911 FALSE
## 12912 FALSE
## 12914 FALSE
## 12915 FALSE
## 12917 FALSE
## 12918 FALSE
## 12920 FALSE
## 12923 FALSE
## 12924 FALSE
## 12925 FALSE
## 12927 FALSE
## 12928 FALSE
## 12938 FALSE
## 12939 FALSE
## 12941 FALSE
## 12943 FALSE
## 12947 FALSE
## 12958 FALSE
## 12959 FALSE
## 12961 FALSE
## 12962 FALSE
## 12964 FALSE
## 12967 FALSE
## 12972 FALSE
## 12975 FALSE
## 12976 FALSE
## 12981 FALSE
## 12987 FALSE
## 12989 FALSE
## 12993 FALSE
## 12995 FALSE
## 13006 FALSE
## 13011 FALSE
## 13014 FALSE
## 13018 FALSE
## 13019 FALSE
## 13020 FALSE
## 13033 FALSE
## 13038 FALSE
## 13041 FALSE
## 13045 FALSE
## 13053 FALSE
## 13054 FALSE
## 13056 FALSE
## 13066 FALSE
## 13069 FALSE
## 13071 FALSE
## 13073 FALSE
## 13076 FALSE
## 13077 FALSE
## 13079 FALSE
## 13083 FALSE
## 13085 FALSE
## 13092 FALSE
## 13094 FALSE
## 13095 FALSE
## 13098 FALSE
## 13103 FALSE
## 13105 FALSE
## 13106 FALSE
## 13117 FALSE
## 13121 FALSE
## 13122 FALSE
## 13127 FALSE
## 13133 FALSE
## 13135 FALSE
## 13136 FALSE
## 13149 FALSE
## 13151 FALSE
## 13160 FALSE
## 13161 FALSE
## 13167 FALSE
## 13181 FALSE
## 13191 FALSE
## 13192 FALSE
## 13193 FALSE
## 13197 FALSE
## 13229 FALSE
## 13248 FALSE
## 13267 FALSE
## 13276 FALSE
## 13279 FALSE
## 13284 FALSE
## 13285 FALSE
## 13286 FALSE
## 13288 FALSE
## 13289 FALSE
## 13291 FALSE
## 13292 FALSE
## 13293 FALSE
## 13295 FALSE
## 13297 FALSE
## 13298 FALSE
## 13301 FALSE
## 13302 FALSE
## 13305 FALSE
## 13306 FALSE
## 13307 FALSE
## 13308 FALSE
## 13309 FALSE
## 13310 FALSE
## 13313 FALSE
## 13314 FALSE
## 13315 FALSE
## 13316 FALSE
## 13322 FALSE
## 13323 FALSE
## 13324 FALSE
## 13328 FALSE
## 13330 FALSE
## 13336 FALSE
## 13337 FALSE
## 13339 FALSE
## 13341 FALSE
## 13350 FALSE
## 13352 FALSE
## 13354 FALSE
## 13357 FALSE
## 13361 FALSE
## 13368 FALSE
## 13371 FALSE
## 13374 FALSE
## 13382 FALSE
## 13385 FALSE
## 13391 FALSE
## 13402 FALSE
## 13413 FALSE
## 13423 FALSE
## 13427 FALSE
## 13430 FALSE
## 13435 FALSE
## 13436 FALSE
## 13437 FALSE
## 13438 FALSE
## 13439 FALSE
## 13440 FALSE
## 13442 FALSE
## 13446 FALSE
## 13447 FALSE
## 13450 FALSE
## 13451 FALSE
## 13453 FALSE
## 13458 FALSE
## 13460 FALSE
## 13462 FALSE
## 13463 FALSE
## 13470 FALSE
## 13476 FALSE
## 13479 FALSE
## 13483 FALSE
## 13486 FALSE
## 13488 FALSE
## 13489 FALSE
## 13493 FALSE
## 13494 FALSE
## 13497 FALSE
## 13502 FALSE
## 13512 FALSE
## 13517 FALSE
## 13518 FALSE
## 13525 FALSE
## 13528 FALSE
## 13531 FALSE
## 13553 FALSE
## 13554 FALSE
## 13555 FALSE
## 13561 FALSE
## 13562 FALSE
## 13565 FALSE
## 13573 FALSE
## 13577 FALSE
## 13580 FALSE
## 13583 FALSE
## 13591 FALSE
## 13594 FALSE
## 13599 FALSE
## 13602 FALSE
## 13607 FALSE
## 13610 FALSE
## 13611 FALSE
## 13614 FALSE
## 13620 FALSE
## 13625 FALSE
## 13628 FALSE
## 13629 FALSE
## 13630 FALSE
## 13631 FALSE
## 13632 FALSE
## 13634 FALSE
## 13635 FALSE
## 13640 FALSE
## 13641 FALSE
## 13643 FALSE
## 13646 FALSE
## 13653 FALSE
## 13655 FALSE
## 13656 FALSE
## 13658 FALSE
## 13659 FALSE
## 13660 FALSE
## 13668 FALSE
## 13669 FALSE
## 13670 FALSE
## 13678 FALSE
## 13690 FALSE
## 13691 FALSE
## 13692 FALSE
## 13697 FALSE
## 13703 FALSE
## 13704 FALSE
## 13705 FALSE
## 13709 FALSE
## 13710 FALSE
## 13718 FALSE
## 13722 FALSE
## 13728 FALSE
## 13731 FALSE
## 13732 FALSE
## 13743 FALSE
## 13744 FALSE
## 13745 FALSE
## 13754 FALSE
## 13755 FALSE
## 13769 FALSE
## 13809 FALSE
## 13810 FALSE
## 13811 FALSE
## 13812 FALSE
## 13814 FALSE
## 13817 FALSE
## 13822 FALSE
## 13823 FALSE
## 13826 FALSE
## 13830 FALSE
## 13831 FALSE
## 13832 FALSE
## 13833 FALSE
## 13834 FALSE
## 13835 FALSE
## 13837 FALSE
## 13838 FALSE
## 13839 FALSE
## 13840 FALSE
## 13841 FALSE
## 13844 FALSE
## 13845 FALSE
## 13846 FALSE
## 13847 FALSE
## 13851 FALSE
## 13854 FALSE
## 13855 FALSE
## 13859 FALSE
## 13861 FALSE
## 13867 FALSE
## 13873 FALSE
## 13881 FALSE
## 13882 FALSE
## 13886 FALSE
## 13888 FALSE
## 13889 FALSE
## 13892 FALSE
## 13895 FALSE
## 13896 FALSE
## 13898 FALSE
## 13899 FALSE
## 13901 FALSE
## 13902 FALSE
## 13906 FALSE
## 13909 FALSE
## 13913 FALSE
## 13917 FALSE
## 13919 FALSE
## 13921 FALSE
## 13922 FALSE
## 13923 FALSE
## 13924 FALSE
## 13925 FALSE
## 13926 FALSE
## 13927 FALSE
## 13930 FALSE
## 13934 FALSE
## 13939 FALSE
## 13942 FALSE
## 13943 FALSE
## 13945 FALSE
## 13946 FALSE
## 13947 FALSE
## 13948 FALSE
## 13951 FALSE
## 13953 FALSE
## 13964 FALSE
## 13965 FALSE
## 13966 FALSE
## 13974 FALSE
## 13976 FALSE
## 13979 FALSE
## 13982 FALSE
## 13992 FALSE
## 13993 FALSE
## 13999 FALSE
## 14003 FALSE
## 14004 FALSE
## 14006 FALSE
## 14011 FALSE
## 14015 FALSE
## 14029 FALSE
## 14042 FALSE
## 14043 FALSE
## 14052 FALSE
## 14057 FALSE
## 14068 FALSE
## 14071 FALSE
## 14082 FALSE
## 14088 FALSE
## 14093 FALSE
## 14094 FALSE
## 14103 FALSE
## 14116 FALSE
## 14117 FALSE
## 14136 FALSE
## 14143 FALSE
## 14152 FALSE
## 14153 FALSE
## 14154 FALSE
## 14155 FALSE
## 14156 FALSE
## 14157 FALSE
## 14158 FALSE
## 14159 FALSE
## 14162 FALSE
## 14163 FALSE
## 14167 FALSE
## 14168 FALSE
## 14169 FALSE
## 14170 FALSE
## 14174 FALSE
## 14176 FALSE
## 14178 FALSE
## 14179 FALSE
## 14182 FALSE
## 14183 FALSE
## 14187 FALSE
## 14189 FALSE
## 14190 FALSE
## 14195 FALSE
## 14198 FALSE
## 14200 FALSE
## 14208 FALSE
## 14210 FALSE
## 14213 FALSE
## 14214 FALSE
## 14215 FALSE
## 14225 FALSE
## 14230 FALSE
## 14232 FALSE
## 14233 FALSE
## 14236 FALSE
## 14250 FALSE
## 14262 FALSE
## 14275 FALSE
## 14276 FALSE
## 14285 FALSE
## 14286 FALSE
## 14297 FALSE
## 14298 FALSE
## 14299 FALSE
## 14301 FALSE
## 14308 FALSE
## 14310 FALSE
## 14311 FALSE
## 14326 FALSE
## 14337 FALSE
## 14338 FALSE
## 14340 FALSE
## 14342 FALSE
## 14343 FALSE
## 14345 FALSE
## 14357 FALSE
## 14358 FALSE
## 14360 FALSE
## 14363 FALSE
## 14364 FALSE
## 14368 FALSE
## 14380 FALSE
## 14381 FALSE
## 14382 FALSE
## 14383 FALSE
## 14384 FALSE
## 14387 FALSE
## 14388 FALSE
## 14389 FALSE
## 14399 FALSE
## 14400 FALSE
## 14401 FALSE
## 14403 FALSE
## 14406 FALSE
## 14415 FALSE
## 14416 FALSE
## 14425 FALSE
## 14430 FALSE
## 14431 FALSE
## 14432 FALSE
## 14433 FALSE
## 14439 FALSE
## 14452 FALSE
## 14481 FALSE
## 14483 FALSE
## 14485 FALSE
## 14503 FALSE
## 14518 FALSE
## 14524 FALSE
## 14532 FALSE
## 14534 FALSE
## 14541 FALSE
## 14551 FALSE
## 14567 FALSE
## 14568 FALSE
## 14569 FALSE
## 14571 FALSE
## 14578 FALSE
## 14583 FALSE
## 14604 FALSE
## 14622 FALSE
## 14624 FALSE
## 14625 FALSE
## 14626 FALSE
## 14627 FALSE
## 14628 FALSE
## 14629 FALSE
## 14633 FALSE
## 14635 FALSE
## 14639 FALSE
## 14640 FALSE
## 14641 FALSE
## 14646 FALSE
## 14648 FALSE
## 14652 FALSE
## 14654 FALSE
## 14655 FALSE
## 14658 FALSE
## 14663 FALSE
## 14664 FALSE
## 14665 FALSE
## 14666 FALSE
## 14667 FALSE
## 14668 FALSE
## 14670 FALSE
## 14674 FALSE
## 14676 FALSE
## 14678 FALSE
## 14682 FALSE
## 14684 FALSE
## 14689 FALSE
## 14698 FALSE
## 14709 FALSE
## 14710 FALSE
## 14712 FALSE
## 14713 FALSE
## 14715 FALSE
## 14716 FALSE
## 14717 FALSE
## 14718 FALSE
## 14720 FALSE
## 14721 FALSE
## 14723 FALSE
## 14728 FALSE
## 14730 FALSE
## 14743 FALSE
## 14748 FALSE
## 14760 FALSE
## 14775 FALSE
## 14779 FALSE
## 14790 FALSE
## 14806 FALSE
## 14809 FALSE
## 14813 FALSE
## 14816 FALSE
## 14818 FALSE
## 14819 FALSE
## 14824 FALSE
## 14837 FALSE
## 14839 FALSE
## 14840 FALSE
## 14855 FALSE
## 14856 FALSE
## 14859 FALSE
## 14862 FALSE
## 14863 FALSE
## 14864 FALSE
## 14866 FALSE
## 14867 FALSE
## 14868 FALSE
## 14869 FALSE
## 14870 FALSE
## 14872 FALSE
## 14874 FALSE
## 14875 FALSE
## 14876 FALSE
## 14877 FALSE
## 14878 FALSE
## 14879 FALSE
## 14882 FALSE
## 14883 FALSE
## 14887 FALSE
## 14889 FALSE
## 14890 FALSE
## 14891 FALSE
## 14892 FALSE
## 14893 FALSE
## 14898 FALSE
## 14899 FALSE
## 14900 FALSE
## 14903 FALSE
## 14905 FALSE
## 14910 FALSE
## 14918 FALSE
## 14927 FALSE
## 14935 FALSE
## 14936 FALSE
## 14941 FALSE
## 14946 FALSE
## 14948 FALSE
## 14953 FALSE
## 14957 FALSE
## 14959 FALSE
## 14962 FALSE
## 14963 FALSE
## 14968 FALSE
## 14971 FALSE
## 14985 FALSE
## 14988 FALSE
## 14993 FALSE
## 14996 FALSE
## 15001 FALSE
## 15003 FALSE
## 15005 FALSE
## 15009 FALSE
## 15010 FALSE
## 15011 FALSE
## 15012 FALSE
## 15013 FALSE
## 15014 FALSE
## 15015 FALSE
## 15016 FALSE
## 15017 FALSE
## 15018 FALSE
## 15019 FALSE
## 15020 FALSE
## 15021 FALSE
## 15024 FALSE
## 15025 FALSE
## 15026 FALSE
## 15028 FALSE
## 15029 FALSE
## 15032 FALSE
## 15034 FALSE
## 15037 FALSE
## 15042 FALSE
## 15045 FALSE
## 15046 FALSE
## 15047 FALSE
## 15052 FALSE
## 15053 FALSE
## 15057 FALSE
## 15058 FALSE
## 15061 FALSE
## 15079 FALSE
## 15081 FALSE
## 15087 FALSE
## 15096 FALSE
## 15103 FALSE
## 15105 FALSE
## 15120 FALSE
## 15121 FALSE
## 15122 FALSE
## 15123 FALSE
## 15124 FALSE
## 15126 FALSE
## 15128 FALSE
## 15129 FALSE
## 15130 FALSE
## 15132 FALSE
## 15136 FALSE
## 15137 FALSE
## 15139 FALSE
## 15143 FALSE
## 15144 FALSE
## 15146 FALSE
## 15150 FALSE
## 15155 FALSE
## 15158 FALSE
## 15161 FALSE
## 15163 FALSE
## 15168 FALSE
## 15170 FALSE
## 15172 FALSE
## 15175 FALSE
## 15177 FALSE
## 15179 FALSE
## 15181 FALSE
## 15183 FALSE
## 15184 FALSE
## 15190 FALSE
## 15191 FALSE
## 15192 FALSE
## 15193 FALSE
## 15195 FALSE
## 15200 FALSE
## 15207 FALSE
## 15210 FALSE
## 15214 FALSE
## 15218 FALSE
## 15220 FALSE
## 15226 FALSE
## 15228 FALSE
## 15243 FALSE
## 15244 FALSE
## 15248 FALSE
## 15254 FALSE
## 15256 FALSE
## 15258 FALSE
## 15265 FALSE
## 15276 FALSE
## 15277 FALSE
## 15278 FALSE
## 15279 FALSE
## 15280 FALSE
## 15281 FALSE
## 15282 FALSE
## 15283 FALSE
## 15284 FALSE
## 15285 FALSE
## 15286 FALSE
## 15287 FALSE
## 15288 FALSE
## 15290 FALSE
## 15292 FALSE
## 15294 FALSE
## 15297 FALSE
## 15298 FALSE
## 15300 FALSE
## 15308 FALSE
## 15309 FALSE
## 15311 FALSE
## 15319 FALSE
## 15323 FALSE
## 15327 FALSE
## 15333 FALSE
## 15334 FALSE
## 15336 FALSE
## 15337 FALSE
## 15339 FALSE
## 15340 FALSE
## 15345 FALSE
## 15352 FALSE
## 15354 FALSE
## 15360 FALSE
## 15362 FALSE
## 15364 FALSE
## 15373 FALSE
## 15395 FALSE
## 15396 FALSE
## 15398 FALSE
## 15399 FALSE
## 15400 FALSE
## 15401 FALSE
## 15402 FALSE
## 15405 FALSE
## 15406 FALSE
## 15407 FALSE
## 15408 FALSE
## 15415 FALSE
## 15419 FALSE
## 15425 FALSE
## 15433 FALSE
## 15441 FALSE
## 15444 FALSE
## 15468 FALSE
## 15469 FALSE
## 15470 FALSE
## 15471 FALSE
## 15472 FALSE
## 15474 FALSE
## 15475 FALSE
## 15476 FALSE
## 15477 FALSE
## 15478 FALSE
## 15481 FALSE
## 15483 FALSE
## 15486 FALSE
## 15487 FALSE
## 15493 FALSE
## 15494 FALSE
## 15498 FALSE
## 15499 FALSE
## 15500 FALSE
## 15505 FALSE
## 15506 FALSE
## 15508 FALSE
## 15515 FALSE
## 15516 FALSE
## 15519 FALSE
## 15520 FALSE
## 15523 FALSE
## 15526 FALSE
## 15527 FALSE
## 15531 FALSE
## 15533 FALSE
## 15534 FALSE
## 15536 FALSE
## 15538 FALSE
## 15543 FALSE
## 15546 FALSE
## 15551 FALSE
## 15553 FALSE
## 15554 FALSE
## 15560 FALSE
## 15565 FALSE
## 15568 FALSE
## 15596 FALSE
## 15597 FALSE
## 15598 FALSE
## 15599 FALSE
## 15600 FALSE
## 15601 FALSE
## 15602 FALSE
## 15603 FALSE
## 15604 FALSE
## 15605 FALSE
## 15606 FALSE
## 15607 FALSE
## 15609 FALSE
## 15610 FALSE
## 15611 FALSE
## 15612 FALSE
## 15615 FALSE
## 15617 FALSE
## 15620 FALSE
## 15623 FALSE
## 15624 FALSE
## 15626 FALSE
## 15627 FALSE
## 15633 FALSE
## 15642 FALSE
## 15644 FALSE
## 15646 FALSE
## 15648 FALSE
## 15653 FALSE
## 15657 FALSE
## 15660 FALSE
## 15661 FALSE
## 15666 FALSE
## 15676 FALSE
## 15680 FALSE
## 15693 FALSE
## 15706 FALSE
## 15708 FALSE
## 15712 FALSE
## 15719 FALSE
## 15721 FALSE
## 15722 FALSE
## 15728 FALSE
## 15734 FALSE
## 15740 FALSE
## 15741 FALSE
## 15744 FALSE
## 15747 FALSE
## 15748 FALSE
## 15749 FALSE
## 15750 FALSE
## 15751 FALSE
## 15752 FALSE
## 15753 FALSE
## 15755 FALSE
## 15759 FALSE
## 15761 FALSE
## 15762 FALSE
## 15764 FALSE
## 15766 FALSE
## 15768 FALSE
## 15771 FALSE
## 15772 FALSE
## 15774 FALSE
## 15775 FALSE
## 15776 FALSE
## 15778 FALSE
## 15787 FALSE
## 15789 FALSE
## 15790 FALSE
## 15791 FALSE
## 15792 FALSE
## 15794 FALSE
## 15795 FALSE
## 15796 FALSE
## 15798 FALSE
## 15801 FALSE
## 15806 FALSE
## 15807 FALSE
## 15812 FALSE
## 15813 FALSE
## 15816 FALSE
## 15817 FALSE
## 15819 FALSE
## 15820 FALSE
## 15821 FALSE
## 15822 FALSE
## 15823 FALSE
## 15825 FALSE
## 15826 FALSE
## 15827 FALSE
## 15829 FALSE
## 15831 FALSE
## 15833 FALSE
## 15836 FALSE
## 15837 FALSE
## 15838 FALSE
## 15844 FALSE
## 15845 FALSE
## 15846 FALSE
## 15847 FALSE
## 15848 FALSE
## 15854 FALSE
## 15855 FALSE
## 15859 FALSE
## 15860 FALSE
## 15861 FALSE
## 15862 FALSE
## 15863 FALSE
## 15864 FALSE
## 15865 FALSE
## 15866 FALSE
## 15868 FALSE
## 15869 FALSE
## 15870 FALSE
## 15872 FALSE
## 15876 FALSE
## 15880 FALSE
## 15886 FALSE
## 15890 FALSE
## 15891 FALSE
## 15894 FALSE
## 15900 FALSE
## 15902 FALSE
## 15904 FALSE
## 15906 FALSE
## 15907 FALSE
## 15909 FALSE
## 15914 FALSE
## 15921 FALSE
## 15922 FALSE
## 15924 FALSE
## 15926 FALSE
## 15930 FALSE
## 15934 FALSE
## 15935 FALSE
## 15936 FALSE
## 15938 FALSE
## 15939 FALSE
## 15949 FALSE
## 15955 FALSE
## 15960 FALSE
## 15970 FALSE
## 16036 FALSE
## 16078 FALSE
## 16101 FALSE
## 16153 FALSE
## 16207 FALSE
## 16211 FALSE
## 16264 FALSE
## 16265 FALSE
## 16266 FALSE
## 16272 FALSE
## 16275 FALSE
## 16341 FALSE
## 16372 FALSE
## 16373 FALSE
## 16385 FALSE
## 16386 FALSE
## 16398 FALSE
## 16508 FALSE
## 16517 FALSE
## 16597 FALSE
## 16700 FALSE
## 16775 FALSE
## 16823 FALSE
## 16897 FALSE
## 16994 FALSE
## 17006 FALSE
## 17025 FALSE
## 17026 FALSE
## 17027 FALSE
## 17028 FALSE
## 17031 FALSE
## 17034 FALSE
## 17035 FALSE
## 17037 FALSE
## 17038 FALSE
## 17039 FALSE
## 17040 FALSE
## 17041 FALSE
## 17043 FALSE
## 17045 FALSE
## 17046 FALSE
## 17050 FALSE
## 17053 FALSE
## 17055 FALSE
## 17057 FALSE
## 17058 FALSE
## 17059 FALSE
## 17060 FALSE
## 17061 FALSE
## 17062 FALSE
## 17065 FALSE
## 17066 FALSE
## 17074 FALSE
## 17075 FALSE
## 17076 FALSE
## 17077 FALSE
## 17079 FALSE
## 17084 FALSE
## 17086 FALSE
## 17090 FALSE
## 17093 FALSE
## 17101 FALSE
## 17112 FALSE
## 17120 FALSE
## 17123 FALSE
## 17124 FALSE
## 17129 FALSE
## 17132 FALSE
## 17137 FALSE
## 17140 FALSE
## 17145 FALSE
## 17152 FALSE
## 17153 FALSE
## 17156 FALSE
## 17161 FALSE
## 17165 FALSE
## 17174 FALSE
## 17179 FALSE
```

```r
set.seed(1100)
ggnet2(net2, alpha = 0.75, color = ifelse(network.vertex.names(net2) %in% seninfo$Official.Twitter[seninfo$Party.affiliation == "Democratic Party"], "#2E86C1", ifelse(network.vertex.names(net2) %in% seninfo$Official.Twitter[seninfo$Party.affiliation == "Republican Party"], "tomato", "#27AE60")),size = "degree", edge.alpha = 0.5, edge.size =  ) + guides(size = FALSE)
```

![](assignment4_senate_twitter_files/figure-html/unnamed-chunk-18-1.png)<!-- -->


```r
mentcount <- as.data.frame(table(mentsen))



colnames(mentcount) <- c('Senator','Senator_Mentioned','weight')
mentcount$weight <- mentcount$weight/2
g2=graph.data.frame(mentcount)
mentcount1 <- mentcount[mentcount$weight > 0,]
net3 <- network(mentcount1[,c('Senator','Senator_Mentioned')], directed = TRUE, names.eval = "weights")



set.seed(190)
ggnet2(net3, alpha = 0.75, color = ifelse(network.vertex.names(net3) %in% seninfo$Official.Twitter[seninfo$Party.affiliation == "Democratic Party"], "#2E86C1", ifelse(network.vertex.names(net3) %in% seninfo$Official.Twitter[seninfo$Party.affiliation == "Republican Party"], "tomato", "#27AE60")), size="degree" ,size.cut = 5, edge.alpha = 0.5, edge.size = mentcount1$weight/5) + guides(size = FALSE)
```

![](assignment4_senate_twitter_files/figure-html/unnamed-chunk-19-1.png)<!-- -->

```r
#Generally party members mention their own members, but there are some Republican senators that are often mentioned by Democratic
#Senators
```


#### c) BONUS ONLY: Who is popular on Twitter?

Using the twitter handles, access the user information of the senators to identify the number of followers they have (obviously, this will require to actually connect to the Twitter server). Re-do the previous graph object but now use the number of followers (or some transformation of that info) to size the nodes. Comment how graph degree centrality (via mentions) and the number of followers are related. 



```r
sens <- unique(senator_tweets$screen_name)
sens <- as.data.frame(sens)

sens$num <- as.data.frame(lookup_users(sens$sens))

sens$number <- sens$num$followers_count

sens <- sens[order(sens$sens),]
```

```r
mentcount2 <- mentcount1

mentcount2$numbers <- ifelse(mentcount2$Senator %in% sens$sens, sens$number, mentcount2$numbers)

net4 <- network(mentcount2[,c('Senator','Senator_Mentioned')], directed = TRUE, names.eval = "weights")


set.seed(190)
ggnet2(net4, alpha = 0.75, color = ifelse(network.vertex.names(net4) %in% seninfo$Official.Twitter[seninfo$Party.affiliation == "Democratic Party"], "#2E86C1", ifelse(network.vertex.names(net4) %in% seninfo$Official.Twitter[seninfo$Party.affiliation == "Republican Party"], "tomato", "#27AE60")), size= sens$number , edge.alpha = 0.5, edge.size = mentcount2$weight/5) + guides(size = FALSE) 
```

![](assignment4_senate_twitter_files/figure-html/unnamed-chunk-22-1.png)<!-- -->

```r
#It can be seen that generally, more mentions means more followers. There is one Independent Senator who is an exception.
```


## Submission

Please follow the [instructions](/Exercises/homework_submission_instructions.md) to submit your homework. The homework is due on Thursday, April 12.

## Please stay honest!

If you do come across something online that provides part of the analysis / code etc., please no wholesale copying of other ideas. We are trying to evaluate your abilities to visualized data not the ability to do internet searches. Also, this is an individually assigned exercise -- please keep your solution to yourself.
