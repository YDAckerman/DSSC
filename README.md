# DSSC
---
title: Davis Short Story Club
output: html_document
---

## Bring on the libraries
```{r, echo = TRUE, message = FALSE}
library(plyr)
library(dplyr)
library(ggplot2)
library(wesanderson)
```

## Load and 'clean' the data!
## (my favorite part...)
```{r, echo = TRUE, message = FALSE}
## load in that literary data
d <- read.csv("~/Desktop/ssc.csv", stringsAsFactors = FALSE)
## re-write some history for democracy's sake
d$Date.Chosen[1:7] <- NA
## convert the date characters to date objects
d$Date.Chosen <- as.Date(d$Date.Chosen, format = "%m/%d/%y")
## remove new-string characters
d$Picker <- gsub("\\n", "", d$Picker)
```

## get snarky in the face of inconvenience - it helps...
```{r, echo = TRUE, message = FALSE}
## deal with the problem of names...
## because what would ssc be without a gratuitous,
## vectorized implementation of a for loop?
pickers <- unique(d$Picker)
guess_who <- ldply(pickers, function(whoami){
    if(whoami == ""){return(NULL)}
    dopplegangers <- pickers[grep(whoami, pickers, ignore.case = TRUE)]
    ## some people decided to conjoin, which we should watch out for:
    dopplegangers <- dopplegangers[!grepl("/", dopplegangers)]
    ## let's not do this more than we have to:
    if(length(dopplegangers) > 1){
        return(
            data.frame(dopplegangers = dopplegangers,
                       whoami = whoami,
                       stringsAsFactors = FALSE)
            )
    } else {
        return(NULL)
    }
})

## how do we choose which doppleganger to keep!?
strlen <- function(x){
    length(unlist(strsplit(x,"")))
}

## longer is better:
guess_who$len <- unlist(llply(guess_who$dopplegangers, strlen))

## shape the corrections data
guess_who <- guess_who %>%
    dplyr::group_by(whoami) %>%
    dplyr::arrange(desc(len)) %>%
    top_n(1) %>%
    dplyr::rename(Picker = whoami) %>%
    dplyr::ungroup()

## Draxx them sklounst
d <- left_join(d, guess_who, by = "Picker") %>%
    dplyr::mutate(Picker =
    	          ifelse(!is.na(dopplegangers), dopplegangers, Picker)) %>%
    dplyr::select(-dopplegangers, -len)
```

## deal with missing data, unfortunately, neither Bon Iver nor Jewel are at our disposal...
```{r, echo = TRUE, message = FALSE}
## since NA's will disappear, like all rapping greats, let's estimate
## dates for each of them
est1 <- as.Date("10/16/11", format = "%m/%d/%y")
est2 <- as.Date("11/27/11", format = "%m/%d/%y")
fill_nas <- c(seq.Date(from = est1, to = est2, by = 7),
              as.Date("01/06/14", format = "%m/%d/%y"))

## fill'er up
d$Date.Chosen[is.na(d$Date.Chosen)] <-  fill_nas
```

## let ggplot provide an air of legitimacy
```{r, echo = TRUE, message = FALSE}
## picture time!
pal <- wes_palette("Moonrise3")
tmp <- d %>% group_by(Picker) %>%
    dplyr::summarise(count = n(),
                      min_date = min(Date.Chosen)) %>%
       arrange(desc(count))
tmp$col <- as.character(rep(1:5, 10)[1:dim(tmp)[1]])
## ggplot(tmp, aes(x = min_date, y = count,
##                                  label = Picker, color = col)) +
##     scale_colour_manual(values = pal) +
##     xlab("Date First Story Was Contributed") +
##     ylab("Total Number of Stories Contributed") +
##     geom_text(check_overlap = TRUE, size = 2, angle = 45) +
##     theme(legend.position="none")
```

![](fig1.png)

## another bright idea...
```{r, echo = TRUE, message = FALSE}
## let's rank everyone by the number of stories contributed:
ranks <- d %>%
    group_by(Picker) %>%
    dplyr::summarise(rank = n())
d <- left_join(d, ranks, by = "Picker")
```

## and see what it looks like
```{r, echo = TRUE, message = FALSE}
pal <- wes_palette("Zissou", length(unique(d$Picker)), type = "continuous")
## ggplot(d, aes(x = Date.Chosen, y = rank,
##               label = Picker, color = Picker)) +
##     scale_color_manual(values = pal) +
##     xlab("SSC Date") +
##     ylab("Total Number of Stories Contributed") +
##     geom_text(check_overlap = TRUE, size = 2, angle = 90) +
##     theme(legend.position="none")
```

![](fig2.png)

## see any patterns?
