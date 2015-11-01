library(rgeos)
library(maptools)
library(ggplot2)
library(dplyr)
library(stringr)
library(reshape2)
library(RColorBrewer)


#a <- read.csv("india.csv")

educationalLevels <- function(df,peopleType,type,state) {
   
    print(peopleType)
    print(state)
    print(type)
    dim(df)
   
    b <- filter(df,Area.Name==state & Total..Rural..Urban==type)
    #b <- filter(a,Area.Name=="KERALA" & Total..Rural..Urban=="Rural")
    
    # Subset columns with persons
    people <- select(b,matches(peopleType,ignore.case=FALSE))
    l <-paste("...",peopleType,sep="")
    names(people) <- gsub(l,"",names(people))
    
   
    # Compute the percentage of people in the age group (4,5,6..)  as a percentage of total people in group  
    l <- dim(people)
    for(i in 2:l[1]) {
        for(j in 2:l[2]) {
            # people[5,8]  <- people[5,8]/people[1,8] *100 - In
            people[i,j] <- people[i,j]/people[1,j]*100
                           
        }
    }
    
    age <- b[,7]
    people <- cbind(age,people)
    
    
    v <- c(1,7:15)
    m1 <- people[2:23,v]
    
    # Needed to add a '0' so that the numeric and lexicographic ordering is fine
    m1$age = as.character(m1$age)
    m1$age[2] ="05"
    m1$age[3] ="06"
    m1$age[4] ="07"
    m1$age[5] ="08"
    m1$age[6] ="09"
    
    m2 <- melt(m1,id.vars="age")
    
    # Add a title
    atitle <- paste("Distribution of literacy among",tolower(type),tolower(peopleType), "in", state,"over different age groups")
   
    ggplot(m2,aes(x=age,y=value,fill=variable)) +     geom_bar(stat = "identity") +
         xlab("Age") + ylab("Percentage from different age groups") +
        ggtitle(atitle)
    
}
