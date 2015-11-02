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

################################################################################################
#
#
###############################################################################################

bar <- function(df,peopleType,type,state,literacyLevel) {
    dim(df)
    print(peopleType)
    print(type)
    print(state)
    print(literacyLevel)
    #a <- read.csv("education.csv")
    #colnames(a) <- gsub("Educational.level...","",colnames(a))
    
    
    #a$Area.Name <-gsub("State - ","",a$Area.Name)
    #a$Area.Name <- gsub("\\d+","",a$Area.Name)
    
    # Remove trailing spaces
    #a$Area.Name <- gsub("[[:space:]]*$","",a$Area.Name)
    
    b <- filter(a,Area.Name==state & Total..Rural..Urban==type)
    #b <- filter(a,Area.Name=="KERALA" & Total..Rural..Urban=="Rural")
    
  
    
    # Select colums from 8 - 21
    c <- b[,7:19]
    # Set names
    names(c) <-c("Age","Persons","Males","Females","EduPersons","EduMales",
                 "EduFemales","IlliteratePersons","IlliterateMales",
                 "IlliterateFemales","LiteratePersons","LiterateMales","LiterateFemales")
    
    # Subset columns with persons
    people <- select(c,matches(peopleType,ignore.case=FALSE))
    
    # Calculate males percent as percent of total males
    peoplePercent <- people[,2:4]/people[,1]*100
    
    # Add the age column
    peoplePercent <- cbind(c[1],peoplePercent)
    
    
    # Drop the 1st row
    peoplePercent <- peoplePercent[2:length(rownames(peoplePercent)),]
    
    #Do the same for the India to plot the national average
    m <- filter(a,Area.Name=="INDIA" & Total..Rural..Urban==type)
    
    # Select colums from 8 - 21
    n <- m[,7:19]
    # Set names
    names(n) <-c("Age","Persons","Males","Females","EduPersons","EduMales",
                            "EduFemales","IlliteratePersons","IlliterateMales",
                            "IlliterateFemales","LiteratePersons","LiterateMales","LiterateFemales")
    
    
    # Subset columns with persons
    natPeople <- select(n,matches(peopleType,ignore.case=FALSE))
    
    # Calculate males percent as percent of total males
    natPeoplePercent <- natPeople[,2:4]/natPeople[,1]*100
    
    # Add the age column
    natPeoplePercent <- cbind(c[1],natPeoplePercent)

    # Drop the 1st row
    natPeoplePercent <- natPeoplePercent[2:length(rownames(natPeoplePercent)),]
    
    
    #Use a color palette
    pal <- colorRampPalette(c("yellow","blue"))
    colors=pal(22)
    
    # Create the column from the literacyLevel input
    u <- literacyLevel
    v <- paste(literacyLevel,peopleType,sep="")
    w <- which(names(peoplePercent) == v)
    
    natPeople <- barplot(natPeoplePercent[,w],names.arg=natPeoplePercent$Age,
                         col="white",border=NA)
    
    barplot(peoplePercent[,w],names.arg=peoplePercent$Age,col=colors,ylim=c(0,100),xlab="Age",
            ylab="Percent",main="Test")
    
    with(data=natPeoplePercent,lines(natPeople,natPeoplePercent[,w],col="black",lty=3,lwd=3))
    
    ################################################################################
    ################################################################################
   
   
    
    
    
}
allPercent <- function() {
    b <- filter(a,Area.Name==state & Total..Rural..Urban==type)
    
    # Select colums from 8 - 21
    c <- b[,7:19]
    # Set names
    names(c) <-c("Age","Persons","Males","Females","PersonsEdu","MalesEdu",
                 "FemalesEdu","IlliteratePersons","IlliterateMales",
                 "IlliterateFemales","LiteratePersons","LiterateMales","LiterateFemales")
    
    males <- select(c,matches("Males",ignore.case=FALSE))
    females <- select(c,matches("Females",ignore.case=FALSE))
    persons <- select(c,matches("Persons",ignore.case=FALSE))
    
    
    # Calculate males percent as percent of total males
    malesPercent <- males[,2:4]/males[,1]*100
    # Calculate females percent as percent of total females
    femalesPercent <- females[,2:4]/females[,1]*100
    # Calculate persons percent as percent of total persons
    personsPercent <- persons[,2:4]/persons[,1]*100
    
    # Add the age column
    malesPercent <- cbind(c[1],malesPercent)
    femalesPercent <- cbind(c[1],femalesPercent)
    personsPercent <- cbind(c[1],personsPercent)
    
    # Drop the 1st row
    malesPercent <- malesPercent[2:length(rownames(malesPercent)),]
    femalesPercent <- femalesPercent[2:length(rownames(femalesPercent)),]
    personsPercent <- personsPercent[2:length(rownames(personsPercent)),]
    
    
    
    
    #Use a color palette
    pal <- colorRampPalette(c("yellow","blue"))
    colors=pal(22)
    
    barplot(malesPercent$MalesEdu,names.arg=malesPercent$Age,col=colors)
    with(data=IndiaMalesPercent,lines(indiaPersons,MalesEdu,col="black",lty=3,lwd=4))
    
    barplot(femalesPercent$FemalesEdu,names.arg=femalesPercent$Age,col=colors)
    with(data=IndiaFemalesPercent,lines(indiaPersons,FemalesEdu,col="black",lty=4,lwd=4))
    
    
    barplot(personsPercent$PersonsEdu,names.arg=personsPercent$Age,col=colors)
    with(data=IndiaPersonsPercent,lines(indiaPersons,PersonsEdu,col="black",lty=4,lwd=4))
    
    
    persons <- barplot(personsPercent$PersonsEdu,names.arg=personsPercent$Age,
                       col="white",border=NA)
    
    with(data=personsPercent,lines(persons,PersonsEdu,col="black"))
    with(data=malesPercent,lines(persons,MalesEdu,col="blue"))
    with(data=femalesPercent,lines(persons,FemalesEdu,col="red"))
}
