Hold<- as.character(readline(prompt ="Type Enter to proceed Hold#1 statements")) 
library(XML)
library(methods)
##Intialisation
PaymentontimeRating <- 0
Rating <- 0
Not_active_reccexpanse_rating <- 0
Avg6datesRating <- 0
Yescheck <- 0
##Intialisation
    ReadXML <- xmlParse("SARAVANAN C1")
Rootnode <- xmlRoot(ReadXML)
Salarycredits <- Rootnode[[4]][1]
Salarycredits <- as.list(Salarycredits)
Salarycredits<- Salarycredits[[1]]
Salarycredits
Salarycredits<- xmlToList(Salarycredits)
#"  \"1\"\n\t" represent SalaryCredit=1
if(Salarycredits[[1]]==" \"1\"\n\t"){
    RatingBAd <- print("Salary Credit=Rating:1")
    Avg6dates <- Rootnode[["avgBalanceOf6Dates"]][1]
    Avg6dates <- as.list(Avg6dates)
    Avg6dates <- xmlToList(Avg6dates[[1]])
    Avg6dates <- as.numeric(substr(Avg6dates,3,9))
    a2 <- " Checking monthly avarge balance Which is "
    text <- "and is greater than 4000 threshold we have set"
    text1 <- "and is less than 4000 threshold we have set"
    if (Avg6dates>4000) {
        Avg6datesRating <- 1
        print(paste(a2,print(Avg6dates),text))
    }else if (Avg6dates<4000) {
        Avg6datesRating <- -1
        print(paste(a2,Avg6dates,text1))
    }
    
}else if(Salarycredits[[1]]==" \"6\"\n\t"){
    Rating <-6 
        print("Salary Credit=Rating:6")
        Avg6dates <- Rootnode[["avgBalanceOf6Dates"]][1]
        Avg6dates <- as.list(Avg6dates)
        Avg6dates <- xmlToList(Avg6dates[[1]])
        Avg6dates <- as.numeric(substr(Avg6dates,3,9))
        a2 <- " Checking monthly avarge balance Which is "
        text <- "and is greater than 4000 threshold we have set"
        text1 <- "and is less than 4000 threshold we have set"
        if (Avg6dates>4000) {
            Avg6datesRating <- 1
            print(paste(a2,print(Avg6dates),text))
        }else if (Avg6dates<4000) {
            Avg6datesRating <- -1
            print(paste(a2,Avg6dates,text1))
        }
    
}else if(Salarycredits[[1]]==" \"0\"\n\t"){
    Rating <-0 
        print("Salary Credit=Rating:0")  
        Avg6dates <- Rootnode[["avgBalanceOf6Dates"]][1]
        Avg6dates <- as.list(Avg6dates)
        Avg6dates <- xmlToList(Avg6dates[[1]])
        Avg6dates <- as.numeric(substr(Avg6dates,3,9))
        a2 <- " Checking monthly avarge balance Which is "
        text <- "and is greater than 4000 threshold we have set"
        text1 <- "and is less than 4000 threshold we have set"
        if (Avg6dates>4000) {
            Avg6datesRating <- 1
            print(paste(a2,print(Avg6dates),text))
        }else if (Avg6dates<4000) {
            Avg6datesRating <- -1
            print(paste(a2,Avg6dates,text1))
        }
        
}else if(Salarycredits[[1]]==" \"4\"\n\t"){
    Rating <- 4
        print("Salary Credit=Rating:4") 
        Avg6dates <- Rootnode[["avgBalanceOf6Dates"]][1]
        Avg6dates <- as.list(Avg6dates)
        Avg6dates <- xmlToList(Avg6dates[[1]])
        Avg6dates <- as.numeric(substr(Avg6dates,3,9))
        a2 <- " Checking monthly avarge balance Which is "
        text <- "and is greater than 4000 threshold we have set"
        text1 <- "and is less than 4000 threshold we have set"
        if (Avg6dates>4000) {
            Avg6datesRating <- 1
            print(paste(a2,print(Avg6dates),text))
        }else if (Avg6dates<4000) {
            Avg6datesRating <- -1
            print(paste(a2,Avg6dates,text1))
        }
        
}else if(Salarycredits[[1]]==" \"2\"\n\t"){
    Rating <- 2
        print("Salary Credit=Rating:2")
        Avg6dates <- Rootnode[["avgBalanceOf6Dates"]][1]
        Avg6dates <- as.list(Avg6dates)
        Avg6dates <- xmlToList(Avg6dates[[1]])
        Avg6dates <- as.numeric(substr(Avg6dates,3,9))
        a2 <- " Checking monthly avarge balance Which is "
        text <- "and is greater than 4000 threshold we have set"
        text1 <- "and is less than 4000 threshold we have set"
        if (Avg6dates>4000) {
            Avg6datesRating <- 1
            print(paste(a2,Avg6dates,text))
        }else if (Avg6dates<4000) {
            Avg6datesRating <- -1
            print(paste(a2,Avg6dates,text1))
        }
        
}else if(Salarycredits[[1]]==" \"7\"\n\t"){
    Rating <- 6
        print("Salary Credit=Rating:6")
        Avg6dates <- Rootnode[["avgBalanceOf6Dates"]][1]
        Avg6dates <- as.list(Avg6dates)
        Avg6dates <- xmlToList(Avg6dates[[1]])
        Avg6dates <- as.numeric(substr(Avg6dates,3,9))
        a2 <- " Checking monthly avarge balance Which is "
        text <- "and is greater than 4000 threshold we have set"
        text1 <- "and is less than 4000 threshold we have set"
        if (Avg6dates>4000) {
            print(paste(a2,Avg6dates,text))
        }else if (Avg6dates<4000) {
            Avg6datesRating <- -1
            print(paste(a2,Avg6dates,text1))
        }
        
        
}

readline(prompt ="Checking for recurrance expanses ever made Hold#2")
##Checking for recurrance expanses ever made or active right now
recc_expansetype <- Rootnode[["RecurringExpensescat"]][1]
recc_expansetype <- as.list(recc_expansetype)
recc_expansetype <- recc_expansetype[[1]]
recc_expansetype <-  xmlToList(recc_expansetype)
recc_expansetype
if (recc_expansetype==" \"Empty\"\n\t") {
Not_active_reccexpanse_rating <- .5
print("No reccurance expanses")
}else if (recc_expansetype=="\"Yes\"\n\t\n\t") {
    Yescheck <- 1
    print("check if recurrance expanses of (type=CC,Loan,Other) made on time")
}else if (recc_expansetype=="\"Yes\"\n\t") {
    Yescheck <- 1
    print("check if recurrance expanses of (type=CC,Loan,Other) made on time")
}
##Checking for recurrance expanses ever made or active right now END
##Grabbing details of CC,Load,other
recc_expansetype <- getNodeSet(Rootnode,"//RecurringExpenses/KnownTypes")
if (Yescheck==1) {
    recc_expansetype    
    }
readline(prompt ="Scroll up to check if CC bill,Loan,Other are paid((If there are)) on time,PRESS CTRL+F and find keywords suchs OVERDUE etc Hold#3")

##Grabbing details of CC END
#Bases on Insights choose Good,overdue,overduetwice,overduemultiple 
Outcome<- as.character(readline(prompt ="Enter:")) 
Reason <- as.character(readline(prompt = "Why:"))
if (Outcome=="Good"){ 
    PaymentontimeRating <- 1
    paste("PaymentontimeRating=",PaymentontimeRating)
}else if (Outcome=="overdue") {
    PaymentontimeRating <- -1
    paste(print("PaymentontimeRating="),PaymentontimeRating)
}else if (Outcome=="overduetwice") {
    PaymentontimeRating <- -2
    paste("PaymentontimeRating=",PaymentontimeRating)
}else if (Outcome=="overduemultiple"){
    PaymentontimeRating <- -3
    paste("PaymentontimeRating=",PaymentontimeRating)
}
#Bases on Insights choose Good,overdue,overduetwice,overduemultiple  END

print(Rating)
print(PaymentontimeRating)
print(Not_active_reccexpanse_rating)
print(Avg6datesRating)
Overall_Rating <- Rating+PaymentontimeRating+Not_active_reccexpanse_rating+Avg6datesRating
Overall_Rating
print(Overall_Rating)

