##The firs step is to read the tables and join them for the training and test data

##Read and prepare the tables (test group)

	dados <- read.table("UCI HAR Dataset/test/X_test.txt")

	labelos <- read.table("UCI HAR Dataset/test/Y_test.txt")

	subj <- read.table("UCI HAR Dataset/test/subject_test.txt")

	nomelabel<-read.table("UCI HAR Dataset/activity_labels.txt")

	collumnnames = read.table("UCI HAR Dataset/features.txt")

	##Associates the names of the variables to the variables

	names(dados)<-as.character(collumnnames[,2])

	##Union of the three tables, using olnly the names of "dados" that contains "means" or "std deviation" measures

	tabela <- cbind(labelos, subj, dados[,grep("mean()|std()", names(dados))])

	names(tabela)[c(2)] <- c("subject")

	##Creates a new table whith the names of the activity

	tabelaTest<- merge(nomelabel, tabela, by.x="V1", by.y="V1", all=TRUE)

	names(tabelaTest)[c(1, 2)] <- c("activitynumber", "activity")

##Read and prepare the tables (TRAIN group)

	dados <- read.table("UCI HAR Dataset/train/X_train.txt")

	labelos <- read.table("UCI HAR Dataset/train/Y_train.txt")

	subj <- read.table("UCI HAR Dataset/train/subject_train.txt")

	nomelabel<-read.table("UCI HAR Dataset/activity_labels.txt")

	collumnnames = read.table("UCI HAR Dataset/features.txt")

	##Associates the names of the variables to the variables

	names(dados)<-as.character(collumnnames[,2])

	##Union of the three tables, using olnly the names of "dados" that contains "means" or "std deviation" measures

	tabela <- cbind(labelos, subj, dados[,grep("mean()|std()", names(dados))])

	names(tabela)[c(2)] <- c("subject")

	##Creates a new table whith the names of the activity

	tabelaTrain<- merge(nomelabel, tabela, by.x="V1", by.y="V1", all=TRUE)

	names(tabelaTrain)[c(1, 2)] <- c("activitynumber", "activity")

#########################################

##joins both tables, for train and test group and sets the result at "tabela"

	tabela<- rbind(tabelaTrain, tabelaTest)


## averages each variable for each activity and each subject

	tabelamean <- aggregate(tabela[4:82], by = list(activity = tabela$activity, subject = tabela$subject), FUN = "mean")

## inputs the text "average of" befor each variable to clarify their meaning

	names(tabelamean)[3:81] <- paste("average of ",names(tabela3)[3:81])

## averages each variable for each activity and each subject

	write.table(tabelamean, file = "tabelamean.txt", row.name=FALSE)


##Creates the Codebook

 coetab <- data.frame(names(tabela))

	##1st part
coetab[grep("mean", coetab[,1]),2] <- "mean value of "

coetab[grep("std", coetab[,1]),2] <- "standard deviation of "

	##2nd part
coetab[grep("Mag", coetab[,1]),2] <- paste(coetab[grep("Mag", coetab[,1]),2], "Magnitude of ")
coetab[grep("Mag", coetab[,1], invert=TRUE),2] <- paste(coetab[grep("Mag", coetab[,1], invert=TRUE),2], "Signal of ")

	##3nd part
coetab[grep("Body", coetab[,1]),2] <- paste(coetab[grep("Body", coetab[,1]),2], "Body ")
coetab[grep("Gravity", coetab[,1]),2] <- paste(coetab[grep("Gravity", coetab[,1]),2], "Gravity ")

	##4th part
coetab[grep("Acc", coetab[,1]),2] <- paste(coetab[grep("Acc", coetab[,1]),2], "linear ")
coetab[grep("Gyro", coetab[,1]),2] <- paste(coetab[grep("Gyro", coetab[,1]),2], "angular ")

	##5th part
coetab[grep("Jerk", coetab[,1]),2] <- paste(coetab[grep("Jerk", coetab[,1]),2], "jerk ")

	##6th part
coetab[substr(coetab[,1], 1, 1)=="t" ,2] <- paste(coetab[substr(coetab[,1], 1, 1) == "t",2] , "acceleration time ")
coetab[substr(coetab[,1], 1, 1)=="f" ,2] <- paste(coetab[substr(coetab[,1], 1, 1) == "f",2] , "acceleration frequency ")


	##7th part
coetab[grep("-X", coetab[,1]),2] <- paste(coetab[grep("-X", coetab[,1]),2], " at axis X")
coetab[grep("-Y", coetab[,1]),2] <- paste(coetab[grep("-Y", coetab[,1]),2], " at axis Y")
coetab[grep("-Z", coetab[,1]),2] <- paste(coetab[grep("-Z", coetab[,1]),2], " at axis Z")

	##describes the 3 first collumns
coetab[1:3,1]<-names(tabela)[1:3]
coetab[1:3,2]<-c("number (code) of the activity", "description of the activity", "subject code")

	##creates the codebook txt file
write.table(coetab, file = "CodeBook.txt", row.name=FALSE, col.name=FALSE)
