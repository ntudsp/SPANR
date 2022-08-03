#This file is accompanies SPANRanalysis.Rmd and contains helper functions 
#Bhan Lam, 2022
#NTU

#function to transform data to long
transform2long<-function(data,trackData,trackInfo,participantID,columnnames,noOfTracks,noOfQns){
        #intitalise
        finalData <- data.frame(
                ID=numeric(),
                NoiseType=character(),
                Masker=character(),
                SPL=character(),
                ANC=character(),
                annoycat=numeric(),
                annoyscale=numeric(), 
                eventful=numeric(),
                vibrant=numeric(),
                pleasant=numeric(),
                calm=numeric(),
                uneventful=numeric(),
                monotonous=numeric(),
                annoying=numeric(),
                chaotic=numeric(),
                loudness=numeric(),
                desc=character())
        for (i in 1:noOfTracks){
                partData<-cbind(participantID,trackInfo[trackData[,i],],
                                data[(noOfQns*(i-1)+1):(noOfQns*i)]) %>%
                        `colnames<-`(columnnames)
                finalData<-rbind(finalData,partData)
        }
        return(finalData)
}

fromASC<-function(listOfFiles, columnnames){
        
        #initialise df
        final.df<-data.frame(
                time=numeric(), 
                stimuliID=numeric(), #stimuli ID
                run=numeric(), #measurement run
                HATS.L=numeric(), HATS.R=numeric(),
                mic5=numeric(), mic13=numeric(),
                mic14=numeric(), mic15=numeric(),
                mic17=numeric(), mic18=numeric()
        )
        
        #extract data from asc files
        for(i in 1:length(listOfFiles)){
                temp.df<-read.csv(listOfFiles[i],header = FALSE,sep = "\t") %>%
                        `colnames<-`(columnnames) %>%
                        mutate(run=as.numeric(gsub("[()]","", #get run no.
                                        str_extract(string = listOfFiles[i], 
                                                    pattern = "\\([0-9]+\\)"))),
                               stimuliID=as.numeric(gsub("[_(]","", #get stimuli ID
                                              str_extract(string = listOfFiles[i], 
                                                          pattern = "_[0-9]+\\("))))
                final.df<-rbind(final.df,temp.df)
        }
        return(final.df)
}
