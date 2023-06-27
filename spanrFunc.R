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

#defined values for tidyness
sv.colnames<-c("Filename","Channel","L(C)","L(C).Units",
               "Min(C)","Min(C).Units","Min(C).Time","Min(C).Time.Units",
               "Max(C)","Max(C).Units","Max(C).Time","Max(C).Time.Units",
               "L5(C)","L5(C).Units","L10(C)","L10(C).Units",
               "L50(C)","L50(C).Units","L90(C)","L90(C).Units",
               "L95(C)","L95(C).Units","L(A)","L(A).Units",
               "Min(A)","Min(A).Units","Min(A).Time","Min(A).Time.Units",
               "Max(A)","Max(A).Units","Max(A).Time","Max(A).Time.Units",
               "L5(A)","L5(A).Units","L10(A)","L10(A).Units",
               "L50(A)","L50(A).Units","L90(A)","L90(A).Units",
               "L95(A)","L95(A).Units","N5","N5.Units",
               "Min(N)","Min(N).Units","Min(N).Time","Min(N).Time.Units",
               "Max(N)","Max(N).Units","Max(N).Time","Max(N).Time.Units",
               "N10","N10.Units","N50","N50.Units",
               "N90","N90.Units","N95","N95.Units","S","S.Units",
               "Min(S)","Min(S).Units","Min(S).Time","Min(S).Time.Units",
               "Max(S)","Max(S).Units","Max(S).Time","Max(S).Time.Units",
               "S5","S5.Units","S10","S10.Units",
               "S50","S50.Units","S90","S90.Units",
               "S95","S95.Units","R","R.Units",
               "Min(R)","Min(R).Units","Min(R).Time","Min(R).Time.Units",
               "Max(R)","Max(R).Units","Max(R).Time","Max(R).Time.Units",
               "R5","R5.Units","R10","R10.Units",
               "R50","R50.Units","R90","R90.Units",
               "R95","R95.Units", "Tone","Tone.Units",
               "Min(Tone)","Min(Tone).Units","Min(Tone).Time","Min(Tone).Time.Units",
               "Max(Tone)","Max(Tone).Units","Max(Tone).Time","Max(Tone).Time.Units",
               "Tone5","Tone5.Units","Tone10","Tone10.Units",
               "Tone50","Tone50.Units","Tone90","Tone90.Units",
               "Tone95","Tone95.Units","Min(Tonef)","Min(Tonef).Units",
               "Min(Tonef).Time","Min(Tonef).Time.Units",
               "Max(Tonef)","Max(Tonef).Units","Max(Tonef).Time","Max(Tonef).Time.Units",
               "Tonef5","Tonef5.Units","Tonef10","Tonef10.Units",
               "Tonef50","Tonef50.Units","Tonef90","Tonef90.Units",
               "Tonef95","Tonef95.Units","Fluc","Fluc.Units",
               "Min(Fluc)","Min(Fluc).Units","Min(Fluc).Time","Min(Fluc).Time.Units",
               "Max(Fluc)","Max(Fluc).Units","Max(Fluc).Time","Max(Fluc).Time.Units",
               "Fluc5","Fluc5.Units","Fluc10","Fluc10.Units",
               "Fluc50","Fluc50.Units","Fluc90","Fluc90.Units",
               "Fluc95","Fluc95.Units")

params.udrTest<-
        c("L(C)","Max(C)","L5(C)","L10(C)","L50(C)","L90(C)","L95(C)",
          "L(A)","Max(A)","L5(A)","L10(A)","L50(A)","L90(A)","L95(A)",
          "Max(N)","N5","N10","N50","N90","N95",
          "Max(S)","S","S5","S10","S50","S90","S95",
          "Max(R)","R","R5","R10","R50","R90","R95",
          "Max(Tone)","Tone5","Tone10","Tone50","Tone90","Tone95",
          "Max(Fluc)","Fluc5","Fluc10","Fluc50","Fluc90","Fluc95")

#function for BA plots

baplotOpts<-function(data,X,Y,color, #ggplot aes
                     fg.XY, #facet grid formula
                     np,#no. of params
                     #hline for mean diff, upper LoA, lower LoA
                     hl.mean, hl.upper, hl.lower, 
                     #lower/upper 95% limits of lwr/upr LoA & mean
                     lwr.ymin,lwr.ymax,upr.ymin,upr.ymax,m.ymin,m.ymax,
                     geom.text.size,theme.size,geom.point.size,colorl,
                     xlim.low,xlim.upp,ylim.low,ylim.upp,ylabel,xlabel){
        ggplot(data = data,
               aes(x={{X}},y={{Y}},color={{color}})) +
                facet_grid({{fg.XY}}) +
                geom_point(size=geom.point.size,alpha=0.5) + #add differences
                #average difference line (mean)
                geom_hline(aes(yintercept = {{hl.mean}}), color=colorl[5]) +
                geom_text(aes(xlim.upp,{{hl.mean}},
                              label = paste("M =",as.character(round({{hl.mean}},2))), 
                              vjust = -0.5,hjust="right"), 
                          color=colorl[5],size=geom.text.size, 
                          check_overlap = T) +
                #95% cf. int. mean
                geom_rect(data = data[np,], #prevent drawing rect every repeated row
                          aes(xmin = -Inf, xmax = Inf, 
                              ymin = {{m.ymin}}, ymax = {{m.ymax}}),
                          fill = colorl[5], alpha = 0.2,color = NA) +
                #lower bound line
                geom_hline(aes(yintercept = {{hl.lower}}), 
                           color = colorl[4], linetype="dashed") +
                geom_text(aes(xlim.upp,{{hl.lower}},
                              label = paste("M - 1.96SD:",as.character(round({{hl.lower}},2))), 
                              vjust = 2.5,hjust="right"), 
                          color=colorl[4],size=geom.text.size,
                          check_overlap = T, parse = F) +
                geom_rect(data = data[np,],aes(xmin = -Inf, xmax = Inf, 
                                                     ymin = {{lwr.ymin}}, ymax = {{lwr.ymax}}),
                          fill = colorl[4], alpha = 0.2,color = NA) +
                #upper bound line
                geom_hline(aes(yintercept = {{hl.upper}}), 
                           color = colorl[4], linetype="dashed")  +
                geom_text(aes(xlim.upp,{{hl.upper}},
                              label = paste("M + 1.96SD:",as.character(round({{hl.upper}},2))), 
                              vjust = -1.5 ,hjust="right"), size=geom.text.size,
                          color=colorl[4],check_overlap = T, parse = F) +
                geom_rect(data = data[np,],aes(xmin = -Inf, xmax = Inf, 
                                                     ymin = {{upr.ymin}}, ymax = {{upr.ymax}}),
                          fill = colorl[4], alpha = 0.2,color = NA) +
                xlim(xlim.low, xlim.upp) + ylim(ylim.low, ylim.upp) +
                scale_color_brewer(palette = "Set1",) +
                ylab(label = ylabel) + xlab(label = xlabel) +
                theme(text = element_text(size=theme.size),legend.position="none",
                      axis.text.x = element_text(angle = 30, 
                                                 vjust = 0.75, hjust=0.5))
}

ordinalReliability <- function(x){
        #Polychoric correlation using Fox's method
        poly.x <- POLYCHORIC_R(x, method = 'Fox',verbose = F)
        
        #alpha and omega
        alpha.x <- MBESS::ci.reliability(S=poly.x,N=nrow(x),type = "alpha",interval.type='none')
        omega.x <- MBESS::ci.reliability(S=poly.x,N=nrow(x),type = "omega",interval.type='none')
        
        print(paste0("Cronbach's Alpha (Ordinal): ",round(alpha.x$est,2)))
        print(paste0("Mcdonald's Omega (Ordinal): ",round(omega.x$est,2)))
        
        return(list(alpha.x$est,omega.x$est,poly.x))
}