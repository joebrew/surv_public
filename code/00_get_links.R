###################
#SET DATE / TIME PARAMETERS
###################
today <- Sys.Date() 
yesterday <- today - 1

###################
#DEFINE AND SET WD
###################
if ( Sys.info()["sysname"] == "Linux" ){
  # Joe's linux
  if(Sys.info()["user"] == "joebrew"){
    private_today <- paste0("/media/joebrew/JB/fdoh/private/surv/", today)
    private <- "/media/joebrew/JB/fdoh/private/surv"
    private_historical <- "/media/joebrew/JB/fdoh/private/surv/historical"
    public_gis <- "/media/joebrew/JB/fdoh/private/surv/gis"
  }
  # Ben's linux
  else {
    private_today <- paste0("/home/benbrew/Documents/private/surv/", today)
    private <- "/home/benbrew/Documents/private/surv/"
    private_historical <- "/home/benbrew/Documents/private/surv/historical"
    public_gis <- "/home/benbrew/Documents/surv_public/gis"
  }
  # Joe's Windows computers:
} else {
  private_today <- paste0("E:/fdoh/private/surv/", today)
  private <- "E:/fdoh/private/surv"
  private_historical <- "E:/fdoh/private/surv/historical"
  public_gis <- "E:/fdoh/private/surv/gis"
}

# SET WD
setwd(private_today) 

get_link <- function(syndrome = NULL,
                     patient_location = "alachua",
                     hospital_location = NULL,
                     start_date = NULL,
                     end_date = NULL,
                     text = TRUE){
  
  # Patient location
  if(!is.null(patient_location)){
    patient_location <- paste0("patientLoc=", patient_location, "&")
  } else{
    patient_location <- ""
  }
  
  # Hospital location
  if(!is.null(hospital_location)){
    hospital_location <- paste0("geography=", hospital_location, "&")
  } else {
    hospital_location <- ""
  }
  
  # Syndrome
  if(!is.null(syndrome)){
    syndrome <- paste0("medicalGrouping=", syndrome, "&")
  } else {
    syndrome <- ""
  }
  
  # Start date
  if(is.null(start_date)){
    start_date <- paste0("startDate=", format(Sys.Date() - 7, format = "%d%b%Y"), "&")
  } else {
    start_date <- format(start_date, format = "%d%b%Y")
  }
  
  # End date
  if(is.null(end_date)){
    end_date <- paste0("endDate=", format(Sys.Date() - 1, format = "%d%b%Y"), "&")
  } else {
    end_date <- format(end_date, format = "%d%b%Y")
  }
  
  if(text){
    
    if(is.null(hospital_location)){
      link <- paste0("https://www.essencefl.com/florida_5_1_14/servlet/PlainDataDetailsServlet?ageCDCILI=all&", 
                     start_date, 
                     "medicalGroupingSystem=essencesyndromes&initPulseOx=all&sex=all&geographySystem=hospitalregion&predomRace=all&dom=all&patientClass=all&timeResolution=daily&doy=all&censusRaceBlackPercGroup=all&", 
                     end_date, "
                   dow=all&clinicalImpression=all&ageTenYear=all&detector=probrepswitch&geography=all&", 
                     patient_location, 
                     "age=all&dischargeDiagnosis=all&year=all&medicalSubGrouping=all&datasource=va_hosp&censusRaceAsianPercGroup=all&percentParam=noPercent&", 
                     syndrome, 
                     "timeInterval=all&aqtTarget=datadetails&hospitalGrouping=all&agerange=all&censusRaceHawaiianPercGroup=all&ccddFreeText=all&predomHispanic=all&initTemp=all&diagnosisType=all&censusRaceAmerIndPercGroup=all&dispositionCategory=all&medianIncomeGroup=all&agedistribute=all&month=all&ccddCategory=all&censusRaceOtherPercGroup=all&censusRaceWhitePercGroup=all&week=all&quarter=all")
      
    } else {
      link <- paste0("https://www.essencefl.com/florida_5_1_14/servlet/PlainDataDetailsServlet?ageCDCILI=all&", 
                     start_date, 
                     "medicalGroupingSystem=essencesyndromes&geographySystem=hospitalregion&dom=all&timeResolution=daily&doy=all&",
                     end_date, "dow=all&detector=probrepswitch&", 
                     patient_location, 
                     hospital_location, 
                     "year=all&datasource=va_hosp&percentParam=noPercent&", 
                     syndrome, 
                     "aqtTarget=DataDetails&month=all&week=all&quarter=all")
#                      "medicalGroupingSystem=essencesyndromes&initPulseOx=all&sex=all&geographySystem=hospitalregion&predomRace=all&dom=all&patientClass=all&timeResolution=daily&doy=all&censusRaceBlackPercGroup=all&", 
#                      end_date, 
#                      "dow=all&clinicalImpression=all&ageTenYear=all&detector=probrepswitch&", 
#                      hospital_location, 
#                      patient_location, 
#                      "age=all&dischargeDiagnosis=all&year=all&medicalSubGrouping=all&datasource=va_hosp&censusRaceAsianPercGroup=all&percentParam=noPercent&", 
#                      syndrome, 
#                      "timeInterval=all&aqtTarget=datadetails&hospitalGrouping=all&agerange=all&censusRaceHawaiianPercGroup=all&ccddFreeText=all&predomHispanic=all&initTemp=all&diagnosisType=all&censusRaceAmerIndPercGroup=all&dispositionCategory=all&medianIncomeGroup=all&agedistribute=all&month=all&ccddCategory=all&censusRaceOtherPercGroup=all&censusRaceWhitePercGroup=all&week=all&quarter=all")
      
    }

     } else{
    link <- paste0("https://www.essencefl.com/florida_5_1_14/servlet/DataDetailsServlet?", 
                   start_date, 
                   "medicalGroupingSystem=essencesyndromes&geographySystem=hospitalregion&dom=all&timeResolution=daily&doy=all&",
                   end_date, "dow=all&detector=probrepswitch&", 
                   patient_location, 
                   hospital_location, 
                   "year=all&datasource=va_hosp&percentParam=noPercent&", 
                   syndrome, 
                   "aqtTarget=DataDetails&month=all&week=all&quarter=all")
  }

  return(link)  
}



# CREATE DATAFRAME OF links
df <- data.frame(file = paste0(c("alless", "alless2", "roi", "roi2",
                          "gi", "ili", "neuro", "rash", "resp"), ".txt"))
df$link <- NA
df$link[which(df$file == "alless.txt")] <- get_link()
df$link[which(df$file == "alless2.txt")] <- get_link(patient_location = NULL,
                                                     hospital_location = "alachua")
df$link[which(df$file == "roi.txt")] <- get_link(syndrome = "RecordsOfInterest")
df$link[which(df$file == "roi2.txt")] <- get_link(patient_location = NULL,
                                                  hospital_location = "alachua",
                                                  syndrome = "RecordsOfInterest")
df$link[which(df$file == "gi.txt")] <- get_link("gi")
df$link[which(df$file == "ili.txt")] <- get_link("ili")
df$link[which(df$file == "neuro.txt")] <- get_link("neuro")
df$link[which(df$file == "rash.txt")] <- get_link("rash")
df$link[which(df$file == "resp.txt")] <- get_link("resp")

# Function to open link
# open_link <- function(syndrome){
#   browseURL(df$link[which(df$file == paste0(syndrome, ".txt"))])
# }

# Open each of the URLs
# open_link("alless")
# open_link("alless2")
# open_link("roi")
# open_link("roi2")
# open_link("gi")
# open_link("ili")
# open_link("neuro")
# open_link("rash")
# open_link("resp")

# WRITE A CSV WITH MOST RECENT LINKS
setwd(private)
write.csv(df, 'todays_links.csv')

#this are 9 files i need to download everday
#all cases of everything alless
#all caases of anybody who went to ac hospital alless2
#roi records of interest
#roi2 records of interest at ac hospital
#download links to surv in private, change names to the names above
#change name to functions


