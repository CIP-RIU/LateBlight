#######################################
# LATE BLIGHT FORECASTING MODEL FOR PERÚ
# International Potato Center ; Lima, Perú
# Authors: Quispe, M., Juarez, H.
#######################################

# Install and call packages
pkgs = c("maptools", "data.table", "tidyr", "dplyr")
# install.packages(pkgs)
lapply(pkgs, library, character.only = TRUE)

######################################

# function for ranges
in_range <- function(val,min,max) {
  return(val >= min & val <= max);
}

in_range2 <- function(val,min,max) {
  return(val > min & val <= max);
}

# function for calculate blight units
# INPUTS: hrhours>90 , avg temp, variety type (resistance)

calc_bu <- function(hhr,htavg,vt) {
  if(htavg > 27 & hhr == 24) {
    return(0);
    
  } else if(in_range2(htavg,22.5,27)) {
      
      if (vt == "s") {   
        
        if(hhr == 6) {
          return(0);
        }
        
        else if(in_range(hhr,7.0,9.0)) {
          return(1);
        }
        
        else if(in_range(hhr,10.0,12.0)) {
          return(2);
        }
        
        else if(in_range(hhr,13.0,15.0)) {
          return(3);
        }
        
        else if(in_range(hhr,16.0,18.0)) {
          return(4);
        }
        
        else if(in_range(hhr,19.0,24.0)) {
          return(5);
          
        }else {
          return(0);
        }
        
      }
      
      else if (vt == "ms") {
        
        if(hhr == 9) {
          return(0);
        }
        
        else if(in_range(hhr,10.0,18.0)) {
          return(1);
        }
        
        else if(in_range(hhr,19.0,24.0)) {
          return(2);
          
        } else{
          return(0);
        }
      }
      
      else if (vt == "mr" | vt == "r" | vt == "hr") {
        
        if(hhr == 15) {
          return(0);
        }
        
        else if(in_range(hhr,16.0,24.0)) {
          return(1);
          
        } else{
          return(0);
        }
      }
    
      else {
        return(0)
      }
      
    }
    
    else if(in_range2(htavg,12.5,22.5)) {
      
      if(vt == "s") {     
        
        if(hhr == 6) {
          return(0);
        }
        
        else if(in_range(hhr,7.0,9.0)) {
          return(5);
        }
        
        else if(in_range(hhr,10.0,12.0)) {
          return(6);
        }
        
        else if(in_range(hhr,13.0,24.0)) {
          return(7);
        }
        
        else{
          return(0);
        }
      }
      
      else if(vt == "ms") {
        
        if(hhr == 6) {
          return(0);
        }
        
        else if(hhr == 7) {
          return(1);
        }
        
        else if(hhr == 8) {
          return(2);
        }
        
        else if(hhr == 9) {
          return(3);
        }
        
        else if(hhr == 10) {
          return(4);
        }
        
        else if(in_range(hhr,11.0,12.0)) {
          return(5);
        }
        
        else if(in_range(hhr,13.0,24.0)) {
          return(6);
        }
        
        else{
          return(0)
        }
      }
      
      else if(vt == "mr" | vt == "r" | vt == "hr") {
        
        if(hhr == 6) {
          return(0);
        }
        
        else if(hhr == 7) {
          return(1);
        }
        
        else if(hhr == 8) {
          return(2);
        }
        
        else if(hhr == 9) {
          return(3);
        }
        
        else if(in_range(hhr,10.0,12.0)) {
          return(4);
        }
        
        else if(in_range(hhr,13.0,24.0)) {
          return(5);
        }
        else {
          return(0);
        }
      }
      
      else{
        return(0)
      }
    }
    
    else if(in_range2(htavg,7.5,12.5)) {    
      
      if(vt == "s") {
        
        if(hhr == 6) {
          return(0);
        }
        
        else if(hhr == 7) {
          return(1);
        }
        
        else if(in_range(hhr,8.0,9.0)) {
          return(2);
        }
        
        else if(hhr == 10) {
          return(3);
        }
        
        else if(in_range(hhr,11.0,12.0)) {
          return(4);
        }
        
        else if(in_range(hhr,13.0,15.0)) {
          return(5);
        }
        
        else if(in_range(hhr,16.0,24.0)) {
          return(6);
        }
        
        else{
          return(0)
        }
      }  
      
      else if(vt == "ms") {
        
        if(hhr == 6) {
          return(0);
        }
        
        else if(in_range(hhr,7.0,9.0)) {
          return(1);
        }
        
        else if(in_range(hhr,10.0,12.0)) {
          return(2);
        }
        
        else if(in_range(hhr,13.0,15.0)) {
          return(3);
        }
        
        else if(in_range(hhr,16.0,18.0)) {
          return(4);
        }
        
        else if(in_range(hhr,19.0,24.0)) {
          return(5);
        }
        
        else{
          return(0);
        }
      }
      
      else if(vt == "mr" | vt == "r" | vt == "hr") {
        
        if(hhr == 9) {
          return(0);
        }
        
        else if(in_range(hhr,10.0,12.0)) {
          return(1);
        }
        
        else if(in_range(hhr,13.0,15.0)) {
          return(2);
        }
        
        else if(in_range(hhr,16.0,24.0)) {
          return(3);
        }
        else {
          return(0);
        }
      }
      
      else {
        return(0);
      }
    }
    
    else if(in_range(htavg,3,7.5)) {    
      
      if(vt == "s") {
        
        if(hhr == 9) {
          return(0);
        }
        
        else if(in_range(hhr,10.0,12.0)) {
          return(1);
        }
        
        else if(in_range(hhr,13.0,15.0)) {
          return(2);
        }
        
        else if(in_range(hhr,16.0,18.0)) {
          return(3);
        }
        
        else if(in_range(hhr,19.0,24.0)) {
          return(4);
        }
        
        else{
          return(0);
        }
      }
      
      else if(vt == "ms") {
        
        if(hhr == 12) {
          return(0);
        }
        
        else if(in_range(hhr,13.0,24.0)) {
          return(1);
        }
        
        else{
          return(0);
        }
      }
      
      else if(vt == "mr" | vt == "r" | vt == "hr") {
        
        if(hhr == 18) {
          return(0);
        }
        
        else if(in_range(hhr,19.0,24.0)) {
          return(1);
        }
        
        else{
          return(0)
        }
      }
      
      else{
        return(0)
      }
    }
    
    else if(htavg < 3 & hhr == 24) {     
      return(0)
      }
    
    else {
      return(0)
      
    }
    
  }

# function for calculate fungicide units
# INPUTS: rain mm, dsa - days since fungicide aplication 

calc_fu <- function(rain,dsa) {
  if(rain < 1 & rain > 0) {
    return(1);
    
  } else {
    
    if(dsa == 1) {
      
      if(in_range(rain,1.0,1.45)) {
        return(4);
      }
      
      else if(in_range2(rain,1.45,3.45)) {
        return(5);
      }
      
      else if(in_range2(rain,3.45,6.0)) {
        return(6);
      }
      
      else if(rain > 6) {
        return(7);
      }
      
      else{
        return(0);
      }
    }
    
    else if(dsa == 2) {
      
      if(in_range(rain,1.0,1.45)) {
        return(3);
      }
      
      else if(in_range2(rain,1.45,4.45)) {
        return(4);
      }
      
      else if(in_range2(rain,4.45,8.0)) {
        return(5);
      }
      
      else if(rain > 8) {
        return(6);
      }
      
      else{
        return(0);
      }
    }
    
    else if(dsa == 3) {
      
      if(in_range(rain,1.0,2.45)) {
        return(3);
      }
      
      else if(in_range2(rain,2.45,5.0)) {
        return(4);
      }
      
      else if(rain > 5) {
        return(5);
      }
      
      else{
        return(0);
      }
    }
    
    else if(in_range(dsa,4.0,5.0)) {
      
      if(in_range(rain,1.0,2.45)) {
        return(3);
      }
      
      else if(in_range2(rain,2.45,8)) {
        return(4);
      }
      
      else if(rain > 8) {
        return(5);
      }
      
      else{
        return(0);
      }
    }
    
    else if(in_range(dsa,6.0,9.0)) {
      
      if(in_range(rain,1.0,4.0)) {
        return(3);
      }
      
      else if(rain > 4) {
        return(4);
      }
      
      else{
        return(0);
      }
    }
    
    else if (in_range(dsa,10.0,14.0)) {
      
      if(in_range(rain,1.0,1.45)) {
        return(2);
      }
      
      else if(in_range2(rain,1.45,8.0)) {
        return(3);
      }
      
      else if(rain > 8) {
        return(4);
      }
      
      else{
        return(0);
      }
    }     
    
    else if (dsa > 14) {
      
      if(in_range(rain,1.0,8.0)) {
        return(2);
      }
      
      else if(rain > 8) {
        return(3);
      } 
      
      else{
        return(0);
      }
    }
    
    else {
      return(0);
    }
    
  }
  
}

# function for decision rules
# INPUTS: abu, afu 

check_bu_cutoff <- function(abu,vt) {
  
  if(vt == "s" & abu >= 30) {
    return(TRUE);
    
  } else if(vt == "ms" & abu >= 35) {
    return(TRUE);
    
  } else if(vt == "mr" & abu >= 40) {
    return(TRUE);
    
  } else if(vt == "r" & abu >= 45) {
    return(TRUE);
    
  } else if(vt == "hr" & abu >= 50){
    return(TRUE);  
    
  } else {
    return(FALSE);
  }
}

check_fu_cutoff <- function(afu,vt) {
  
  if(vt == "s" & afu >= 15) {
    return(TRUE);
    
  } else if(vt == "ms" & afu >= 20) {
    return(TRUE);
    
  } else if(vt == "mr" & afu >= 25) {
    return(TRUE);
    
  } else if(vt == "r" & afu >= 30) {
    return(TRUE);
    
  } else if(vt == "hr" & afu >= 35){
    return(TRUE);  
    
  } else {
    return(FALSE);
  }
}

# function for calculate humidity relative hourly 
# INPUTS: meterological data, coordinates (lat y long)

calculate_hhr <- function(climdata, lon, lat) {
  
  #########################
  
  date_df <- as.Date(climdata[[1]], format = "%Y-%m-%d")
  tn <- as.numeric(climdata[[4]])
  tx <- as.numeric(climdata[[3]])
  tavg <- (tn+tx)/2
  hrn <- as.numeric(climdata[[8]])
  hrx <- as.numeric(climdata[[7]])
  hravg <- (hrn+hrx)/2
  tdew <- tavg - ( (100 - hravg) / 5)
  rain <- as.numeric(climdata[[5]])
  
  
  # Calculate hourly for sunrise and sunset
  
  crds <- matrix(c(lon[[1]], lat[[1]]), ncol = 2)
  
  sunrise <- sunriset(crds, as.POSIXct(date_df),
                      proj4string=CRS("+proj=longlat +datum=WGS84"),
                      direction=c("sunrise"), POSIXct.out= F)
  
  sunset <- sunriset(crds, as.POSIXct(date_df),
                     proj4string=CRS("+proj=longlat +datum=WGS84"),
                     direction=c("sunset"), POSIXct.out= F)
  
  
  df_format_hhr <- data.frame(date_df[-c(length(date_df),1)], tn[-c(length(tn),1)], tn[-c(1:2)], 
                              tx[-c(length(tx),1)], tx[-c(length(tx), length(tx) - 1)], 
                              tdew[-c(length(tdew),1)],
                              sunrise[-c(length(sunrise),1)], sunset[-c(length(sunset),1)], 
                              sunset[-c(length(sunset),length(sunset) - 1)])
  
  names(df_format_hhr) <- c("date", "tmin", "tminnext", 
                            "tmax", "tmaxold", "tdew",
                            "sunrise", "sunset", "sunset old")
  
  #########################
  
  # Tn temp min, Tx temp max, To sunset , Tp temp min next
  # Hn sunrise, Hx h temp max, Ho h sunset , Hp h temp min next
  # date <- df_in_hhr[[1]]
  
  #####
  
  model_temp_hr <- function(df_in_hhr){
    
    Tn <- as.numeric(df_in_hhr[[2]])
    Tp <- as.numeric(df_in_hhr[[3]])
    Tx <- as.numeric(df_in_hhr[[4]])
    Hn <- as.numeric(df_in_hhr[[7]]) * 24
    Ho <- as.numeric(df_in_hhr[[8]]) * 24
    tdew <- as.numeric(df_in_hhr[[6]])
    
    #####
    
    Tp_old <- Tn
    Hp_old <- Hn + 24
    Tx_old <- as.numeric(df_in_hhr[[5]])
    Ho_old <- as.numeric(df_in_hhr[[9]]) * 24
    To_old <- Tx_old - 0.39*(Tx_old - Tp_old)
    
    # Parameters for model 
    To <- Tx - 0.39*(Tx - Tp)
    Hp <- Hn + 24
    Hx <- Ho - 4
    
    alpha <- Tx - Tn
    r <- Tx - To
    beta1 <- (Tp - To) / sqrt(Hp - Ho)
    beta2 <- (Tp_old - To_old) / sqrt(Hp_old - Ho_old)
  
    t <- 1:24
    T_model <- 0
  
    for(i in 1:24) {
        
      if(t[i] > Hn & t[i] <= Hx){
        
        T_model[i] <- Tn + alpha * ( ((t[i]-Hn) / (Hx-Hn)) * (pi/2) )
      
        }
      
      else if(t[i] > Hx & t[i] <= Ho){
        
        T_model[i] <- To + r * sin( (pi/2) + ( ((t[i]-Hx)/4) * (pi/2) ) )
      
        }
    
      else if( t[i] > Ho & t[i] <= 24 ){
        
        T_model[i] <- To + beta1 * sqrt( (t[i] - Ho) )
      
        }
      
      else if (t[i] >= 1 & t[i] <= Hn){
        
        T_model[i] <- To_old + beta2 * sqrt( (t[i]+24) - Ho_old) 
        
      }
      
      
      else {
        T_model[i] <- "Error"
      
        }
        
    }

  
  # Buck formula for es and e (kPa)
  
  es <- 0.61121 * exp( ( (18.678 - (T_model/234.5)) * T_model) / (257.14 + T_model))
  e <- 0.61121 * exp( ( (18.678 - (tdew/234.5)) * tdew) / (257.14 + tdew))
  
  # hr 
  hr <- (e/es) * 100
  df <- data.frame(T_model, hr)
  
  return(df)
  }
  
  
  df_temp_hr_hour <- apply(df_format_hhr, 1 , function(x) model_temp_hr(x))
  
  df_temp_hr_hour_simcast <- list()
  
  for (i in 1:(length(df_temp_hr_hour)-1)) {
    
    df_temp_hr_hour_simcast[[i]] <- rbind(df_temp_hr_hour[[i]][13:24,], 
                                          df_temp_hr_hour[[i+1]][1:12,]);
    
  }
 
  
  hr_limit90_temp <- function(x){
    
    
    temp_model_prom <- ifelse( nrow(x[ x[,2] > 90, ]) > 0, 
                               mean(x[ x[,2] > 90, 1]),
                               mean(x[ , 1]))
                           
    
    hr_model_hours <- nrow(x[ x[,2] > 90, ])
    df <- data.frame(temp_model_prom, hr_model_hours)
    return(df)
  }
  
  
  list_hours <- lapply(df_temp_hr_hour_simcast, function(x) hr_limit90_temp(x))
  df_hours <- rbindlist(list_hours)
  

  df_output_hr <- data.frame(date_df[-c(length(date_df),1,2)], df_hours[, 2],
                             tavg[-c(length(date_df),1,2)], rain[-c(length(rain),1,2)])
                                     
  names(df_output_hr) <- c("date", "hr90_hour", "tavg_C", "rain_mm")
  
  print("Run temperature and humidity model...")   
  
  return(df_output_hr)

  }


# function for SIMCAST model Fry et. al 1983

simcast_model <- function(df_in_simcast, vt){
 
  vt <- as.character(vt[[1]])
  
  lines <- list()
  for(i in 1:nrow(df_in_simcast)){
    lines[[i]] = df_in_simcast[i,]
  }
  
  #########
  firstApplication <- TRUE
  returnValue <- 1
  bu <- 0
  fu <- 0
  bua <- 0
  fua <- 0
  afu <- 0
  app <- 0
  i <- 0
  last_bua <- 0
  last_fua <- 0
  app_ctr <- 0
  days_since_app <- 0
  min_day <- 7
  abu <- 0
  tabu <- 0
  tafu <- 0
  #begin <- 0
  end <- length(lines)
  ###############
  
  bu_final <- 0
  fu_final <- 0
  bua_final <- 0
  fua_final <- 0
  abu_final <- 0
  afu_final <- 0
  app_final <- 0
  
  for (k in 1:end){
    line <- lines[[k]]
    day <- line[[1]]
    hhr <- as.numeric(line[[2]])
    htavg <- as.numeric(line[[3]])
    rain <- as.numeric(line[[4]])
    bu <- calc_bu(hhr, htavg, vt)
    last_bua <- bua
    bua <- bu + bua
    
    if(abu != 1 && afu != 1 && days_since_app <= 0){
      days_since_app <- 0
      
    } else {
      days_since_app <- days_since_app + 1
      fu = calc_fu(rain, days_since_app)
      last_fua = fu
      fua = fu + fua
    }
    
    # modify && by ll (and "!") if you need the first app according to the dss
    
    if(days_since_app > min_day && !firstApplication){
      
      if( check_bu_cutoff(bua, vt) ) {
        bua <- 0
        fua <- 0
      }
      
      if( check_fu_cutoff(fua, vt) ) {
        bua <- 0
        fua <- 0
      }
    }
    
    if (bua >= last_bua || days_since_app <= min_day && !firstApplication) {
      app <- FALSE
      abu <- 0
      
      } else {
      abu <- 1
      app <- TRUE
      app_ctr <- app_ctr + 1
      days_since_app <- 0
      #firstApplication <- FALSE
      
      }
    
    if(k == 25) {
      
      abu <- 1
      app <- TRUE
      app_ctr <- app_ctr + 1
      days_since_app <- 0
      firstApplication <- FALSE
      bua <- 0
      fua <- 0
    }
    
    if (fua < last_fua && days_since_app > min_day) {
      afu <- 1
      app <- TRUE
      app_ctr <- app_ctr + 1
      days_since_app <- 0
    
      } else {
      app <- FALSE
      afu <- 0
    }
    
    tabu <- abu + tabu
    tafu <- afu + tafu
    
    bu_final[k] <- bu
    fu_final[k] <- fu
    bua_final[k] <- bua
    fua_final[k] <- fua
    abu_final[k] <- abu
    afu_final[k] <- afu
    app_final[k] <- app_ctr
  }
  
  output_simcast <- data.frame(bu_final, fu_final, bua_final, fua_final,
                                  abu_final, afu_final, app_final)
  
  names(output_simcast) <- c("BU", "FU", "BUA", "FUA", "ABU", "AFU", "APP")
  
  output_scmodel <- cbind(df_in_simcast, output_simcast) 
  
  print("Running simcast model ... Loading")
  
  return(output_scmodel)
}  
  

 ####################
# Import data and sampling

setwd("C:/Users/Asus/Desktop/")
df_clima <- fread("weather_data.csv",header=T)
#df_clima$DAT <- as.Date(df_clima$DAT, format = "%Y-%m-%d")
str(df_clima)

# -2 days initial / +1 day final for growing period
dias_cultivo1 <- seq(as.Date('2007-11-13'),as.Date('2008-04-16'),by = 1)
dias_cultivo2 <- seq(as.Date('2008-07-13'),as.Date('2008-12-16'),by = 1)
dias_cultivo3 <- seq(as.Date('2008-04-13'),as.Date('2008-07-31'),by = 1)

# select area of interest

df_zonas_papa <- read.csv("zonas-papa.csv", header = T, 
                          stringsAsFactors = F, sep =",")
unique(df_zonas_papa$Type)

df_zona1 <- df_zonas_papa[df_zonas_papa$Type == "Sierra Alta [>3,000]", 2]
df_zona2 <- df_zonas_papa[df_zonas_papa$Type == "Sierra Media [2000-3000 masl]", 2]
df_zona3 <- df_zonas_papa[df_zonas_papa$Type == "Costa [0-500 msnm] Interandino [<2,000]", 2]


df_select1 <- df_clima[df_clima$DAT %in% dias_cultivo1 & df_clima$CELL5M %in% df_zona1 , ]
df_select2 <- df_clima[df_clima$DAT %in% dias_cultivo2 & df_clima$CELL5M %in% df_zona2 , ]
df_select3 <- df_clima[df_clima$DAT %in% dias_cultivo3 & df_clima$CELL5M %in% df_zona3 , ]

###############3

df_select1_2 <- df_select1  %>% nest(data = c(DAT, SRAD, TMAX, TMIN, RAIN, WIND, RH_MAX, RH_MIN))
df_select2_2 <- df_select2  %>% nest(data = c(DAT, SRAD, TMAX, TMIN, RAIN, WIND, RH_MAX, RH_MIN))
df_select3_2 <- df_select3  %>% nest(data = c(DAT, SRAD, TMAX, TMIN, RAIN, WIND, RH_MAX, RH_MIN))

rm(df_clima, df_select1, df_select2, df_select3)
head(df_select1_2)

df_coords <- read.csv("data_coords.csv", stringsAsFactors = F, sep = ",")

df_merg1 <- inner_join(df_select1_2, df_coords, by="CELL5M")
df_merg2 <- inner_join(df_select2_2, df_coords, by="CELL5M")
df_merg3 <- inner_join(df_select3_2, df_coords, by="CELL5M")

###################

# inputs for dataframe with date, hr>90, tavg and rain: 
# climate data, lon, lat

input_runsimcast1 <- apply(df_merg1, 1, function(x) calculate_hhr(x[[2]], x[[10]], x[[9]]))
input_runsimcast2 <- apply(df_merg2, 1, function(x) calculate_hhr(x[[2]], x[[10]], x[[9]]))
input_runsimcast3 <- apply(df_merg3, 1, function(x) calculate_hhr(x[[2]], x[[10]], x[[9]]))

# inputs for run simcast: 
# input_runsimcast, variety type (resistence)


output_runsimcast1 <- lapply(input_runsimcast1,
                           function(x) simcast_model(x, "r"))
output_runsimcast2 <- lapply(input_runsimcast2,
                             function(x) simcast_model(x, "r"))
output_runsimcast3 <- lapply(input_runsimcast3,
                             function(x) simcast_model(x, "r"))


#############################################

# Create raster
library(raster)
library(ggplot2)
library(scales)
library(ggsn)
library(openxlsx)

runsimcast_app1 <- lapply(output_runsimcast1,  function(x) x[[11]][length(x[[11]])])
runsimcast_app2 <- lapply(output_runsimcast2,  function(x) x[[11]][length(x[[11]])])
runsimcast_app3 <- lapply(output_runsimcast3,  function(x) x[[11]][length(x[[11]])])

runsimcast_bu_app1 <- lapply(output_runsimcast1,  function(x) nrow(x[x$ABU == 1,]))
runsimcast_bu_app2 <- lapply(output_runsimcast2,  function(x) nrow(x[x$ABU == 1,]))
runsimcast_bu_app3 <- lapply(output_runsimcast3,  function(x) nrow(x[x$ABU == 1,]))

runsimcast_fu_app1 <- lapply(output_runsimcast1,  function(x) nrow(x[x$AFU == 1,]))
runsimcast_fu_app2 <- lapply(output_runsimcast2,  function(x) nrow(x[x$AFU == 1,]))
runsimcast_fu_app3 <- lapply(output_runsimcast3,  function(x) nrow(x[x$AFU == 1,]))



df_app_coords1 <- cbind( df_merg1[,c(10,9)],
                         app_bu_total = as.numeric(cbind(runsimcast_bu_app1)),
                         app_fu_total = as.numeric(cbind(runsimcast_fu_app1 )),
                         app_total = as.numeric(cbind(runsimcast_app1)))

df_app_coords1$type <- "Sierra Alta [>3000 msnm]"
  
df_app_coords2 <- cbind( df_merg2[,c(10,9)], 
                         app_bu_total = as.numeric(cbind(runsimcast_bu_app2)),
                         app_fu_total = as.numeric(cbind(runsimcast_fu_app2)),
                         app_total = as.numeric(cbind(runsimcast_app2)))

df_app_coords2$type <- "Sierra Media [2000-3000 msnm]"

df_app_coords3 <- cbind( df_merg3[,c(10,9)], 
                         app_bu_total = as.numeric(cbind(runsimcast_bu_app3)),
                         app_fu_total = as.numeric(cbind(runsimcast_fu_app3)),
                         app_total = as.numeric(cbind(runsimcast_app3)))

df_app_coords3$type <- "Costa [0-500 msnm] - Interandino [<2000 msnm]"

df_app_coords <- rbind(df_app_coords1, df_app_coords2, df_app_coords3, 
                       fill = T)

df_raster <- rasterFromXYZ(df_app_coords[,c(1,2,5)])
proj4string(df_raster)=CRS("+proj=longlat +datum=WGS84")


###########################
# Save data 

setwd("C:/Users/Asus/Documents/R/simcast-lbf-cip/mapas")
writeRaster(df_raster, filename="R-08.asc", format='ascii', overwrite=TRUE)
write.xlsx(df_app_coords, "R-08.xlsx", row.names = F)

######
peru <- getData("GADM", country= "PER", level=1)
peru <- fortify(peru)  
df_pol_peru <- data.frame(peru)

summary(df_app_coords$app_total) 


#######

df_app_coords <- df_app_coords %>%
  mutate(app_intervals = cut(app_total, breaks = c(-1, 5, 10, 12, 15, 50)))


plot_data <- ggplot() + 
  
                theme_bw()  +
                
                geom_tile(data = df_app_coords , aes(x = X, y = Y, fill = app_intervals)) +
                
                scale_fill_manual(values = c("blue","green", "yellow", 
                                             "orange", "red" ),
                                  name = "Number of total applications", 
                                  labels = c("0 - 5", "5 - 10", "10 - 12", 
                                             "12 - 15", "> 15")) +
              
                geom_map(data= df_pol_peru,
                         map= df_pol_peru,
                         aes(x=long,y=lat,map_id=id,group=group),
                         fill=NA,
                         colour="black") +
                
                coord_equal(xlim= c(-82.5, -66.5),
                            ylim= c(-20 , 0 )) +
                
                labs(title = "FUNGICIDE APPLICATION MODEL FOR POTATO LATE BLIGHT - R - 2008",
                     x = "Longitude",
                     y = "Latitude") +
                
                north(df_pol_peru, location = "topleft", symbol = 3, scale = 0.1) +
                
                scalebar(df_pol_peru, location = "bottomleft" , dist = 250, 
                         height = 0.02, st.size = 2,
                         dist_unit = "km", transform = TRUE, model = "WGS84") +
                
                theme(legend.position = "right", 
                      plot.title = element_text(hjust = 0.5, size = 12))


plot_data
 


library(microbenchmark)
microbenchmark()







