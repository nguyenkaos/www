###########################################
######_____________OUTILS_R_________#######
###########################################
###  LIBRARY

library(quantmod)
library(TTR) 
library(httr) 
library(jsonlite)


###########################################
######__1___BASIC___________________#######
###########################################

is_matrix <- function(dd) {
    if (is.vector(dd)){
        return (FALSE)
    } 
    tryCatch({ 
        if (dim(dd)[1] > 0 && dim(dd)[2] > 0) {
            return(TRUE)
        } else {
            return(FALSE)
        }
    }, error = function(e) { 
        return (FALSE)
    } );   
    
}

tomatrix <- function(dd){
    if( is_matrix(dd) ){
        return(dd)
    } else {
        ddd = matrix(dd, ncol=length(dd))
        return(ddd)
    }
}


### using naive correlation 
C_cor=get("C_cor", asNamespace("stats"))

### try_catch bloc
#  tryCatch({ 
#      ii=1/0;  
#  }, error = function(e) { 
#    print(paste('[ERROR]',e))
#  } );   


### require R.utils
checktime <- function(func,TimeOut=9999){
    try(withTimeout({func;},timeout=TimeOut, onTimeout="silent" ), TRUE) -> ii
    signal=inherits(ii,"try-error")
    return(list(signal=signal,res=ii))
}


### TELEGRAM => bot logmo
log_dev <- function(text){ 
    tryCatch({ 
        TELEGRAM_API = "https://api.telegram.org/bot726898103:AAGNsQGgF81ok7fIZKlm2vC-ij8cs38G3LY/sendMessage?chat_id=350717638&parse_mode=Markdown&text="
        url_telegram = paste(TELEGRAM_API, text, sep="")
        r = GET(url_telegram)
    }, error = function(e) { 
   print(text)
 } );   
}




###########################################
######_ 2 - FONCTION UTIL FOR STOCK VN   ##
###########################################

## Get Data for a stock in VIETNAM Market
get_data_symbol <- function(symbol, toDate, nbDay=60){ 
    ## toDate = '14/05/2018'  'dd/mm/yyyy' 
    todate = as.numeric(as.POSIXct(toDate, format="%d/%m/%Y"))
    fromdate = todate - 3600*24*nbDay
    res=c()

  for ( ii in 1:4 ){
      tryCatch({
          r <- GET(paste('https://api.vietstock.vn/ta/history?symbol=',symbol,'&resolution=D&from=',fromdate,'&to=',todate, sep=""), timeout(60),add_headers('User-Agent'= 'Mozilla/5.0 (Linux; Android 6.0; Nexus 5 Build/MRA58N) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/83.0.4103.97 Mobile Safari/537.36'))
          aa = content(r) 
          if (length(aa) < 1 ){
              return(res)
          }

          aa <- fromJSON(aa)
          vol = aa[['v']]
          if (length(vol) < 2){
            return(res)
          }
          close = aa[['c']] / 1000
          high = aa[['h']] / 1000
          low = aa[['l']] / 1000
          open = aa[['o']] / 1000
          timestamp = aa[['t']]
          res = cbind(close,vol,high,low,open,timestamp)
          break;
      }, error = function(e) {
          # print('iii');
         passs = 1
         Sys.sleep(40)
      });
  }
    return(res)
}


## Get Result for a stock Predict in VIETNAM Market
check_result_symbol <- function(symbol,toDate,postDay=40){
    # toDate = "dd/mm/yyy"
    # dd=substring(toDate,1,2)
    # mm=substring(toDate,4,5)
    # yy=substring(toDate,7,10)
    # mydate = paste(dd,mm,yy,sep="")
    fromdate = as.numeric(as.POSIXct(toDate, format="%d/%m/%Y"))
    todate  = fromdate + 3600*24*postDay
    res=c()
    j=1
 
    tryCatch({ 

        ok = 0
        for(iii in 1:18){
            if(ok == 0) {
                print(paste("in check_result_symbol ", Sys.time()))
                tryCatch({
                    r <- GET(paste('https://api.vietstock.vn/ta/history?symbol=',symbol,'&resolution=D&from=',fromdate,'&to=',todate, sep=""), timeout(45), add_headers('User-Agent'= 'Mozilla/5.0 (Linux; Android 6.0; Nexus 5 Build/MRA58N) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/83.0.4103.97 Mobile Safari/537.36'))
                    aa = content(r) 
                    if (length(aa) < 1 ){
                        return(res)
                    }
                    aa <- fromJSON(aa)
                    if ( length(aa[['c']]) < 1) {return(c())} 
                    close = aa[['c']] / 1000
                    lastPoint = close[1]
                    peak = max(close[4:length(close)]) 
                    result = ( peak - lastPoint ) / lastPoint 
                    return (round(result,4) )
                }, error = function(e) {  passs = 1 });
            }
            if(ok == 0){
                Sys.sleep(30)
            } else {break;}
        } 

    }, error = function(e) {
        passs = 1
    } );
    return(c())
    return (round(result,4) )
}


## Check if 'toDate' is a trading day or not
check_today_is_trade <- function(toDate){
  result = FALSE
    todate = as.numeric(as.POSIXct(toDate, format="%d/%m/%Y")) + 3600*10
    fromdate = todate - 3600*24*20
    
    for ( ii in 1:20 ){
      tryCatch({
          r <- GET(paste('https://api.vietstock.vn/ta/history?symbol=AAA&resolution=D&from=',fromdate,'&to=',todate, sep=""), timeout(60), add_headers('User-Agent'= 'Mozilla/5.0 (Linux; Android 6.0; Nexus 5 Build/MRA58N) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/83.0.4103.97 Mobile Safari/537.36'))
        aa = content(r)
        if (length(aa) < 1 ){
            return(res)
        }
        aa <- fromJSON(aa)
        timestamp = aa[['t']]
        time_sell_text = format(as.Date(as.POSIXct(timestamp[length(timestamp)], origin="1970-01-01")),"%d/%m/%Y")
        if (toDate == time_sell_text){
          result = TRUE
        }
          break;
      }, error = function(e) {
          # print('iii');
         Sys.sleep(40)
      });
  }

  return(result)
}



get_syms_vn <- function(){
    ## Get All Syms Stock VN
    library(httr)
    r <- GET("https://raw.githubusercontent.com/nguyenkaos/www/master/syms.txt")
    aa = content(r)
    syms = strsplit(aa,'\n')[[1]]
    return(syms)
}

get_syms_w <- function(){
    ## Get All Syms Stock US
    library(httr)
    r <- GET("https://raw.githubusercontent.com/nguyenkaos/www/master/symsx.txt")
    aa = content(r)
    syms = strsplit(aa,'\n')[[1]]
    return(syms)
}



############# DATE & TIME ##############

get_toDate3 <- function(toDate){
    ## convert 31/01/2010 => 20100131
    if ( !grepl("/", toDate) ) {
        return(toDate)
    }
    dday = substring(toDate, 1,2)
    dmonth = substring(toDate, 4,5)
    dyear = substring(toDate, 7,10)  
    toDate3 = paste(dyear,dmonth,dday,sep="")
    return(toDate3)
}

get_toDate <- function(toDate){
    ## convert 20100131 => 31/01/2010 
    if ( grepl("/", toDate) ) {
        return(toDate)
    }
    dday = substring(toDate, 7,8)
    dmonth = substring(toDate, 5,6)
    dyear = substring(toDate, 1,4)  
    toDate0 = paste(dday, dmonth, dyear, sep="/")
    return(toDate0)
}

get_timestamp <- function(toDate){
    ## convert 31/01/2010  => ts
    my_ts = as.numeric(as.POSIXct(toDate, format="%d/%m/%Y"))
    return(my_ts)
}

get_timestamp_from_toDate3 <- function(toDate){
    ## convert 20100131  => ts
    my_ts = as.numeric(as.POSIXct(toDate, format="%Y%m%d"))
    return(my_ts)
}


