#' readKOSIS
#'
#' Read KOSIS statistic table into data frame using URL
#'
#' @export
readKOSIS = function(url) {

  url = url %>% .s_rpl("\\.api_kosis", .api_kosis)

  response <- GET(url)
  data <- fromJSON(content(response, "text", encoding = "UTF-8"))
  df <- as.data.frame(data)
  return(df)
}

#' mkURLs
#'
#' Make URL list
#'
#' @export
.mkURLs = function() {
  a = c("건설공사비지수(2020년기준)"="https://kosis.kr/openapi/Param/statisticsParameterData.do?method=getList&apiKey=.api_kosis&itmId=16397AAA0+&objL1=ALL&objL2=&objL3=&objL4=&objL5=&objL6=&objL7=&objL8=&format=json&jsonVD=Y&prdSe=M&newEstPrdCnt=72&orgId=397&tblId=DT_39701_A003")
  URLs <<- data.frame(TBL_NM = names(a), URL = a)
}
