#' readKOSIS
#'
#' Read KOSIS statistic table into data frame using URL
#'
#' @param url URL of KOSIS statistic table
#' @return Data frame of KOSIS statistic table
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
#' @return URL list
#' @export
.mkURLs = function() {
  a = c("건설공사비지수(2020년기준)"="https://kosis.kr/openapi/Param/statisticsParameterData.do?method=getList&apiKey=.api_kosis&itmId=16397AAA0+&objL1=ALL&objL2=&objL3=&objL4=&objL5=&objL6=&objL7=&objL8=&format=json&jsonVD=Y&prdSe=M&newEstPrdCnt=72&orgId=397&tblId=DT_39701_A003")
  URLs <<- data.frame(TBL_NM = names(a), URL = a)
}
#' monthly_to_daily_simple
#'
#' 월별 데이터를 일별 데이터로 변환하는 함수
#'
#' @param posix_dates POSIXct 클래스의 날짜 벡터
#' @param values 변환할 값 벡터
#' @return 변환된 일별 데이터 프레임
#' @export
monthly_to_daily_simple <- function(posix_dates, values) {
  require(lubridate)

  # 시작일과 종료일 설정
  start_date <- min(posix_dates)
  end_date <- max(posix_dates)

  # 일별 날짜 시퀀스 생성 (원본 데이터의 시간을 유지)
  ref_time <- format(posix_dates[1], "%H:%M:%S")
  daily_dates <- seq.POSIXt(
    from = start_date + hours(hour(posix_dates[1])) +
      minutes(minute(posix_dates[1])) +
      seconds(second(posix_dates[1])),
    to = end_date,
    by = "day"
  )

  # 선형 보간 적용
  daily_values <- approx(
    x = as.numeric(posix_dates),
    y = values,
    xout = as.numeric(daily_dates),
    method = "linear"
  )$y

  # 결과 데이터프레임 생성
  result <- data.frame(
    x = daily_dates,
    y = daily_values
  )

  return(result)
}
#' change2index
#'
#' 변동률을 지수로 변환
#'
#' @param x 변동률 벡터
#' @param .percent 변동률이 백분율인지 여부
#' @return 지수 벡터
#' @export
change2index = function(x, .percent = T) {
  y = c(100, cumprod(1 + x[2:length(x)]/ifelse(.percent,100,1)) * 100)
  return(y)
}
