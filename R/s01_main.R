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
#' .plot_duo
#'
#' 여러 데이터를 비교하는 그래프를 그리는 함수
#'
#' @param dat 데이터 프레임
#' @param X x축 변수명
#' @param Y y축 변수명
#' @param Z 비교 변수명
#' @param W 자료 분리 변수명
#' @param alt 비교군(범례) 목록
#' @param .w 비교군(범례) 목록에 대응하는 자료 분리 변수값
#' @param xlbl x축 레이블
#' @param ylbl y축 레이블
#' @param xcri x축 기준값
#' @param ycri y축 기준값
#' @param a_ratio 세로:가로 비율
#' @param .guide 범례 표시 여부
#' @param .fntSz 폰트 크기
#' @param .percent y축 백분율 표시 여부
#' @param .digits y축 소수점 자리수
#' @param .ylim y축 한계값
#' @param .yAxis y축 교차값
#' @return 그래프와 t.test 결과
#' @export
.plot_duo = function(dat=irr, X = "CD", Y = "value", Z = "대안", W = "임대기간"
                     , alt = c("비교"="비교", "현재"="현재")
                     , .w = c("10년", "10년")
                     , xlbl = "CD금리(%)", ylbl="value", ylbl_add = ""
                     , xcri = c("3.5"=3.5)
                     , ycri=c(0)
                     , a_ratio = 3/4
                     , .guide = T
                     , .fntSz = 16
                     , .percent = T
                     , .digits = 3
                     , .ylim = -9
                     , .yAxis = 0
                     , .bgCol = "grey95"
) {

  if (length(alt) == length(.w)) {
    dat = dat[dat[[Y]] > .ylim,]
    dat$x = dat[[X]]
    dat$y = dat[[Y]]
    dat$z = dat[[Z]]
    if (!is.na(W)) {
      dat$w = dat[[W]]
      dbi = 1:length(alt) %>% lapply(function(i) dat %>% .flt(z %in% alt[i], w == .w[i]) %>% mutate(비교 = names(alt[i])))
    } else {
      if (any(is.null(names(alt)))) names(alt) = alt
      dbi = 1:length(alt) %>% lapply(function(i) dat %>% .flt(z %in% alt[i]) %>% mutate(비교 = names(alt[i])))
    }

    # db1 = dat %>% .flt(대안 %in% alt[1], 임대기간 == .t[1]) %>% mutate(비교 = names(alt[1]))
    # db2 = dat %>% .flt(대안 %in% alt[2], 임대기간 == .t[2]) %>% mutate(비교 = names(alt[2]))

    library(aStarterFont)
    library(ggplot2)

    .shTxAuto()
    .shTxBasic()
    .shTxBold()

    X0 = function(db) {
      기준 = .yAxis
      xcri_0 = db %>% .flt(y >= 기준)
      if (nrow(xcri_0 > 0) && min(xcri_0$y) <= 기준 + 0.1 ^ .digits) {
        xcri_0 = xcri_0 %>% .flt(y == min(xcri_0$y)) %>% .$x
      } else {
        xcri_0 = NA
      }
      if (is.na(xcri_0[1])) {
        xcri_0 = db %>% .flt(y <= 0)
        if (nrow(xcri_0 > 0) && max(xcri_0$y) >= 기준 - 0.1 ^ .digits) {
          xcri_0 = xcri_0 %>% .flt(y == max(xcri_0$y)) %>% .$x
        } else {
          xcri_0 = NA
        }
      }
      if (!is.na(xcri_0[1])) {
        XisDt = inherits(dat$x[1], "POSIXct")
        if (XisDt) {
          names(xcri_0) = .s_rpl(format(xcri_0, format = "%y/%m"), "/0", "/")
        } else {
          names(xcri_0) = paste0(formatC(xcri_0, digits = 2, format = "f", small.mark = "."), "\n",ylbl,"=0")
        }
      }

      return(xcri_0)
    }

    ycri_i = function(db, x) {
      .con = abs(db$x - x)
      .con = .con == min(.con)
      db$y[.con][1]
    }

    xtic = c(xcri, sapply(1:length(alt), function(i) X0(dbi[[i]]))); xtic = xtic[!is.na(xtic)]
    ytic_i = sapply(xcri, function(i) sapply(1:length(alt), function(ii) ycri_i(dbi[[ii]], x = i)))
    ytic_i = ytic_i[!is.na(ytic_i)]
    if (length(ytic_i) > 1) {
      ytic_i = round(ytic_i, .digits) %>% unique() %>% sort()
      cond = c(T, ytic_i[2:length(ytic_i)] - ytic_i[2:length(ytic_i) - 1] > 0.1 ^ .digits * 5)
      ytic_i = ytic_i[cond]
    }

    ytic = c(ycri, ytic_i); ytic = ytic[!is.na(ytic)]
    if (.percent) {
      names(ytic) = scales::percent(ytic, accuracy = 0.1 ^ (.digits - 2))
    } else {
      names(ytic) = ytic
    }


    glnAl = 0.7
    glnTh = 0
    glnTp = 2

    db = bind_rows(dbi)

    a = ggplot(data = db, aes(x = x, y = y, linewidth = 비교)) +
      scale_linewidth_manual(values = 0.1 + 1:length(alt) * 0.2)+
      geom_line() +
      ylab(paste0(ylbl, ylbl_add)) +
      xlab(xlbl) +
      scale_x_continuous(breaks = xtic, labels = names(xtic)) +
      scale_y_continuous(labels = names(ytic), breaks = ytic, limits = c(min(c(db$y, .ylim, .yAxis)), max(c(db$y, .ylim, .yAxis)))) +
      geom_vline(xintercept = xtic, linetype = glnTp, alpha = glnAl, linewidth = glnTh) +
      geom_hline(yintercept = ytic, linetype = glnTp, alpha = glnAl, linewidth = glnTh) +
      geom_hline(yintercept = .yAxis, linewidth = 0.2) +
      theme(text = element_text(family = "basic", size = .fntSz)
            , axis.title = element_text(size = .fntSz - 3)
            #, legend.title = element_text(size = .fntSz - 3)
            , aspect.ratio = a_ratio
            , legend.position = c(0, 0)
            , legend.justification = c(0, 0)
            , legend.background = element_rect(fill = .bgCol)
            #, legend.direction = "horizontal"
            , panel.grid = element_blank()
            , legend.key.height = unit(0.8, "lines")
            , legend.byrow = T
            , panel.background = element_rect(fill = .bgCol)
            # x축 텍스트 줄간격(lineheight) 조절
            , axis.text.x = element_text(lineheight = 0.5)
      )
    if (.guide) {
      a = a + guides(linewidth = guide_legend(title = NULL, order = 1, ncol = 2))
    } else {
      a = a + guides(linewidth = "none")
    }
    # t.test
    # 비교
    if (length(alt) > 1) {
      .t_test = lapply(
        2:length(alt) - 1
        , function(i) {
          lapply((i+1):length(alt)
                 , function(ii) {
                   .t1 = t.test(y ~ 비교, data = bind_rows(dbi[[i]], dbi[[ii]]))
                   .t1 = list(검정 = .t1, 차이 = abs(Reduce("-",.t1$estimate)))
                 })
        }
      )
    } else {
      .t_test = NA
    }

    return(list(
      출력 = a
      , 비교 = .t_test
    ))
  } else {
    return(cat("비교군(alt)과 비교군주석(W)의 개수가 일치하지 않음.\n"))
  }

}
