## Pull JSON data from the following REST API
## 국민권익위원회_민원빅데이터_분석정보_API_2023
## https://www.data.go.kr/tcs/dss/selectApiDataDetailView.do?publicDataPk=15114773#/tab_layer_detail_function

url <- "http://apis.data.go.kr/1140100/minAnalsInfoView6/minGndrClsfDocCnt"

library(httr)
library(jsonlite)

out <-  GET(
  url = url,
  query = list(
    serviceKey = "YOUR_SERVICE_KEY",
  )
)

## Content of the GET
content(out)

## 남성/여성 구분
## 즉 성별 민원 분야별 민원 건수 정보
## 그런데 연령대별, 민원 발생지별은 API endpoint가 없음?