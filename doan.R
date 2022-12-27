library(quantmod)
library(ggplot2)
library(shiny)
library(rjson)
library(RCurl)
library(dplyr)
library(tidyverse)
library(tidyquant)  
library(graphics)

ui<-
  fluidPage(
    title = "Chứng Khoán",
    width = "100%",
    height = "100%",
    fixedPage(
    fluidRow(
      column(6,
        column(3,img(src='./logo.png',height ="100%",width ="100%",
                     style="max-width:100px;max-height:100px;min-width:80px;min-height:50px")
        ),
        column(9,
          absolutePanel(
            img(src='./searchRight.png', width="15px", height="15px",
                style="margin:8px"
            )
          ),
          textInput("search", "AAPL", "AAPL", width = "100%",placeholder = "Tìm kiếm mã nào"),
          style ="margin-top:10px"
        ),
        
      ),
      
      column(6,
        navbarPage("",
          tabPanel("Biểu Đồ"),
          tabPanel("Tin Tức"),
          tabPanel("Chứng Khoán"),
          tabPanel("Phân Tích"),
          navbarMenu("Chế Độ",
            tabPanel("Chế độ sáng"),
            tabPanel("Chế độ tối"),
            tabPanel("Bật chế độ pháo"),
            tabPanel("Tắt chế độ pháo"),
          ),
        ),
      ),
      style = "margin-top:20px"
    ),
  ),
  hr(),
  
  absolutePanel(img(src="./fireworksShow.gif", width="200", height="200",),top = "40%",),
  absolutePanel(img(src="./fireworksShow.gif", width="200", height="200",),top = "40%",right = "0"),
  uiOutput("uitableStock"),
  
  fixedPage(
    fluidPage(
      h4("BIỂU ĐỒ CỔ PHIẾU"),
    ),
    hr(),
    fluidRow(
      column(8,
        uiOutput("uimainView"),
        plotOutput("plotMainView", height="350"),
      ),
      
      column(4,
              fluidPage(
                     selectInput("mode", "Chọn Chế Độ:",
                                 list(`Kiểu chế độ` = list("Đồ thị dạng đường", "Đồ thị hình nến", "Đồ thị hình thanh"),
                                      `Các loại biểu đồ khác` = list("Biểu đồ lợi nhuận", "Biểu đồ nhiều chứng khoán"),
                                      `Các loại mô hình dự đoán` = list("Mô hình hồi quy tuyến tính")
                                      )
                     ),
                   uiOutput("uiMode"),
                   uiOutput("uiLineChart"),
                   uiOutput("uiDate"),
                   uiOutput("uiSummary"),
                   verbatimTextOutput("summary"),
              ),
            # div(
            #  div(
             #    div("Mã chứng khoán",style="height:40px;padding-top: 10px;"),
              #   mainPanel(uiOutput("uiSymbol"),style="width:100%;padding:0"),
               #  style="flex:1;text-align:center;border-right:1px solid #000"
               #),
               #div(
                # div("Giá hiện tại (USD)",style="height:40px;padding-top: 10px;"),
                # mainPanel(uiOutput("uiPrice"),style="width:100%;padding:0"),
                # style="flex:1;text-align:center;border-right:1px solid #000"
               # ),
               # style="display:flex;background-color:#f2f2f2;color:#000",
             # ),
             #style="padding:0"
      ),
      style="margin:0"
    ),
  ),
  
  tags$head(
    tags$style(HTML("
      html{
        height:100%;
      }
      
      body{
        height:100%;
        background-color:#0F0F0F;
        color:#fff;
      }
      
      #uitableStock>.container-fluid::-webkit-scrollbar, body::-webkit-scrollbar{
        width: 12px;
      }
      
      #uitableStock>.container-fluid::-webkit-scrollbar-track, body::-webkit-scrollbar-track{
        box-shadow: inset 0 0 5px grey;
        border-radius: 10px;
      }
      
      #uitableStock>.container-fluid::-webkit-scrollbar-thumb, body::-webkit-scrollbar-thumb{
        background-image: linear-gradient(to bottom right, red, yellow);
        border-radius: 10px;
      }
      
      #search{
        padding-left:30px
      }
      
      .navbar-static-top{
        margin:0px
      }
      
      .navbar-nav>li{
        font-size:13px
      }
      
      .navbar-header,#search-label{
        display:none
      }
      
      hr{
        border-top: 1px solid #f2f2f2;
      }
      
      hrBottom{
        border-bottom: 1px solid #f2f2f2;
      }
      
      #uimainView{
        display:flex;
        flex-direction: column;
      }
      
      .dropdown-menu>li>a{
        height:40px;display: flex;align-items: center;font-size:12px
      }
      
      #plotMainView>img{
        padding-right: 20px
      }
      
      .shiny-output-error{
        visibility:hidden;
        width:0;
        height:0;
      }
      
      .shiny-text-output,#uiDate{
        max-width:300px;
      }
      
      #uiSummary{
        margin-bottom:8px;
      }
      
      #buttonStock{
        padding:8px 12px;
        background-color:#f2f2f2;
        border-radius:4px;
      }
      
      #uitableStock{
        position: absolute;
        top:50%;
        left:50%;
        transform:translate(-50%,-50%);
        z-index:999;
      }
      
      #uitableStock>.container-fluid {
        background:#282828;
        border-radius:8px;
        max-height:450px;
        overflow-y: auto;
        padding-top:30px;
      }
      #iconCloseModal{
        position: absolute;
        top: 0;
        right: 0;
        transform: translate(-100%,50%);
      }
      .datepicker{
        background: #282828;
      }
      
      #future .input-group-addon, #future .form-control:nth-child(1){
        display: none !important;
      }
      
    ",
    )),
    
)
)

server <- function(input, output) {
  
  codeStock <- c("AAPL","GOOG","TSLA","AMZN","META","KO","PEP")
  
  apiAllStock <- function(stock) {
    base_url = "https://query1.finance.yahoo.com/v7/finance/quote?"
    symbol <- c() ### an empty vector
    currentPrice <- c()
    
    for (i in 1:length(stock)) { 
      full_url = paste0( base_url, "county=Boulder", paste("&symbols=",stock[i],sep="") )
      full_url <- URLencode(full_url)
      full_url <- fromJSON(getURL(full_url))
      symbol[i] <- full_url$quoteResponse$result[[1]]$symbol
      currentPrice[i] <- full_url$quoteResponse$result[[1]]$regularMarketPrice
    }
    
    output$uiSymbol <- renderUI({
      tagList(
        tags$div(symbol[1],style="padding:8px 0;border-top:1px solid #000"),
        tags$div(symbol[2],style="padding:8px 0;border-top:1px solid #000"),
        tags$div(symbol[3],style="padding:8px 0;border-top:1px solid #000"),
        tags$div(symbol[4],style="padding:8px 0;border-top:1px solid #000"),
        tags$div(symbol[5],style="padding:8px 0;border-top:1px solid #000"),
        tags$div(symbol[6],style="padding:8px 0;border-top:1px solid #000"),
        
      )
    })
  
    output$uiPrice <- renderUI({
      tagList(
        tags$div(currentPrice[1],style="padding:8px 0;border-top:1px solid #000"),
        tags$div(currentPrice[2],style="padding:8px 0;border-top:1px solid #000"),
        tags$div(currentPrice[3],style="padding:8px 0;border-top:1px solid #000"),
        tags$div(currentPrice[4],style="padding:8px 0;border-top:1px solid #000"),
        tags$div(currentPrice[5],style="padding:8px 0;border-top:1px solid #000"),
        tags$div(currentPrice[6],style="padding:8px 0;border-top:1px solid #000"),
      )
    })
  }
  apiAllStock(codeStock)

  observe({
    valueSearch <- input$search
    valueMode <- input$mode
    
    if(valueSearch %in% codeStock == TRUE){
      output$uimainView <- renderUI({
        tagList(
          tags$div(paste(displayName,"(",symbol,")"),style="font-size: 16px;text-transform: uppercase;"),
          tags$div(
            div(paste("Giá hiện tại: ",currentPirce)),
            div(paste("Giá mở cửa: " ,pirceOpen),style="margin-left:20px"),
            div(paste("Điểm +/-:", priceChange),style="margin-left:20px"),
            div(paste("Điểm %:", priceChangePercent),style="margin-left:20px"),
            div(paste("Giá trần - sàn:", priceDayRange),style="margin-left:20px"),
            style="margin:20px 0;display:flex"
          )
        )
      })
      
      # API
      base_url = "https://query1.finance.yahoo.com/v7/finance/quote?"
      full_url = paste0( base_url, "county=Boulder", paste("&symbols=",valueSearch ,sep="") )
      full_url <- URLencode(full_url)
      full_url <- fromJSON(getURL(full_url))
      
      # FAKE API
      #base_url = "http://localhost:5000/api/stock/"
      #full_url <- paste0(base_url,valueSearch)
      #full_url <- fromJSON(getURL(full_url))
      
      symbol <- full_url$quoteResponse$result[[1]]$symbol
      displayName <- full_url$quoteResponse$result[[1]]$displayName
      currentPirce <- full_url$quoteResponse$result[[1]]$regularMarketPrice
      pirceOpen <- full_url$quoteResponse$result[[1]]$regularMarketPreviousClose
      priceChange <-format(round(full_url$quoteResponse$result[[1]]$regularMarketChange, 2), nsmall = 2)
      priceChangePercent <- format(round(full_url$quoteResponse$result[[1]]$regularMarketChangePercent, 2), nsmall = 2)
      priceDayRange <- full_url$quoteResponse$result[[1]]$regularMarketDayRange
      
      #import dataset 
      dataSet <- paste0("F:/chuyende/dataSet/",valueSearch,".csv")
      dataSet <- read.csv(dataSet)
      dataSet$FakeData <- NA
      
      
      #Clean data
      colnames(dataSet)[6] <- "Price"
      dataSet <- subset(dataSet, select = -c(FakeData))
      
      
     if(valueMode == "Đồ thị dạng đường"){
      
       output$uiLineChart <- renderUI({
         tagList(
           selectInput("forecast", "Các Đường Dự Báo:",
                       list(`Đường dự báo` = list("Mặc định","SMA20","SMA50","SMA100","SMA20 SM50 SMA100"))
           ),
         )
       })
       
       output$uiDate <- renderUI({
         dateRangeInput("dates", "Chọn Ngày",start = "2015-01-01", end = as.character(Sys.Date()))
       })
       
       output$uiSummary <- renderUI({
         tagList(
           div("Bảng Tóm Tắt Về Giá:"),
         )
       })
       
       lineChart <- xts(dataSet[, -1], order.by=as.Date(dataSet$Date))
       
       observe({
         valueForecast <- input$forecast
         valueDate <- input$dates
         valueSummary <- input$summary
         
         selectForecast20 <- NULL
         selectForecast100 <- NULL
         
         
         if( !is.null(valueForecast)) {
           
           lineChart <- subset(lineChart, index(lineChart) >= valueDate[1])
           lineChart <- subset(lineChart, index(lineChart) <= valueDate[2])
           
           output$summary <- renderPrint({
             summary(lineChart$Price)
           })
           
            if(valueForecast == "Mặc định"){
              output$plotMainView <- renderPlot({
                ggplot(lineChart, 
                  #aes x tham số của trục X tính là ngày, tham số của trục Y tính là cột thứ 5
                  aes(x = index(lineChart), y = lineChart[,5])) + 
                  #Thiết lập màu cho đường vẽ
                  geom_line(aes(y = lineChart[,5], color = "Đường Giá")) +
                  scale_color_manual(values=c("Đường Giá" = "gray40")) +
                  theme_bw() +
                  ggtitle(paste("Biểu đồ đường:",displayName)) + 
                  xlab("Ngày") + ylab("Giá") + 
                  #Vị trí Title
                  theme(plot.title = element_text(hjust = 0.5)) 
              })  
            }
           
            if(valueForecast == "SMA20"){
              selectSMA20 <- rollmean(lineChart[,5], 20, fill = list(NA, NULL, NA), align = "right")
              selectSMA20$default <- coredata(selectSMA20)
              
              output$plotMainView <- renderPlot({
                ggplot(lineChart, 
                       #aes x tham số của trục X tính là ngày, tham số của trục Y tính là cột thứ 5
                       aes(x = index(lineChart), y = lineChart[,5])) + 
                  #Thiết lập màu cho đường vẽ
                  geom_line(aes(y = lineChart[,5], color = "Đường Giá")) +
                  
                  geom_line(aes(y = selectSMA20$default, color = "SMA20")) +
                  
                  scale_color_manual(values=c("Đường Giá" = "gray40", "SMA20" = "firebrick4")) +
                
                  theme_bw() +
                  ggtitle(paste("Biểu đồ đường:",displayName)) + 
                  xlab("Ngày") + ylab("Giá") + 
                  #Vị trí Title
                  theme(plot.title = element_text(hjust = 0.5)) 
              }) 
              
            }
            
            if(valueForecast == "SMA50"){
              selectSMA50 <- rollmean(lineChart[,5], 50, fill = list(NA, NULL, NA), align = "right")
              selectSMA50$default <- coredata(selectSMA50)
              output$plotMainView <- renderPlot({
                ggplot(lineChart, 
                  #aes x tham số của trục X tính là ngày, tham số của trục Y tính là cột thứ 5
                  aes(x = index(lineChart), y = lineChart[,5])) + 
                  #Thiết lập màu cho đường vẽ
                  geom_line(aes(y = lineChart[,5], color = "Đường Giá")) +
                  
                  geom_line(aes(y = selectSMA50$default, color = "SMA50")) +
                  
                  scale_color_manual(values=c("Đường Giá" = "gray40", "SMA50" = "darkcyan")) +
                  
                  theme_bw() +
                  ggtitle(paste("Biểu đồ đường:",displayName)) + 
                  xlab("Ngày") + ylab("Giá") + 
                  #Vị trí Title
                  theme(plot.title = element_text(hjust = 0.5)) 
              }) 
            }
           
           if(valueForecast == "SMA100"){
             selectSMA100 <- rollmean(lineChart[,5], 100, fill = list(NA, NULL, NA), align = "right")
             selectSMA100$default <- coredata(selectSMA100)
             output$plotMainView <- renderPlot({
               ggplot(lineChart, 
                      #aes x tham số của trục X tính là ngày, tham số của trục Y tính là cột thứ 5
                      aes(x = index(lineChart), y = lineChart[,5])) + 
                 #Thiết lập màu cho đường vẽ
                 geom_line(aes(y = lineChart[,5], color = "Đường Giá")) +
                 
                 geom_line(aes(y = selectSMA100$default, color = "SMA100")) +
                 
                 scale_color_manual(values=c("Đường Giá" = "gray40", "SMA100" = "blue")) +
                 
                 theme_bw() +
                 ggtitle(paste("Biểu đồ đường:",displayName)) + 
                 xlab("Ngày") + ylab("Giá") + 
                 #Vị trí Title
                 theme(plot.title = element_text(hjust = 0.5)) 
             }) 
           }
           
           if(valueForecast == "SMA20 SM50 SMA100"){
             
             selectSMA20 <- rollmean(lineChart[,5], 20, fill = list(NA, NULL, NA), align = "right")
             selectSMA20$default <- coredata(selectSMA20)
             
             selectSMA50 <- rollmean(lineChart[,5], 50, fill = list(NA, NULL, NA), align = "right")
             selectSMA50$default <- coredata(selectSMA50)
             
             selectSMA100 <- rollmean(lineChart[,5], 100, fill = list(NA, NULL, NA), align = "right")
             selectSMA100$default <- coredata(selectSMA100)
             
             output$plotMainView <- renderPlot({
               ggplot(lineChart, 
                      #aes x tham số của trục X tính là ngày, tham số của trục Y tính là cột thứ 5
                      aes(x = index(lineChart), y = lineChart[,5])) + 
                 #Thiết lập màu cho đường vẽ
                 geom_line(aes(y = lineChart[,5], color = "Đường Giá")) +
                 
                 geom_line(aes(y = selectSMA20$default, color = "SMA20")) +
                 
                 geom_line(aes(y = selectSMA50$default, color = "SMA50")) +
                 
                 geom_line(aes(y = selectSMA100$default, color = "SMA100")) +
                 
                 scale_color_manual(values=c("Đường Giá" = "gray40", "SMA20"="firebrick4","SMA50"="darkcyan","SMA100" = "blue")) +
                 
                 theme_bw() +
                 ggtitle(paste("Biểu đồ đường:",displayName)) + 
                 xlab("Ngày") + ylab("Giá") + 
                 #Vị trí Title
                 theme(plot.title = element_text(hjust = 0.5)) 
             }) 
           }
           
         }
       })
       
     }
    
     if(valueMode == "Đồ thị hình nến"){
       
       output$uiLineChart <- renderUI({
         sliderInput("size",
                     "Số quan sát:",
                     value = 7,
                     min = 1,
                     max = 365)
       })
       
       output$uiSummary <- renderUI({
         tagList(
           div("Bảng Tóm Tắt Về Giá:"),
         )
       })
       
       barChart <- xts(dataSet[, -1], order.by=as.Date(dataSet$Date))
       
       observe({
         valueSize <- input$size
         
         if( !is.null(valueSize)){
         barChart <- tail((barChart),valueSize)
           
         output$plotMainView <- renderPlot({
           ggplot(barChart, aes(x=index(barChart), y=Price)) + 
             geom_candlestick(aes(open=Open, high=High, low=Low, close=Close),
                              colour_up = "darkgreen", colour_down = "darkred",
                              fill_up  = "darkgreen", fill_down  = "darkred") +
             labs( y = "Giá", x = "") +
             theme_bw() +
             ggtitle(paste("Biểu đồ nến:",displayName)) + 
             theme(plot.title = element_text(hjust = 0.5)) 
         })
         
         output$summary <- renderPrint({
           summary(barChart$Price)
         })
         
         output$uiDate <- renderUI({})
        
         }
       })
     }
    
    if(valueMode == "Biểu đồ nhiều chứng khoán"){
      
      output$uiLineChart <- renderUI({
        div("Chọn Cổ Phiếu Ưa Thích: ")
      })
      
      output$summary <- renderPrint({
        summary()
      })
      
      output$uiDate <- renderUI({
        fluidRow(
          column(6,
                 textInput("stock1", "", "AAPL", width = "100%"),
                 textInput("stock2", "", "GOOG", width = "100%"),
          ),
          column(6,
                 textInput("stock3", "", "AMZN", width = "100%"),
                 textInput("stock4", "", "META", width = "100%"),
          ),
        )
      })
      
      observe({
        valueStock1 <- input$stock1 
        valueStock2 <- input$stock2 
        valueStock3 <- input$stock3 
        valueStock4 <- input$stock4
        
        #import dataset
        if( (!is.null(valueStock1)) & (!is.null(valueStock2)) & (!is.null(valueStock3)) & (!is.null(valueStock4)) ){
          if( (valueStock1 %in% codeStock == TRUE) & (valueStock2 %in% codeStock == TRUE) & (valueStock3 %in% codeStock == TRUE) & (valueStock4 %in% codeStock == TRUE) ){
            
            output$uimainView <- renderUI({
              tagList(
                tags$div(paste(displayName,"(",symbol,")"),style="font-size: 16px;text-transform: uppercase;"),
                tags$div(
                  div(paste("Giá hiện tại: ",currentPirce)),
                  div(paste("Giá mở cửa: " ,pirceOpen),style="margin-left:20px"),
                  div(paste("Điểm +/-:", priceChange),style="margin-left:20px"),
                  div(paste("Điểm %:", priceChangePercent),style="margin-left:20px"),
                  div(paste("Giá trần - sàn:", priceDayRange),style="margin-left:20px"),
                  style="margin:20px 0;display:flex"
                )
              )
            })
            
            dataSetStock1 <- paste0("F:/chuyende/dataSet/",valueStock1,".csv")
            dataSetStock1 <- read.csv(dataSetStock1)
            dataSetStock1$symbol <- valueStock1
            
            dataSetStock2 <- paste0("F:/chuyende/dataSet/",valueStock2,".csv")
            dataSetStock2 <- read.csv(dataSetStock2)
            dataSetStock2$symbol <- valueStock2
            
            dataSetStock3 <- paste0("F:/chuyende/dataSet/",valueStock3,".csv")
            dataSetStock3 <- read.csv(dataSetStock3)
            dataSetStock3$symbol <- valueStock3
            
            dataSetStock4 <- paste0("F:/chuyende/dataSet/",valueStock4,".csv")
            dataSetStock4 <- read.csv(dataSetStock4)
            dataSetStock4$symbol <- valueStock4
            
            data_combined <- bind_rows(dataSetStock1, dataSetStock2, dataSetStock3, dataSetStock4)
            # Create a plot showing the stock price for each company over time
            output$plotMainView <- renderPlot({
              ggplot(data_combined, aes(x = Date, y = Adj.Close,group = 1)) +
                geom_line() +
                labs( y = "Giá", x = "") +
                ggtitle(paste("Biểu đồ nhiều chứng khoán:")) + 
                facet_wrap(~ symbol, nrow = 2, ncol = 2) +
                theme(plot.title = element_text(hjust = 0.5))
            })
            
            
            # Lấy dữ liệu theo cột giá
            colDate <- dataSetStock1[,1, drop=FALSE]
            colnames(colDate)[1] <- "Ngày"
            
            colPriceStock1 <- dataSetStock1[,6, drop=FALSE]
            colnames(colPriceStock1)[1] <- valueStock1
              
            colPriceStock2 <- dataSetStock2[,6, drop=FALSE]
            colnames(colPriceStock2)[1] <- valueStock2
            
            colPriceStock3 <- dataSetStock3[,6, drop=FALSE]
            colnames(colPriceStock3)[1] <- valueStock3
            
            colPriceStock4 <- dataSetStock4[,6, drop=FALSE]
            colnames(colPriceStock4)[1] <- valueStock4
            
            dataCloumn <- cbind(colDate, colPriceStock1, colPriceStock2, colPriceStock3, colPriceStock4)
            
            output$uiSummary <- renderUI({
              div(
                div("Tải Dữ Liệu Hoặc Xem Trực Tiếp Dữ Liệu: ",style="margin-bottom:10px"),
                div(
                  output$downloadData <- downloadHandler(
                    filename = function() {
                      paste("dataCloumn-", Sys.Date(), ".csv", sep="")
                    },
                    content = function(file) {
                      write.csv(dataCloumn, file)
                    }
                  ),
                ),
                
                div(
                  actionLink("buttonStock","Xem trực tiếp"),
                  style={"margin-top:20px"}
                ),
              )
            })
            
            observeEvent(input$buttonStock,{
              output$uitableStock <- renderUI({
                fluidPage(
                  output$table <- renderTable({dataCloumn}),
                  actionLink("iconCloseModal",
                  img(src='./iconClose.png',
                    style="width:20px;height:20px;color:#f2f2f2"
                  )
                  )
                )
                
              })
            })
            
          observeEvent(input$iconCloseModal,{
            output$uitableStock <- renderUI({})
          })
    
          } else{
            output$uimainView <- renderUI({
              tagList(
                tags$img(src="./notDataFound.gif",style="height:500px")
              )
            })
            output$plotMainView <- renderPlot({valueSearch},width = "0", height="0")
            
          }
          
        } 
      })
    }
      if(valueMode == "Biểu đồ lợi nhuận"){
        output$uiLineChart <- renderUI({})
        
        output$uiDate <- renderUI({
          dateRangeInput("datesProfit", "Chọn Ngày",start = "2015-01-01", end = as.character(Sys.Date()))
        })
        
        output$uiSummary <- renderUI({})
        
        output$summary <- renderPrint({summary()})
        
        # Lợi nhuận cổ phiếu
        profitStock <- xts(dataSet[, -1], order.by=as.Date(dataSet$Date))
        profitStock_ret <- diff(log( profitStock[,5]))
        profitStock_ret <- profitStock_ret[-1,]
        
        observe({
          valueProfit <- input$datesProfit
          profitStock_ret <- subset(profitStock_ret, index(profitStock_ret) >= valueProfit[1])
          profitStock_ret <- subset(profitStock_ret, index(profitStock_ret) <= valueProfit[2])
          
          output$plotMainView <- renderPlot({
            ggplot(profitStock_ret, aes(x = index(profitStock_ret), y = Price)) +
              geom_line(color = "deepskyblue4") +
              ggtitle("Biểu đồ về lợi nhuận") +
              xlab("") + ylab("") +
              theme_bw() +
              theme(plot.title = element_text(hjust = 0.5))
          })
          
        })
      }
      
      if(valueMode == "Mô hình hồi quy tuyến tính"){
        
        linearRegression <- xts(dataSet[, -1], order.by=as.Date(dataSet$Date))
        
        output$uiLineChart <- renderUI({})
        
        output$uiDate <- renderUI({
            div(
              h5("Cảnh báo: Vì một số lý do, xin vui lòng chọn sau ngày 2018-12-20:"),
              dateRangeInput("future", "Chọn Ngày",start = "",end = "2018-12-21")
            )
        })
        
        #Tách dữ liệu thành tập huấn luyện và tập kiểm tra
        train_data <- dataSet[1:1000,]
        
        observe({
          valueFuture <- input$future
          
          linearRegression <- subset(linearRegression, index(linearRegression) >= "2018-12-21")
          linearRegression <- subset(linearRegression, index(linearRegression) == valueFuture[2])
            
            Open <- train_data$Open
            Volume <- train_data$Volume
            High <- train_data$High
            Low <- train_data$Low
            Price <- train_data$Price
            
            ModalRegression <- lm(Price ~ Open + Volume + High + Low, train_data)
            
          # Phù hợp với mô hình hồi quy tuyến tính bội
          
          output$plotMainView <- renderPlot({
            par(mfrow=c(2,2))
            plot(ModalRegression)
          })
          
          checkDataSelec <- rowSums(linearRegression)
          
          # Kiểm tra xem dữ liệu theo hàng có không
          if(!is.na(checkDataSelec[1])){
            
            valueOpen <- rowSums(linearRegression$Open)
            valueHigh <- rowSums(linearRegression$High)
            valueLow <- rowSums(linearRegression$Low)
            valueVolume <- rowSums(linearRegression$Volume)
            
            prediction <- predict(ModalRegression, data.frame(
              Open=c(valueOpen),
              High=c(valueHigh),
              Low=c(valueLow),
              Volume=c(valueVolume)
            ), interval="prediction")

            output$uiSummary <- renderUI({
              div(
                span("Giá dự đoán:"),
                span(prediction[1])
              )
            })

            output$summary <- renderPrint({summary(ModalRegression)})
            
            
          }else{
            output$summary <- renderPrint({summary()})
            
            output$uiSummary <- renderUI({})
          }
          
        })
      }
      
    } else {
      output$uimainView <- renderUI({
        tagList(
          tags$img(src="./notDataFound.gif",style="height:500px")
        )
      })
      
      output$plotMainView <- renderPlot({valueSearch},width = "0", height="0")
      
      output$uiLineChart <- renderUI({})
      
      output$uiDate <- renderUI({})
      
      output$uiSummary <- renderUI({})
      
      output$summary <- renderPrint({summary()})
    }
  })
}

shinyApp(ui = ui, server = server)







#aapl <- read.csv("F:/chuyende/dataSet/AAPL.csv")

# Tạo chuỗi thời gian
#aapl <- xts(aapl[, -1], order.by=as.Date(aapl$Date))

# Hàm trả về n hàng đầu tiên của tập dữ liệu.
#head(aapl)

# Hàm trả về n hàng cuối cùng của tập dữ liệu.
#tail(aapl)

## Tóm tắt các giá trị trong datase như: 
## Min: Giá trị tối thiểu,
# 1st Qu: Giá trị của phần tư thứ nhất.: 1/4
# Median: Giá trị trung vị
# 3rd Qu: Giá trị của phần tư thứ 3 : 3/4
# Max: giá trị tối đa
#summary(aapl)

# hiển thị cấu trúc bên trong của bất kỳ đối tượng R nào theo cách thu gọn.
#str(aapl)

#ggplot(aapl, 
         #aes x tham số của trục X tính là ngày, tham số của trục Y tính là cột thứ 5
         #aes(x = index(aapl), y = aapl[,5])) + 
         #Thiết lập màu cho đường vẽ
         #geom_line(color = "red") + 
         #ggtitle("Petrobras prices series") + 
         #xlab("Date") + ylab("Price") + 
         #Vị trí Title
         #theme(plot.title = element_text(hjust = 0.5)) +
         #Thiết lập lấy theo tháng 12 tháng 1 lần  
        #scale_x_date(date_breaks = "12 months ")


 #aapl_mm <- subset(aapl, index(aapl) >= "2016-01-01")
 
 # rollmean x là một đối tượng, 10 là aapl_mm10, 
  #Tạo bảng thống kê biến aapl_mm10
 #aapl_mm10 <- rollmean(aapl_mm[,5], 10, fill = list(NA, NULL, NA), align = "right")
  #Tạo bảng thống kê biến aapl_mm30
 #aapl_mm30 <- rollmean(aapl_mm[,5], 30, fill = list(NA, NULL, NA), align = "right")
 
 
  #Thêm biến mm10 vào bảng aapl_mm
 #aapl_mm$mm10 <- coredata(aapl_mm10)
 
  #Thêm biến mm30 vào bảng aapl_mm
 #aapl_mm$mm30 <- coredata(aapl_mm30)
 
 
 #ggplot(aapl_mm, aes(x = index(aapl_mm))) +
   
 #geom_line(aes(y = aapl_mm[,5], color = "aapl")) + ggtitle("Petrobras prices series") +
   
 #geom_line(aes(y = aapl_mm$mm10, color = "MM10")) +
   
 #geom_line(aes(y = aapl_mm$mm30, color = "MM30")) + xlab("Date") + ylab("Price") +
   
 #theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank()) +
   
 #scale_x_date(date_labels = "%b %y", date_breaks = "12 months") +
   
#scale_colour_manual("Series", values=c("aapl"="gray40", "MM10"="firebrick4", "MM30"="darkcyan"))
 
 #aapl_ret <- diff(log( aapl[,5]))
 #apl_ret <- aapl_ret[-1,]
 ## Op(aapl)
 ## Cl(aapl)
 ## Ad(aapl)
 ## Lợi nhuận theo ngày
 ## dailyReturn(aapl)
 # Lợi nhuận theo tuần
 ## weeklyReturn(aapl)
 # Lợi nhuận theo tháng
 ## monthlyReturn(aapl)
 # Lợi nhuận theo quý
 ## quarterlyReturn(aapl)
 # Lợi nhuận theo năm
 ## yearlyReturn(aapl)
# Thống kê bảng aapl_ret
 #summary(aapl_ret)
 #sd(aapl_ret)
 
 
 #ggplot(aapl_ret, aes(x = index(aapl_ret), y = aapl_ret)) +
  # geom_line(color = "deepskyblue4") +
#ggtitle("Petrobras returns series") +
  # xlab("Date") + ylab("Return") +
   #theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "12 months")
 
 #aapl_ret17 <- subset(aapl_ret, index(aapl_ret) > "2022-01-01")
 
 #ggplot(aapl_ret17, aes(x = index(aapl_ret17), y = aapl_ret17)) +
   #geom_line(color = "deepskyblue4") +
   #ggtitle("Petrobras returns series in 2017") + xlab("Date") + ylab("Return") +
   #theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(date_labels = "%b %y", date_breaks = "1 months")
 #summary(aapl_ret17)
 #sd(aapl_ret17)
 
# rm(list = ls())
 
 
 