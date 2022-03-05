


string="AAPL"

getSymbols(Symbols = string,src="yahoo",from="2022-01-02",to="2022-02-02")

a=eval(parse(text = string))



stock=xts(eval(parse(text = string)))
ggplot(stock, aes(x = index(stock), y = stock[,6])) +
  geom_line(color = "darkblue")+
  ggtitle("Petrobras prices series") +
  xlab("Date") + ylab("Price") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_date(date_labels = "%b %y", date_breaks = "6 months")


date="2020-02-02"
pbr_mm <- subset(stock, index(stock) >= as.Date(date))
pbr_mm10 <- rollmean(pbr_mm[,6], 10, fill = list(NA, NULL, NA), align = "right")

pbr_mm30 <- rollmean(pbr_mm[,6], 30, fill = list(NA, NULL, NA), align = "right")

pbr_mm$mm10 <- coredata(pbr_mm10)
pbr_mm$mm30 <- coredata(pbr_mm30)

geom_line(aes(y = pbr_mm$mm10, color = "MM10"))


ggplot(stock, aes(x = index(pbr_mm))) +
  geom_line(aes(y = pbr_mm[,6], color = "PBR")) + ggtitle("Petrobras prices series") +
  geom_line(aes(y = pbr_mm$mm10, color = "MM10")) + xlab("Date") + ylab("Price") +
  theme(plot.title = element_text(hjust = 0.5), panel.border = element_blank()) +
  scale_x_date(date_labels = "%b %y", date_breaks = "3 months") +
  scale_colour_manual("Series", values=c("PBR"="gray40", "MM10"="firebrick4"))




number=10

trainset=head(Cl(stock),length(Cl(stock))-number)
testset=tail(Cl(stock),number)

# Forecast the data
fc_na <- snaive(Cl(stock), h=number)

# Plot the result
autoplot(fc_na) #+ autolayer(ts(testset, start=length(trainset)), series = "Test Data")



p = 2 # Autoregressive order
k = 2 # Seasonal autoregressive order
sarmodel <- Arima(Cl(stock), order=c(p,0,0), 
                  seasonal=list(order=c(k,0,0),period=12))

fsar <- forecast(sarmodel, h=number)

autoplot(fsar) 
+ 
  theme_bw() +
  #scale_y_continuous(labels=percent) +
  scale_x_continuous(limits=c(2022-01-01,2022-02-02)) +
  theme(text = element_text(size=15)) +
  labs(title="Unemployment Rate AR(2)(2) Forecasts", 
       x="", y="",
       fill="Confidence Intervals") +
  theme(legend.position = "bottom")

