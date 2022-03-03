


string="AAPL"

getSymbols(Symbols = string,src="yahoo",from="2020-02-02",to="2021-02-02")

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