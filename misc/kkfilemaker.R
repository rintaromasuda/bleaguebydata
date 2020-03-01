library(bleaguer)
library(reader)

df <- GetGameSummary()
df <- subset(df, Season == "2018-19" & Category == "Regular")
df <- merge(df, b.games.boxscore, by = c("ScheduleKey", "TeamId"))

write.csv(df, "KKTest2_ShiftJis.csv", fileEncoding = "Shift-Jis", row.names = FALSE, quote = FALSE)
readr::write_excel_csv(df, "KKTest2_ExcelUtf8.csv")

                       