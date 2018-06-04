# Encoded in UTF-8

colNames <- c(
  "TEAM",   # チーム名
  "NO",     # 背番号
  "PLAYER", # プレイヤー名
  "PO",     # ポジション
  "G",      # 試合数
  "GS",     # スターター数
  "MIN",    # 総プレイ時間
  "MINPG",  # 平均プレイ時間
  "PTS",    # 得点数
  "PPG",    # 平均得点数
  "FGM",    # フィールドゴール成功数
  "FGA",    # フィールドゴール試投数
  "FGP",    # フィールドゴール成功率
  "TPGM",   # 3Pシュート成功数
  "TPGA",   # 3Pシュート試投数
  "TPGP",   # 3Pシュート成功率
  "FTM",    # フリースロー成功数
  "FTA",    # フリースロー試投数
  "FTP",    # フリースロー成功率
  "OR",     # オフェンスリバウンド数
  "DR",     # ディフェンスリバウンド数
  "TR",     # トータルリバウンド数
  "RPG",    # 平均トータルリバウンド数
  "AS",     # アシスト数
  "APG",    # 平均アシスト数
  "TO",     # ターンオーバー数
  "ST",     # スティール数
  "BS",     # ブロック数
  "BSR",    # 被ブロック数
  "F",      # ファウル数
  "FD",     # 被ファウル数
  "EFF"     # 貢献度
)

b1.2nd <- read.csv("201718_b1.csv",
                   header = TRUE,
                   sep = ",",
                   quote = "",
                   stringsAsFactors = FALSE,
                   col.names = colNames
                   )

playerstats.clean.percentage <- function(df) {
  for (col in c("FGP", "TPGP", "FTP")) {
    df[,col] <- gsub("%", "", df[,col])
    df[,col] <- as.numeric(df[,col])
    df[,col] <- round((df[,col] / 100), 2)
  }
  return(df)
}

playerstats.clean.minute <- function(df) {
  
  return(df)
}

