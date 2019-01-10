df <- read.csv("all_games_201718.csv")

library(dplyr)

df %>%
  group_by(TEAM.B) %>%
  summarise(N = n())

df$POS.B <- df$X2POINTSFGA.B + df$X3POINTSFGA.B - df$OFFENSIVEREBOUNDS.B + (0.44 * df$FREE.THROWSA.B)
df$PPP100.B <- (df$F.B / df$POS.B) * 100

df$X2POINTSFGR_RAW.B <- df$X2POINTSFGR.B
df$X3POINTSFGR_RAW.B <- df$X3POINTSFGR.B
df$FREE.THROWSR_RAW.B <- df$FREE.THROWSR.B

df$FGR.B <- (df$X2POINTSFGM.B + df$X3POINTSFGM.B) / (df$X2POINTSFGA.B + df$X3POINTSFGA.B)
df$X2POINTSFGR.B <- (df$X2POINTSFGM.B) / (df$X2POINTSFGA.B)
df$X3POINTSFGR.B <- (df$X3POINTSFGM.B) / (df$X3POINTSFGA.B)

library(ggplot2)

ggplot(df, aes(x = TEAM.A, y = F.B)) +
  geom_boxplot(color="darkblue", alpha=0.7) +
  xlab("") +
  ylab("���_") +
  ggtitle("2017-18 ���M�����[�V�[�Y��60������") +
  theme(legend.position="none")

ggplot(df, aes(x = TEAM.A, y = FGR.B)) +
  geom_boxplot(color="darkred", alpha=0.7) +
  xlab("") +
  ylab("����̃t�B�[���h�S�[��%") +
  ggtitle("2017-18 ���M�����[�V�[�Y��60������") +
  theme(legend.position="none") +
  scale_y_continuous(labels = scales::percent)

ggplot(df, aes(x = TEAM.A, y = POINTSINTHEPAINT.B, color = TEAM.A)) +
  geom_boxplot(color="darkgreen", alpha=0.7) +
  xlab("") +
  ylab("����̃y�C���g�G���A���ł̓��_") +
  ggtitle("2017-18 ���M�����[�V�[�Y��60������") +
  theme(legend.position="none")

ggplot(df, aes(x = TEAM.A, y = PPP100.B, color = TEAM.A)) +
  geom_boxplot(color="purple", alpha=0.7) +
  xlab("") +
  ylab("����̃I�t�F���X����") +
  ggtitle("2017-18 ���M�����[�V�[�Y��60������") +
  theme(legend.position="none")

ggplot(df, aes(x = TEAM.A, y = X3POINTSFGM.B * 3, color = TEAM.A)) +
  geom_boxplot(color="orange", alpha=0.7) +
  xlab("") +
  ylab("����̃X���[�|�C���g�ɂ�链�_") +
  ggtitle("2017-18 ���M�����[�V�[�Y��60������") +
  theme(legend.position="none")

ggplot(df, aes(x = TEAM.A, y = FASTBREAKPOINTS.B)) +
  geom_boxplot(color="black", alpha=0.7) +
  xlab("") +
  ylab("����̃t�@�X�g�u���C�N�ł̓��_") +
  ggtitle("2017-18 ���M�����[�V�[�Y��60������") +
  theme(legend.position="none")

ggplot(df, aes(x = TEAM.A, y = TURNOVER.B / POS.B)) +
  geom_boxplot(color="blue", alpha=0.7) +
  xlab("") +
  ylab("����̃^�[���I�[�o�[") +
  ggtitle("2017-18 ���M�����[�V�[�Y��60������") +
  theme(legend.position="none")

ggplot(df, aes(x = TEAM.A, y = FGR.B, fill = TEAM.A)) +
  geom_boxplot() +
  xlab("") +
  ylab("����̃t�B�[���h�S�[��%") +
  ggtitle("2017-18 ���M�����[�V�[�Y��60������") +
  theme(legend.position="none")

ggplot(df, aes(x = TEAM.A, y = FGR.B, fill = TEAM.A)) +
  geom_boxplot() +
  xlab("") +
  ylab("����̃t�B�[���h�S�[��%") +
  ggtitle("2017-18 ���M�����[�V�[�Y��60������") +
  theme(legend.position="none")

ggplot(df, aes(x = TEAM.A, y = X2POINTSFGR.B, fill = TEAM.A)) +
  geom_boxplot() +
  xlab("") +
  ylab("�����2P�t�B�[���h�S�[��%") +
  ggtitle("2017-18 ���M�����[�V�[�Y��60������") +
  theme(legend.position="none")

ggplot(df, aes(x = TEAM.A, y = X3POINTSFGR.B, fill = TEAM.A)) +
  geom_boxplot() +
  xlab("") +
  ylab("�����3P�t�B�[���h�S�[��%") +
  ggtitle("2017-18 ���M�����[�V�[�Y��60������") +
  theme(legend.position="none")

ggplot(df, aes(x = TEAM.A, y = X3POINTSFGA.B, fill = TEAM.A)) +
  geom_boxplot() +
  xlab("") +
  ylab("�����3PA") +
  ggtitle("2017-18 ���M�����[�V�[�Y��60������") +
  theme(legend.position="none")

View(df)
median(df$X3POINTSFGA.A)

df.ryukyu <- df %>%
  filter(TEAM.A == "����") 

ggplot(df.ryukyu, aes(x = TEAM.B, y = F.B)) +
  geom_point(color="darkblue", alpha=0.7) +
  xlab("�ΐ푊��") +
  ylab("���_") +
  ggtitle("������2017-18 ���M�����[�V�[�Y��60������") +
  theme(legend.position="none")