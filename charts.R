list.of.packages <- c("data.table","ggplot2","scales","anytime","lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)
rm(list.of.packages, new.packages)

setwd("~/git/di-dashboard-mockup")

pal = list(
  "red" = c("#e84439", "#f8c1b2", "#f0826d", "#bc2629", "#8f1b13"),
  "orange" = c("#eb642b", "#f6bb9d", "#f18e5e", "#d85b31", "#973915"),
  "yellow" = c("#f49b21", "#fccc8e", "#f9b865", "#e48a00", "#a85d00"),
  "pink" = c("#c2135b", "#e4819b", "#d64278", "#ad1257", "#7e1850"),
  "purple" = c("#893f90", "#c189bb", "#a45ea1", "#7b3b89", "#551f65"),
  "blue" = c("#0089cc", "#88bae5", "#5da3d9", "#0071b1", "#0c457b"),
  "green" = c("#109e68", "#92cba9", "#5ab88a", "#1e8259", "#16513a"),
  "grey" = c("#6a6569", "#a9a6aa", "#847e84", "#555053", "#443e42")
)

dat = fread("data.csv")
dat$date = anydate(dat$date)
dat$year = year(dat$date)
dat$quarter = quarter(dat$date)
dat$quarter_str = paste0(dat$year, " Q", dat$quarter)
dat = dat[order(dat$date),]
dat$quarter_str = factor(dat$quarter_str)

c1_dat = subset(dat,department=="Finance" & indicator %in% c("Non-Overhead Staff","All Staff"))

c1_line = ggplot(c1_dat, aes(x=quarter_str, y=value / 100, group=indicator, color=indicator)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values=pal[["red"]]) +
  scale_y_continuous(labels=percent) +
  geom_hline(aes(yintercept=target / 100, linetype="Target")) +
  scale_linetype_manual(values=c(2,2)) +
  labs(x="",y="",linetype="",color="",title="Proportion of staff time spent on projects") +
  guides(color = guide_legend(order = 2),linetype = guide_legend(order = 1)) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.line.x = element_line(color="#443e42"),
    axis.ticks = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust=1)
    )

c2_dat = subset(dat,department=="Finance" & indicator %in% c("Indirect Overheads","Direct Overheads"))
ggplot(c2_dat, aes(x=quarter_str, y=value / 100, group=indicator, fill=indicator)) +
  geom_bar(stat="identity", position="dodge") +
  scale_fill_manual(values=pal[["purple"]]) +
  scale_y_continuous(labels=percent) +
  geom_hline(aes(yintercept=target / 100, linetype="Target")) +
  scale_linetype_manual(values=c(2,2)) +
  labs(x="",y="",linetype="",fill="",title="Proportion of staff time spent on projects") +
  guides(fill = guide_legend(order = 2),linetype = guide_legend(order = 1)) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    axis.line.x = element_line(color="#443e42"),
    axis.ticks = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust=1)
  )
