#设置路径
setwd("C:/GBDdx/dx1101shijie")
getwd()
#加载包
library(easyGBDR)
library(ggplot2)
library(flextable)
library(ggsci)
#读取数据
data <- GBDread(, folder = T, foldername = "C:/GBDdx/dx1101shijie")
data <- subset(data,cause=="Neglected tropical diseases and malaria")
#1负担分析
#1.1地图
mapdata <- subset(data,data$age=="Age-standardized"&
                    data$metric=="Rate"&
                    data$sex=="Both"&
                    data$year=="2021")

inmapdata <- subset(mapdata,measure=="Incidence")
inmap <- ggGBDmap(
  inmapdata,
  variable="val",
  color = "scale_fill_distiller(palette='Spectral',direction = -1,name='ASIR')",
  guide_name = "ASIR"
)
inmap
ggsave("Map incidence.pdf",width = 10,height=8,dpi=300)
ggsave("Map incidence.jpeg",width = 10,height=8,dpi=300)

prmapdata <- subset(mapdata,measure=="Prevalence")
prmap <- ggGBDmap(
  prmapdata,
  variable="val",
  color = "scale_fill_distiller(palette='Spectral',direction = -1,name='ASPR')",
  guide_name = "ASPR"
)
prmap
ggsave("Map prevalence.pdf",width = 10,height=8,dpi=300)
ggsave("Map prevalence.jpeg",width = 10,height=8,dpi=300)

demapdata <- subset(mapdata,measure=="Deaths")
demap <- ggGBDmap(
  demapdata,
  variable="val",
  color = "scale_fill_distiller(palette='Spectral',direction = -1,name='ASDR')",
  guide_name = "ASDER"
)
demap
ggsave("Map deaths.pdf",width = 10,height=8,dpi=300)
ggsave("Map deaths.jpeg",width = 10,height=8,dpi=300)

damapdata <- subset(mapdata,measure=="DALYs (Disability-Adjusted Life Years)")
damap <- ggGBDmap(
  damapdata,
  variable="val",
  color = "scale_fill_distiller(palette='Spectral',direction = -1,name='ASDAR')",
  guide_name = "ASR of DALY"
)
damap
ggsave("Map DALY.pdf",width = 10,height=8,dpi=300)
ggsave("Map DALY.jpeg",width = 10,height=8,dpi=300)

#1.2双坐标




dxadata <- subset(data,year=="2021"&
                    age!="Age-standardized"&
                    age!="All ages"&
                    sex!="Both"&
                    location=="Global")
dxadata$age <- factor(dxadata$age, levels = c("<5 years","5-9 years","10-14 years","15-19 years","20-24 years","25-29 years","30-34 years","35-39 years","40-44 years","45-49 years",     
                                              "50-54 years","55-59 years","60-64 years","65-69 years","70-74 years","75-79 years","80-84 years","85-89 years","90-94 years","95+ years" ))

indxadata <- subset(dxadata,measure=="Incidence")
indx <- ggDx(indxadata, x_axis = "age", ratio = "auto", group = "sex", ASR = F)
indx
ggsave("Dx Incidence.pdf",width = 8,height=6,dpi=300)
ggsave("Dx Incidence.jpeg",width = 8,height=6,dpi=300)
indx


prdxadata <- subset(dxadata,measure=="Prevalence")
prdx <- ggDx(prdxadata, x_axis = "age", ratio = "auto", group = "sex", ASR = F)
prdx <- prdx+theme(
  panel.background = element_rect(fill = "white", color = NA),  # 设置纯白背景
  panel.grid.major = element_blank(),  # 去掉主网格线
  panel.grid.minor = element_blank(),
  axis.text.x = element_text(angle = 45, hjust = 1)  # x轴刻度标签倾斜45度# 去掉次网格线
)
prdx
ggsave("Dx Prevalence.pdf",width = 8,height=6,dpi=300)
ggsave("Dx Prevalence.jpeg",width = 8,height=6,dpi=300)

dedxadata <- subset(dxadata,measure=="Deaths")
dedx <- ggDx(dedxadata, x_axis = "age", ratio = "auto", group = "sex", ASR = F)
dedx <- dedx+theme(
  panel.background = element_rect(fill = "white", color = NA),  # 设置纯白背景
  panel.grid.major = element_blank(),  # 去掉主网格线
  panel.grid.minor = element_blank(),
  axis.text.x = element_text(angle = 45, hjust = 1)  # x轴刻度标签倾斜45度# 去掉次网格线
)
dedx
ggsave("Dx deaths.pdf",width = 8,height=6,dpi=300)
ggsave("Dx deaths.jpeg",width = 8,height=6,dpi=300)

dadxadata <- subset(dxadata,measure=="DALYs (Disability-Adjusted Life Years)")
dadx <- ggDx(dadxadata, x_axis = "age", ratio = "auto", group = "sex", ASR = F)
dadx <- dadx+theme(
  panel.background = element_rect(fill = "white", color = NA),  # 设置纯白背景
  panel.grid.major = element_blank(),  # 去掉主网格线
  panel.grid.minor = element_blank(),
  axis.text.x = element_text(angle = 45, hjust = 1)  # x轴刻度标签倾斜45度# 去掉次网格线
)
dadx
ggsave("Dx DALY.pdf",width = 8,height=6,dpi=300)
ggsave("Dx DALY.jpeg",width = 8,height=6,dpi=300)

#1.3负担表
in21num <- subset(data,year=="2021"&
                    metric=="Number"&
                    age=="All ages"&
                    measure=="Incidence"&
                    sex=="Both")
in21num <- in21num[,c(2,8,9,10)]
in21num$val <- round(in21num$val,0)
in21num$upper <- round(in21num$upper,0)
in21num$lower <- round(in21num$lower,0)
in21num$incount <- paste(in21num$lower,in21num$upper, sep="-")
in21num$incount <- paste(in21num$incount, ")", sep="")
in21num$incount <- paste("(", in21num$incount, sep="")
in21num$incount <- paste(in21num$val, in21num$incount, sep="\n")
in21num <- in21num[,c(1,5)]

in21rate <- subset(data,year=="2021"&
                     metric=="Rate"&
                     age=="Age-standardized"&
                     measure=="Incidence"&
                     sex=="Both")
in21rate <- in21rate[,c(2,8,9,10)]
in21rate$val <- round(in21rate$val,2)
in21rate$upper <- round(in21rate$upper,2)
in21rate$lower <- round(in21rate$lower,2)
in21rate$inrate <- paste(in21rate$lower,in21rate$upper, sep="-")
in21rate$inrate <- paste(in21rate$inrate, ")", sep="")
in21rate$inrate <- paste("(", in21rate$inrate, sep="")
in21rate$inrate <- paste(in21rate$val, in21rate$inrate, sep="\n")
in21rate <- in21rate[,c(1,5)]

pr21num <- subset(data,year=="2021"&
                    metric=="Number"&
                    age=="All ages"&
                    measure=="Prevalence"&
                    sex=="Both")
pr21num <- pr21num[,c(2,8,9,10)]
pr21num$val <- round(pr21num$val,0)
pr21num$upper <- round(pr21num$upper,0)
pr21num$lower <- round(pr21num$lower,0)
pr21num$prcount <- paste(pr21num$lower,pr21num$upper, sep="-")
pr21num$prcount <- paste(pr21num$prcount, ")", sep="")
pr21num$prcount <- paste("(", pr21num$prcount, sep="")
pr21num$prcount <- paste(pr21num$val, pr21num$prcount, sep="\n")
pr21num <- pr21num[,c(1,5)]

pr21rate <- subset(data,year=="2021"&
                     metric=="Rate"&
                     age=="Age-standardized"&
                     measure=="Prevalence"&
                     sex=="Both")
pr21rate <- pr21rate[,c(2,8,9,10)]
pr21rate$val <- round(pr21rate$val,2)
pr21rate$upper <- round(pr21rate$upper,2)
pr21rate$lower <- round(pr21rate$lower,2)
pr21rate$prrate <- paste(pr21rate$lower,pr21rate$upper, sep="-")
pr21rate$prrate <- paste(pr21rate$prrate, ")", sep="")
pr21rate$prrate <- paste("(", pr21rate$prrate, sep="")
pr21rate$prrate <- paste(pr21rate$val, pr21rate$prrate, sep="\n")
pr21rate <- pr21rate[,c(1,5)]

de21num <- subset(data,year=="2021"&
                    metric=="Number"&
                    age=="All ages"&
                    measure=="Deaths"&
                    sex=="Both")
de21num <- de21num[,c(2,8,9,10)]
de21num$val <- round(de21num$val,0)
de21num$upper <- round(de21num$upper,0)
de21num$lower <- round(de21num$lower,0)
de21num$decount <- paste(de21num$lower,de21num$upper, sep="-")
de21num$decount <- paste(de21num$decount, ")", sep="")
de21num$decount <- paste("(", de21num$decount, sep="")
de21num$decount <- paste(de21num$val, de21num$decount, sep="\n")
de21num <- de21num[,c(1,5)]

de21rate <- subset(data,year=="2021"&
                     metric=="Rate"&
                     age=="Age-standardized"&
                     measure=="Deaths"&
                     sex=="Both")
de21rate <- de21rate[,c(2,8,9,10)]
de21rate$val <- round(de21rate$val,2)
de21rate$upper <- round(de21rate$upper,2)
de21rate$lower <- round(de21rate$lower,2)
de21rate$derate <- paste(de21rate$lower,de21rate$upper, sep="-")
de21rate$derate <- paste(de21rate$derate, ")", sep="")
de21rate$derate <- paste("(", de21rate$derate, sep="")
de21rate$derate <- paste(de21rate$val, de21rate$derate, sep="\n")
de21rate <- de21rate[,c(1,5)]

da21num <- subset(data,year=="2021"&
                    metric=="Number"&
                    age=="All ages"&
                    measure=="DALYs (Disability-Adjusted Life Years)"&
                    sex=="Both")
da21num <- da21num[,c(2,8,9,10)]
da21num$val <- round(da21num$val,0)
da21num$upper <- round(da21num$upper,0)
da21num$lower <- round(da21num$lower,0)
da21num$dacount <- paste(da21num$lower,da21num$upper, sep="-")
da21num$dacount <- paste(da21num$dacount, ")", sep="")
da21num$dacount <- paste("(", da21num$dacount, sep="")
da21num$dacount <- paste(da21num$val, da21num$dacount, sep="\n")
da21num <- da21num[,c(1,5)]

da21rate <- subset(data,year=="2021"&
                     metric=="Rate"&
                     age=="Age-standardized"&
                     measure=="DALYs (Disability-Adjusted Life Years)"&
                     sex=="Both")
da21rate <- da21rate[,c(2,8,9,10)]
da21rate$val <- round(da21rate$val,2)
da21rate$upper <- round(da21rate$upper,2)
da21rate$lower <- round(da21rate$lower,2)
da21rate$darate <- paste(da21rate$lower,da21rate$upper, sep="-")
da21rate$darate <- paste(da21rate$darate, ")", sep="")
da21rate$darate <- paste("(", da21rate$darate, sep="")
da21rate$darate <- paste(da21rate$val, da21rate$darate, sep="\n")
da21rate <- da21rate[,c(1,5)]

#2.趋势
#2.1AAPC
aapcdata <- subset(data,age=="Age-standardized"&
                     sex=="Both")

aapcresult <- GBDASR_aapc(
  aapcdata,
  startyear = 1990,
  endyear = 2021,
  model = "ln",
  joinpoints = 3,
  rei_included = F,
  CI = TRUE,
  digits = 2,
  sep = " to ",
  constant_variance = F,
  AAPCrange = NULL
)

inaapc <- subset(aapcresult[["AAPC"]],measure=="Incidence")
inaapc$inaapc <-inaapc$AAPC_95CI 
inaapc <- inaapc[,c(2,17)]

praapc <- subset(aapcresult[["AAPC"]],measure=="Prevalence")
praapc$praapc <-praapc$AAPC_95CI 
praapc <- praapc[,c(2,17)]

deaapc <- subset(aapcresult[["AAPC"]],measure=="Deaths")
deaapc$deaapc <-deaapc$AAPC_95CI 
deaapc <- deaapc[,c(2,17)]

daaapc <- subset(aapcresult[["AAPC"]],measure=="DALYs (Disability-Adjusted Life Years)")
daaapc$daaapc <-daaapc$AAPC_95CI 
daaapc <- daaapc[,c(2,17)]


table1 <- merge(in21num,in21rate,all.x=TRUE,sort=TRUE)
table1 <- merge(table1,pr21num,all.x=TRUE,sort=TRUE)
table1 <- merge(table1,pr21rate,all.x=TRUE,sort=TRUE)
table1 <- merge(table1,de21num,all.x=TRUE,sort=TRUE)
table1 <- merge(table1,de21rate,all.x=TRUE,sort=TRUE)
table1 <- merge(table1,da21num,all.x=TRUE,sort=TRUE)
table1 <- merge(table1,da21rate,all.x=TRUE,sort=TRUE)
table1 <- merge(table1,inaapc,all.x=TRUE,sort=TRUE)
table1 <- merge(table1,praapc,all.x=TRUE,sort=TRUE)
table1 <- merge(table1,deaapc,all.x=TRUE,sort=TRUE)
table1 <- merge(table1,daaapc,all.x=TRUE,sort=TRUE)

###输出表格
table1output <- flextable(table1,col_keys = names(table1))
####竖直方向上居中、顶端对齐、底部对齐
table1output <- valign(table1output, valign="center",part="header")
####水平方向上对齐方式
table1output <- align(table1output,align = "center", part="all")
####字体修改
table1output <- font(table1output,fontname = "Times New Roman", part = "all")
####三线表线条修改 上线修改
table1output <- hline_top(table1output,border = fp_border(color="black",width = 1.5),part = "header")
####三线表线条修改 下线修改
table1output <- hline_bottom(table1output, border = fp_border(color = "black",width = 1.5),part="body")
####三线表线条修改 中间修改
table1output <- hline(table1output,i=1, border = fp_border(color = "black",width = 1),part = "header")
####三线表标题命名
table1output <- set_caption(table1output,"Table1.",autonum = 1, style = "Table Caption")
####三线表自动调整
table1output <- autofit(table1output)
###保存word
save_as_docx(table1output, path = "Tableip.docx")



#2.2Joinpoint图 Global
ingjp <- ggjoinpoint_apc(
  aapcresult,
  location_name = "Global",
  measure_name = "Incidence",
  cause_name = "Neglected tropical diseases and malaria",
  sex_name = "Both",
  age_name = "Age-standardized",
  rei_name = NULL,
  text_line = T,
  facet_name = NULL,
  point_color = "black",
  joinpoint_color = "black",
  line_size = 1
)
ingjp <- ingjp+theme(
  panel.background = element_rect(fill = "white", color = NA),  # 设置纯白背景
  panel.grid.major = element_blank(),  # 去掉主网格线
  panel.grid.minor = element_blank(),
  axis.text.x = element_text(angle = 45, hjust = 1)  # x轴刻度标签倾斜45度# 去掉次网格线
)
ingjp
ggsave("Joinpoint global incidence.pdf",width = 8,height=6,dpi=300)
ggsave("Joinpoint global incidence.jpeg",width = 8,height=6,dpi=300)

prgjp <- ggjoinpoint_apc(
  aapcresult,
  location_name = "Global",
  measure_name = "Prevalence",
  cause_name = "Neglected tropical diseases and malaria",
  sex_name = "Both",
  age_name = "Age-standardized",
  rei_name = NULL,
  text_line = T,
  facet_name = NULL,
  point_color = "black",
  joinpoint_color = "black",
  line_size = 1
)
prgjp <- prgjp+theme(
  panel.background = element_rect(fill = "white", color = NA),  # 设置纯白背景
  panel.grid.major = element_blank(),  # 去掉主网格线
  panel.grid.minor = element_blank(),
  axis.text.x = element_text(angle = 45, hjust = 1)  # x轴刻度标签倾斜45度# 去掉次网格线
)
prgjp
ggsave("Joinpoint global prevalence.pdf",width = 8,height=6,dpi=300)
ggsave("Joinpoint global prevalence.jpeg",width = 8,height=6,dpi=300)

degjp <- ggjoinpoint_apc(
  aapcresult,
  location_name = "Global",
  measure_name = "Deaths",
  cause_name = "Neglected tropical diseases and malaria",
  sex_name = "Both",
  age_name = "Age-standardized",
  rei_name = NULL,
  text_line = T,
  facet_name = NULL,
  point_color = "black",
  joinpoint_color = "black",
  line_size = 1
)
degjp <- degjp+theme(
  panel.background = element_rect(fill = "white", color = NA),  # 设置纯白背景
  panel.grid.major = element_blank(),  # 去掉主网格线
  panel.grid.minor = element_blank(),
  axis.text.x = element_text(angle = 45, hjust = 1)  # x轴刻度标签倾斜45度# 去掉次网格线
)
degjp
ggsave("Joinpoint global deaths.pdf",width = 8,height=6,dpi=300)
ggsave("Joinpoint global deaths.jpeg",width = 8,height=6,dpi=300)

dagjp <- ggjoinpoint_apc(
  aapcresult,
  location_name = "Global",
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  cause_name = "Neglected tropical diseases and malaria",
  sex_name = "Both",
  age_name = "Age-standardized",
  rei_name = NULL,
  text_line = T,
  facet_name = NULL,
  point_color = "black",
  joinpoint_color = "black",
  line_size = 1
)
dagjp <- dagjp+theme(
  panel.background = element_rect(fill = "white", color = NA),  # 设置纯白背景
  panel.grid.major = element_blank(),  # 去掉主网格线
  panel.grid.minor = element_blank(),
  axis.text.x = element_text(angle = 45, hjust = 1)  # x轴刻度标签倾斜45度# 去掉次网格线
)
dagjp
ggsave("Joinpoint global DALY.pdf",width = 8,height=6,dpi=300)
ggsave("Joinpoint global DALY.jpeg",width = 8,height=6,dpi=300)

#2.3Joinpoint图 SDI
inrjp <- ggjoinpoint_compare(
  aapcresult,
  group_name = "location",
  location_name = c("High SDI", "High-middle SDI", "Middle SDI", "Low-middle SDI",
                    "Low SDI"),
  measure_name = "Incidence",
  cause_name = "Neglected tropical diseases and malaria",
  sex_name = "Both",
  age_name = "Age-standardized",
  rei_name = NULL,
  facet_name = NULL,
  color_name = pal_lancet("lanonc")(9)[1:5],
  shape_name = c(14, 15, 16, 17, 18),
  line_size = 1,
  text_line = T
)
inrjp <- inrjp+theme(
  panel.background = element_rect(fill = "white", color = NA),  # 设置纯白背景
  panel.grid.major = element_blank(),  # 去掉主网格线
  panel.grid.minor = element_blank(),
  axis.text.x = element_text(angle = 45, hjust = 1)  # x轴刻度标签倾斜45度# 去掉次网格线
)
inrjp
ggsave("inrjp.pdf",width = 10,height=6,dpi=300)
ggsave("inrjp.jpeg",width = 10,height=6,dpi=300)

prrjp <- ggjoinpoint_compare(
  aapcresult,
  group_name = "location",
  location_name = c("High SDI", "High-middle SDI", "Middle SDI", "Low-middle SDI",
                    "Low SDI"),
  measure_name = "Prevalence",
  cause_name = "Neglected tropical diseases and malaria",
  sex_name = "Both",
  age_name = "Age-standardized",
  rei_name = NULL,
  facet_name = NULL,
  color_name = pal_lancet("lanonc")(9)[1:5],
  shape_name = c(14, 15, 16, 17, 18),
  line_size = 1,
  text_line = T
)
prrjp <- prrjp+theme(
  panel.background = element_rect(fill = "white", color = NA),  # 设置纯白背景
  panel.grid.major = element_blank(),  # 去掉主网格线
  panel.grid.minor = element_blank(),
  axis.text.x = element_text(angle = 45, hjust = 1)  # x轴刻度标签倾斜45度# 去掉次网格线
)
prrjp
ggsave("prrjp.pdf",width = 10,height=6,dpi=300)
ggsave("prrjp.jpeg",width = 10,height=6,dpi=300)

derjp <- ggjoinpoint_compare(
  aapcresult,
  group_name = "location",
  location_name = c("High SDI", "High-middle SDI", "Middle SDI", "Low-middle SDI",
                    "Low SDI"),
  measure_name = "Deaths",
  cause_name = "Neglected tropical diseases and malaria",
  sex_name = "Both",
  age_name = "Age-standardized",
  rei_name = NULL,
  facet_name = NULL,
  color_name = pal_lancet("lanonc")(9)[1:5],
  shape_name = c(14, 15, 16, 17, 18),
  line_size = 1,
  text_line = T
)
derjp <- derjp+theme(
  panel.background = element_rect(fill = "white", color = NA),  # 设置纯白背景
  panel.grid.major = element_blank(),  # 去掉主网格线
  panel.grid.minor = element_blank(),
  axis.text.x = element_text(angle = 45, hjust = 1)  # x轴刻度标签倾斜45度# 去掉次网格线
)
derjp
ggsave("derjp.pdf",width = 10,height=6,dpi=300)
ggsave("derjp.jpeg",width = 10,height=6,dpi=300)

darjp <- ggjoinpoint_compare(
  aapcresult,
  group_name = "location",
  location_name = c("High SDI", "High-middle SDI", "Middle SDI", "Low-middle SDI",
                    "Low SDI"),
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  cause_name = "Neglected tropical diseases and malaria",
  sex_name = "Both",
  age_name = "Age-standardized",
  rei_name = NULL,
  facet_name = NULL,
  color_name = pal_lancet("lanonc")(9)[1:5],
  shape_name = c(14, 15, 16, 17, 18),
  line_size = 1,
  text_line = T
)
darjp <- darjp+theme(
  panel.background = element_rect(fill = "white", color = NA),  # 设置纯白背景
  panel.grid.major = element_blank(),  # 去掉主网格线
  panel.grid.minor = element_blank(),
  axis.text.x = element_text(angle = 45, hjust = 1)  # x轴刻度标签倾斜45度# 去掉次网格线
)
darjp
ggsave("darjp.pdf",width = 10,height=6,dpi=300)
ggsave("darjp.jpeg",width = 10,height=6,dpi=300)

#2.4趋势双坐标图
dxbdata <- subset(data,location=="Global"&
                    sex!="Both"&
                    age=="All ages")

indxbdata <- subset(dxbdata,measure=="Incidence")
indxb <- ggDx(indxbdata, x_axis = "year", ratio = "auto", group = "sex", ASR = F)
indxb
ggsave("dx year Incidence.pdf",width = 8,height=6,dpi=300)
ggsave("dx year Incidence.jpeg",width = 8,height=6,dpi=300)

prdxbdata <- subset(dxbdata,measure=="Prevalence")
prdxb <- ggDx(prdxbdata, x_axis = "year", ratio = "auto", group = "sex", ASR = F)
prdxb <- prdxb+theme(
  panel.background = element_rect(fill = "white", color = NA),  # 设置纯白背景
  panel.grid.major = element_blank(),  # 去掉主网格线
  panel.grid.minor = element_blank(),
  axis.text.x = element_text(angle = 45, hjust = 1)  # x轴刻度标签倾斜45度# 去掉次网格线
)
prdxb
ggsave("dx year prevalence.pdf",width = 8,height=6,dpi=300)
ggsave("dx year prevalence.jpeg",width = 8,height=6,dpi=300)

dedxbdata <- subset(dxbdata,measure=="Deaths")
dedxb <- ggDx(dedxbdata, x_axis = "year", ratio = "auto", group = "sex", ASR = F)
dedxb <- dedxb+theme(
  panel.background = element_rect(fill = "white", color = NA),  # 设置纯白背景
  panel.grid.major = element_blank(),  # 去掉主网格线
  panel.grid.minor = element_blank(),
  axis.text.x = element_text(angle = 45, hjust = 1)  # x轴刻度标签倾斜45度# 去掉次网格线
)
dedxb
ggsave("dx year deaths.pdf",width = 8,height=6,dpi=300)
ggsave("dx year deaths.jpeg",width = 8,height=6,dpi=300)

dadxbdata <- subset(dxbdata,measure=="DALYs (Disability-Adjusted Life Years)")
dadxb <- ggDx(dadxbdata, x_axis = "year", ratio = "auto", group = "sex", ASR = F)
dadxb <- dadxb+theme(
  panel.background = element_rect(fill = "white", color = NA),  # 设置纯白背景
  panel.grid.major = element_blank(),  # 去掉主网格线
  panel.grid.minor = element_blank(),
  axis.text.x = element_text(angle = 45, hjust = 1)  # x轴刻度标签倾斜45度# 去掉次网格线
)
dadxb
ggsave("dx year DALY.pdf",width = 8,height=6,dpi=300)
ggsave("dx year DALY.jpeg",width = 8,height=6,dpi=300)

#3预测
inbpresult <- GBDbapc_prediction(
  data,
  measure_name="Incidence",
  cause_name="Neglected tropical diseases and malaria",
  location_name="Global",
  rei_name = NULL,
  By_sex = F,
  predyear = 2050,
  full_age_adjusted = F,
  rate_lessen = NULL
)

inbp <- ggprediction_ASR(
  inbpresult,
  CI = F,
  predict_start = 2020,
  group_name = "sex",
  location_name = "Global",
  measure_name = "Incidence",
  cause_name = "Neglected tropical diseases and malaria",
  rei_name = NULL,
  sex_name = c("Both","Male","Female")
)
inbp <- inbp+theme(
  panel.background = element_rect(fill = "white", color = NA),  # 设置纯白背景
  panel.grid.major = element_blank(),  # 去掉主网格线
  panel.grid.minor = element_blank(),
  axis.text.x = element_text(angle = 45, hjust = 1)  # x轴刻度标签倾斜45度# 去掉次网格线
)
inbp
ggsave("bapc incidence.pdf",width = 8,height=6,dpi=300)
ggsave("bapc incidence.jpeg",width = 8,height=6,dpi=300)

prbpresult <- GBDbapc_prediction(
  data,
  measure_name="Prevalence",
  cause_name="Neglected tropical diseases and malaria",
  location_name="Global",
  rei_name = NULL,
  By_sex = F,
  predyear = 2050,
  full_age_adjusted = F,
  rate_lessen = NULL
)

prbp <- ggprediction_ASR(
  prbpresult,
  CI = F,
  predict_start = 2020,
  group_name = "sex",
  location_name = "Global",
  measure_name = "Prevalence",
  cause_name = "Neglected tropical diseases and malaria",
  rei_name = NULL,
  sex_name = c("Both","Male","Female")
)
prbp <- prbp+theme(
  panel.background = element_rect(fill = "white", color = NA),  # 设置纯白背景
  panel.grid.major = element_blank(),  # 去掉主网格线
  panel.grid.minor = element_blank(),
  axis.text.x = element_text(angle = 45, hjust = 1)  # x轴刻度标签倾斜45度# 去掉次网格线
)
prbp
ggsave("bapc prevalence.pdf",width = 8,height=6,dpi=300)
ggsave("bapc prevalence.jpeg",width = 8,height=6,dpi=300)

debpresult <- GBDbapc_prediction(
  data,
  measure_name="Deaths",
  cause_name="Neglected tropical diseases and malaria",
  location_name="Global",
  rei_name = NULL,
  By_sex = F,
  predyear = 2050,
  full_age_adjusted = F,
  rate_lessen = NULL
)

debp <- ggprediction_ASR(
  debpresult,
  CI = F,
  predict_start = 2020,
  group_name = "sex",
  location_name = "Global",
  measure_name = "Deaths",
  cause_name = "Neglected tropical diseases and malaria",
  rei_name = NULL,
  sex_name = c("Both","Male","Female")
)
debp <- debp+theme(
  panel.background = element_rect(fill = "white", color = NA),  # 设置纯白背景
  panel.grid.major = element_blank(),  # 去掉主网格线
  panel.grid.minor = element_blank(),
  axis.text.x = element_text(angle = 45, hjust = 1)  # x轴刻度标签倾斜45度# 去掉次网格线
)
debp
ggsave("bapc death.pdf",width = 8,height=6,dpi=300)
ggsave("bapc death.jpeg",width = 8,height=6,dpi=300)


dabpresult <- GBDbapc_prediction(
  data,
  measure_name="DALYs (Disability-Adjusted Life Years)",
  cause_name="Neglected tropical diseases and malaria",
  location_name="Global",
  rei_name = NULL,
  By_sex = F,
  predyear = 2050,
  full_age_adjusted = F,
  rate_lessen = NULL
)

dabp <- ggprediction_ASR(
  dabpresult,
  CI = F,
  predict_start = 2020,
  group_name = "sex",
  location_name = "Global",
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  cause_name = "Neglected tropical diseases and malaria",
  rei_name = NULL,
  sex_name = c("Both","Male","Female")
)
dabp <- dabp+theme(
  panel.background = element_rect(fill = "white", color = NA),  # 设置纯白背景
  panel.grid.major = element_blank(),  # 去掉主网格线
  panel.grid.minor = element_blank(),
  axis.text.x = element_text(angle = 45, hjust = 1)  # x轴刻度标签倾斜45度# 去掉次网格线
)
dabp
ggsave("bapc DALY.pdf",width = 8,height=6,dpi=300)
ggsave("bapc DALY.jpeg",width = 8,height=6,dpi=300)

write.csv(inbpresult[["ASR"]],"bapc result incidence.csv")
write.csv(prbpresult[["ASR"]],"bapc result prevalence.csv")
write.csv(debpresult[["ASR"]],"bapc result death.csv")
write.csv(dabpresult[["ASR"]],"bapc result daly.csv")

#4.SDI相关性图
ridata <- subset(data,age=="Age-standardized"&
                   sex=="Both")
inridata <- subset(ridata,measure=="Incidence")
inri <- ggGBDsdiASR(
  inridata,
  span_val = 0.5,
  se_plot = T,
  rate = "Age-standardized",
  cor = "spearman",
  fig_type = "region"
)
inri <- inri+theme(
  panel.background = element_rect(fill = "white", color = NA),  # 设置纯白背景
  panel.grid.major = element_blank(),  # 去掉主网格线
  panel.grid.minor = element_blank(),
  axis.text.x = element_text(angle = 45, hjust = 1)  # x轴刻度标签倾斜45度# 去掉次网格线
)
inri
ggsave("ri spearman Incidence.pdf",width = 10,height=6,dpi=300)
ggsave("ri spearman Incidence.jpeg",width = 10,height=6,dpi=300)

prridata <- subset(ridata,measure=="Prevalence")
prri <- ggGBDsdiASR(
  prridata,
  span_val = 0.5,
  se_plot = T,
  rate = "Age-standardized",
  cor = "spearman",
  fig_type = "region"
)
prri <- prri+theme(
  panel.background = element_rect(fill = "white", color = NA),  # 设置纯白背景
  panel.grid.major = element_blank(),  # 去掉主网格线
  panel.grid.minor = element_blank(),
  axis.text.x = element_text(angle = 45, hjust = 1)  # x轴刻度标签倾斜45度# 去掉次网格线
)
prri
ggsave("ri spearman prevalence.pdf",width = 10,height=6,dpi=300)
ggsave("ri spearman prevalence.jpeg",width = 10,height=6,dpi=300)

deridata <- subset(ridata,measure=="Deaths")
deri <- ggGBDsdiASR(
  deridata,
  span_val = 0.5,
  se_plot = T,
  rate = "Age-standardized",
  cor = "spearman",
  fig_type = "region"
)
deri <- deri+theme(
  panel.background = element_rect(fill = "white", color = NA),  # 设置纯白背景
  panel.grid.major = element_blank(),  # 去掉主网格线
  panel.grid.minor = element_blank(),
  axis.text.x = element_text(angle = 45, hjust = 1)  # x轴刻度标签倾斜45度# 去掉次网格线
)
deri
ggsave("ri spearman death.pdf",width = 10,height=6,dpi=300)
ggsave("ri spearman death.jpeg",width = 10,height=6,dpi=300)

daridata <- subset(ridata,measure=="DALYs (Disability-Adjusted Life Years)")
dari <- ggGBDsdiASR(
  daridata,
  span_val = 0.5,
  se_plot = T,
  rate = "Age-standardized",
  cor = "spearman",
  fig_type = "region"
)
dari <- dari+theme(
  panel.background = element_rect(fill = "white", color = NA),  # 设置纯白背景
  panel.grid.major = element_blank(),  # 去掉主网格线
  panel.grid.minor = element_blank(),
  axis.text.x = element_text(angle = 45, hjust = 1)  # x轴刻度标签倾斜45度# 去掉次网格线
)
dari
ggsave("ri spearman DALY.pdf",width = 10,height=6,dpi=300)
ggsave("ri spearman DALY.jpeg",width = 10,height=6,dpi=300)

#5.分解方法
dcresult <- GBDdecomposition(
  data,
  byear = 1990,
  compareyear = 2021,
  startage = 0,
  endage = 95,
  percent = "overall difference"
)

dctable <- GBDdecomposition_table(
  dcresult,
  digits = 2,
  measure_name=c("Incidence","Prevalence","Deaths","DALYs (Disability-Adjusted Life Years)"),
  location_name=c("Global","High SDI","High-middle SDI","Middle SDI","Low-middle SDI","Low SDI"),
  sex_name="Both",
  rei_name = NULL,
  cause_name="Neglected tropical diseases and malaria",
)
indc <- ggdecomposition(
  dcresult,
  measure_name="Incidence",
  location_name=c("Global","High SDI","High-middle SDI","Middle SDI","Low-middle SDI","Low SDI"),
  sex_name="Both",
  rei_name = NULL,
  cause_name="Neglected tropical diseases and malaria",
  percent = F
)
indc
ggsave("Decomposition incidence.pdf",width = 8,height=6,dpi=300)
ggsave("Decomposition incidence.jpeg",width = 8,height=6,dpi=300)

prdc <- ggdecomposition(
  dcresult,
  measure_name="Prevalence",
  location_name=c("Global","High SDI","High-middle SDI","Middle SDI","Low-middle SDI","Low SDI"),
  sex_name="Both",
  rei_name = NULL,
  cause_name="Neglected tropical diseases and malaria",
  percent = F
)
prdc
ggsave("Decomposition prevalence.pdf",width = 8,height=6,dpi=300)
ggsave("Decomposition prevalence.jpeg",width = 8,height=6,dpi=300)

dedc <- ggdecomposition(
  dcresult,
  measure_name="Deaths",
  location_name=c("Global","High SDI","High-middle SDI","Middle SDI","Low-middle SDI","Low SDI"),
  sex_name="Both",
  rei_name = NULL,
  cause_name="Neglected tropical diseases and malaria",
  percent = F
)
dedc
ggsave("Decomposition death.pdf",width = 8,height=6,dpi=300)
ggsave("Decomposition death.jpeg",width = 8,height=6,dpi=300)

dadc <- ggdecomposition(
  dcresult,
  measure_name="DALYs (Disability-Adjusted Life Years)",
  location_name=c("Global","High SDI","High-middle SDI","Middle SDI","Low-middle SDI","Low SDI"),
  sex_name="Both",
  rei_name = NULL,
  cause_name="Neglected tropical diseases and malaria",
  percent = F
)
dadc
ggsave("decomposition DALY.pdf",width = 8,height=6,dpi=300)
ggsave("decomposition DALY.jpeg",width = 8,height=6,dpi=300)

write.csv(dcresult,"decomposition result.csv")


#6.Slope index
siresult <- GBDslope_index(
  data,
  all_age_range = NULL,
  SDI = F,
  GBDregion = F,
  SuperGBDregion = F
)
write.csv(siresult[["slope"]],"slope index result.csv")

insi <- ggslope_index(
  siresult,
  model = "rlm",
  color_name = c("#6699FF", "#990000"),
  group_name ="year",
  region_name = "All included",
  measure_name = "Incidence",
  sex_name = "Both",
  cause_name = "Neglected tropical diseases and malaria",
  rei_name = NULL,
  age_name = "Age-standardized",
  year_name = c(1990, 2021),
  country_label = NULL,
  population_count = 1e+06
)
insi <- insi+theme(
  panel.background = element_rect(fill = "white", color = NA),  # 设置纯白背景
  panel.grid.major = element_blank(),  # 去掉主网格线
  panel.grid.minor = element_blank(),
  axis.text.x = element_text(angle = 45, hjust = 1)  # x轴刻度标签倾斜45度# 去掉次网格线
)
insi
ggsave("slope incidence.pdf",width = 8,height=6,dpi=300)
ggsave("slope incidence.jpeg",width = 8,height=6,dpi=300)

prsi <- ggslope_index(
  siresult,
  model = "rlm",
  color_name = c("#6699FF", "#990000"),
  group_name ="year",
  region_name = "All included",
  measure_name = "Prevalence",
  sex_name = "Both",
  cause_name = "Neglected tropical diseases and malaria",
  rei_name = NULL,
  age_name = "Age-standardized",
  year_name = c(1990, 2021),
  country_label = NULL,
  population_count = 1e+06
)
prsi <- prsi+theme(
  panel.background = element_rect(fill = "white", color = NA),  # 设置纯白背景
  panel.grid.major = element_blank(),  # 去掉主网格线
  panel.grid.minor = element_blank(),
  axis.text.x = element_text(angle = 45, hjust = 1)  # x轴刻度标签倾斜45度# 去掉次网格线
)
prsi
ggsave("slope prevalence.pdf",width = 8,height=6,dpi=300)
ggsave("slope prevalence.jpeg",width = 8,height=6,dpi=300)

desi <- ggslope_index(
  siresult,
  model = "rlm",
  color_name = c("#6699FF", "#990000"),
  group_name ="year",
  region_name = "All included",
  measure_name = "Deaths",
  sex_name = "Both",
  cause_name = "Neglected tropical diseases and malaria",
  rei_name = NULL,
  age_name = "Age-standardized",
  year_name = c(1990, 2021),
  country_label = NULL,
  population_count = 1e+06
)
desi <- desi+theme(
  panel.background = element_rect(fill = "white", color = NA),  # 设置纯白背景
  panel.grid.major = element_blank(),  # 去掉主网格线
  panel.grid.minor = element_blank(),
  axis.text.x = element_text(angle = 45, hjust = 1)  # x轴刻度标签倾斜45度# 去掉次网格线
)
desi
ggsave("slope deaths.pdf",width = 8,height=6,dpi=300)
ggsave("slope deaths.jpeg",width = 8,height=6,dpi=300)

dasi <- ggslope_index(
  siresult,
  model = "rlm",
  color_name = c("#6699FF", "#990000"),
  group_name ="year",
  region_name = "All included",
  measure_name = "DALYs (Disability-Adjusted Life Years)",
  sex_name = "Both",
  cause_name = "Neglected tropical diseases and malaria",
  rei_name = NULL,
  age_name = "Age-standardized",
  year_name = c(1990, 2021),
  country_label = NULL,
  population_count = 1e+06
)
dasi <- dasi+theme(
  panel.background = element_rect(fill = "white", color = NA),  # 设置纯白背景
  panel.grid.major = element_blank(),  # 去掉主网格线
  panel.grid.minor = element_blank(),
  axis.text.x = element_text(angle = 45, hjust = 1)  # x轴刻度标签倾斜45度# 去掉次网格线
)
dasi
ggsave("slope DALY.pdf",width = 8,height=6,dpi=300)
ggsave("slope DALY.jpeg",width = 8,height=6,dpi=300)

