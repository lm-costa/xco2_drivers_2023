# data loading
df <- read.csv('data-raw/xco2_luc.csv')
sif <- read.csv('data-raw/sif.csv')
gpp <- read.csv('data-raw/gpp.csv')
power <-  read.csv('data-raw/power.csv',sep=';')
co2e <- read.csv('data-raw/co2emission.csv',sep=';')


# pre - processing

## detrending xco2
summary(lm(xco2 ~ x, data = df |> 
             dplyr::mutate(
               x = 1:dplyr::n(),
               date = lubridate::as_datetime(time_),
               date= lubridate::as_date(date)
             ) |> 
             dplyr::select(-c(time_,date_)) |> 
             dplyr::filter(lubridate::year(date)>2014, 
                           lubridate::year(date)<2022)
))


xco2 <- df |> 
  dplyr::mutate(
    X = Field1,
    date = lubridate::as_datetime(time_),
    date= lubridate::as_date(date)) |> 
  dplyr::select(-c(time_,date_)) |> 
  dplyr::filter(lubridate::year(date)>2014, 
                lubridate::year(date)<2022) |> 
  dplyr::mutate(
    xco2_est= 395.9 + 0.0001339*X,
    delta = xco2_est-xco2,
    xco2_reg=(395.9-delta)-(mean(xco2)-395.9),
    month=lubridate::month(date),
    year=lubridate::year(date)
  )  |> 
  dplyr::group_by(month,year) |> 
  dplyr::summarise(
    xco2_obs=mean(xco2),
    xco2_sd=sd(xco2),
    xco2_est=mean(xco2_reg),
    xco2_adj_sd=sd(xco2_reg)
  )


sif <- sif |> 
  dplyr::filter(sif_757>0) |> 
  dplyr::mutate(
    date=lubridate::as_datetime(time,origin='1990-01-01 00:00:00'),
    date=lubridate::as_date(date),
    year=lubridate::year(date),
    month=lubridate::month(date)
  ) |> 
  dplyr::group_by(year,month) |> 
  dplyr::summarise(
    SIF_757=mean(sif_757),
    SIF_757sd=sd(sif_757)
  )
gpp <- gpp |> 
  dplyr::mutate(
    year=lubridate::year(Date),
    month=lubridate::month(Date)
  ) |> 
  dplyr::group_by(year,month) |> 
  dplyr::summarise(
    GPP=sum(MOD17A2H_006_Gpp_500m)
  )

power <- power |> 
  dplyr::mutate(
    year=substr(YYYYMMDD,7,10),
    month=substr(YYYYMMDD,4,5),
    day=substr(YYYYMMDD,1,2),
    date=lubridate::make_date(year,month,day),
  ) |> 
  dplyr::mutate(
    year=lubridate::year(date),
    month=lubridate::month(date)
  ) |> 
  dplyr::group_by(lon,lat,year,month) |>
  dplyr::summarise(
    Qg = mean(ALLSKY_SFC_SW_DWN),
    Qg_sd = sd(ALLSKY_SFC_SW_DWN),
    Prec = sum(PRECTOTCORR)
  ) |> 
  dplyr::group_by(year,month) |> 
  dplyr::summarise(
    Qg = mean(Qg),
    Qg_sd = mean(Qg_sd),
    Prec = mean(Prec))


## aggregatating

tab <- xco2 |> 
  dplyr::mutate(
    XCO2=xco2_obs,
    XCO2r=xco2_est
  ) |> 
  dplyr::select(year,month,XCO2,XCO2r) |> 
  dplyr::left_join(sif |> dplyr::select(year,month,SIF_757)) |> 
  dplyr::left_join(gpp |> dplyr::select(year,month,GPP)) |> 
  dplyr::left_join(power |> dplyr::select(year,month,Prec,Qg)) |> 
  dplyr::filter(year>2014 & year < 2022) |>
  na.omit()

tab_me <-xco2 |> 
  dplyr::mutate(
    XCO2=xco2_obs,
    XCO2r=xco2_est
  ) |> 
  dplyr::select(year,month,XCO2, xco2_sd,XCO2r, xco2_adj_sd) |> 
  dplyr::left_join(sif ) |> 
  dplyr::left_join(gpp ) |> 
  dplyr::left_join(power) |> 
  dplyr::filter(year>2014 & year < 2022)

write.csv(tab_me,'data/mean_tab.csv') # table with means and standard deviations

# Correlation analysis
## spearman
tab|> 
  GGally::ggpairs(columns = 3:8, 
                  upper=list(continuous = GGally::wrap("cor", 
                                                       method = "spearman")))
ggplot2::ggsave('img/xco2_correl2.png',units="in", width=8, height=6,
                dpi=1000)

## PCA 
library(factoextra)

data_pca <- tab[4:8]
attach(data_pca)

data(decathlon2)
decathlon2.active <- tab[1:83, 4:8]
head(decathlon2.active[,1:5])

res.pca <- prcomp(data_pca, scale = TRUE)


fviz_pca_var(res.pca,
             col.var = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
             
)

ggplot2::ggsave('img/xco2_pca.png',units="in", width=8, height=6,
                dpi=1000)



# temporal distribution

xco2 |> 
  dplyr::filter(year>2014 & year < 2022) |> 
  dplyr::mutate(
    date= lubridate::as_date(stringr::str_c(
      year,month,'1',sep = '-'
    )) 
  ) |> 
  ggplot2::ggplot()+
  ggplot2::geom_point(ggplot2::aes(x=date, y=xco2_obs,
                                   color='XCO2', shape='XCO2',
  ))+
  ggplot2::geom_line(ggplot2::aes(x=date, y=xco2_obs,color='XCO2' ))+
  ggplot2::geom_point(ggplot2::aes(x=date,y=xco2_est,
                                   color='XCO2r', shape='XCO2r', 
  ))+
  ggplot2::geom_line(ggplot2::aes(x=date,y=xco2_est,color='XCO2r'))+
  ggplot2::geom_smooth(ggplot2::aes(x=date, y=xco2_obs), method = 'lm')+
  ggplot2::geom_smooth(data = xco2 |> 
                         dplyr::filter(year>2016 & year < 2022) |> 
                         dplyr::mutate(
                           date= lubridate::as_date(stringr::str_c(
                             year,month,'1',sep = '-'
                           ))),
                       ggplot2::aes(x=date,y=xco2_est),
                       method = 'lm'
  )+
  ggpubr::stat_regline_equation(ggplot2::aes(x=date,y=xco2_obs,color='XCO2',
                                             label =  paste(ggplot2::after_stat(eq.label),
                                                            sep = "*plain(\",\")~~")),
                                label.y = 411)+
  ggpubr::stat_cor(
    ggplot2::aes(x=date,y=xco2_obs,color='XCO2'),
    label.y = 409.8
  )+
  ggpubr::stat_regline_equation(data = xco2 |> 
                                  dplyr::filter(year>2016 & year < 2022) |> 
                                  dplyr::mutate(
                                    date= lubridate::as_date(stringr::str_c(
                                      year,month,'1',sep = '-'
                                    ))),
                                ggplot2::aes(x=date,y=xco2_est,color = 'XCO2r',
                                             label =  paste(expression('y = 380 + 0.0008'~'x'),
                                                            sep = "*plain(\",\")~~")),
                                label.y = 395)+
  ggpubr::stat_cor(data = xco2 |> 
                     dplyr::filter(year>2016 & year < 2022) |> 
                     dplyr::mutate(
                       date= lubridate::as_date(stringr::str_c(
                         year,month,'1',sep = '-'
                       ))),
                   ggplot2::aes(x=date,y=xco2_est,color = 'XCO2r'),
                   label.y = 393.7
  )+
  ggplot2::annotate('text', 
                    y=412,
                    x=lubridate::as_date('2019-06-01'), 
                    label=expression('Xco'[2]),
                    color='darkred')+
  ggplot2::annotate('text', 
                    y=394,
                    x=lubridate::as_date('2019-06-01'), 
                    label=expression('Xco'[2][R]),
                    color='red')+
  ggplot2::scale_color_manual(name='',
                              breaks=c('XCO2','XCO2r'),
                              values=c('XCO2'='darkred', 'XCO2r'='red')
  )+
  ggplot2::scale_shape_manual(name='',
                              breaks=c('XCO2','XCO2r'),
                              values=c('XCO2'=16, 'XCO2r'= 17),
  )+
  ggplot2::theme_bw()+
  ggplot2::xlab('Date')+
  ggplot2::ylab(expression(
    'Xco'[2]~' (ppm)'
  ))+
  ggplot2::theme(
    axis.text = ggplot2::element_text(size=12, color='black'),
    axis.ticks = ggplot2::element_line(size,color='black')
  )
ggplot2::ggsave('img/xco2_.png',units="in", width=8, height=4,
                dpi=1000)


mod <- xco2 |> 
  dplyr::filter(year>2016 & year < 2022) |> 
  dplyr::mutate(
    date= lubridate::as_date(stringr::str_c(
      year,month,'1',sep = '-'
    ))) |> 
  lm(formula=xco2_est ~date)


dfn <- broom::augment(mod)

ggplot2::ggplot(dfn, ggplot2::aes(x = .fitted, y = .resid)) + 
  ggplot2::geom_point()+
  ggplot2::geom_smooth(method = 'lm')+
  ggplot2::theme_bw()+
  ggplot2::ggtitle(expression('XCO'[2][R]~' vs date'))

ggplot2::ggsave('img/xco2__residual.png',units="in", width=5, height=4,
                dpi=1000)

sif |> 
  dplyr::filter(year>2014 & year < 2022) |> 
  dplyr::mutate(
    date= lubridate::as_date(stringr::str_c(
      year,month,'1',sep = '-'
    )) 
  ) |> 
  ggplot2::ggplot(ggplot2::aes(x=date, y=SIF_757))+
  ggplot2::geom_point(color='dark green')+
  ggplot2::geom_line(color='dark green')+
  ggplot2::theme_bw()+
  ggplot2::xlab('Date')+
  ggplot2::ylab(expression(
    'SIF 757nm (Wm'^-2~'sr'^-1~mu~'m'^-1~')'
  ))+
  ggplot2::theme(
    axis.text = ggplot2::element_text(size=12,color='black'),
    axis.ticks = ggplot2::element_line(color='black')
  )

ggplot2::ggsave('img/sif.png',units="in", width=5, height=4,
                dpi=1000)


gpp |> 
  dplyr::filter(year>2014 & year < 2022) |> 
  dplyr::mutate(
    date= lubridate::as_date(stringr::str_c(
      year,month,'1',sep = '-'
    )) 
  )  |> 
  ggplot2::ggplot(ggplot2::aes(x=date, y=GPP))+
  ggplot2::geom_point(color='dark green')+
  ggplot2::geom_line(color='dark green')+
  ggplot2::theme_bw()+
  ggplot2::xlab('Date')+
  ggplot2::ylab(expression(
    'GPP (kg C '~m^-2~')'
  ))+
  ggplot2::theme(
    axis.text = ggplot2::element_text(size=12,color='black'),
    axis.ticks = ggplot2::element_line(color='black')
  )

ggplot2::ggsave('img/gpp.png',units="in", width=5, height=4,
                dpi=1000)


power |> 
  dplyr::filter(year>2014 & year < 2022) |> 
  dplyr::mutate(
    date= lubridate::as_date(stringr::str_c(
      year,month,'1',sep = '-'
    )) 
  )  |> 
  ggplot2::ggplot(ggplot2::aes(x=date, y=Qg))+
  ggplot2::geom_point(color='dark orange')+
  ggplot2::geom_line(color='dark orange')+
  ggplot2::theme_bw()+
  ggplot2::xlab('Date')+
  ggplot2::ylab(expression(
    'Qg (MJ m'^-2~'dia'^-1~')'
  ))+
  ggplot2::theme(
    axis.text = ggplot2::element_text(size=12,color='black'),
    axis.ticks = ggplot2::element_line(color='black')
  )


ggplot2::ggsave('img/qg.png',units="in", width=5, height=4,
                dpi=1000)

power |> 
  dplyr::filter(year>2014 & year < 2022) |> 
  dplyr::mutate(
    date= lubridate::as_date(stringr::str_c(
      year,month,'1',sep = '-'
    )) 
  )  |> 
  ggplot2::ggplot(ggplot2::aes(x=date, y=Prec))+
  ggplot2::geom_point(color='dark blue')+
  ggplot2::geom_line(color='dark blue')+
  ggplot2::theme_bw()+
  ggplot2::xlab('Date')+
  ggplot2::ylab(expression(
    'Precipitação (mm)'
  ))+
  ggplot2::theme(
    axis.text = ggplot2::element_text(size=12,color='black'),
    axis.ticks = ggplot2::element_line(color='black')
  )


ggplot2::ggsave('img/prec.png',units="in", width=5, height=4,
                dpi=1000)

# Linear regressions

mod <- tab |>
  lm(formula = XCO2r~SIF_757)

dfn <- broom::augment(mod)

ggplot2::ggplot(dfn, ggplot2::aes(x = .fitted, y = .resid)) + 
  ggplot2::geom_point()+
  ggplot2::geom_smooth(method = 'lm')+
  ggplot2::theme_bw()+
  ggplot2::ggtitle(expression('XCO'[2][R]~' vs SIF'))

ggplot2::ggsave('img/xco2_sif_residual.png',units="in", width=5, height=4,
                dpi=1000)

pvalue <- summary(mod)$coefficients[2,4]

tab |> 
  ggplot2::ggplot(ggplot2::aes(x=SIF_757,y=XCO2r))+
  ggplot2::geom_point()+
  ggplot2::geom_smooth(method = 'lm')+
  ggplot2::annotate('text', 
                    y=388.5,
                    x=0.195, 
                    label=paste0('p-value = ',signif(pvalue,3))
  )+
  ggpubr::stat_regline_equation(ggplot2::aes(
    label =  paste(ggplot2::after_stat(eq.label), 
                   ggplot2::after_stat(rr.label),
                   sep = "*plain(\",\")~~")),
    label.y=389
    ) +
  ggplot2::theme_bw()+
  ggplot2::theme(
    axis.text = ggplot2::element_text(color='black'),
    axis.ticks = ggplot2::element_line(color='black')
  )+
  ggplot2::ylab(expression(
    'Xco'[2][R]~' (ppm)'
  ))+
  ggplot2::xlab(expression(
    'SIF 757nm (Wm'^-2~'sr'^-1~mu~'m'^-1~')'
  ))

ggplot2::ggsave('img/xco2_sif.png',units="in", width=5, height=4,
                dpi=1000)

mod <- tab |>
  lm(formula = XCO2r~GPP)

dfn <- broom::augment(mod)

ggplot2::ggplot(dfn, ggplot2::aes(x = .fitted, y = .resid)) + 
  ggplot2::geom_point()+
  ggplot2::geom_smooth(method = 'lm')+
  ggplot2::theme_bw()+
  ggplot2::ggtitle(expression('XCO'[2][R]~' vs GPP'))

ggplot2::ggsave('img/xco2_gpp_residual.png',units="in", width=5, height=4,
                dpi=1000)

pvalue <- summary(mod)$coefficients[2,4]

tab |> 
  ggplot2::ggplot(ggplot2::aes(x=GPP,y=XCO2r))+
  ggplot2::geom_point()+
  ggplot2::geom_smooth(method = 'lm')+
  ggpubr::stat_regline_equation(ggplot2::aes(
    label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~")),
    label.y = 389) +
  ggplot2::theme_bw()+
  ggplot2::theme(
    axis.text = ggplot2::element_text(color='black'),
    axis.ticks = ggplot2::element_line(color='black')
  )+
  ggplot2::annotate('text', 
                    y=388.5,
                    x=6.5, 
                    label=paste0('p-value = ',signif(pvalue,3))
  )+
  ggplot2::xlab(expression('GPP ( g C m'^-2~'month'^-1~')'))+
  ggplot2::ylab(expression(
    'Xco'[2][R]~' (ppm)'
  ))

ggplot2::ggsave('img/xco2_gpp.png',units="in", width=5, height=4,
                dpi=1000)


mod <- tab |>
  lm(formula = GPP~SIF_757)

dfn <- broom::augment(mod)

ggplot2::ggplot(dfn, ggplot2::aes(x = .fitted, y = .resid)) + 
  ggplot2::geom_point()+
  ggplot2::geom_smooth(method = 'lm')+
  ggplot2::theme_bw()+
  ggplot2::ggtitle(expression('GPP vs SIF'))

ggplot2::ggsave('img/sif_gpp_residual.png',units="in", width=5, height=4,
                dpi=1000)
pvalue <- summary(mod)$coefficients[2,4]

tab |> 
  ggplot2::ggplot(ggplot2::aes(x=SIF_757,y=GPP))+
  ggplot2::geom_point()+
  ggplot2::geom_smooth(method = 'lm')+
  ggpubr::stat_regline_equation(ggplot2::aes(
    label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~")),
    label.y = 15.5) +
  ggplot2::theme(
    axis.text = ggplot2::element_text(color='black')
  )+
  ggplot2::theme_bw()+
  ggplot2::theme(
    axis.text = ggplot2::element_text(color='black'),
    axis.ticks = ggplot2::element_line(color='black')
  )+
  ggplot2::annotate('text', 
                    y=14.5,
                    x=0.195, 
                    label=paste0('p-value = ',signif(pvalue,3))
  )+
  ggplot2::ylab(expression('GPP ( g C m'^-2~'month'^-1~')'))+
  ggplot2::xlab(expression(
    'SIF 757nm (Wm'^-2~'sr'^-1~mu~'m'^-1~')'
  ))

ggplot2::ggsave('img/gpp_sif.png',units="in", width=5, height=4,
                dpi=1000)

mod <- tab |>
  lm(formula = SIF_757~Prec)

dfn <- broom::augment(mod)

ggplot2::ggplot(dfn, ggplot2::aes(x = .fitted, y = .resid)) + 
  ggplot2::geom_point()+
  ggplot2::geom_smooth(method = 'lm')+
  ggplot2::theme_bw()+
  ggplot2::ggtitle(expression('SIF vs Prec'))

ggplot2::ggsave('img/sif_prec_residual.png',units="in", width=5, height=4,
                dpi=1000)

pvalue <- summary(mod)$coefficients[2,4]

tab |> 
  ggplot2::ggplot(ggplot2::aes(y=SIF_757,x=Prec))+
  ggplot2::geom_point()+
  ggplot2::geom_smooth(method = 'lm')+
  ggpubr::stat_regline_equation(ggplot2::aes(
    label =  paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~~")),
    label.y = 0.5) +
  ggplot2::theme_bw()+
  ggplot2::theme(
    axis.text = ggplot2::element_text(color='black'),
    axis.ticks = ggplot2::element_line(color='black')
  )+
  ggplot2::annotate('text', 
                    y=0.45,
                    x=45, 
                    label=paste0('p-value = ',signif(pvalue,3))
  )+
  ggplot2::xlab(expression('Prec (mm month'^-1~')'))+
  ggplot2::ylab(expression(
    'SIF 757nm (Wm'^-2~'sr'^-1~mu~'m'^-1~')'
  ))

ggplot2::ggsave('img/sif_prec.png',units="in", width=5, height=4,
                dpi=1000)

# MK test
tab_mk <- xco2 |> 
  dplyr::filter(year>2014 & year < 2022) |> 
  dplyr::mutate(
    date= lubridate::as_date(stringr::str_c(
      year,month,'1',sep = '-'
    )) 
  )

trend::mk.test(tab_mk$xco2_est,alternative = 'greater')

trend::mk.test(gpp$GPP,alternative='less')
trend::mk.test(sif$SIF_757,alternative='less')
trend::mk.test(power$Qg|> na.omit(), alternative='less')
trend::mk.test(power$Prec|> na.omit(), alternative='less')



# Xco2 land uses
xco2_forest <- df |> 
  dplyr::mutate(
    date = lubridate::as_datetime(time_),
    date= lubridate::as_date(date),
    lu = dplyr::case_when(
      RASTERVALU == 1 ~ 'Forest',
      RASTERVALU == 10 ~ 'Other Natural',
      RASTERVALU == 14 ~ 'Farming',
      RASTERVALU == 22 ~ 'Non vegetaded'
    )
  ) |> 
  dplyr::select(-c(time_,date_)) |> 
  dplyr::filter(lu == 'Forest') |> 
  dplyr::mutate(
    X = 1:dplyr::n()
  ) |> 
  dplyr::select(X,lon,lat,date,lu,xco2,uncertanty)

summary(lm(xco2~X,data=xco2_forest))

xco2_forest <- xco2_forest |> 
  dplyr::mutate(
    xco2_est= 395.9 + 0.0004783*X,
    delta = xco2_est-xco2,
    xco2_reg=(395.9-delta)-(mean(xco2)-395.9),
    month=lubridate::month(date),
    year=lubridate::year(date)
  )

xco2_farming <- df |> 
  dplyr::mutate(
    date = lubridate::as_datetime(time_),
    date= lubridate::as_date(date),
    lu = dplyr::case_when(
      RASTERVALU == 1 ~ 'Forest',
      RASTERVALU == 10 ~ 'Other Natural',
      RASTERVALU == 14 ~ 'Farming',
      RASTERVALU == 22 ~ 'Non vegetaded'
    )
  ) |> 
  dplyr::select(-c(time_,date_)) |> 
  dplyr::filter(lu == 'Farming') |> 
  dplyr::mutate(
    X = 1:dplyr::n()
  ) |> 
  dplyr::select(X,lon,lat,date,lu,xco2,uncertanty)

summary(lm(xco2~X,data=xco2_farming))

xco2_farming <- xco2_farming |> 
  dplyr::mutate(
    xco2_est= 395.8 + 0.000206*X,
    delta = xco2_est-xco2,
    xco2_reg=(395.8-delta)-(mean(xco2)-395.8),
    month=lubridate::month(date),
    year=lubridate::year(date)
  )

xco2_lu <- rbind(xco2_farming,xco2_forest)

pv_farming <- summary(lm(XCO2reg~date,
                 data= xco2_lu |> 
                   dplyr::group_by(year,month,lu) |> 
                   dplyr::summarise(
                     XCO2reg = mean(xco2_reg),
                     regsd =sd(xco2_reg),
                     XCO2obs= mean(xco2),
                     obssd=sd(xco2)
                   ) |> 
                   dplyr::mutate(date=lubridate::make_date(year,month,'1')) |> 
                   dplyr::filter(lu=='Farming',
                                 year>2016) 
                 ))$coefficients[2,4]

mod <- lm(XCO2reg~date,
          data= xco2_lu |>
            dplyr::group_by(year,month,lu) |>
            dplyr::summarise(
              XCO2reg = mean(xco2_reg),
              regsd =sd(xco2_reg),
              XCO2obs= mean(xco2),
              obssd=sd(xco2)
            ) |>
            dplyr::mutate(date=lubridate::make_date(year,month,'1')) |>
            dplyr::filter(lu=='Farming',
                          year>2016)
)

dfn <- broom::augment(mod)

ggplot2::ggplot(dfn, ggplot2::aes(x = .fitted, y = .resid)) + 
  ggplot2::geom_point()+
  ggplot2::geom_smooth(method = 'lm')+
  ggplot2::theme_bw()+
  ggplot2::ggtitle(expression('XCO'[2][R]~'Farming vs date'))

ggplot2::ggsave('img/xco2farming_residual.png',units="in", width=5, height=4,
                dpi=1000)

pv_forest <- summary(lm(XCO2reg~date,
                        data= xco2_lu |>
                          dplyr::group_by(year,month,lu) |>
                          dplyr::summarise(
                            XCO2reg = mean(xco2_reg),
                            regsd =sd(xco2_reg),
                            XCO2obs= mean(xco2),
                            obssd=sd(xco2)
                          ) |>
                          dplyr::mutate(date=lubridate::make_date(year,month,'1')) |>
                          dplyr::filter(lu=='Forest',
                                        year>2016)
                        ))$coefficients[2,4]
mod <- lm(XCO2reg~date,
          data= xco2_lu |>
            dplyr::group_by(year,month,lu) |>
            dplyr::summarise(
              XCO2reg = mean(xco2_reg),
              regsd =sd(xco2_reg),
              XCO2obs= mean(xco2),
              obssd=sd(xco2)
            ) |>
            dplyr::mutate(date=lubridate::make_date(year,month,'1')) |>
            dplyr::filter(lu=='Forest',
                          year>2016)
          )

dfn <- broom::augment(mod)

ggplot2::ggplot(dfn, ggplot2::aes(x = .fitted, y = .resid)) + 
  ggplot2::geom_point()+
  ggplot2::geom_smooth(method = 'lm')+
  ggplot2::theme_bw()+
  ggplot2::ggtitle(expression('XCO'[2][R]~'Forest vs date'))

ggplot2::ggsave('img/xco2forest_residual.png',units="in", width=5, height=4,
                dpi=1000)

xco2_lu |> 
  dplyr::group_by(year,month,lu) |> 
  dplyr::mutate(
    pvalue_ = dplyr::case_when(
      lu == 'Forest'~pv_forest,
      lu=='Farming'~pv_farming
    )
  ) |> 
  dplyr::summarise(
    XCO2reg = mean(xco2_reg),
    regsd =sd(xco2_reg),
    XCO2obs= mean(xco2),
    obssd=sd(xco2),
    pvalue_=mean(pvalue_)
  ) |> 
  dplyr::mutate(date=lubridate::make_date(year,month,'1'),
                LU = lu) |>
  dplyr::filter(year>2014 & year < 2022) |> 
  ggplot2::ggplot(ggplot2::aes(x=date,y=XCO2reg,col=LU
  ))+
  ggplot2::geom_point()+
  ggplot2::geom_line()+
  ggplot2::geom_smooth(data=xco2_lu |>
                         dplyr::group_by(year,month,lu) |> 
                         dplyr::mutate(
                           pvalue_ = dplyr::case_when(
                             lu == 'Forest'~pv_forest,
                             lu=='Farming'~pv_farming
                           )
                         ) |> 
                         dplyr::summarise(
                           XCO2reg = mean(xco2_reg),
                           regsd =sd(xco2_reg),
                           XCO2obs= mean(xco2),
                           obssd=sd(xco2),
                           pvalue_=mean(pvalue_)
                         ) |>
                         dplyr::mutate(date=lubridate::make_date(year,month,'1'),
                                       LU = lu) |> 
                         dplyr::filter(year>2016, year < 2022),
                       ggplot2::aes(x=date,y=XCO2reg, col=LU),
                       method='lm',
                       se=F
  )+
  ggpubr::stat_regline_equation(data=  xco2_lu |>
                                  dplyr::group_by(year,month,lu) |> 
                                  dplyr::mutate(
                                    pvalue_ = dplyr::case_when(
                                      lu == 'Forest'~pv_forest,
                                      lu=='Farming'~pv_farming
                                    )
                                  ) |> 
                                  dplyr::summarise(
                                    XCO2reg = mean(xco2_reg),
                                    regsd =sd(xco2_reg),
                                    XCO2obs= mean(xco2),
                                    obssd=sd(xco2),
                                    pvalue_=mean(pvalue_)
                                  ) |>
                                  dplyr::mutate(date=lubridate::make_date(year,month,'1'),
                                                LU = lu) |> 
                                  dplyr::filter(year>2016, year < 2022),
                                ggplot2::aes(y=XCO2reg,col=LU,
                                             label =  paste(..eq.label.., 
                                                            ..rr.label..,
                                                            c(signif(pv_farming,3),
                                                              signif(pv_forest,3)), 
                                                            sep = "*plain(\",\")~~"))
  ) +
  ggplot2::theme_bw()+
  ggplot2::ylab('XCO'[2][R]~' (ppm)')+
  ggplot2::xlab('')

ggplot2::ggsave('img/xco2_lu.png',units="in", width=7, height=4,
                dpi=1000)

mkfo <- xco2_lu |> 
  dplyr::filter(lu =='Forest') |> 
  dplyr::group_by(year,month) |> 
  dplyr::summarise(
    XCO2reg = mean(xco2_reg))
trend::mk.test(mkfo$XCO2reg,alternative = 'greater')

mkfar <- xco2_lu |> 
  dplyr::filter(lu =='Farming') |> 
  dplyr::group_by(year,month) |> 
  dplyr::summarise(
    XCO2reg = mean(xco2_reg))
trend::mk.test(mkfar$XCO2reg,alternative = 'greater')


xco2_lu_m <- xco2_lu |> 
  dplyr::group_by(year,month,lu) |> 
  dplyr::summarise(xco2=mean(xco2_reg))


mod_aov <- aov(xco2~lu,data=xco2_lu_m)
summary(mod_aov)


shapiro.test(mod_aov$residuals)

tk_xco2 <- agricolae::HSD.test(mod_aov,'lu',group=T,console=T,alpha = .01)


# CO2 emission

co2e |> 
  dplyr::mutate(PF = FP,
                SF = FS,
                year=ano) |> 
  tidyr::pivot_longer(
    cols = "PF":"SF",
    names_to = "defor_class",
    values_to = "co2emi") |> 
  ggplot2::ggplot(ggplot2::aes(x=year,y=co2emi/1000000,col=defor_class))+
  ggplot2::geom_point()+
  ggplot2::geom_line()+
  ggplot2::geom_smooth(data = co2e |> 
                         dplyr::mutate(PF = FP,
                                       SF = FS,
                                       year=ano) |> 
                         tidyr::pivot_longer(
                           cols = "PF":"SF",
                           names_to = "defor_class",
                           values_to = "co2emi") |> 
                         dplyr::filter(year>2016),
                       ggplot2::aes(x=year,y=co2emi/1000000,col=defor_class),
                       method = 'lm',
                       se=F
  )+
  ggpubr::stat_cor(
    data = co2e |>  
      dplyr::mutate(PF = FP,
                    SF = FS,
                    year=ano) |> 
      tidyr::pivot_longer(
        cols = "PF":"SF",
        names_to = "defor_class",
        values_to = "co2emi") |> 
      dplyr::filter(year>2016),
    ggplot2::aes(x=year,y=co2emi/1000000,col=defor_class)
  )+
  ggplot2::labs(
    x='',
    y= 'ECO'[2]~' (t \u00D7 '*10^6~')',
    color='')+
  ggplot2::theme_bw()

ggplot2::ggsave('img/co2_emi_set.png',units="in", width=5, height=4,
                dpi=1000)



mod <- lm(Total~year,data=co2e |> 
            dplyr::mutate(year=ano) |> 
            dplyr::filter(year>2016)
          )

dfn <- broom::augment(mod)

ggplot2::ggplot(dfn, ggplot2::aes(x = .fitted, y = .resid)) + 
  ggplot2::geom_point()+
  ggplot2::geom_smooth(method = 'lm')+
  ggplot2::theme_bw()+
  ggplot2::ggtitle(expression('ECO'[2]~' vs year'))

ggplot2::ggsave('img/eco2_residual.png',units="in", width=5, height=4,
                dpi=1000)
pv_eco2 <- summary(mod)$coefficients[2,4]
co2e |> 
  dplyr::mutate(year=ano) |> 
  ggplot2::ggplot(ggplot2::aes(x=year,y=Total/1000000))+
  ggplot2::geom_point()+
  ggplot2::geom_line()+
  ggplot2::geom_smooth(data=co2e |> 
                         dplyr::mutate(year=ano) |> 
                         dplyr::filter(year>2016),
                       ggplot2::aes(x=year,y=Total/1000000), method='lm', se=F)+
  ggpubr::stat_regline_equation(data=  co2e |>
                                  dplyr::mutate(year=ano) |> 
                                  dplyr::filter(year>2016),
                                ggplot2::aes(
                                  label =  paste(..eq.label.., 
                                                 ..rr.label..,
                                                 signif(pv_eco2,3),
                                                 sep = "*plain(\",\")~~")))+
  ggplot2::theme_bw()+
  ggplot2::labs(x='',
                y=expression('ECO'[2]~' (t \u00D7 10'^6~')'))

ggplot2::ggsave('img/co2_emi_tot.png',units="in", width=5, height=4,
                dpi=1000)
a <- xco2 |> 
  dplyr::mutate(
    XCO2=xco2_obs,
    XCO2reg=xco2_est
  ) |> 
  dplyr::group_by(year) |> 
  dplyr::summarise(
    xco2_reg = mean(XCO2reg)
  ) |> 
  dplyr::filter(year > 2014, year < 2022) |> 
  dplyr::left_join(co2e |> dplyr::mutate(year=ano)) |> 
  dplyr::mutate(
    pf = FP/1e6,
    sf = FS/1e6,
    t = Total/1e6)


mod <- lm(xco2_reg ~ t, data=a)


dfn <- broom::augment(mod)

ggplot2::ggplot(dfn, ggplot2::aes(x = .fitted, y = .resid)) + 
  ggplot2::geom_point()+
  ggplot2::geom_smooth(method = 'lm')+
  ggplot2::theme_bw()+
  ggplot2::ggtitle(expression('XCO'[2][R]~' vs ECO'[2]))

ggplot2::ggsave('img/eco2_vs_xco2_residual.png',units="in", width=5, height=4,
                dpi=1000)
pv_xco_eco <- summary(mod)$coefficients[2,4]

xco2 |> 
  dplyr::mutate(
    XCO2=xco2_obs,
    XCO2reg=xco2_est
  ) |> 
  dplyr::group_by(year) |> 
  dplyr::summarise(
    xco2_reg = mean(XCO2reg)
  ) |> 
  dplyr::filter(year > 2014, year < 2022) |> 
  dplyr::left_join(co2e |> dplyr::mutate(year=ano)) |> 
  dplyr::mutate(
    pf = FP/1e6,
    sf = FS/1e6,
    t = Total/1e6
  ) |> 
  ggplot2::ggplot(ggplot2::aes(x=t,y=xco2_reg))+
  ggplot2::geom_point()+
  ggplot2::geom_smooth(method='lm',se=F)+
  ggpubr::stat_regline_equation(
    ggplot2::aes(
      label =  paste(..eq.label.., 
                     ..rr.label..,
                     signif(pv_xco_eco,3),
                     sep = "*plain(\",\")~~"))
  ) +
  ggplot2::theme_bw()+
  ggplot2::xlab('ECO'[2]~' (t \u00D7 10'^6~')')+
  ggplot2::ylab('Xco'[2][R]~' (ppm)')

ggplot2::ggsave('img/xco2_co2_t.png',units="in", width=5, height=4,
                dpi=1000)
