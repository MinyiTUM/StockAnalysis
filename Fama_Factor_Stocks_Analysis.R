
library(dplyr) 
library(lubridate)
library(zoo)
library(xtable)
library(data.table)
#load the libraries of package dplyr,lubridate,zoo,data.table and xtable
# for further data manipulation.

# [DATA CLEANING]

pmHKG_group<-pmHKG%>%filter(!is.na(RET))
#filter out the monthly observations that the variable RET.USD is missing.

pmHKG_group<-pmHKG_group%>%
        group_by(month = lubridate::floor_date(Date, "month"),.add=FALSE)%>%
        filter(month>="1989-12-01")
#choose 12/1989 as starting point for raw data set 
#and add month column for following manipulation steps,
#without the rounded month by floor_date, diff(Date) is not equally spread in the data frame.
#e.g. 03-31 to 04-28 VS 05-30 to 06-30

pmHKG_group<-pmHKG_group%>%
        filter(mv>quantile(.data= .,na.omit(mv), probs = 0.05)|(mv>quantile(.data= .,na.omit(mv), probs = 0.1)&up>quantile(.data= .,na.omit(up), probs = 0.05)))
#filter out penny stocks´observations each month

pmHKG_group<-pmHKG_group%>%ungroup()
#ungroup the data frame pmHKG_group

pmHKG_group<-pmHKG_group%>%
        group_by(Id)%>%mutate(monReturn=ifelse(lag(RET.USD)==0,-1,RET.USD/lag(RET.USD)-1),monReturn=round(monReturn,3))%>%
        na.omit()
#calculate monthly return using total return index. 
#If RET.USD exists value like 0, monReturn will return minus infinite. We handle this case by setting monReturn is -1.

pmHKG_group<-pmHKG_group%>%
        mutate(monReturn=ifelse((monReturn>3 | lag(monReturn)>3)& (1+monReturn)*(1+lag(monReturn))-1<0.50,NA,monReturn))%>%
        filter(!is.na(monReturn))
# monthly return reversals dynamic filter

pmHKG_group<-pmHKG_group%>%
        mutate(monReturn=ifelse(monReturn>3,NA,monReturn))%>%
        filter(!is.na(monReturn))
# set all return to missing,for which the return is greater than 300 percent.

# [DATA CLEANING SUMMARY TABLE CREATION]


coverage<-pmHKG_group%>%ungroup()%>%group_by(Year)%>%summarize(NumberFirms=n_distinct(Id),MarketValue_Mean=mean(mv),Market_Median=mean(monReturn),UnadjustedPrice=sd(up))
var.labels = c(NumberFirms="Number of Firms", MarketValue_Mean="Market Value(Mean)",Market_Median="Market Value(Median)",MonthlyReturn="Monthly Return",UnadjustedPrice="Unadjusted Price(Standard Deviation)")
tli.table<-xtable(coverage,align=c(rep("c",7)),digits = c(0,0,0,3,3,3,3),caption="\\tt Summary Of Hong Kong Stocks")
print(tli.table,floating= T,latex.environments = "center",include.rownames=F,scalebox = 0.9)
#Create cleaned data summary table


# [CALCULATE CUMULATIVE RETURN FOR 12-2 MONTH]

pmHKG_momStr<-pmHKG_group%>%
        ungroup()%>%
        group_by(Id,grp=cumsum(c(1,(diff(month)>31|diff(month)<0))))
#We set all the observations which have consecutive date as well as same Id in one group.

pmHKG_momStr_formation<-pmHKG_momStr%>%
        filter(n()>=13)%>%
        mutate(CumulateRe_12_2=rollapplyr(monReturn,11,function(x)prod(x+1)-1,fill=NA),CumulateRe_12_2=round(CumulateRe_12_2,3))%>%
        na.omit()
#We choose only stocks have valid 11 months returns in the formation phase
#We also choose only stocks have valid lagged MV and monthly Return for the holding phase.
#Therefore the n()here is larger than 13 but not larger than 11.

pmHKG_momStr_formation<-pmHKG_momStr_formation%>%
        ungroup%>%
        mutate(country=NULL,RET.USD=NULL,RET=NULL,mv=NULL,up=NULL,Year=NULL,monReturn=NULL,Date=NULL,grp=NULL)
#we leave only two key joining variables Id,month and one variable CumulateRe_12_2  for left outer join later.

pmHKG_momStr<-merge(x=pmHKG_momStr,y=pmHKG_momStr_formation,by=c("Id","month"),all.x=TRUE)
#we create a spreadsheet wise data frame with monthly return, market value and corresponding 11 months cumulative return
rm(pmHKG_momStr_formation)
#pmHKG_momStr_formation is intermediate place for saving cumulative return result.
#We delete it after left joining the data frame pmHKG_momStr.

pmHKG_momStr<-pmHKG_momStr%>%
        group_by(grp)%>%
        mutate(CumulateRe_12_2=lag(CumulateRe_12_2,2))%>%
        mutate(mv=lag(mv,1))%>%
        filter(!is.na(CumulateRe_12_2))
#These three lines code try to show the potential winners and losers with one month prior cumulative return 
#corresponded lagged mv and current monthly return

pmHKG_momStr<-pmHKG_momStr%>%
        ungroup()%>%
        select(-grp)
#Hide the unneeded variable grp and ungroup the data frame


# [CONSTRUCT CONVENTIONAL 12-2 MOMENTUM STRATEGY]
winner_momStr<-pmHKG_momStr%>%group_by(
        month)%>%filter(
        CumulateRe_12_2>=quantile(.data= .,na.omit(CumulateRe_12_2), probs = 0.9))%>%arrange(
        month)%>%select(
        -country&-up&-RET)
loser_momStr<-pmHKG_momStr%>%group_by(
        month)%>%filter(
        CumulateRe_12_2<=quantile(.data= .,na.omit(CumulateRe_12_2), probs = 0.1))%>%arrange(
        month)%>%select(
        -country&-up&-RET)
#create winner stocks and loser stocks data frame

winner_momStr<-winner_momStr%>%
        mutate(mv_VW=mv/sum(mv),mv_VW=round(mv_VW,3))%>%
        mutate(mv_EW=1/n(),mv_EW=round(mv_EW,3))%>%mutate(winner_loser="W")

loser_momStr<-loser_momStr%>%
        mutate(mv_VW=mv/sum(mv),mv_VW=round(mv_VW,3))%>%
        mutate(mv_EW=1/n(),mv_EW=round(mv_EW,3))%>%mutate(winner_loser="L")
#label winners and losers and calculate value-weight and equal-weight of selected stocks

Result_12_2<-rbind(winner_momStr,loser_momStr)
#combine winner and loser data frames together for further trading.

Result_12_2<-Result_12_2%>%arrange(month)
#reorder the rows, show each month with winners and losers at the same time

Result_12_2<-Result_12_2%>%
        mutate(LS_VW=ifelse(winner_loser=="W",mv_VW*monReturn,-(mv_VW*monReturn)),LS_VW=round(LS_VW,3))%>%
        mutate(LS_EW=ifelse(winner_loser=="W",mv_EW*monReturn,-(mv_EW*monReturn)),LS_EW=round(LS_EW,3))%>%
        summarise(LSreturnVW=sum(LS_VW),LSreturnEW=sum(LS_EW))
#long winers and short losers using value-weighted method as well as equal-weighted method
#and hold the portfolio for one month
#rebalance monthly

# [SHARPE RATIO CALCULATION]
Result_12_2<-Result_12_2%>%mutate(Year=year(month))
#extract year from "month" variable as grouping variable
#It is used for calculate annualized sharpe ratio
SH_12_2_MomVW<-aggregate(
        x=list(SH_VW=Result_12_2$LSreturnVW,SH_EW=Result_12_2$LSreturnEW), 
        by=list(ReturnSharpe = Result_12_2$Year),
        FUN= function(x) round(mean(x)/sd(x), digits=3))


# [SHARPE DOUBLE SORTED MOMENTUM FACTOR]
pmHKG_Brs1<-pmHKG_momStr%>%group_by(month)%>%mutate(brs_1=case_when(
        CumulateRe_12_2<=quantile(.data= .,CumulateRe_12_2,probs = 0.3)~"Loser",
        CumulateRe_12_2>=quantile(.data= .,CumulateRe_12_2,probs = 0.7)~"Winner",TRUE~"Neutral"))
#Sort stocks into Winner, Neutral and Loser portfolios according to the 12-2 returns based on the 70% and 30% quantile

pmHKG_Brs1<-pmHKG_Brs1%>%mutate(
        grpl=lubridate::quarter(month, fiscal_start = 7))%>%arrange(
        month)
#Goal is to group July(t) to June(t+1) into one group
#arbitrary set the fiscal year starting on July and end next year June

pmHKG_Brs1<-pmHKG_Brs1%>%ungroup()%>%group_by(Year)%>%mutate(sizeBrsYear=case_when(
        grpl == 1 | grpl == 2 ~ (Year + 1), 
        grpl == 3 | grpl == 4 ~ (Year) ) )%>%select(-grpl)
#every year, e.g. quarter 3+4(1991) belong to current semester year(1991)
#quarter 1+2 belong to next fiscal year (1992)

pmHKG_Brs1<-setDT(pmHKG_Brs1)
#set pmHKG_Brs1 as data.table for easier manipulation

temp2<-pmHKG_Brs1[lubridate::month(Date)==6L|sizeBrsYear==2019L,
                  .(Big=stats::quantile(mv,probs=0.9),Small=stats::quantile(mv,probs=0.1)),
                  by=sizeBrsYear]
#minimum date of sample data is 1991-02-28.Between 1992-02 and 1992-06, we use the size classification in 1992-06
#1992-07 till 1993-06 use also the size classification in 1992-06

temp2<-temp2[,
             .(sizeBrsYear,Big=shift(Big,1,fill=Big[1],type="lag"),Small=shift(Small,1,fill=Small[1],type="lag"))]
#these two lines try to generate size classification in June(t) for July(t)- June(t+1)

pmHKG_Brs1[temp2,on="sizeBrsYear",`:=`(Big=i.Big,Small=i.Small)]
#temp2 is a data frame for saving the intermediate results calculating for mv in June every year
#we join temp2 with pmHKG_Brs1 data frames

pmHKG_Brs1[mv>=Big,brs_2:=c("BigStock")][mv<=Big,
                                         brs_2:=c("SmallStock")][mv<Big&mv>Small,
                                                                 brs_2:=c("Middle")]
#we label small stocks and big stocks based on the 90% and 10% aggregate lagged June size breakpoints

temp[,c("sizeBrsYear","Big","Small","Year"):=NULL][brs_2=="SmallStock"|brs_2=="BigStock",
                                                   `:=`(vw_mv=round(mv/sum(mv),3),ew_mv=round(1/(.N),3)),by=.(month,brs_1)]
