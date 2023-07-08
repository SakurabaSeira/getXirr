library(lubridate)
library(tvm)
#生成连续13个月的时间序列 方便后续取用
dates=Sys.Date()+months(0:12)
getXirr<-function(x_rental,x_total,x_buy,n,x_check,x_lock){
  cashflow=c()
  date=c()
  x_monthly=(x_rental/12)*0.92;
  #进货价格
  cashflow=append(cashflow,-x_buy)
  date=append(date,dates[1])
  #邮费
  cashflow=append(cashflow,-25)
  date=append(date,dates[1])
  #质检费
  if(x_check>0){
    cashflow=append(cashflow,-x_check)
    date=append(date,dates[1])
  }
  #租金
  for(i in 1:12){
    if(i==1){
      cashflow=append(cashflow,x_monthly*n)
      date=append(date,dates[i])
    }
    else if(i<=12-n){
      cashflow=append(cashflow,x_monthly)
      date=append(date,dates[i])
    }
    #加锁费
    if(x_lock>0){
      cashflow=append(cashflow,-x_lock)
      date=append(date,dates[i])
    }
  }
  #租完还订单的买断回收
  if(x_rental<x_total){
    cashflow=append(cashflow,(x_total-x_rental)*0.92)
    date=append(date,dates[13])
  }
  result=xirr(cf = cashflow, d = date)
  print(result)
}
#读取数据集
row=read.table("dataset.csv",header=T, sep=",")
#计算每种情况的年化收益率
datas=c()
for(i in (1:12)){
  xr=getXirr(as.numeric(row[i,3]),as.numeric(row[i,4]),as.numeric(row[i,5]),as.numeric(row[i,6]),as.numeric(row[i,7]),as.numeric(row[i,8]))
  datas=append(datas,xr*100)
}
#绘制柱状图
img=as.matrix(data.frame('iPhone14 PM' = datas[1:4],
                         'Xiaomi 13' = datas[5:8],
                         'iQOO11 Pro' = datas[9:12]))
barplot(img,                                        
        col = c("#1b98e0", "#5bf8f0","#252223", "#858486"),
        beside = TRUE, xlab="手机型号", ylab="年化收益率(%)",
        names.arg=c('iPhone14 ProMax','Xiaomi 13','vivo iQOO11 Pro'))

legend("top",                                   
       legend = c("租完送(1期)", "租完送(2期)", "租完还(1期)", "租完还(2期)"),
       fill = c("#1b98e0", "#5bf8f0","#252223", "#858486"))
