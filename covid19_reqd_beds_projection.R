coronasimr<-function(cases,los_median,los_95,tol=25,nreps=1000) {

packages_reqd<-c("parallel","tidyr","dplyr","ggplot2","gridExtra")
new.packages<-packages_reqd[!(packages_reqd %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
require(parallel)
require(tidyr)
require(dplyr)
require(ggplot2)
require(gridExtra)

# calculate lognormal distribution parameters from given information (los_mean and los_95)
erfinv<-function(x) qnorm((1+x)/2)/sqrt(2)
meanlog<-log(los_median)
sdlog<-((sqrt(2)*erfinv(0.9))^-1)*log(los_95/los_median)

# determine whether epidemic curve uncertainty is to be considered
if (tol>0 & tol<=100) {
  uncert<-TRUE
} else {
  uncert<-FALSE
}

inputs<-cases
inputs<-inputs %>%
  mutate(dates=as.Date(dates,"%d/%m/%Y")) %>%
  gather("metric","value",-dates)
if (uncert==TRUE) {
  inputs_sim<-lapply(1:nreps, function(i) {
    set.seed(i)
    scaler<-tol/100*max(rnorm(1,mean=0,sd=1/3),-1)
    data.frame(
      dates=inputs$dates,
      metric=paste0("rep",i),
      value=inputs$value*(1+scaler)
    )
  })
  inputs_sim<-do.call("rbind",inputs_sim)
  inputs<-rbind(inputs,inputs_sim)
  #get CIs
  inputs_sim_ci<-inputs %>%
    group_by(dates) %>%
    filter(substr(metric,1,3)=="rep") %>%
    summarise(q30=quantile(value,0.3),
              q15=quantile(value,0.15),
              q05=quantile(value,0.05),
              q025=quantile(value,0.025),
              q01=quantile(value,0.01),
              q70=quantile(value,0.7),
              q85=quantile(value,0.85),
              q95=quantile(value,0.95),
              q975=quantile(value,0.975),
              q99=quantile(value,0.99)) %>%
    gather("metric","value",-dates)
  inputs<-rbind(inputs,inputs_sim_ci)
}


cl<-makeCluster(detectCores()-1)
clusterExport(cl,c("inputs","nreps","meanlog","sdlog","uncert"),envir=environment())
outputs_sim<-parLapply(cl,1:nreps,function(i) {
  set.seed(i)
  lambda_vec<-inputs$value[which(inputs$metric==ifelse(uncert==TRUE,paste0("rep",i),"hospitalisations"))]
  tmp<-unlist(
    lapply(1:length(unique(inputs$dates)), function(y) {
    unlist(
      sapply(round(rlnorm(
        rpois(1,lambda=lambda_vec[y]),
        meanlog=meanlog,sdlog=sdlog)),
      function(z) y-1+1:z))
  }))
  as.numeric(table(factor(tmp,levels=1:max(tmp))))
})
stopCluster(cl)
outputs_sim<-lapply(outputs_sim, function(i) {
  length(i)<-length(unique(inputs$dates))*1.33
  i[is.na(i)]<-0
  i
})
outputs_sim<-data.frame(do.call(cbind,outputs_sim))
outputs_sim<-data.frame(cbind(seq(from=min(inputs$dates),by=1,length.out=nrow(outputs_sim)),outputs_sim))
names(outputs_sim)<-c("dates",paste0("rep",1:nreps))
outputs_sim<-gather(outputs_sim,"metric","value",-dates)

outputs_sim_ci<-outputs_sim %>%
  group_by(dates) %>%
  filter(substr(metric,1,3)=="rep") %>%
  summarise(mean=mean(value),median=median(value),
            q30=quantile(value,0.3),
            q15=quantile(value,0.15),
            q05=quantile(value,0.05),
            q025=quantile(value,0.025),
            q01=quantile(value,0.01),
            q70=quantile(value,0.7),
            q85=quantile(value,0.85),
            q95=quantile(value,0.95),
            q975=quantile(value,0.975),
            q99=quantile(value,0.99)) %>%
  gather("metric","value",-dates)
outputs<-rbind(outputs_sim,outputs_sim_ci)


####################################################################################################################################################################

plot1<-inputs %>%
  spread(metric,value) %>%
  ggplot(aes(x=dates)) +
  labs(title="Inputs: Cases requiring hospitalisation (per day)") +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank()) +
  scale_x_date(date_breaks="months",date_labels="%b-%y",limits=c(min(outputs$dates),max(outputs$dates)))
if (uncert==TRUE) {
  plot1<-plot1 +
  geom_ribbon(aes(ymin=q01,ymax=q025),fill="grey") +
  geom_ribbon(aes(ymin=q025,ymax=q05),fill="dodgerblue1") +
  geom_ribbon(aes(ymin=q05,ymax=q15),fill="dodgerblue2") +
  geom_ribbon(aes(ymin=q15,ymax=q30),fill="dodgerblue3") +
  geom_ribbon(aes(ymin=q30,ymax=q70),fill="dodgerblue4") +
  geom_ribbon(aes(ymin=q70,ymax=q85),fill="dodgerblue3") +
  geom_ribbon(aes(ymin=q85,ymax=q95),fill="dodgerblue2") +
  geom_ribbon(aes(ymin=q95,ymax=q975),fill="dodgerblue1") +
  geom_ribbon(aes(ymin=q975,ymax=q99),fill="grey")
}
plot1<-plot1 +
  geom_line(aes(y=hospitalisations))


plot2<-outputs %>%
  spread(metric,value) %>%
  ggplot(aes(x=dates)) +
  geom_ribbon(aes(ymin=q01,ymax=q025),fill="yellow1") +
  geom_ribbon(aes(ymin=q025,ymax=q05),fill="orange2") +
  geom_ribbon(aes(ymin=q05,ymax=q15),fill="orangered3") +
  geom_ribbon(aes(ymin=q15,ymax=q30),fill="red3") +
  geom_ribbon(aes(ymin=q30,ymax=q70),fill="red2") +
  geom_ribbon(aes(ymin=q70,ymax=q85),fill="red3") +
  geom_ribbon(aes(ymin=q85,ymax=q95),fill="orangered3") +
  geom_ribbon(aes(ymin=q95,ymax=q975),fill="orange2") +
  geom_ribbon(aes(ymin=q975,ymax=q99),fill="yellow1") +
  geom_line(aes(y=median)) +
  labs(title="Outputs: Bed requirement") +
  theme(axis.title.x=element_blank(),axis.title.y=element_blank()) +
  scale_x_date(date_breaks="months",date_labels="%b-%y")


filename_ext<-paste0("_los_med=",los_median,"_los_q95th=",los_95,"_tol=",tol,"_nreps=",nreps)

pdf(paste0("output_plot",filename_ext,".pdf"),height=8,width=9)
grid.arrange(plot1,plot2,nrow=2)
dev.off()

png(paste0("output_plot",filename_ext,".png"),height=8,width=9,units="in",res=800)
grid.arrange(plot1,plot2,nrow=2)
dev.off()

inputs$type<-"inputs"
outputs$type<-"outputs"
combined<-rbind(inputs,outputs)

write.csv(combined,paste0("output_data",filename_ext,".csv"),row.names=FALSE)

peak_summary<-outputs %>%
  filter(type=="outputs") %>%
  spread(metric,value) %>%
  slice(which.max(median))

print(paste("Peak occurs at:",peak_summary$dates),quote=FALSE)
print(paste("Beds required at peak (median):",round(peak_summary$median,0)),quote=FALSE)
print(paste("Beds required at peak (lower 95% CI):",round(peak_summary$q025,0)),quote=FALSE)
print(paste("Beds required at peak (upper 95% CI):",round(peak_summary$q975,0)),quote=FALSE)
print(paste("Outputs stored in:",getwd()),quote=FALSE)
    
return(combined)

}



