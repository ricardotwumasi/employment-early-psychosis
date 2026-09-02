## Supported Employment Meta-Analysis #####

# http://www.metafor-project.org/doku.php/tips:assembling_data_smd
# m1i<- mean, sd1i<- sd, n1i<- num subject
# m2i<- mean, sd2i<- sd, n2i<- num subject

# Load libraries #######
library(ggplot2) 
library(metafor)
library(stargazer)
library(ConfoundedMeta)

# Extract authors for figures
setAuthors = function(df){
  df$authors<- as.vector(sapply(strsplit(df$article,','),function(x) x[[1]][1]))
  df$authors<- paste(df$authors,' ',df$year)
  df$authors<- as.factor(df$authors)
  return(df)
}

### Competitive Employment ANY ####
# Intent-to-Treat analysis
# using the total numbers randomized for this analysis
df<- readRDS('competitive_employment_any.rds')

df<- subset(df, df$ips_any>0)
df$country<- as.factor(df$country)
df<- setAuthors(df)
df<- df[order(df$authors),]
# compute rates for table
df$ips_any_rate<- df$ips_any / df$n_ips_randomized
df$tau_any_rate<- df$tau_any / df$n_tau_randomized

res <- rma(ai=ips_any, bi=n_ips_randomized,
           ci=tau_any, di=n_tau_randomized,
#          mods = ~ df$length,
           data=df, measure="RR",
           slab=authors, method="REML")
summary(res)
exp(c(res$b, res$ci.lb, res$ci.ub)) #convert from logRR to RR for paper

par(mar=c(4,4,1,2)) # #decrease margins so the full space is used
forest(res, xlim=c(-6, 8), #atransf=exp, #at=log(c(0.05, 0.25, 1, 76)),
       cex=0.75, ylim=c(-2, 30),
       xlab="Log Risk Ratio", mlab="", psize=1)
text(-6, -1, pos=4, cex=0.75, bquote("RE Model for All Studies"))
text(-6, -2, pos=4, cex=0.75, bquote(paste("(Q = ",
                                            .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                            ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                            .(formatC(res$I2, digits=1, format="f")), "%)")))
text(-6, 29, "Study", pos=4)
text(8, 29, "Log RR [95% CI]", pos=2)
text(0, 30, c("CE At Any Time"))

#proportion analysis
stronger_than( .q= log(1.2),
                 .yr  = as.numeric(res$b), 
                 .vyr = as.numeric(res$vb),
                 .t2  = res$tau2,
                 .vt2 = res$se.tau2^2,
                 CI.level = 0.95, .tail = "above")

stronger_than( .q= log(0.8),
                 .yr  = as.numeric(res$b), 
                 .vyr = as.numeric(res$vb),
                 .t2  = res$tau2,
                 .vt2 = res$se.tau2^2,
                 CI.level = 0.95, .tail = "below")



### Competitive Employment AT END OF STUDY ####
df<- readRDS('competitive_employment_end.rds')
df<- setAuthors(df)
df<- subset(df, df$ips_end>0)
df<- df[order(df$authors),]

res <- rma(ai=ips_end, bi=n_ips_analyzed,
           ci=tau_end, di=n_tau_analyzed,
#          mods = ~ df$length,
           data=df, measure="RR",
           slab=authors, method="REML")
summary(res)
exp(c(res$b, res$ci.lb, res$ci.ub)) #convert from logRR to RR for paper

par(mar=c(4,4,1,2)) # #decrease margins so the full space is used
forest(res, xlim=c(-6,10), 
       cex=0.75, ylim=c(-2, 23),
       xlab="Risk Ratio", mlab="", psize=1)
text(-6, -1, pos=4, cex=0.75, bquote("RE Model for All Studies"))
text(-6, -2, pos=4, cex=0.75, bquote(paste("(Q = ",
                                            .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                            ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                            .(formatC(res$I2, digits=1, format="f")), "%)")))
text(-6, 22, "Study", pos=4)
text(8, 22, "Log RR [95% CI]", pos=2)
text(0, 23, c("CE At End"))

#proportion analysis
stronger_than( .q= log(1.2),
                 .yr  = as.numeric(res$b), 
                 .vyr = as.numeric(res$vb),
                 .t2  = res$tau2,
                 .vt2 = res$se.tau2^2,
                 CI.level = 0.95, .tail = "above")
stronger_than( .q= log(0.8),
                 .yr  = as.numeric(res$b), 
                 .vyr = as.numeric(res$vb),
                 .t2  = res$tau2,
                 .vt2 = res$se.tau2^2,
                 CI.level = 0.95, .tail = "below")

### Time to First Competitive Employment Job ####
df<- readRDS('time_to_first_ce.rds')
df<- setAuthors(df) 
df<- df[order(df$authors),]
x<- escalc(measure="SMD", 
           m1i= df$ips_mean,
           sd1i=df$ips_sd,
           n1i= df$ips_n,
           m2i= df$tau_mean,
           sd2i=df$tau_sd,
           n2i= df$tau_n)
res <- rma(yi, vi, slab=df$authors, 
#          mods = ~ df$length,
           data=x)
summary(res)

par(mar=c(4,4,1,2)) 
forest(res, xlim=c(-6, 8), 
       cex=0.75, ylim=c(-2, 23),
       xlab="SMD", mlab="", psize=1)
text(-6, -1, pos=4, cex=0.75, bquote("RE Model for All Studies"))
text(-6, -2, pos=4, cex=0.75, bquote(paste("(Q = ",
                                            .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                            ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                            .(formatC(res$I2, digits=1, format="f")), "%)")))
text(-6, 22, "Study", pos=4)
text(8, 22, "SMD [95% CI]", pos=2)
text(0, 23, c("Time to First CE Job"))

# NB: can't do proportion analysis because of lack of heterogeneity

### Job Tenure for CE #####
df<- readRDS('job_tenure.rds')
df<- setAuthors(df)
df<- df[order(df$authors),]
x<- escalc(measure="SMD", 
           m1i= df$ips_mean,
           sd1i=df$ips_sd,
           n1i= df$ips_n,
           m2i= df$tau_mean,
           sd2i=df$tau_sd,
           n2i= df$tau_n)

res <- rma(yi, vi, 
#          mods = ~ df$length,
           slab=df$authors, data=x)
summary(res)

par(mar=c(4,4,1,2)) 
forest(res, xlim=c(-6, 8),
       cex=0.75, ylim=c(-2, 23),
       xlab="SMD", mlab="", psize=1)
text(-6, -1, pos=4, cex=0.75, bquote("RE Model for All Studies"))
text(-6, -2, pos=4, cex=0.75, bquote(paste("(Q = ",
                                            .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                            ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                            .(formatC(res$I2, digits=1, format="f")), "%)")))
text(-6, 22, "Study", pos=4)
text(8, 22, "SMD [95% CI]", pos=2)
text(0, 23, c("Job Tenure"))


#proportion analysis
stronger_than( .q= 0.2,
                 .yr  = as.numeric(res$b), 
                 .vyr = as.numeric(res$vb),
                 .t2  = res$tau2,
                 .vt2 = res$se.tau2^2,
                 CI.level = 0.95, .tail = "above");

stronger_than( .q=-0.2,
                 .yr  = as.numeric(res$b), 
                 .vyr = as.numeric(res$vb),
                 .t2  = res$tau2,
                 .vt2 = res$se.tau2^2,
                 CI.level = 0.95, .tail = "below");

# Job Length - Total time spent working in CE jobs ####
df<- readRDS('job_length.rds')
df<- setAuthors(df)
df<- df[order(df$authors),]
x<- escalc(measure="SMD", 
           m1i= df$ips_mean,
           sd1i=df$ips_sd,
           n1i= df$ips_n,
           m2i= df$tau_mean,
           sd2i=df$tau_sd,
           n2i= df$tau_n)
res <- rma(yi, vi, 
#          mods= ~ df$length,
           slab=df$authors, data=x)
summary(res)

par(mar=c(4,4,1,2)) 
forest(res, xlim=c(-6,13),
       cex=0.75, ylim=c(-2, 23),
       xlab="SMD", mlab="", psize=1)
text(-6, -1, pos=4, cex=0.75, bquote("RE Model for All Studies"))
text(-6, -2, pos=4, cex=0.75, bquote(paste("(Q = ",
                                            .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                            ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                            .(formatC(res$I2, digits=1, format="f")), "%)")))
text(-6, 22, "Study", pos=4)
text(12, 22, "SMD [95% CI]", pos=2)
text(0, 23, c("Job Length"))

#proportion analysis
stronger_than( .q= 0.2,
                 .yr  = as.numeric(res$b), 
                 .vyr = as.numeric(res$vb),
                 .t2  = res$tau2,
                 .vt2 = res$se.tau2^2,
                 CI.level = 0.95, .tail = "above");

stronger_than( .q=-0.2,
                 .yr  = as.numeric(res$b), 
                 .vyr = as.numeric(res$vb),
                 .t2  = res$tau2,
                 .vt2 = res$se.tau2^2,
                 CI.level = 0.95, .tail = "below");

### Competitive Employment Income ####
df<- readRDS('income.rds')
df<- setAuthors(df)
df<- df[order(df$authors),]
x<- escalc(measure="SMD", 
           m1i= df$ips_mean,
           sd1i=df$ips_sd,
           n1i= df$ips_n,
           m2i= df$tau_mean,
           sd2i=df$tau_sd,
           n2i= df$tau_n)
res <- rma(yi, vi, 
#          mods= ~ df$length,
           slab=df$authors, data=x)
summary(res)

par(mar=c(4,4,1,2)) 
forest(res, xlim=c(-6,14), 
       cex=0.75, ylim=c(-2, 23),
       xlab="SMD", mlab="", psize=1)
text(-6, -1, pos=4, cex=0.75, bquote("RE Model for All Studies"))
text(-6, -2, pos=4, cex=0.75, bquote(paste("(Q = ",
                                            .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                            ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                            .(formatC(res$I2, digits=1, format="f")), "%)")))
text(-6, 22, "Study", pos=4)
text(12, 22, "SMD [95% CI]", pos=2)
text(0, 23, c("CE Income"))

stronger_than( .q= 0.20,
                 .yr  = as.numeric(res$b), 
                 .vyr = as.numeric(res$vb),
                 .t2  = res$tau2,
                 .vt2 = res$se.tau2^2,
                 CI.level = 0.95, .tail = "above");

stronger_than( .q=-0.20,
                 .yr  = as.numeric(res$b), 
                 .vyr = as.numeric(res$vb),
                 .t2  = res$tau2,
                 .vt2 = res$se.tau2^2,
                 CI.level = 0.95, .tail = "below");


### Quality of Life ####
df<- readRDS('quality_of_life.rds')
df<- setAuthors(df)
df<- df[order(df$authors),]
x<- escalc(measure="SMD", 
           m1i= df$ips_mean,
           sd1i=df$ips_sd,
           n1i= df$ips_n,
           m2i= df$tau_mean,
           sd2i=df$tau_sd,
           n2i= df$tau_n)
res <- rma(yi, vi, 
           slab=df$authors, 
#           mods = ~ df$length,
           data=x)
summary(res)

par(mar=c(4,4,1,2)) 
forest(res, xlim=c(-6, 8),
       cex=0.75, ylim=c(-2, 23),
       xlab="SMD", mlab="", psize=1)
text(-6, -1, pos=4, cex=0.75, bquote("RE Model for All Studies"))
text(-6, -2, pos=4, cex=0.75, bquote(paste("(Q = ",
                                            .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                            ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                            .(formatC(res$I2, digits=1, format="f")), "%)")))
text(-6, 22, "Study", pos=4)
text(8, 22, "SMD [95% CI]", pos=2)
text(0, 23, c("Quality of Life"))

# proportion analysis
stronger_than( .q= 0.20,
                 .yr  = as.numeric(res$b), 
                 .vyr = as.numeric(res$vb),
                 .t2  = res$tau2,
                 .vt2 = res$se.tau2^2,
                 CI.level = 0.95, .tail = "above");

stronger_than( .q=-0.20,
                 .yr  = as.numeric(res$b), 
                 .vyr = as.numeric(res$vb),
                 .t2  = res$tau2,
                 .vt2 = res$se.tau2^2,
                 CI.level = 0.95, .tail = "below");

### Global Functioning ####
df<- readRDS('global_functioning.rds')
df<- setAuthors(df)
df<- df[order(df$authors),]
x<- escalc(measure="SMD", 
           m1i= df$ips_mean,
           sd1i=df$ips_sd,
           n1i= df$n_ips_analyzed,
           m2i= df$tau_mean,
           sd2i=df$tau_sd,
           n2i= df$n_tau_analyzed)
res <- rma(yi, vi, slab=df$authors, data=x)
res <- rma(yi, vi, 
           slab=df$authors, 
#           mods = ~ df$length,
           data=x)
summary(res)

par(mar=c(4,4,1,2)) 
forest(res, xlim=c(-6, 8), 
       cex=0.75, ylim=c(-2, 23),
       xlab="SMD", mlab="", psize=1)
text(-6, -1, pos=4, cex=0.75, bquote("RE Model for All Studies"))
text(-6, -2, pos=4, cex=0.75, bquote(paste("(Q = ",
                                            .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                            ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                            .(formatC(res$I2, digits=1, format="f")), "%)")))
text(-6, 22, "Study", pos=4)
text(8, 22, "SMD [95% CI]", pos=2)
text(0, 23, c("Global Functioning"))

stronger_than( .q= 0.2,
                 .yr  = as.numeric(res$b), 
                 .vyr = as.numeric(res$vb),
                 .t2  = res$tau2,
                 .vt2 = res$se.tau2^2,
                 CI.level = 0.95, .tail = "above");

stronger_than( .q=-0.2,
                 .yr  = as.numeric(res$b), 
                 .vyr = as.numeric(res$vb),
                 .t2  = res$tau2,
                 .vt2 = res$se.tau2^2,
                 CI.level = 0.95, .tail = "below");

## Mental Health ####
df<- readRDS('mental_health.rds')
df<- setAuthors(df)
df<- df[order(df$authors),]
x<- escalc(measure="SMD", 
           m1i= df$ips_mean,
           sd1i=df$ips_sd,
           n1i= df$n_ips_analyzed,
           m2i= df$tau_mean,
           sd2i=df$tau_sd,
           n2i= df$n_tau_analyzed)
res <- rma(yi, vi, 
           slab=df$authors, 
#           mods = ~ df$length,
           data=x)
summary(res)

par(mar=c(4,4,1,2)) 
forest(res, xlim=c(-6, 8), 
       cex=0.75, ylim=c(-2, 23),
       xlab="SMD", mlab="", psize=1)
text(-6, -1, pos=4, cex=0.75, bquote("RE Model for All Studies"))
text(-6, -2, pos=4, cex=0.75, bquote(paste("(Q = ",
                                            .(formatC(res$QE, digits=2, format="f")), ", df = ", .(res$k - res$p),
                                            ", p = ", .(formatC(res$QEp, digits=2, format="f")), "; ", I^2, " = ",
                                            .(formatC(res$I2, digits=1, format="f")), "%)")))
text(-6, 22, "Study", pos=4)
text(8, 22, "SMD [95% CI]", pos=2)
text(0, 23, c("Mental Health"))

stronger_than( .q= 0.2,
               .yr  = as.numeric(res$b), 
               .vyr = as.numeric(res$vb),
               .t2  = res$tau2,
               .vt2 = res$se.tau2^2,
               CI.level = 0.95, .tail = "above")

stronger_than( .q=-0.2,
               .yr  = as.numeric(res$b), 
               .vyr = as.numeric(res$vb),
               .t2  = res$tau2,
               .vt2 = res$se.tau2^2,
               CI.level = 0.95, .tail = "below")






