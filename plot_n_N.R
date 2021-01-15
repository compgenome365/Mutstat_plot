#### example data
#### This actually would come from 
#### simulation files
N = rpois(3000, 400)
n = rbinom(3000, 400, 0.3 )
df1 = data.frame(N = N, n = n)

N = rpois(1000, 400)
n = rbinom(1000, 400, 0.3 )
df2 = data.frame(N = N, n = n)

N = rpois(2000, 400)
n = rbinom(2000, 400, 0.3 )
df3 = data.frame(N = N, n = n)

N = rpois(3000, 400)
n = rbinom(3000, 400, 0.3 )
df4 = data.frame(N = N, n = n)

N = rpois(1000, 400)
n = rbinom(1000, 400, 0.3 )
df5 = data.frame(N = N, n = n)

N = rpois(4000, 400)
n = rbinom(4000, 400, 0.3 )
df6 = data.frame(N = N, n = n)

################################
### I think original plots were 
### generated from my shiny app
### That's why low resolution

### prepare the data
### you need a list of data frames
### see example above


### original data
### downloaded from https://dcc.icgc.org/releases/PCAWG/consensus_snv_indel
sample_str <- c("a284fe9d-9afd-4472-8d70-243ca9be5c2c.consensus.20160830.somatic.snv_mnv.vcf",
                "3e012b50-06d1-4120-971b-5e54139b00ee.consensus.20160830.somatic.snv_mnv.vcf",
                "63fef3fe-c622-11e3-bf01-24c6515278c0.consensus.20160830.somatic.snv_mnv.vcf")
df <- c()
for(i in 1:3){
  
  fName = sprintf("C:\\Users\\Subhajit.Sengupta\\Desktop\\data_snv\\snv_mnv\\%s", sample_str[i])
  df0 <- fread(fName)
  n_alt = as.integer(sapply(df0$INFO,function(x){str_split(str_split(x,"t_alt_count")[[1]][2],"[=;]")[[1]][2]}))
  n_ref = as.integer(sapply(df0$INFO,function(x){str_split(str_split(x,"t_alt_count")[[1]][2],"[=;]")[[1]][4]}))
  k1 = which(is.na(n_alt))
  k2 = which(is.na(n_ref))
  k = unique(c(k1,k2))
  n_alt = n_alt[-k]
  n_ref = n_ref[-k]
  tmp = data.frame(n=n_alt, N = n_alt+n_ref)
  df[[i]] <- tmp
}

saveRDS(df,file="C:/Users/Subhajit.Sengupta/Desktop/df.rds")
#df = list(df1,df2,df3,df4,df5,df6)

library(ggplot2)
library(gridExtra)

plot_sctter_n_N <- function(df){

  K = length(df)
  plist <- c()
  for(i in 1:K){
    plist[[i]] <- ggplot(df[[i]], aes(x = n/N, y = N))+ 
                    geom_point()
  }
  
  #do.call("grid.arrange", c(plist, ncol=2))
  ggsave(file = "C:/Users/Subhajit.Sengupta/Desktop/plot_real_3.pdf", arrangeGrob(grobs = plist, ncol = 3),
         height = 8, width = 12,dpi = 200)
}

