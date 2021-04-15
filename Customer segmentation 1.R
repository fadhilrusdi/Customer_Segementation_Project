library(readr)
library(ggplot2)

#Data Preparation

#read file csv
pelanggan <- read.csv('Customer_segments.txt', sep="\t")

#ubah menjadi matrix
pelanggan_matrix <- data.matrix(pelanggan[c("Jenis.Kelamin", "Profesi", "Tipe.Residen")])

#Penggabungan data
pelanggan <- data.frame(pelanggan, pelanggan_matrix)

#Normalisasi Nilai
pelanggan$NilaiBelanjaSetahun <- pelanggan$NilaiBelanjaSetahun/1000000

#Membuat Data Master
Profesi <- unique(pelanggan[c("Profesi","Profesi.1")])
Jenis.Kelamin <- unique(pelanggan[c("Jenis.Kelamin","Jenis.Kelamin.1")])
Tipe.Residen <- unique(pelanggan[c("Tipe.Residen","Tipe.Residen.1")])
field_yang_digunakan = c("Jenis.Kelamin.1", "Umur", "Profesi.1", "Tipe.Residen.1","NilaiBelanjaSetahun")

#Part K-means

set.seed(100)

#fungsi kmeans untuk membentuk 5 cluster dengan 25 skenario random dan simpan ke dalam variable segmentasi
segmentasi <- kmeans(x=pelanggan[field_yang_digunakan], centers=5, nstart=25)

#analisa clustering vector
pelanggan$cluster <- segmentasi$cluster

#analisa Analisa Hasil Cluster
segmentasi

#sse
sse <- sapply(1:10,function(param_k){kmeans(pelanggan[field_yang_digunakan], 
                                            param_k, nstart=25)$tot.withinss})

#Grafik Elbow Effect
jumlah_cluster_max <- 10
ssdata = data.frame(cluster=c(1:jumlah_cluster_max),sse)
ggplot(ssdata, aes(x=cluster,y=sse)) +
  geom_line(color="red") + geom_point() +
  ylab("Within Cluster Sum of Squares") + xlab("Jumlah Cluster") +
  geom_text(aes(label=format(round(sse, 2), nsmall = 2)),hjust=-0.2, vjust=-0.5) +
  scale_x_discrete(limits=c(1:jumlah_cluster_max))

#menamai cluster
#Lengkapi dengan dua vector bernama cluster dan Nama.Segmen

Segmen.Pelanggan <- data.frame(cluster=c(1,2,3,4,5), 
                               Nama.Segmen=c("Silver Youth Gals",
                               "Diamond Senior Member", "Gold Young Professional", 
                               "Diamond Professional", "Silver Mid Professional"))

#Menggabungkan seluruh aset ke dalam variable Identitas.Cluster
Identitas.Cluster <- list(Profesi=Profesi, Jenis.Kelamin=Jenis.Kelamin, 
                          Tipe.Residen=Tipe.Residen, Segmentasi=segmentasi, 
                          Segmen.Pelanggan=Segmen.Pelanggan, 
                          field_yang_digunakan=field_yang_digunakan)

saveRDS(Identitas.Cluster,"cluster.rds")

#data baru
databaru <- data.frame(Customer_ID="CUST-100", 
                       Nama.Pelanggan="Rudi Wilamar",
                       Umur=20,Jenis.Kelamin="Wanita",
                       Profesi="Pelajar",
                       Tipe.Residen="Cluster",
                       NilaiBelanjaSetahun=3.5)

Identitas.Cluster <- readRDS(file="cluster.rds")

databaru <- merge(databaru, Identitas.Cluster$Profesi)
databaru <- merge(databaru, Identitas.Cluster$Jenis.Kelamin)
databaru <- merge(databaru, Identitas.Cluster$Tipe.Residen)

#menentukan data baru di cluster mana

which.min(sapply( 1:5, function( x ) sum( ( 
  databaru[Identitas.Cluster$field_yang_digunakan] - 
    Identitas.Cluster$Segmentasi$centers[x,])^2 ) ))
Identitas.Cluster$Segmen.Pelanggan[which.min(sapply( 1:5, function( x ) 
  sum( ( databaru
         [Identitas.Cluster$field_yang_digunakan] - 
           Identitas.Cluster$Segmentasi$centers[x,])^2 
  ) )),]

