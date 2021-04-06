library(readr)

#read file csv
pelanggan <- read.csv('Customer_segments.txt', sep="\t")

#ubah menjadi matrix
pelanggan_matrix <- data.matrix(pelanggan[c("Jenis.Kelamin", "Profesi", "Tipe.Residen")])

#Penggabungan data
pelanggan <- data.frame(pelanggan, pelanggan_matrix)

#Normalisasi Nilai
pelanggan$NilaiBelanjaSetahun <- pelanggan$NilaiBelanjaSetahun/1000000





