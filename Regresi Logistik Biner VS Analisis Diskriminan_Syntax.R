#Regresi Logistik Biner Dan Analisis Diskriminan#
library(caret)
library(car)
library(ResourceSelection)
library(MASS)
library(MVN)
library(dplyr)
library(heplots)
library(biotools)
##########################ANALISIS DISKRIMINA############################
#PREPROCESSING DATA#
# Membaca data dan menghapus kolom ID
data_bankloan <- read.csv("C:/Lessons/SEM 6/bankloan.csv", sep = ',')
head(data_bankloan)
data_bankloan <- data_bankloan[ , !(names(data_bankloan) %in% c("ID"))]
head(data_bankloan)
#Melihat dimensi data_bankloan awal
dimensi_data_bankloan <- dim(data_bankloan)
dimensi_data_bankloan
# Melihat struktur data_bankloan
str(data_bankloan)

#Pindahkan kolom "Personal.Loan" ke kolom terakhir
data_bankloan <- data_bankloan[, c(setdiff(names(data_bankloan), "Personal.Loan"), "Personal.Loan")]
head(data_bankloan)
# Mengubah Personal.Loan menjadi faktor
data_bankloan$Personal.Loan <- as.factor(data_bankloan$Personal.Loan)

# 1. Mengecek  dam menghapus missing values
sum(is.na(data_bankloan))
# Menghapus baris dengan missing values
data_bankloan<- na.omit(data_bankloan)

# 2. Menghitung IQR dan menghilangkan outlier untuk beberapa kolom
remove_outliers <- function(data_bankloan, columns) {
  for (col in columns) {
    Q1 <- quantile(data_bankloan[[col]], 0.25, na.rm = TRUE)
    Q3 <- quantile(data_bankloan[[col]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    data_bankloan <- data_bankloan[!(data_bankloan[[col]] < (Q1 - 1.5 * IQR) | data_bankloan[[col]] > (Q3 + 1.5 * IQR)), ]
  }
  return(data_bankloan)
}
# Daftar kolom yang ingin dihilangkan outlier
columns_to_process <- c("Age","Experience", "Income", "ZIP.Code", "Family","CCAvg", "Education", "Mortgage","Securities.Account","CD.Account","Online", "CreditCard")
# Menghapus outlier dari kolom yang dipilih
data_bankloan <- remove_outliers(data_bankloan, columns_to_process)

dimensi_data_bankloan <- dim(data_bankloan)
dimensi_data_bankloan
data_bankloan

# 3. Menghitung standar deviasi untuk setiap kolom
sd_values <- apply(data_bankloan[, 1:12], 2, sd)
sd_values
# Identifikasi kolom yang memiliki standar deviasi nol
columns_to_remove_sd <- which(sd_values == 0)

# Periksa kolom yang akan dihapus
if(length(columns_to_remove_sd) > 0) {
  print("Kolom yang akan dihapus karena std = 0:")
  print(names(data_bankloan)[columns_to_remove_sd])
} else {
  print("Tidak ada kolom dengan standar deviasi nol.")
}

# Hapus kolom yang memiliki standar deviasi nol
if(length(columns_to_remove_sd) > 0) {
  data_bankloan_clean <- data_bankloan[, -columns_to_remove_sd]
} else {
  data_bankloan_clean <- data_bankloan
}
data_bankloan_clean
dim(data_bankloan_clean)
head(data_bankloan_clean)
# 4. Menghapus Kolom Yang Sangat Berkorelasi
y=data_bankloan_clean[ , 11]
x=data_bankloan_clean[ , 1:10]
# Menghitung matriks korelasi
cor_matrix <- cor(x)
head(cor_matrix)
# Identifikasi kolom yang sangat berkorelasi (korelasi > 0.99)
columns_to_remove_cor <- findCorrelation(cor_matrix, cutoff = 0.99)
# Periksa kolom yang akan dihapus
if(length(columns_to_remove_cor) > 0) {
  print("Kolom yang akan dihapus karena korelasi > 0.99:")
  print(names(data_bankloan)[columns_to_remove_cor])
} else {
  print("Tidak ada kolom yang sangat berkorelasi")
}

# Hapus kolom yang sangat berkorelasi
if(length(columns_to_remove_cor) > 0) {
  data_bankloan_clean <- data_bankloan_clean[, -columns_to_remove_cor]
} else {
  data_bankloan_clean <- data_bankloan_clean
}
data_bankloan_clean
dim(data_bankloan_clean)

# 5.Menghapus kolom yang memiliki IQR 0
# Menghitung nilai IQR untuk setiap kolom
iqr_values <- apply(data_bankloan_clean[, 1:9], 2, IQR)
iqr_values
# Identifikasi kolom yang memiliki IQR 0
columns_to_remove_iqr <- which(iqr_values == 0)
# Periksa kolom yang akan dihapus karena IQR = 0
if(length(columns_to_remove_iqr) > 0) {
  print("Kolom yang akan dihapus karena IQR = 0:")
  print(names(data_bankloan_clean)[columns_to_remove_iqr])
} else {
  print("Tidak ada kolom dengan IQR nol.")
}

# Hapus kolom yang memiliki IQR nol jika ada
if(length(columns_to_remove_iqr) > 0) {
  data_bankloan_clean <- data_bankloan_clean[, -columns_to_remove_iqr]
}
head(data_bankloan_clean)

##########################DEFINISI TRAINING TESTING############################
#Mendefinisikan kembali data_bankloan dengan berisi variabel yang sudah di cleaning
data_bankloan<-data_bankloan_clean
set.seed(123)
#Membagi data menjadi training dan testing
indeks <- createDataPartition(data_bankloan$Personal.Loan, p = 0.7, list = FALSE)
data_training <- data_bankloan[indeks, ]
data_testing <- data_bankloan[-indeks, ]
dim(data_training)
dim(data_testing)
dim(data_bankloan)

########ANALISIS DISKRIMINAN PADA DATA BANKLOAN########

#Spesifikasi Variabel
y=data_training[ , 10]
x=data_training[ , 1:9]

###UJI ASUMSI###
#UJI NORMALITAS MULTIVARIAT
# Lakukan uji normalitas multivariat secara formal
normality_test <- mvn(x, mvnTest = "mardia")
print(normality_test)
# Menampilkan Q-Q plot multivariat untuk memeriksa normalitas multivariat
hasildata <- mvn(data = x, multivariatePlot = 'qq')

# UJI HOMOGENITAS KOVARIAN
box_m_test <- boxM(data=x, group=y)
print(box_m_test)

#UJI WILK LAMBDA
m <- manova(formula = cbind(data_training$Experience, data_training$Income, data_training$ZIP.Code,
                            data_training$Family, data_training$CCAvg, data_training$Education, data_training$Mortgage, data_training$Online,
                            data_training$CreditCard) ~ data_training$Personal.Loan)
summary(object = m, test = 'Wilks')

#UJI NON MULTIKOLINIERITAS
VIF=function(x){
  VIF=diag(solve(cor(x)))
  result=ifelse(VIF>10,"mulicolinearity", "non multicolinearity")
  data1=data.frame(VIF,result)
  return(data1)
}
VIF(x)


#Analisis Diskriminan
linearDA <- lda(formula = Personal.Loan ~., data = data_training)
linearDA

plot(linearDA, col = as.integer(data_training$Personal.Loan))

# Melakukan prediksi
predicted <- predict(object = linearDA, newdata = data_testing)
# Pastikan prediksi dilakukan pada data testing yang benar
predicted <- predict(linearDA, data_testing)

# Memastikan panjang data pengujian dan prediksi sama
print(paste("Panjang data pengujian:", nrow(data_testing)))
print(paste("Panjang prediksi:", length(predicted$class)))

# Menghitung confusion matrix jika panjang vektor sama
if (nrow(data_testing) == length(predicted$class)) {
  conf_matrix <- table(actual = data_testing$Personal.Loan, predicted = predicted$class)
  print(conf_matrix)
} else {
  stop("Panjang data pengujian dan prediksi tidak sama.")
}

# Menghitung akurasi model
accuracy <- sum(predicted$class == data_testing$Personal.Loan) / nrow(data_testing)

# Menghitung precision, recall, dan f1-score
precision <- posPredValue(conf_matrix, positive = "1")
recall <- sensitivity(conf_matrix, positive = "1")
f1 <- 2 * (precision * recall) / (precision + recall)

# Menampilkan hasil accuracy, precision, recall, dan f1-score
print(paste("Akurasi:", accuracy))
print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1-Score:", f1))




#########################################REGRESI LOGISTIK#####################################
#PREPROCESSING DATA#
# Membaca data dan menghapus kolom ID
data_bankloan <- read.csv("C:/Lessons/SEM 6/bankloan.csv", sep = ',')
head(data_bankloan)
data_bankloan <- data_bankloan[ , !(names(data_bankloan) %in% c("ID"))]
head(data_bankloan)
#Melihat dimensi data_bankloan awal
dimensi_data_bankloan <- dim(data_bankloan)
dimensi_data_bankloan
# Melihat struktur data_bankloan
str(data_bankloan)

#Pindahkan kolom "Personal.Loan" ke kolom terakhir
data_bankloan <- data_bankloan[, c(setdiff(names(data_bankloan), "Personal.Loan"), "Personal.Loan")]
head(data_bankloan)
# Mengubah Personal.Loan menjadi faktor
data_bankloan$Personal.Loan <- as.factor(data_bankloan$Personal.Loan)

# 1. Mengecek  dam menghapus missing values
sum(is.na(data_bankloan))
# Menghapus baris dengan missing values
data_bankloan<- na.omit(data_bankloan)

# 2. Menghitung IQR dan menghilangkan outlier untuk beberapa kolom
remove_outliers <- function(data_bankloan, columns) {
  for (col in columns) {
    Q1 <- quantile(data_bankloan[[col]], 0.25, na.rm = TRUE)
    Q3 <- quantile(data_bankloan[[col]], 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    data_bankloan <- data_bankloan[!(data_bankloan[[col]] < (Q1 - 1.5 * IQR) | data_bankloan[[col]] > (Q3 + 1.5 * IQR)), ]
  }
  return(data_bankloan)
}
# Daftar kolom yang ingin dihilangkan outlier
columns_to_process <- c("Age","Experience", "Income", "ZIP.Code", "Family","CCAvg", "Education", "Mortgage","Securities.Account","CD.Account","Online", "CreditCard")
# Menghapus outlier dari kolom yang dipilih
data_bankloan <- remove_outliers(data_bankloan, columns_to_process)

dimensi_data_bankloan <- dim(data_bankloan)
dimensi_data_bankloan
data_bankloan

# 3. Menghitung standar deviasi untuk setiap kolom
sd_values <- apply(data_bankloan[, 1:12], 2, sd)
sd_values
# Identifikasi kolom yang memiliki standar deviasi nol
columns_to_remove_sd <- which(sd_values == 0)

# Periksa kolom yang akan dihapus
if(length(columns_to_remove_sd) > 0) {
  print("Kolom yang akan dihapus karena std = 0:")
  print(names(data_bankloan)[columns_to_remove_sd])
} else {
  print("Tidak ada kolom dengan standar deviasi nol.")
}

# Hapus kolom yang memiliki standar deviasi nol
if(length(columns_to_remove_sd) > 0) {
  data_bankloan_clean <- data_bankloan[, -columns_to_remove_sd]
} else {
  data_bankloan_clean <- data_bankloan
}
data_bankloan_clean
dim(data_bankloan_clean)
head(data_bankloan_clean)
# 4. Menghapus Kolom Yang Sangat Berkorelasi
y=data_bankloan_clean[ , 11]
x=data_bankloan_clean[ , 1:10]
# Menghitung matriks korelasi
cor_matrix <- cor(x)
head(cor_matrix)
# Identifikasi kolom yang sangat berkorelasi (korelasi > 0.99)
columns_to_remove_cor <- findCorrelation(cor_matrix, cutoff = 0.99)
# Periksa kolom yang akan dihapus
if(length(columns_to_remove_cor) > 0) {
  print("Kolom yang akan dihapus karena korelasi > 0.99:")
  print(names(data_bankloan)[columns_to_remove_cor])
} else {
  print("Tidak ada kolom yang sangat berkorelasi")
}

# Hapus kolom yang sangat berkorelasi
if(length(columns_to_remove_cor) > 0) {
  data_bankloan_clean <- data_bankloan_clean[, -columns_to_remove_cor]
} else {
  data_bankloan_clean <- data_bankloan_clean
}
data_bankloan_clean
dim(data_bankloan_clean)

# 5.Menghapus kolom yang memiliki IQR 0
# Menghitung nilai IQR untuk setiap kolom
iqr_values <- apply(data_bankloan_clean[, 1:9], 2, IQR)
iqr_values
# Identifikasi kolom yang memiliki IQR 0
columns_to_remove_iqr <- which(iqr_values == 0)
# Periksa kolom yang akan dihapus karena IQR = 0
if(length(columns_to_remove_iqr) > 0) {
  print("Kolom yang akan dihapus karena IQR = 0:")
  print(names(data_bankloan_clean)[columns_to_remove_iqr])
} else {
  print("Tidak ada kolom dengan IQR nol.")
}

# Hapus kolom yang memiliki IQR nol jika ada
if(length(columns_to_remove_iqr) > 0) {
  data_bankloan_clean <- data_bankloan_clean[, -columns_to_remove_iqr]
}
head(data_bankloan_clean)

#########################################DEFINISI TRAINING TESTING#####################################
#Mendefinisikan kembali data_bankloan dengan berisi variabel yang sudah di cleaning
data_bankloan<-data_bankloan_clean

set.seed(123)
# Split data (70% training, 30% testing)
indeks <- createDataPartition(data_bankloan$Personal.Loan, p = 0.7, list = FALSE)
data_training <- data_bankloan[indeks, ]
data_testing <- data_bankloan[-indeks, ]

###################UJI RASIO LIKELIHOOD##################
# Bangun model lengkap
model_logistik_lengkap <- glm(Personal.Loan ~ ., data = data_training, family = binomial)

# Bangun model nol (sederhana)
model_logistik_nol <- glm(Personal.Loan ~ 1, data = data_training, family = binomial)

# Hitung nilai uji likelihood ratio
lrt_stat <- 2 * (logLik(model_logistik_lengkap) - logLik(model_logistik_nol))

# Hitung derajat kebebasan
df <- length(coef(model_logistik_lengkap)) - length(coef(model_logistik_nol))

# Hitung nilai p-value
p_value <- pchisq(lrt_stat, df, lower.tail = FALSE)

# Tampilkan hasil
cat("--------------------------Uji Rasio Likelihood----------------------------")
print(paste("Nilai uji likelihood ratio:", lrt_stat))
print(paste("Derajat kebebasan:", df))
print(paste("Nilai p-value:", p_value))

# Bandingkan dengan alpha (tingkat signifikansi)
alpha <- 0.05
if (p_value < alpha) {
  print("Tolak H0: Secara bersama-sama variabel bebas memengaruhi model")
} else {
  print("Terima H0: Secara bersama-sama variabel bebas tidak memengaruhi model")
}
cat("--------------------------------------------------------------------------")

#########################################UJI GOODNESS OF FIT#####################################
# Evaluasi Goodness of fit
predicted_values <- predict(model_logistik_lengkap, newdata = data_training, type = "response")
hoslem_test <- hoslem.test(data_training$Personal.Loan, predicted_values, g = 11)

# Tampilkan hasil
print("Uji Hosmer-Lemeshow Test:")
print(hoslem_test)

cat("--------------------------Uji Goodness of Fit----------------------------")
# Interpretasi hasil
cat("\nInterpretasi:\n")
if (hoslem_test$p.value < 0.05) {
  cat("Nilai p-value (", hoslem_test$p.value, ") < alpha (0.05).\n")
  cat("Tolak H0: Model tidak sesuai (observasi dan prediksi berbeda).\n")
} else {
  cat("Nilai p-value (", hoslem_test$p.value, ") >= alpha (0.05).\n")
  cat("Terima H0: Model sesuai (observasi dan prediksi tidak berbeda).\n")
}
cat("--------------------------------------------------------------------------")

#################################################UJI WALD#########################################
# Mengambil estimasi koefisien
coef_est <- coef(model_logistik_lengkap)

# Mengambil kuadrat standar error
std_err <- summary(model_logistik_lengkap)$coefficients[, "Std. Error"]

# Menghitung statistik uji Wald
wald_stat <- (coef_est / std_err)^2

# Menghitung nilai p-value
p_value <- pchisq(wald_stat, df = 1, lower.tail = FALSE)  # df = 1 karena satu koefisien yang diuji

cat("--------------------------------Uji Wald----------------------------------")
# Tampilkan hasil
print("Uji Wald Test:")
print("")

for (i in 1:length(coef_est)) {
  cat("Variabel:", names(coef_est)[i], "\n")
  cat("Estimasi Koefisien:", coef_est[i], "\n")
  cat("Kuadrat Standar Error:", std_err[i]^2, "\n")
  cat("Statistik uji Wald:", wald_stat[i], "\n")
  cat("Nilai p-value:", p_value[i], "\n")
  if (p_value[i] < 0.05) {
    cat("Tolak H0: Variabel bebas signifikan terhadap model\n\n")
  } else {
    cat("Terima H0: Variabel bebas tidak signifikan terhadap model\n\n")
  }
}

# Menyimpan nama variabel yang tidak signifikan
non_significant_vars <- names(coef_est)[p_value >= 0.05]

# Menampilkan variabel yang tidak signifikan
cat("Variabel yang tidak signifikan:\n")
print(non_significant_vars)





################################################# UJI RASIO LIKELIHOOD (PART 2) #########################################
# Mengambil hanya variabel yang signifikan berdasarkan uji Wald
significant_vars <- names(coef_est)[p_value < 0.05]

# Exclude the Intercept term if it's present
significant_vars <- significant_vars[significant_vars != "(Intercept)"]

# Bangun model regresi logistik hanya dengan variabel yang signifikan
# Mengonversi significant_vars menjadi formula
formula_significant <- as.formula(paste("Personal.Loan ~", paste(significant_vars, collapse = "+")))
model_logistik_significant <- glm(formula_significant, data = data_training, family = binomial)

# Bangun model nol (sederhana)
model_logistik_nol_significant <- glm(Personal.Loan ~ 1, data = data_training, family = binomial)

# Hitung nilai uji likelihood ratio
lrt_stat_significant <- 2 * (logLik(model_logistik_significant) - logLik(model_logistik_nol_significant))

# Hitung derajat kebebasan
df_significant <- length(coef(model_logistik_significant)) - length(coef(model_logistik_nol_significant))

# Hitung nilai p-value
p_value_significant <- pchisq(lrt_stat_significant, df_significant, lower.tail = FALSE)

# Tampilkan hasil uji rasio likelihood
cat("--------------------------Uji Rasio Likelihood (Variabel Signifikan)----------------------------")
print(paste("Nilai uji likelihood ratio:", lrt_stat_significant))
print(paste("Derajat kebebasan:", df_significant))
print(paste("Nilai p-value:", p_value_significant))

# Bandingkan dengan alpha (tingkat signifikansi)
alpha <- 0.05
if (p_value_significant < alpha) {
  print("Tolak H0: Secara bersama-sama variabel bebas yang signifikan memengaruhi model")
} else {
  print("Terima H0: Secara bersama-sama variabel bebas yang signifikan tidak memengaruhi model")
}
cat("-------------------------------------------------------------------------------------------------")

#################################################UJI GOODNESS OF FIT (PART 2)#########################################
# Evaluasi Goodness of fit hanya dengan variabel yang signifikan
hoslem_test_significant <- hoslem.test(data_training$Personal.Loan, fitted(model_logistik_significant), g = 7)

# Tampilkan hasil
cat("--------------------------Uji Goodness of Fit (Variabel Signifikan)----------------------------")
print("Uji Hosmer-Lemeshow Test:")
print(hoslem_test_significant)

# Interpretasi hasil
cat("\nInterpretasi:\n")
if (hoslem_test_significant$p.value < 0.05) {
  cat("Nilai p-value (", hoslem_test_significant$p.value, ") < alpha (0.05).\n")
  cat("Tolak H0: Model tidak sesuai (observasi dan prediksi berbeda).\n")
} else {
  cat("Nilai p-value (", hoslem_test_significant$p.value, ") >= alpha (0.05).\n")
  cat("Terima H0: Model sesuai (observasi dan prediksi tidak berbeda).\n")
}
cat("------------------------------------------------------------------------------------------------")

#################################################UJI WALD (PART 2)#########################################
# Mengambil estimasi koefisien hanya untuk variabel yang signifikan
coef_est_significant <- coef(model_logistik_significant)

# Mengambil kuadrat standar error hanya untuk variabel yang signifikan
std_err_significant <- summary(model_logistik_significant)$coefficients[, "Std. Error"]

# Menghitung statistik uji Wald
wald_stat_significant <- (coef_est_significant / std_err_significant)^2

# Menampilkan panjang vektor
#cat("Panjang vektor coef_est_significant:", length(coef_est_significant), "\n")
#cat("Panjang vektor std_err_significant:", length(std_err_significant), "\n")

# Memeriksa variabel yang mungkin menyebabkan perbedaan panjang
#cat("Variabel yang mungkin menyebabkan perbedaan panjang:\n")
#print(setdiff(names(model_logistik_significant$coefficients), names(coef_est_significant)))
#print(setdiff(names(model_logistik_significant$coefficients), names(std_err_significant)))


# Menghitung nilai p-value
p_value_significant <- pchisq(wald_stat_significant, df = 1, lower.tail = FALSE)  # df = 1 karena satu koefisien yang diuji

cat("--------------------------------Uji Wald (Variabel Signifikan)----------------------------------")
# Tampilkan hasil
print("Uji Wald Test:")
print("")

for (i in 1:length(coef_est_significant)) {
  cat("Variabel:", names(coef_est_significant)[i], "\n")
  cat("Estimasi Koefisien:", coef_est_significant[i], "\n")
  cat("Kuadrat Standar Error:", std_err_significant[i]^2, "\n")
  cat("Statistik uji Wald:", wald_stat_significant[i], "\n")
  cat("Nilai p-value:", p_value_significant[i], "\n")
  if (p_value_significant[i] < 0.05) {
    cat("Tolak H0: Variabel bebas signifikan terhadap model\n\n")
  } else {
    cat("Terima H0: Variabel bebas tidak signifikan terhadap model\n\n")
  }
}



#################################################MODEL AKHIRRR#########################################
# Mengonversi significant_vars menjadi formula
formula_significant <- as.formula(paste("Personal.Loan ~", paste(significant_vars, collapse = "+")))
model_logistik_significant <- glm(formula_significant, data = data_training, family = binomial)

# Evaluasi Multikolineritas
vif_values <- vif(model_logistik_significant)
print(vif_values)

# Prediksi pada data testing
predictions <- predict(model_logistik_significant, newdata = data_testing, type = "response")

# Ubah prediksi menjadi kelas biner menggunakan threshold 0.5
predicted_classes <- ifelse(predictions >= 0.5, 1, 0)

# Hitung matriks kebingungan
confusion_matrix <- table(data_testing$Personal.Loan, predicted_classes)

# Tampilkan Confusion Matrix
print("Confusion Matrix:")
print(confusion_matrix)

# Hitung nilai precision, recall, dan F1 score
TP <- confusion_matrix[2, 2]
FP <- confusion_matrix[1, 2]
FN <- confusion_matrix[2, 1]
TN <- confusion_matrix[1, 1]

precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1_score <- 2 * (precision * recall) / (precision + recall)

# Hitung akurasi dalam persentase
accuracy <- (TP + TN) / sum(confusion_matrix) * 100

# Tampilkan nilai precision, recall, F1 score, dan akurasi dalam persentase
print(paste("Precision:", precision))
print(paste("Recall:", recall))
print(paste("F1 Score:", f1_score))
print(paste("Accuracy:", accuracy, "%"))

