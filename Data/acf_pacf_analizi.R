# ============================================================
#  ACF ve PACF Analizi - Ankara Su Verileri
#  Tüm veri setleri için ACF ve PACF plotları
# ============================================================

# Gerekli kütüphaneler
library(readxl)
library(dplyr)
library(tidyr)

# Grafikleri tek bir PDF dosyasına kaydet (isteğe bağlı)
# pdf("acf_pacf_grafikleri.pdf", width = 10, height = 7)

# Renk ve görsel ayarlar
old_par <- par(no.readonly = TRUE)  # Mevcut ayarları sakla


# ============================================================
# 1. BARAJ SU VERİSİ  (Aylık - En Uzun Zaman Serisi)
# ============================================================
cat("=== 1. Baraj'a Gelen Su Miktarı (Aylık) ===\n")

baraj_raw <- read_excel("baraj_su.xlsx", skip = 1)
colnames(baraj_raw) <- c("YIL", "OCAK", "SUBAT", "MART", "NISAN",
                          "MAYIS", "HAZIRAN", "TEMMUZ", "AGUSTOS",
                          "EYLUL", "EKIM", "KASIM", "ARALIK", "TOPLAM")

baraj_data <- baraj_raw %>%
  select(-TOPLAM) %>%
  filter(!is.na(YIL), is.numeric(YIL) | is.integer(YIL))

# Geniş → uzun formata çevir ve tarihe göre sırala
ay_sirasi <- c("OCAK", "SUBAT", "MART", "NISAN", "MAYIS", "HAZIRAN",
               "TEMMUZ", "AGUSTOS", "EYLUL", "EKIM", "KASIM", "ARALIK")

baraj_long <- baraj_data %>%
  pivot_longer(cols = all_of(ay_sirasi), names_to = "AY", values_to = "SU") %>%
  mutate(AY_NO = match(AY, ay_sirasi)) %>%
  arrange(YIL, AY_NO) %>%
  filter(!is.na(SU))

# ts nesnesi: yıllık frekans 12 (aylık)
baraj_ts <- ts(baraj_long$SU,
               start = c(min(baraj_long$YIL), 1),
               frequency = 12)

cat(sprintf("  Gözlem sayısı: %d (%.0f yıl × 12 ay)\n",
            length(baraj_ts), length(baraj_ts) / 12))

par(mfrow = c(2, 1), mar = c(4, 4.5, 3.5, 2))
acf(baraj_ts,
    lag.max   = 48,
    main      = "Barajlara Gelen Aylık Su Miktarı — ACF",
    xlab      = "Gecikme (ay)",
    ylab      = "Otokorelasyon",
    col       = "steelblue",
    lwd       = 2)

pacf(baraj_ts,
     lag.max  = 48,
     main     = "Barajlara Gelen Aylık Su Miktarı — PACF",
     xlab     = "Gecikme (ay)",
     ylab     = "Kısmi Otokorelasyon",
     col      = "tomato3",
     lwd      = 2)


# ============================================================
# 2. YAĞIŞ VERİSİ  (Günlük)
# ============================================================
cat("\n=== 2. Yağış Verisi (Günlük) ===\n")

yagis_raw <- read_excel("yagis.xlsx")
colnames(yagis_raw)[1:5] <- c("tarih", "tavg", "tmin", "tmax", "yagis_mm")

# Eksik yağış değerlerini 0 say (kurak gün)
yagis_raw$yagis_mm[is.na(yagis_raw$yagis_mm)] <- 0

cat(sprintf("  Gözlem sayısı: %d günlük kayıt\n", nrow(yagis_raw)))

# -- 2a. Günlük Yağış --
par(mfrow = c(2, 1), mar = c(4, 4.5, 3.5, 2))
acf(yagis_raw$yagis_mm,
    lag.max = 60,
    main    = "Günlük Yağış Miktarı (mm) — ACF",
    xlab    = "Gecikme (gün)",
    ylab    = "Otokorelasyon",
    col     = "steelblue",
    lwd     = 2)

pacf(yagis_raw$yagis_mm,
     lag.max = 60,
     main    = "Günlük Yağış Miktarı (mm) — PACF",
     xlab    = "Gecikme (gün)",
     ylab    = "Kısmi Otokorelasyon",
     col     = "tomato3",
     lwd     = 2)

# -- 2b. Günlük Ortalama Sıcaklık --
par(mfrow = c(2, 1), mar = c(4, 4.5, 3.5, 2))
acf(yagis_raw$tavg,
    lag.max   = 60,
    main      = "Günlük Ortalama Sıcaklık (°C) — ACF",
    xlab      = "Gecikme (gün)",
    ylab      = "Otokorelasyon",
    col       = "steelblue",
    lwd       = 2,
    na.action = na.pass)

pacf(yagis_raw$tavg,
     lag.max   = 60,
     main      = "Günlük Ortalama Sıcaklık (°C) — PACF",
     xlab      = "Gecikme (gün)",
     ylab      = "Kısmi Otokorelasyon",
     col       = "tomato3",
     lwd       = 2,
     na.action = na.pass)


# ============================================================
# 3. DAĞITILAN ve ÇEKİLEN SU  (2 Yılda Bir, 2004–2022)
# ============================================================
cat("\n=== 3. Dağıtılan ve Çekilen Su (2 Yılda Bir) ===\n")
cat("  NOT: Yalnızca 10 gözlem — lag sınırı kısa tutulmuştur.\n")

dagcek <- data.frame(
  yil       = c(2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018, 2020, 2022),
  dagitilan = c(216483830, 219725065, 195552342, 214922886, 235359903,
                252431487, 266848317, 285242245, 316548695, 321945098),
  cekilen   = c(344572, 379122, 333466, 338112, 385673, 393174,
                443129, 475199, 502458, 505696),
  kisi_basi = c(246, 241, 210, 202, 217, 211, 227, 239, 246, 242)
)

# -- 3a. Dağıtılan Su --
par(mfrow = c(2, 1), mar = c(4, 4.5, 3.5, 2))
acf(dagcek$dagitilan,
    lag.max = 5,
    main    = "Dağıtılan Su Miktarı (m³/Yıl) — ACF",
    xlab    = "Gecikme",
    ylab    = "Otokorelasyon",
    col     = "steelblue",
    lwd     = 2)

pacf(dagcek$dagitilan,
     lag.max = 5,
     main    = "Dağıtılan Su Miktarı (m³/Yıl) — PACF",
     xlab    = "Gecikme",
     ylab    = "Kısmi Otokorelasyon",
     col     = "tomato3",
     lwd     = 2)

# -- 3b. Çekilen Su --
par(mfrow = c(2, 1), mar = c(4, 4.5, 3.5, 2))
acf(dagcek$cekilen,
    lag.max = 5,
    main    = "Çekilen Toplam Su Miktarı (Bin m³/Yıl) — ACF",
    xlab    = "Gecikme",
    ylab    = "Otokorelasyon",
    col     = "steelblue",
    lwd     = 2)

pacf(dagcek$cekilen,
     lag.max = 5,
     main    = "Çekilen Toplam Su Miktarı (Bin m³/Yıl) — PACF",
     xlab    = "Gecikme",
     ylab    = "Kısmi Otokorelasyon",
     col     = "tomato3",
     lwd     = 2)

# -- 3c. Kişi Başı Günlük Su --
par(mfrow = c(2, 1), mar = c(4, 4.5, 3.5, 2))
acf(dagcek$kisi_basi,
    lag.max = 5,
    main    = "Kişi Başı Çekilen Günlük Su (L/Kişi-Gün) — ACF",
    xlab    = "Gecikme",
    ylab    = "Otokorelasyon",
    col     = "steelblue",
    lwd     = 2)

pacf(dagcek$kisi_basi,
     lag.max = 5,
     main    = "Kişi Başı Çekilen Günlük Su (L/Kişi-Gün) — PACF",
     xlab    = "Gecikme",
     ylab    = "Kısmi Otokorelasyon",
     col     = "tomato3",
     lwd     = 2)


# ============================================================
# 4. SULAR NEREDEN GELİYOR  (2001–2022, Yıllık / 2 Yılda Bir)
# ============================================================
cat("\n=== 4. Su Kaynakları (Baraj, Kaynak, Kuyu) ===\n")

kaynaklar <- data.frame(
  yil    = c(2001, 2002, 2003, 2004, 2006, 2008, 2010, 2012, 2014, 2016, 2018, 2020, 2022),
  baraj  = c(277668, 288172, 295570, 310690, 340215, 303554, 315021,
             362566, 365789, 422551, 465052, 491093, 497980),
  kaynak = c(16685, 14228, 13160, 13068, 14744, 16750, 13406,
             3402, 3550, 3500, NA, NA, NA),
  kuyu   = c(22240, 24090, 23385, 20778, 20817, 13100, 9544,
             19494, 23834, 17078, 10147, 11366, 7716)
)

# -- 4a. Barajdan Çekilen Su --
par(mfrow = c(2, 1), mar = c(4, 4.5, 3.5, 2))
acf(kaynaklar$baraj,
    lag.max = 6,
    main    = "Barajdan Çekilen Su (Bin m³/Yıl) — ACF",
    xlab    = "Gecikme",
    ylab    = "Otokorelasyon",
    col     = "steelblue",
    lwd     = 2)

pacf(kaynaklar$baraj,
     lag.max = 6,
     main    = "Barajdan Çekilen Su (Bin m³/Yıl) — PACF",
     xlab    = "Gecikme",
     ylab    = "Kısmi Otokorelasyon",
     col     = "tomato3",
     lwd     = 2)

# -- 4b. Kuyudan Çekilen Su --
par(mfrow = c(2, 1), mar = c(4, 4.5, 3.5, 2))
acf(kaynaklar$kuyu,
    lag.max = 6,
    main    = "Kuyudan Çekilen Su (Bin m³/Yıl) — ACF",
    xlab    = "Gecikme",
    ylab    = "Otokorelasyon",
    col     = "steelblue",
    lwd     = 2)

pacf(kaynaklar$kuyu,
     lag.max = 6,
     main    = "Kuyudan Çekilen Su (Bin m³/Yıl) — PACF",
     xlab    = "Gecikme",
     ylab    = "Kısmi Otokorelasyon",
     col     = "tomato3",
     lwd     = 2)


# ============================================================
# 5. ABONE ve NÜFUS VERİSİ  (Yıllık, 2007–2025)
# ============================================================
cat("\n=== 5. Ankara Nüfusu ve Abone Sayısı (Yıllık) ===\n")

nufus_data <- data.frame(
  yil    = 2007:2025,
  nufus  = c(4466756, 4548939, 4650802, 4771716, 4890893, 4965542,
             5045083, 5150072, 5270575, 5346518, 5445026, 5503985,
             5639076, 5663322, 5747325, 5782285, 5803482, 5864049, 5910320),
  abone  = c(NA, 1599253, NA, 1834043, NA, 1861686, NA, 2206710,
             NA, 2177525, NA, 2300689, NA, 2379626, NA, 2530986,
             NA, NA, NA)
)

# -- 5a. Nüfus --
par(mfrow = c(2, 1), mar = c(4, 4.5, 3.5, 2))
acf(nufus_data$nufus,
    lag.max = 10,
    main    = "Ankara Nüfusu — ACF",
    xlab    = "Gecikme (yıl)",
    ylab    = "Otokorelasyon",
    col     = "steelblue",
    lwd     = 2)

pacf(nufus_data$nufus,
     lag.max = 10,
     main    = "Ankara Nüfusu — PACF",
     xlab    = "Gecikme (yıl)",
     ylab    = "Kısmi Otokorelasyon",
     col     = "tomato3",
     lwd     = 2)

# -- 5b. Abone Sayısı (sadece gözlemlenen yıllar) --
abone_obs <- nufus_data %>% filter(!is.na(abone))
cat(sprintf("  Abone verisi gözlem sayısı: %d\n", nrow(abone_obs)))

par(mfrow = c(2, 1), mar = c(4, 4.5, 3.5, 2))
acf(abone_obs$abone,
    lag.max = 4,
    main    = "Dağıtılan Suyun Abone Sayısı — ACF",
    xlab    = "Gecikme",
    ylab    = "Otokorelasyon",
    col     = "steelblue",
    lwd     = 2)

pacf(abone_obs$abone,
     lag.max = 4,
     main    = "Dağıtılan Suyun Abone Sayısı — PACF",
     xlab    = "Gecikme",
     ylab    = "Kısmi Otokorelasyon",
     col     = "tomato3",
     lwd     = 2)


# ============================================================
# Orijinal grafik parametrelerini geri yükle
# ============================================================
par(old_par)

# Grafikleri PDF'e kaydetmek için:
# pdf("acf_pacf_grafikleri.pdf", width = 10, height = 7) kodunu
# scriptin başında açıp, son satıra dev.off() ekleyebilirsiniz.

cat("\n✓ Tüm ACF ve PACF grafikleri başarıyla oluşturuldu.\n")
