# ===============================
# K??T??PHANELER
# ===============================
library(readxl)
library(dplyr)
library(lubridate)

# ===============================
# SAB??TLER
# ===============================
MIN_DINLENME_SURESI <- 6   # saat
SOFOR_SAYISI <- 10
SEFER_SURESI <- 3          # saat
soforler <- paste0("S", 1:SOFOR_SAYISI)

# ===============================
# ORTAK IDLE TIME FONKS??YONU
# ===============================
hesapla_idle_time <- function(cozum) {
  toplam <- 0
  for (s in names(cozum)) {
    atamalar <- cozum[[s]]$Atamalar
    if (length(atamalar) <= 1) next
    
    atamalar_sorted <- atamalar[
      order(sapply(atamalar, function(x) x$Kalkis))
    ]
    
    for (i in 2:length(atamalar_sorted)) {
      bosluk <- as.numeric(difftime(
        atamalar_sorted[[i]]$Kalkis,
        atamalar_sorted[[i-1]]$Varis,
        units = "hours"
      ))
      toplam <- toplam + max(bosluk, 0)
    }
  }
  return(toplam)
}

# ===============================
# GREEDY ATAMA
# ===============================
greedy_atama_olustur <- function(seferler_df, min_dinlenme) {
  
  BASLANGIC_ZAMANI <- min(seferler_df$Kalkis)
  
  sofor_durumu <- lapply(soforler, function(s) {
    list(
      SonVaris = BASLANGIC_ZAMANI,
      DinlenmeBitis = BASLANGIC_ZAMANI,
      Atamalar = list()
    )
  })
  names(sofor_durumu) <- soforler
  
  for (i in 1:nrow(seferler_df)) {
    sefer <- seferler_df[i, ]
    en_iyi_sofor <- NULL
    min_bekleme <- Inf
    
    for (sofor_id in soforler) {
      durum <- sofor_durumu[[sofor_id]]
      
      if (sefer$Kalkis >= durum$DinlenmeBitis) {
        bekleme <- as.numeric(difftime(
          sefer$Kalkis, durum$SonVaris, units = "hours"
        ))
        
        if (bekleme < min_bekleme) {
          min_bekleme <- bekleme
          en_iyi_sofor <- sofor_id
        }
      }
    }
    
    if (!is.null(en_iyi_sofor)) {
      sofor_durumu[[en_iyi_sofor]]$Atamalar <- append(
        sofor_durumu[[en_iyi_sofor]]$Atamalar,
        list(sefer)
      )
      sofor_durumu[[en_iyi_sofor]]$SonVaris <- sefer$Varis
      sofor_durumu[[en_iyi_sofor]]$DinlenmeBitis <- sefer$Varis + hours(min_dinlenme)
    }
  }
  
  return(list(
    cozum = sofor_durumu,
    maliyet = hesapla_idle_time(sofor_durumu)
  ))
}

tabu_search <- function(initial_solution,
                        max_iter = 100,
                        tabu_tenure = 5,
                        min_dinlenme = 6) {
  
  # ===============================
  # Yard??mc??: Idle time hesapla
  # ===============================
  hesapla_idle_time <- function(cozum) {
    toplam <- 0
    for (s in names(cozum)) {
      atamalar <- cozum[[s]]$Atamalar
      if (length(atamalar) <= 1) next
      
      atamalar <- atamalar[order(sapply(atamalar, function(x) x$Kalkis))]
      
      for (i in 2:length(atamalar)) {
        bosluk <- as.numeric(
          difftime(atamalar[[i]]$Kalkis,
                   atamalar[[i-1]]$Varis,
                   units = "hours")
        )
        toplam <- toplam + max(bosluk, 0)
      }
    }
    return(toplam)
  }
  
  # ===============================
  # Ba??lang????
  # ===============================
  current <- initial_solution
  best <- initial_solution
  
  current_cost <- hesapla_idle_time(current)
  best_cost <- current_cost
  
  tabu_list <- list()
  
  # ===============================
  # Ana d??ng??
  # ===============================
  for (iter in 1:max_iter) {
    
    best_neighbor <- NULL
    best_neighbor_cost <- Inf
    best_move <- NULL
    
    soforler <- names(current)
    
    # ---------- Kom??u ??ret ----------
    for (trial in 1:100) {
      
      # SADECE dolu ??of??r se??
      dolu_soforler <- soforler[
        sapply(soforler, function(s)
          length(current[[s]]$Atamalar) > 0)
      ]
      if (length(dolu_soforler) == 0) break
      
      s1 <- sample(dolu_soforler, 1)
      s2 <- sample(setdiff(soforler, s1), 1)
      
      idx <- sample(seq_len(length(current[[s1]]$Atamalar)), 1)
      sefer <- current[[s1]]$Atamalar[[idx]]
      
      yeni <- current
      yeni[[s1]]$Atamalar <- yeni[[s1]]$Atamalar[-idx]
      yeni[[s2]]$Atamalar <- append(yeni[[s2]]$Atamalar, list(sefer))
      
      # ---------- Feasibility ----------
      atamalar2 <- yeni[[s2]]$Atamalar
      atamalar2 <- atamalar2[order(sapply(atamalar2, function(x) x$Kalkis))]
      
      feasible <- TRUE
      if (length(atamalar2) > 1) {
        for (i in 2:length(atamalar2)) {
          if (as.numeric(difftime(atamalar2[[i]]$Kalkis,
                                  atamalar2[[i-1]]$Varis,
                                  units = "hours")) < min_dinlenme) {
            feasible <- FALSE
            break
          }
        }
      }
      if (!feasible) next
      
      move_id <- paste(s1, "->", s2, sefer$SeferID)
      
      # ---------- Tabu kontrol ----------
      if (move_id %in% tabu_list &&
          hesapla_idle_time(yeni) >= best_cost) {
        next
      }
      
      maliyet <- hesapla_idle_time(yeni)
      
      if (maliyet < best_neighbor_cost) {
        best_neighbor <- yeni
        best_neighbor_cost <- maliyet
        best_move <- move_id
      }
    }
    
    if (is.null(best_neighbor)) next
    
    # ---------- G??ncelle ----------
    current <- best_neighbor
    current_cost <- best_neighbor_cost
    
    tabu_list <- c(tabu_list, best_move)
    if (length(tabu_list) > tabu_tenure) {
      tabu_list <- tabu_list[-1]
    }
    
    if (current_cost < best_cost) {
      best <- current
      best_cost <- current_cost
    }
    
    cat("Iter:", iter,
        "| Current:", round(current_cost, 1),
        "| Best:", round(best_cost, 1), "\n")
  }
  
  return(list(best_solution = best, best_cost = best_cost))
}


# ANA PROGRAM
# ===============================
dosya_yolu <- "C:\\Users\\90505\\Desktop\\courses\\emu4\\emu427 heuristic\\proje\\seferler.xlsx"

veri <- read_excel(dosya_yolu)

veri_hazir <- veri %>%
  mutate(
    Kalkis = as.POSIXct(KalkisZamani),
    Varis = Kalkis + hours(SEFER_SURESI)
  ) %>%
  arrange(Kalkis)

greedy_sonuc <- greedy_atama_olustur(veri_hazir, MIN_DINLENME_SURESI)
cat("Greedy Idle Time:", greedy_sonuc$maliyet, "\n")

sonuc_tabu <- tabu_search(initial_solution,
                          max_iter = 100,
                          tabu_tenure = 5,
                          min_dinlenme = 6)

cat("En iyi maliyet:", sonuc_tabu$best_cost, "\n")

# ===============================
# Kontrol Fonksiyonu: Minimum Dinlenme S??resi
# ===============================
kontrol_dinlenme <- function(cozum, min_dinlenme = 6) {
  
  uyumsuz_seferler <- list()
  
  for (s in names(cozum)) {
    atamalar <- cozum[[s]]$Atamalar
    if(length(atamalar) <= 1) next
    
    # Kalk???? saatine g??re s??rala
    atamalar <- atamalar[order(sapply(atamalar, function(x) x$Kalkis))]
    
    for(i in 2:length(atamalar)) {
      bosluk <- as.numeric(difftime(atamalar[[i]]$Kalkis,
                                    atamalar[[i-1]]$Varis,
                                    units="hours"))
      if(bosluk < min_dinlenme) {
        uyumsuz_seferler <- append(uyumsuz_seferler, list(
          data.frame(
            Sofor = s,
            OncekiSefer = atamalar[[i-1]]$SeferID,
            SonrakiSefer = atamalar[[i]]$SeferID,
            DinlenmeSuresi = bosluk
          )
        ))
      }
    }
  }
  
  # ????kt??
  if(length(uyumsuz_seferler) == 0) {
    cat("??? Tum seferler minimum dinlenme s??resine uyuyor\n")
    # Ekstra tablo olarak g??sterelim
    print(data.frame(Durum="OK", Aciklama="Hicbir dinlenme ihlali yok"))
    return(TRUE)
  } else {
    uyumsuz_tab <- do.call(rbind, uyumsuz_seferler)
    cat("??? Dinlenme s??resi ihlali olan seferler:\n")
    print(uyumsuz_tab)
    return(FALSE)
  }
}

# ===============================
# Fonksiyonu ??A??IR
# ===============================
# Burada sonuc_tabu$best_solution senin tabu ????z??m??n
kontrol_dinlenme(sonuc_tabu$best_solution, min_dinlenme = 6)
