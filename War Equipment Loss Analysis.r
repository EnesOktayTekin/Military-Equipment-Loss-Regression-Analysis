# Gerekli Kütüphaneler
library(tidyverse)
library(mice)
library(car)
library(caret)
library(lmtest)

# Veri Yükleme ve Ön İşleme
prepare_data <- function(data) {
  # Gerekli değişkenleri seç
  selected_vars <- c("day", "aircraft", "helicopter", "tank", "APC", 
                     "field.artillery", "MRL", "military.auto", "fuel.tank", 
                     "drone", "naval.ship", "anti.aircraft.warfare", "special.equipment")
  
  war_data <- data[selected_vars]
  
  # Eksik Veri Kontrolü ve Doldurma
  if(sum(is.na(war_data)) > 0) {
    imputed_data <- mice(war_data, m = 5, method = 'pmm')
    war_data <- complete(imputed_data, 3)
  }
  
  return(war_data)
}

# Modelleme Fonksiyonu
build_regression_models <- function(data) {
  # Veri Bölme
  set.seed(145)
  train_index <- createDataPartition(data$military.auto, p = 0.8, list = FALSE)
  train_data <- data[train_index, ]
  test_data <- data[-train_index, ]
  
  # Model 1: Tüm Değişkenlerle
  model1 <- lm(military.auto ~ ., data = train_data)
  
  # Model 2: Tank Değişkeni Çıkarılmış
  model2 <- lm(military.auto ~ . - tank, data = train_data)
  
  # Aykırı Değer Tespiti
  cook_dist <- cooks.distance(model2)
  outlier_threshold <- 4 / length(cook_dist)
  outliers <- which(cook_dist > outlier_threshold)
  
  # Model 3: Aykırı Değerler Çıkarılmış
  train_data_clean <- train_data[-outliers, ]
  model3 <- lm(military.auto ~ . - tank, data = train_data_clean)
  
  # Model 4: VIF Analizi Sonrası
  model4 <- step(model3, direction = "both")
  
  # Model 5: Aşamalı Regresyon
  model5 <- step(lm(military.auto ~ 1, data = train_data_clean), 
                 scope = formula(model3), 
                 direction = "both")
  
  # Model Performans Değerlendirme
  evaluate_models <- function(model, test_data) {
    predictions <- predict(model, test_data)
    list(
      R2 = R2(predictions, test_data$military.auto),
      RMSE = RMSE(predictions, test_data$military.auto),
      MAE = MAE(predictions, test_data$military.auto),
      AIC = AIC(model),
      BIC = BIC(model)
    )
  }
  
  # Modellerin Performansını Hesapla
  model_performance <- list(
    Model1 = evaluate_models(model1, test_data),
    Model2 = evaluate_models(model2, test_data),
    Model3 = evaluate_models(model3, test_data),
    Model4 = evaluate_models(model4, test_data),
    Model5 = evaluate_models(model5, test_data)
  )
  
  # Otokorelasyon Testi
  durbin_watson_test <- dwtest(model5)
  
  return(list(
    Models = list(model1, model2, model3, model4, model5),
    Performance = model_performance,
    DurbinWatson = durbin_watson_test
  ))
}

# Ana Çalıştırma Fonksiyonu
main_analysis <- function(data) {
  # Veriyi Hazırla
  prepared_data <- prepare_data(data)
  
  # Modelleri Oluştur
  analysis_results <- build_regression_models(prepared_data)
  
  # Sonuçları Yazdır
  print("Model Performansları:")
  print(analysis_results$Performance)
  
  # Durbin-Watson Test Sonucu
  print("Durbin-Watson Testi:")
  print(analysis_results$DurbinWatson)
  
  return(analysis_results)
}


# results <- main_analysis(russia_losses_equipment)

#Graphics

library(ggplot2)
library(gridExtra)
library(plotly)

# Model Performans Karşılaştırma Grafikleri Fonksiyonu
create_model_comparison_plots <- function(analysis_results) {
  # Performans verilerini DataFrame'e dönüştür
  performance_df <- data.frame(
    Model = names(analysis_results$Performance),
    R2 = sapply(analysis_results$Performance, `[[`, "R2"),
    RMSE = sapply(analysis_results$Performance, `[[`, "RMSE"),
    MAE = sapply(analysis_results$Performance, `[[`, "MAE"),
    AIC = sapply(analysis_results$Performance, `[[`, "AIC"),
    BIC = sapply(analysis_results$Performance, `[[`, "BIC")
  )
  
  # 1. R-kare Karşılaştırma Grafik
  r2_plot <- ggplot(performance_df, aes(x = Model, y = R2, fill = Model)) +
    geom_bar(stat = "identity") +
    labs(title = "Model Performansı: R-kare Karşılaştırması", 
         y = "R-kare Değeri") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_brewer(palette = "Set3")
  
  # 2. RMSE Karşılaştırma Grafik
  rmse_plot <- ggplot(performance_df, aes(x = Model, y = RMSE, fill = Model)) +
    geom_bar(stat = "identity") +
    labs(title = "Model Performansı: RMSE Karşılaştırması", 
         y = "Kök Ortalama Kare Hatası") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_brewer(palette = "Set2")
  
  # 3. Çoklu Performans Metrikleri Karşılaştırma
  performance_long <- tidyr::pivot_longer(
    performance_df, 
    cols = c(R2, RMSE, MAE, AIC, BIC), 
    names_to = "Metric", 
    values_to = "Value"
  )
  
  multi_metric_plot <- ggplot(performance_long, 
                               aes(x = Model, y = Value, fill = Metric)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Çoklu Model Performans Metrikleri",
         y = "Değer") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_brewer(palette = "Set1")
  
  # 4. İnteraktif 3D Scatter Plot (Plotly)
  interactive_3d_plot <- plot_ly(
    performance_df, 
    x = ~R2, 
    y = ~RMSE, 
    z = ~MAE,
    type = 'scatter3d', 
    mode = 'markers',
    text = ~Model,
    color = ~Model,
    colors = 'Spectral',
    marker = list(size = 10, opacity = 0.7)
  ) %>%
    layout(
      title = "3D Model Performans Karşılaştırması",
      scene = list(
        xaxis = list(title = "R2"),
        yaxis = list(title = "RMSE"),
        zaxis = list(title = "MAE")
      )
    )
  
  # Grafikleri Kaydet
  ggsave("r2_comparison.png", r2_plot, width = 10, height = 6)
  ggsave("rmse_comparison.png", rmse_plot, width = 10, height = 6)
  ggsave("multi_metric_comparison.png", multi_metric_plot, width = 12, height = 7)
  
  # HTML'de kaydedilen interaktif grafik
  htmlwidgets::saveWidget(
    interactive_3d_plot, 
    "model_performance_3d.html"
  )
  
  # Grafikleri Göster ve Return Et
  grid.arrange(r2_plot, rmse_plot, multi_metric_plot, ncol = 2)
  
  return(list(
    R2Plot = r2_plot,
    RMSEPlot = rmse_plot,
    MultiMetricPlot = multi_metric_plot,
    Interactive3DPlot = interactive_3d_plot
  ))
}

# plots <- create_model_comparison_plots(analysis_results)