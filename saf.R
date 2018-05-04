MyData <- read.csv(file="C:\\Users\\Admin\\Desktop\\User Identification From Walking Activity\\1.csv", header=TRUE, sep=",")
X_Acceleration = MyData$X[1100:1200]
plot.ts(X_Acceleration, main = "Sample Pedestrian X-Axis Acceleration from Android Sensor")
pgram = spec.pgram(X_Acceleration, kernel("modified.daniell", c(1,1)), taper=0.1, main = "Modified Daniell Kernel Span 1 - Convolution")
key_freq_ind <- c(1, which(diff(sign(diff(pgram$spec)))==-2) + 1)
key_freq <- pgram$freq[key_freq_ind]
abline(v=key_freq, lty=2)
time_frame_ts = ts(X_Acceleration)
t_test <- 1:length(X_Acceleration)
top_freq <- key_freq[order(pgram$spec[key_freq_ind], decreasing = T)][1:5]
periodic_terms_test <- do.call(cbind, lapply(top_freq, function(freq) {
  cbind(cos(2 * pi * freq * t_test), sin(2 * pi * freq * t_test))
}))
df_test <- data.frame(time_frame_ts, t_test, periodic_terms_test)
fit_final_test <- lm(time_frame_ts ~ ., df_test)
plot(t_test,X_Acceleration, lty=2, col="black", main = "Sample Spectral Model Fitting")
lines(t_test, fit_final_test$fitted.values, lty=2, col="black")

