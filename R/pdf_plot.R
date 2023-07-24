pdf_plot = function(df, tz, pdffile, statenames) {
  df$date = as.Date(df$time, tz = tz)
  uDates = unique(df$date)
  
  nstates = 8
  colo = c(7, 1:6, 8)
  
  CX = 1
  # graphics.off()
  Nplots = length(uDates)
  
  pdf(file = pdffile, width = 15, height = 12)
  if (Nplots <= 9) {
    par(mfrow = c(3, 3), mar = c(2, 2, 2, 1))
  } else if (Nplots <= 12) {
    par(mfrow = c(4, 3), mar = c(2, 2, 2, 1))
  } else if (Nplots > 12) {
    Nplots = 12 # only plot first 12 days
  }
  for (k in 1:Nplots) {
    Dt = df[which(df$date == uDates[k]),]
    if (nrow(Dt) > 10) {
      XLIM = as.numeric(range(Dt$lon))
      YLIM = as.numeric(range(Dt$lat))
      if (k == 3) {
        XLIM[2] = XLIM[2] + 0.001
      }
      # Calculate total distance traveled
      TotDist = round(sum(Dt$distance_m, na.rm = TRUE) / 1000)
      # Calculate time span of date points
      TotTime = round(diff(as.numeric(range(Dt$time))) / 3600)
      select = which(Dt$state == 8)
      plotTitle = paste0(uDates[k], " | Cumulative dist:", TotDist, "km | Time span:", TotTime, "hr")
      plot(Dt$lon[select], Dt$lat[select], type = "p", pch = 20,
           col = colo[8], ylim = YLIM, xlim = XLIM, cex = CX, bty = "l",
           main = plotTitle, cex.main = 1.4)
      for (j in 7:1) {
        select = which(Dt$state == j)
        if (length(select) > 0) {
          lines(Dt$lon[select], Dt$lat[select],
                type = "p", pch = 20, col = colo[j], cex = CX)
        }
      }
      if (k == 3) {
        legend("right", legend = statenames, col = colo, ncol = 1,
               pch = 19, bty = "n", cex = 1.3, bg = "white", box.col = "black")
      }
    }
  }
  dev.off()
}