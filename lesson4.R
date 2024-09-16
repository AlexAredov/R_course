df = data.frame(
  dose = c(20, 30, 40, 45, 60),
  drug_a = c(16, 20, 27, 40, 60),
  drug_b = c(15, 18, 25, 31, 40)
)

plot(df$dose,
     df$drug_a,
     type='b',
     lty = 2,
     col="blue",
     pch=2,
     main = "Dose-effect", 
     font.main=4, 
     ylab = "effect",
     xlab = "dose"
)
lines(df$dose,
      df$drug_b,
      type="b",
      lty=3,
      pch=c(0,1,4),
      col="red"
)
abline(h=50,lty=3)


legend('topleft',
       legend=c("Drug A","Drug B"),
       lty=c(2,3),
       col=c("blue","red"),
       pch=c(2,c(0,1,4))
)



