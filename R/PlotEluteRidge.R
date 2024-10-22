GetEluteRidge <- function(dat){
library(ggdendro)
library(ggridges)
library(reshape2)
# Normalize to 0-1
dat <-
  dat[, apply(dat, 2, sd) != 0]
dat <-
  dat - apply(dat, 1, function(x){min(x, na.rm = TRUE)})
dat <-
  dat / apply(dat, 1, function(x){max(x, na.rm = TRUE)})
dat <-
  dat ^ 3

df_long <-
  melt(as.matrix(dat), varnames = c("Protein", "Frac"), value.name = "Peptide")
p <-
  ggplot(
    df_long,
    aes(
      x = `Frac`,
      y = `Protein`,
      height = `Peptide`,
      group = `Protein`
    )
  ) +
  geom_density_ridges2(
    stat = "identity",
    scale = 0.9,
    fill = "#3182bd") +
  theme_bw() +
  theme(text = element_text(size = 12, colour = "#000000"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 10, colour = "#000000"),
        axis.ticks.y = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        # panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_blank(),
        legend.position = "none") +
  labs(`x` = "Eluted Fractionations", `y` = "Subunits", `title` = "")
return(p)
}

