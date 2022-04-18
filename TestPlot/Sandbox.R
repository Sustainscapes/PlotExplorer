library(ggtern)

axis <- function(title) {

  list(

    title = title,

    titlefont = list(

      size = 20

    ),

    tickfont = list(

      size = 15

    ),

    tickcolor = 'rgba(0,0,0,0)',

    ticklen = 5

  )

}


Data <- Base %>%
  as.data.frame() %>%
  dplyr::select(-geometry)

Data$Data <- sample(LETTERS[c(1,2)], size = nrow(Data), replace = T, prob = c(0.9, 0.1))


G <- ggtern(data = Data, aes(x = grime_R, y = grime_C, z = grime_S)) +
  geom_point(aes(color = rgb, size = Data), alpha = 0.5) + scale_color_identity() + ggtern::theme_rgbw() +
  zlab('Stress tolerator') + xlab('Ruderal') + ylab('Competitor')

