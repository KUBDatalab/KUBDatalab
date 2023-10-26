#' @title StatSpiral
#' @description StatSpiral returnerer det vi skal bruge for at lave geom_spiral
#' @details details here
#' @param param 1
#' @rdname StatSpiral
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 Stat
#' @export
StatSpiral <- ggplot2::ggproto("StatSpiral",
                               `_inherit` = ggplot2::Stat,
                      required_aes = c("x", "group"),

                      compute_group = function(data, scales, n = 12) {
                        # Check for NULL data
                        if(is.null(data)){stop("Data cannot be NULL.")}
                        a <- 1
                        b <- 1
                        theta <- seq(0, n*(2*pi), length.out=1300)

                        seglen <- (a/2) * ( theta*sqrt(1+theta^2) +
                                              log(theta + sqrt(1+theta^2)) )

                        p <- data$x / sum(data$x)
                        plen <- p * seglen[length(seglen)]

                        x <- (a + b*theta) * cos(theta)
                        y <- (a + b*theta) * sin(theta)

                        df_list <- list()

                        prev_index <- 1
                        col <- 1:length(data$x)
                        for (i in 1:length(data$x)) {
                          if (i == length(data$x)) {
                            curr_index <- length(seglen)
                          } else {
                            curr_index <- which(seglen >= cumsum(plen)[i])[1]
                          }

                          segment_df <- data.frame(
                            x = x[prev_index:curr_index],
                            y = y[prev_index:curr_index],
                            col = col[i],
                            group = data$group[1]
                          )

                          df_list[[i]] <- segment_df
                          prev_index <- curr_index
                        }

                        full_df <- do.call(rbind, df_list)

                        grid <- full_df

                        grid
                      }
)

