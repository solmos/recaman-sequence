library(tidyverse)
# Generate the first n elements of the Recaman's sequence
get_recaman <- function(n) {
    recaman_seq <- numeric(n)
    for (i in 1:length(recaman_seq)) {
        candidate <- recaman_seq[i] - i
        if (candidate > 0 & !(candidate %in% recaman_seq)) {
            recaman_seq[i + 1] <- candidate
        } else recaman_seq[i + 1] <- recaman_seq[i] + i
    }
    recaman_seq <- recaman_seq[-length(recaman_seq)]
    recaman_seq
}

# Get semicircle paths
construct_arc <- function(start, stop, type) {
    r <- abs(start - stop) / 2
    x0 <- min(c(start, stop)) + r
    y0 <- 0
    if (type == "up_forward") {
        theta <- seq(pi, 0, -0.01)
    } else if (type == "up_backwards") {
        theta <- seq(0, pi, 0.01)
    } else if (type == "down_forward") {
        theta <- seq(pi, 2 * pi, 0.01)
    } else if (type == "down_backwards") {
        theta <- seq(2 * pi, pi, -0.01)
    }
    x <- r * cos(theta) + x0
    y <- r * sin(theta) + y0
    df <- data.frame(x, y)
}

# Plot the first n elements of the Recaman's sequence
plot_recaman <- function(n, size = 1, alpha = 0.8) {
    recaman_seq <- get_recaman(n)
    df <- data.frame(start = recaman_seq,
                     stop = lead(recaman_seq),
                     # Alternating position of the semicircles
                     side = rep_len(c("down", "up"), length(recaman_seq))) %>% 
        mutate(direction = ifelse(stop - start > 0, "forward", "backwards"),
               type = paste(side, direction, sep = "_")) %>% 
        filter(!is.na(stop))
    l <- Map(construct_arc, start = df$start, stop = df$stop, type = df$type)
    df2 <- do.call("rbind", l)
    ggplot(df2, aes(x, y)) +
        geom_path(alpha = alpha, size = size) +
        coord_fixed() +
        theme_void()
}
plot_recaman(20, size = 2)
plot_recaman(66, size = 1)
plot_recaman(500, size = 0.5, alpha = 0.8)

# Do you like the drawing? Save it!
# ggsave("choose_a_name.png", height=3, width=5, units='in', dpi=800)
