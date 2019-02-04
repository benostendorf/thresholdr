##' Custom survival plots
##' This function is modified after the function in '~/src/functions.R', and is
##' partly inspired by the Imvigor210Corebiologies R package (Mariathasan et al., Nature, 2018).
##' @param survFit survFit object
##' @param filename character or \code{NULL}; define filename when exporting pdf
##' @param title character; plot title
##' @param custom_palette character; custom color palette
##' @param custom_legends character; custom legend labels (take note of warning below)
##' @param width numeric; width of pdf export
##' @param height numeric; height of pdf export
##' @param xlab character; x-axis label
##' @param ylab character; y-axis label
##' @import survival
##' @import survminer
##' @import grDevices
##' @import graphics
##' @author Benjamin Ostendorf
##' @export
survival_custom <- function(survFit,
                            filename = NULL,
                            title = NULL,
                            custom_palette = NULL,
                            custom_legends = NULL,
                            width = 2,
                            height = 2.2,
                            xlab = "Overall survival (years)",
                            ylab = "Survival probability") {

  ## Warning re: correct order of custom labels
  if(!is.null(custom_legends)) {
    message("When passing custom legends ensure proper order of labels in following df:")
    print(data.frame(custom_legends = custom_legends,
                     names_in_df = sapply(strsplit(names(survFit$strata), "="), "[", 2)))
  }

  ## Export plot as pdf if filename is passed to function
  if (is.character(filename)) {
    pdf(
      file = filename,
      width = width,
      height = height,
      pointsize = 7,
      useDingbats = FALSE
    )
  }

  ## Define color palette for plot depending on number of groups
  ifelse(
    length(survFit$strata) < 5,
    pal <- color_palettes$four_colors[1:length(survFit$strata)],
    pal <- color_palettes$six_colors[1:length(survFit$strata)]
  )
  ## Integrate custom palettes
  ifelse(is.null(custom_palette),
         pal <- pal,
         pal <- custom_palette)

  ## Plot
  # ifelse(is.null(title), par(mar = c(2.3, 2.7, 0, 0)), par(mar = c(2.3, 2.7, 1.7, 0)))
  p <- plot(
    survFit,
    frame = FALSE,
    lwd = 1.5,
    col = pal,
    xscale = 365.25,
    mark.time = TRUE,
    cex = 0.5,
    cex.axis = 5 / 7,
    yaxt = "n",
    xaxt = "n"
  )
  axis(1,
       at = seq(0, 175, 25),
       seq(0, 175, 25),
       cex.axis = 5 / 7,
       lwd = 5 / 8,
       tck = -0.025,
       padj = -1.8
  )
  axis(2,
       at = seq(0, 1, 0.2),
       seq(0, 1, 0.2),
       las = 2,
       cex.axis = 5 / 7,
       lwd = 5 / 8,
       tck = -0.025,
       hadj = 0.5)
  title(xlab = xlab,
        line = 1.3,
        cex.lab = 6 / 7)
  title(ylab = ylab,
        line = 1.8,
        cex.lab = 6 / 7)
  ## Plot title
  mtext(title,
        side = 3,
        # at = 0.5 * max(survFit$time),
        adj = 0.35,
        line = 0.5)

  ## Generate vector for legend labels with group sizes
  groups <- paste0(sapply(strsplit(names(survFit$strata), "="), "[", 2), " (", survFit$n, ")")

  ## Integrate custom legend labels
  ifelse(
    is.null(custom_legends),
    groups <- groups,
    groups <- paste0(custom_legends, " (", survFit$n, ")")
  )

  ## Legend; here, both colors and labels can reversed to put E4 on top in analogy to curves
  ## by adding 'rev()' to both the assignments for 'legend' and 'col'
  legend(
    "topright",
    legend = groups,
    col = pal,
    lty = 1,
    bty = "n",
    cex = 5 / 7
  )

  ## Include log-rank p-value
  text(0.8 * max(survFit$time), 0.4,
       paste0("p = ", format.pval(surv_pvalue(survFit)$pval, digits = 2)),
       cex = 6 /7)

  ## Close pdf export
  if (is.character(filename)) {
    dev.off()

    ## Crop whitespace
    system(paste("pdfcrop", filename, filename))

    message("Plot saved under ", filename)
  }
}

# scale_fill_Publication <- function(...){
#   discrete_scale("fill","Publication", manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
# }
# scale_colour_Publication <- function(...){
#   discrete_scale("colour","Publication", manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
# }
