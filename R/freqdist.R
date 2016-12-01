#' Generates a Frequency Distribution with columns for raw frequencies, percentages, and cumulative percentages.
#' @usage freqdist(x)
#' @param x Variable on which the frequency distribution is generated.
#' @return - Returns the frequency distribution.
#' @author Thomas Feldman and Kenneth W. Moffett
#' @examples
#' # Create Sample Data Frame
#'
#' data <- c(1,2,3,4,4)
#' data <- as.data.frame(data)
#'
#' # Run Frequency Distribution Command on Data Frame
#'
#' freqdist(data)
#' @export
freqdist <- function(x) {
  frequencydistribution <- cbind(table(x))
  percentage <- prop.table(frequencydistribution, margin = 2)*100
  frequencydistribution <- as.data.frame(frequencydistribution)
  percentage <- as.data.frame(percentage)
  colnames(percentage) <- "percentage"
  colnames(frequencydistribution) <- "frequencies"
  cumulativesum <- cbind(cumsum(frequencydistribution))
  cumulativesum <- as.data.frame(cumulativesum)
  cumulativesum$cumulativepercentage <- cumulativesum$frequencies/sum(frequencydistribution$frequencies) * 100
  colnames(cumulativesum)[1] <- "cumulativesum"
  frequencydistribution <- cbind(frequencydistribution, percentage)
  frequencydistribution <- cbind(frequencydistribution, cumulativesum)
  frequencydistribution <- subset(frequencydistribution, select = c("frequencies", "percentage", "cumulativepercentage"))
  totals <- c(sum(frequencydistribution$frequencies), sum(frequencydistribution$percentage), frequencydistribution[nrow(frequencydistribution), 'cumulativepercentage'])
  frequencydistribution <- rbind(frequencydistribution, totals)
  rownames(frequencydistribution)[nrow(frequencydistribution)] <- "Totals"
  frequencydistribution
}
