#' Calculate payment for mortgage for a fixed rate.
#'
#'
#' @description This function is to calculate mortgage payment for a fixed rate.
#'
#' @param loan numeric, Loan amount
#' @param amortPeriod numeric, Number of months for the amortization period.
#' @param interestRate numeric, The annual interest rate (%).
#' @param payFreq character, Can be one of \code{monthly}, \code{semi-monthly},
#'                           \code{weekly}, \code{biweekly}. Currently, only support \code{monthly}.
#' @param term numeric, Number of years for the current term. If missing, the term is
#'                      the same as the amortPeriod.
#' @param prepayPct numeric, Prepayment percentage, it supports multiple options for comparison.
#'                           If missing, this is no prepare plan.
#' @param prepayMount numeric, Prepayment amount, it supports multiple options for comparison.
#'                           If missing, this is no prepare plan.
#' @return each payment
#' @note This function produce similar but different results when comparing to
#'       mortgage calculators online (see descriptions).
#'
#'
#'
#' @importFrom data.table ':=' data.table
#' @importFrom dplyr '%>%'
#' @importFrom FinancialMath amort.period
#'
#' @examples
#' \dontrun{
#' ## loan 364000 for 20 years (240 months) with an interest rate of 0.359 no term specification
#' paymenttable <- mortgagePayment(loan = 364000,
#'                             amortPeriod = 240,
#'                             interestRate = 4.6,
#'                             payFreq = "monthly")
#'
#' ## loan 364000 for 20 years (240 months) with an interest rate of 0.359 term = 5years
#' paymenttable <- mortgagePayment(loan = 364000,
#'                             amortPeriod = 240,
#'                             interestRate = 4.6,
#'                             payFreq = "monthly",
#'                             term = 5)
#' ## loan 364000 for 20 years (240 months) with an interest rate of 0.359 term = 5years
#' ## with prepay options
#' paymenttable <- mortgagePayment(loan = 364000,
#'                             amortPeriod = 240,
#'                             interestRate = 4.79,
#'                             payFreq = "monthly",
#'                             term = 5,
#'                             prepayMount = c(100, 120, 150, 170, 210))
#'
#'
#' }
#'
#'
#' @export
#' @docType methods
#' @rdname mortgagePayment
#'
#' @author Yong Luo
mortgagePayment <- function(loan,
                        amortPeriod,
                        interestRate,
                        payFreq,
                        term = NA,
                        prepayPct = NA,
                        prepayMount = NA){
  if(is.na(term)){
    term <- amortPeriod/12 # same as the amortPeriod
  }
  if(tolower(payFreq) == "monthly"){
    pf <- 12
  } else {
    stop("please specify a correct payment frequency.")
  }
  term_end <- term*12
  eachpay <- amort.table(Loan = loan,
                         n = amortPeriod,
                         pmt = NA,
                         i = interestRate/100,
                         ic = pf,
                         pf = pf)
  schedule_table <- eachpay$Schedule %>% data.frame()
  schedule_table$payNum <- as.numeric(row.names(schedule_table))
  schedule_table <- data.table(schedule_table)
  basepayment <- schedule_table$Payment[1]
  schedule_table_interm <- schedule_table[payNum <= term_end,
                                          .(loan = loan,
                                            `interest rate (%)` = interestRate,
                                            `amortization period (months)` = amortPeriod,
                                            `term (years)` = term,
                                            `payment freq` = payFreq,
                                            `prepay option` = "none",
                                            `prepay caution` = as.character(NA),
                                            `each payment` = round(mean(Payment), 2),
                                            `total payment` = sum(Payment),
                                            `total interest paid` = sum(Interest.Paid),
                                            `total principal paid` = sum(Principal.Paid),
                                            balance = min(Balance))]
  prepayPct <- unique(prepayPct)
  for (indiprepaypct in prepayPct) {
    if(!is.na(indiprepaypct)){
      if(indiprepaypct > 15){
        prepayCaution <- "passes 15% of payment, check with lender"
      } else {
        prepayCaution <- as.character(NA)
      }
      pmt_prepay <- round(basepayment*(1+indiprepaypct/100), 2)
      eachpay_prepay <- amort.table(Loan = loan,
                                    pmt = pmt_prepay,
                                    i = interestRate/100,
                                    ic = pf,
                                    pf = pf)
      schedule_table_prepay <- eachpay_prepay$Schedule %>% data.frame()
      schedule_table_prepay$payNum <- as.numeric(row.names(schedule_table_prepay))
      schedule_table_prepay <- data.table(schedule_table_prepay)
      amortPeriod <- nrow(schedule_table_prepay)
      schedule_table_prepay_interm <- schedule_table_prepay[payNum <= term_end,
                                                            .(loan = loan,
                                                              `interest rate (%)` = interestRate,
                                                              `amortization period (months)` = amortPeriod,
                                                              `term (years)` = term,
                                                              `payment freq` = payFreq,
                                                              `prepay option` = paste0("by ", indiprepaypct, " pct"),
                                                              `prepay caution` = prepayCaution,
                                                              `each payment` = round(mean(Payment), 2),
                                                              `total payment` = sum(Payment),
                                                              `total interest paid` = sum(Interest.Paid),
                                                              `total principal paid` = sum(Principal.Paid),
                                                              balance = min(Balance))]
      schedule_table_interm <- rbind(schedule_table_interm,
                                     schedule_table_prepay_interm)
    }
  }


  prepayMount <- unique(prepayMount)
  for (indiprepaymt in prepayMount) {
    if(!is.na(indiprepaymt)){
      if(indiprepaymt/basepayment > 0.15){
        prepayCaution <- "passes 15% of payment, check with lender"
      } else {
        prepayCaution <- as.character(NA)
      }
      pmt_prepay <- basepayment + indiprepaymt
      eachpay_prepay <- amort.table(Loan = loan,
                                    pmt = pmt_prepay,
                                    i = interestRate/100,
                                    ic = pf,
                                    pf = pf)
      schedule_table_prepay <- eachpay_prepay$Schedule %>% data.frame()
      schedule_table_prepay$payNum <- as.numeric(row.names(schedule_table_prepay))
      schedule_table_prepay <- data.table(schedule_table_prepay)
      amortPeriod <- nrow(schedule_table_prepay)
      schedule_table_prepay_interm <- schedule_table_prepay[payNum <= term_end,
                                                            .(loan = loan,
                                                              `interest rate (%)` = interestRate,
                                                              `amortization period (months)` = amortPeriod,
                                                              `term (years)` = term,
                                                              `payment freq` = payFreq,
                                                              `prepay option` = paste0("add ", indiprepaymt),
                                                              `prepay caution` = prepayCaution,
                                                              `each payment` = round(mean(Payment), 2),
                                                              `total payment` = sum(Payment),
                                                              `total interest paid` = sum(Interest.Paid),
                                                              `total principal paid` = sum(Principal.Paid),
                                                              balance = min(Balance))]
      schedule_table_interm <- rbind(schedule_table_interm,
                                     schedule_table_prepay_interm)
    }
  }
  return(schedule_table_interm)
}
