income_tax <- function(income) {
    # This naive implementation is faster than the fancy version by 3x
    if (income <= 18200) return(0)
    if (income > 18200 & income <= 37000) return((income - 18200) * .19)
    if (income > 37000 & income <= 80000) return(3572 + (income - 37000) * .325)
    if (income > 80000 & income <= 180000) return(17547 + (income - 80000) * .37)
    if (income > 180000) return(54547 + (income - 180000) * .45)

}


lito <- function(income, amount = 445, taper_brackets = c(37000, 66667),
                 taper_rate = .015) {
    # Low Income Tax Offset
    if (income > max(taper_brackets)) {
        lito <- 0
    } else if (income < min(taper_brackets)) {
        lito <- amount
    } else {
        lito <- amount - (income - min(taper_brackets)) * taper_rate
    }
    return(lito)
}


medicare_levy <- function(income, levy_rate = .015,
                          shading_brackets = c(20542,24167),
                          shading_rates = c(0,.1)) {
    # The medicare levy is a flat tax of the levy rate, 'shaded in' over certain
    # low income thresholds.
    if (income > max(shading_brackets)) {
        # No low income exemption = flat tax
        levy <- income * levy_rate
    } else {
        # If you're below the partial threshold then income up to the bottom
        # threshold is exempt and income above the bottom threshold is taxed at
        # 10 per cent.
        income_by_bracket <- diff(c(0, pmin(income, shading_brackets)))
        levy <- sum(income_by_bracket * shading_rates)
    }
    return(levy)
}


tax_owed <- function(income) {
    # Calculate the tax owed including personal income tax, medicare levy and
    # the LITO
    pit_amt <- income_tax(income)
    med_amt <- medicare_levy(income)
    lito_amt <- lito(income)
    # LITO is non refundable, so income tax owed can't be less than 0
    if ((pit_amt + med_amt - lito_amt) < 0) {
        return(0)
    } else {
        return(pit_amt + med_amt - lito_amt)
    }
}


revenue_increase <- function(income, deductions, cap_level) {
    # Additional revenue gained per taxpayer by capping deductions
    if (deductions < cap_level) return(0)
    # income - cap_level = before tax income under cap
    # income - deductions = before tax income without cap
    #
    # So the excess revenue is the difference between what the person's tax bill
    # would be with and without a deductions cap. Without a cap, an invidual
    # would have access to greater levels of deductions and therefore a smaller
    # tax bill.
    tax_owed(income - cap_level) - tax_owed(income - deductions)
}
