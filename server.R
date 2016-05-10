shinyServer(function(input, output, session) {
  current.p <- 0.0
  current.a <- 0.0
  current.d <- 0.0

  observe({
    if (current.p != input$sliderP) {
      updateNumericInput(session, "numericP", NULL, input$sliderP)
      current.p <<- input$sliderP
    }
  })

  observe({
    if (is.na(input$numericP)) {
      # do nothing
    } else if (current.p != input$numericP) {
      updateSliderInput(session, "sliderP", NULL, input$numericP)
      current.p <<- input$numericP
    }
  })

  observe({
    if (current.a != input$sliderA) {
      updateNumericInput(session, "numericA", NULL, input$sliderA)
      current.a <<- input$sliderA
    }
  })

  observe({
    if (is.na(input$numericA)) {
      # do nothing
    } else if (current.a != input$numericA) {
      updateSliderInput(session, "sliderA", NULL, input$numericA)
      current.a <<- input$numericA
    }
  })

  observe({
    if (current.d != input$sliderD) {
      updateNumericInput(session, "numericD", NULL, input$sliderD)
      current.d <<- input$sliderD
    }
  })

  observe({
    if (is.na(input$numericD)) {
      # do nothing
    } else if (current.d != input$numericD) {
      updateSliderInput(session, "sliderD", NULL, input$numericD)
      current.d <<- input$numericD
    }
  })

  # plot function encapsulates calculations
  output$plot <- renderPlot({
    p  <- input$sliderP
    a  <- input$sliderA
    d  <- input$sliderD
    mu <- a*(2*p - 1) + 2*d*p*(1-p)
    beta <- a + d*(1-2*p)
    # Genotype values
    gAA  <- -a
    gAB  <- d
    gBB  <- a
    varG <- (1-p)^2 * (gAA-mu)^2 + 2*p*(1-p)*(gAB-mu)^2 + p^2*(gBB-mu)^2
    # Breeding values for the 3 genotypes
    bvAA <- -2*p*beta
    bvAB <- (1-2*p)*beta
    bvBB <- 2*(1-p)*beta
    varBV <- (1-p)^2 * (bvAA)^2 + 2*p*(1-p)*(bvAB)^2 + p^2*(bvBB)^2
    # Variance components
    Va <- 2*p*(1-p)*beta^2
    Vd <- (2*p*(1-p)*d)^2
    Vg <- Va + Vd
    # For plotting
    x <- c(0,1,2)
    y <- c(-a,d,a)
    w <- c((1-p)^2, 2*p*(1-p),p^2)
    cex.val <- (1 + w)^2
    plot(x, y, cex = cex.val, ylim = c(-10, 10), xaxt = "n", xlab = "Allele counts", ylab = "")
    axis(1, at = c(0, 1, 2), labels = c(expression(A[2]*A[2]), expression(A[1]*A[2]), expression(A[1]*A[1])))
    abline(lm(y ~ x))
  })
})
