# =========================================================
# Shiny App: Probability Concepts
# Requires: shiny only (all plots use base R graphics)
# Topics: Concepts & Notation, Addition Rule,
#         Multiplication Rule, Conditional Probability &
#         Law of Total Probability, Bayes' Theorem
# =========================================================


# https://hbctraining.github.io/Training-modules/RShiny/lessons/shinylive.html
# Run the shinylive::export line to populate the docs folder 
# so that shinylive works from github
#shinylive::export(appdir = "../ProbabilityConceptsRules/", destdir = "docs")
#httpuv::runStaticServer("docs/", port = 8008)

# ---------------------------
# Colour palette (consistent with other apps)
# ---------------------------
col_A     <- "#1B9E77"   # Dark2 green  – Event A
col_B     <- "#D95F02"   # Dark2 orange – Event B
col_AB    <- "#7570B3"   # Dark2 purple – Intersection A∩B
col_only  <- "#E7298A"   # Dark2 pink   – highlight / complement
col_bg    <- "#f9f9f9"
btn_blue  <- "#2196F3"

# ---------------------------
# Helper: draw a Venn diagram using base R
#   type: "indep", "mutex", "overlap"
#   highlight: NULL, "A", "B", "AuB", "AiB", "Ac", "AgivenB"
# ---------------------------
draw_venn <- function(type = "overlap",
                      highlight = NULL,
                      pA = 0.4, pB = 0.4, pAB = 0.16,
                      label_A = "A", label_B = "B") {
  
  old_par <- par(mar = c(1, 1, 2, 1), bg = col_bg)
  on.exit(par(old_par), add = TRUE)
  
  plot(0, 0, type = "n", xlim = c(-1.8, 1.8), ylim = c(-1.2, 1.2),
       asp = 1, axes = FALSE, xlab = "", ylab = "",
       main = "")
  
  # Rectangle = sample space S
  rect(-1.75, -1.15, 1.75, 1.15, border = "#888888", lwd = 2, col = "#EBEBEB")
  text(-1.55, 1.0, "S", cex = 1.4, col = "#555555", font = 2)
  
  theta <- seq(0, 2 * pi, length.out = 300)
  
  # Circle centres & radii
  if (type == "mutex") {
    cx_A <- -0.85; cx_B <- 0.85
  } else {
    cx_A <- -0.45; cx_B <- 0.45
  }
  cy <- 0; r <- 0.70
  
  xs_A <- cx_A + r * cos(theta)
  ys_A <- cy  + r * sin(theta)
  xs_B <- cx_B + r * cos(theta)
  ys_B <- cy  + r * sin(theta)
  
  # --- fill colours ---
  fill_A  <- adjustcolor(col_A,  alpha.f = 0.30)
  fill_B  <- adjustcolor(col_B,  alpha.f = 0.30)
  fill_hi <- adjustcolor(col_only, alpha.f = 0.45)
  none    <- adjustcolor("white", alpha.f = 0)
  
  if (is.null(highlight)) {
    polygon(xs_A, ys_A, col = fill_A, border = col_A, lwd = 2)
    polygon(xs_B, ys_B, col = fill_B, border = col_B, lwd = 2)
    
  } else if (highlight == "A") {
    polygon(xs_A, ys_A, col = fill_hi, border = col_A, lwd = 2)
    polygon(xs_B, ys_B, col = fill_B,  border = col_B, lwd = 2)
    
  } else if (highlight == "B") {
    polygon(xs_A, ys_A, col = fill_A,  border = col_A, lwd = 2)
    polygon(xs_B, ys_B, col = fill_hi, border = col_B, lwd = 2)
    
  } else if (highlight == "AuB") {
    polygon(xs_A, ys_A, col = fill_hi, border = col_A, lwd = 2)
    polygon(xs_B, ys_B, col = fill_hi, border = col_B, lwd = 2)
    
  } else if (highlight == "AiB") {
    polygon(xs_A, ys_A, col = fill_A, border = col_A, lwd = 2)
    polygon(xs_B, ys_B, col = fill_B, border = col_B, lwd = 2)
    # Analytically compute the lens-shaped intersection boundary
    d <- cx_B - cx_A
    if (d < 2 * r) {
      alpha <- acos(d / (2 * r))
      arc_A <- seq(-alpha, alpha, length.out = 200)
      ax <- cx_A + r * cos(arc_A)
      ay <- cy   + r * sin(arc_A)
      arc_B <- seq(pi - alpha, pi + alpha, length.out = 200)
      bx <- cx_B + r * cos(arc_B)
      by <- cy   + r * sin(arc_B)
      lens_x <- c(ax, rev(bx))
      lens_y <- c(ay, rev(by))
      polygon(lens_x, lens_y, col = fill_hi, border = NA)
    }
    
  } else if (highlight == "Ac") {
    rect(-1.75, -1.15, 1.75, 1.15, border = "#888888", lwd = 2,
         col = adjustcolor(col_only, alpha.f = 0.30))
    text(-1.55, 1.0, "S", cex = 1.4, col = "#555555", font = 2)
    polygon(xs_A, ys_A, col = "white", border = col_A, lwd = 2)
    polygon(xs_B, ys_B, col = fill_B,  border = col_B, lwd = 2)
    
  } else if (highlight == "AgivenB") {
    polygon(xs_A, ys_A, col = fill_A,  border = col_A, lwd = 2)
    polygon(xs_B, ys_B, col = fill_B,  border = col_B, lwd = 2)
    # Analytically compute lens for AgivenB highlight
    d <- cx_B - cx_A
    if (d < 2 * r) {
      alpha <- acos(d / (2 * r))
      arc_A <- seq(-alpha, alpha, length.out = 200)
      ax <- cx_A + r * cos(arc_A)
      ay <- cy   + r * sin(arc_A)
      arc_B <- seq(pi - alpha, pi + alpha, length.out = 200)
      bx <- cx_B + r * cos(arc_B)
      by <- cy   + r * sin(arc_B)
      lens_x <- c(ax, rev(bx))
      lens_y <- c(ay, rev(by))
      polygon(lens_x, lens_y, col = fill_hi, border = NA)
    }
    # Thicker dashed B border to indicate "given B"
    polygon(xs_B, ys_B, col = none, border = col_B, lwd = 3, lty = 2)
    
  } else {
    polygon(xs_A, ys_A, col = fill_A, border = col_A, lwd = 2)
    polygon(xs_B, ys_B, col = fill_B, border = col_B, lwd = 2)
  }
  
  # Labels
  text(cx_A - 0.35, 0.75, label_A, col = col_A, cex = 1.5, font = 2)
  text(cx_B + 0.35, 0.75, label_B, col = col_B, cex = 1.5, font = 2)
  
  # Probability annotations
  if (!is.null(pA) && type != "mutex") {
    hi_active    <- !is.null(highlight) && highlight %in% c("AuB", "AiB", "AgivenB")
    label_col    <- if (!is.null(highlight) && highlight == "AuB") "white" else col_A
    label_col_B  <- if (!is.null(highlight) && highlight == "AuB") "white" else col_B
    label_col_AB <- if (hi_active) "white" else col_AB
    text(cx_A - 0.2, 0, paste0("P(A)=", round(pA, 2)),
         col = label_col, cex = 0.85)
    text(cx_B + 0.2, 0, paste0("P(B)=", round(pB, 2)),
         col = label_col_B, cex = 0.85)
    mid_x <- (cx_A + cx_B) / 2
    text(mid_x, 0, paste0("P(A\u2229B)\n=", round(pAB, 2)),
         col = label_col_AB, cex = 0.78, font = 2)
  }
  if (type == "mutex") {
    mutex_col_A <- if (!is.null(highlight) && highlight == "AuB") "white" else col_A
    mutex_col_B <- if (!is.null(highlight) && highlight == "AuB") "white" else col_B
    text(cx_A, 0, paste0("P(A)=", round(pA, 2)), col = mutex_col_A, cex = 0.85)
    text(cx_B, 0, paste0("P(B)=", round(pB, 2)), col = mutex_col_B, cex = 0.85)
    text(0, 0, "P(A\u2229B)=0", col = "#666666", cex = 0.85)
  }
}

# ---------------------------
# Helper: styled info box
# ---------------------------
info_box <- function(..., border_col = "#2196F3") {
  div(style = paste0(
    "border-left: 4px solid ", border_col, ";",
    "background-color: #f0f6ff;",
    "border-radius: 4px;",
    "padding: 10px 14px;",
    "margin-bottom: 10px;",
    "font-size: 95%;"
  ), ...)
}

formula_box <- function(...) {
  div(style = paste0(
    "background-color: #fff8e1;",
    "border: 1px solid #f9a825;",
    "border-radius: 4px;",
    "padding: 10px 14px;",
    "margin: 8px 0;",
    "font-size: 100%;",
    "font-family: 'Courier New', monospace;"
  ), ...)
}

# =========================================================
# UI
# =========================================================
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML(paste0("
      body { font-size: 15px; }
      .nav-tabs > li > a { font-size: 14px; }
      .well { background-color: #f7f7f7; border: 1px solid #ddd; }
      .sidebar { background-color: #f7f7f7; padding: 15px;
                 border-radius: 8px; border: 1px solid #ddd; }
      .btn-run {
        background-color: ", btn_blue, ";
        color: white; border: none;
        padding: 6px 14px; font-size: 14px; margin-top: 4px;
      }
      .btn-run:hover  { background-color: #1769aa; color: white; }
      .btn-run:active { transform: scale(0.97); }
      .result-box {
        background-color: #e8f5e9;
        border: 1px solid #66bb6a;
        border-radius: 4px;
        padding: 10px 14px;
        margin-top: 10px;
        font-size: 100%;
      }
      h4 { color: #333; margin-top: 6px; }
    ")))
  ),
  
  titlePanel("Probability: Concepts and Rules",
             windowTitle = "Probability"),
  
  sidebarLayout(
    
    # ------ SIDEBAR ------
    sidebarPanel(
      width = 3,
      uiOutput("sidebar_controls"),
      tags$hr(),
      helpText("Glenn Tattersall, PhD"),
      helpText("For use in BIOL 3P96 - Biostatistics")
    ),
    
    # ------ MAIN PANEL ------
    mainPanel(
      width = 9,
      
      tabsetPanel(
        id = "main_tabs",
        type = "tabs",
        
        # ---- TAB 1: Concepts & Notation ----
        tabPanel(
          title = "\u2460 Concepts & Notation",
          value = "concepts",
          br(),
          
          fluidRow(
            column(12,
                   info_box(
                     strong("What is probability?"),
                     p("Probability measures how likely an event is to occur. It always falls
                  between 0 (impossible) and 1 (certain). We write the probability of event A
                  as ", strong("P(A)"), "."),
                     border_col = "#1B9E77"
                   )
            )
          ),
          
          fluidRow(
            column(6,
                   h4("Venn Diagram"),
                   plotOutput("venn_concepts", height = "280px"),
                   uiOutput("venn_concepts_caption")
            ),
            column(6,
                   h4("Choose a concept to explore"),
                   uiOutput("concept_explanation")
            )
          ),
          
          br(),
          fluidRow(
            column(12,
                   h4("Key Definitions"),
                   wellPanel(
                     tags$table(
                       style = "width:100%; border-collapse: collapse;",
                       tags$tr(
                         style = "background-color:#e3f2fd;",
                         tags$th(style = "padding:6px 10px;", "Term"),
                         tags$th(style = "padding:6px 10px;", "Meaning"),
                         tags$th(style = "padding:6px 10px;", "Notation")
                       ),
                       tags$tr(
                         tags$td(style = "padding:6px 10px;", strong("Event")),
                         tags$td(style = "padding:6px 10px;",
                                 "Any outcome or set of outcomes we care about"),
                         tags$td(style = "padding:6px 10px;", "A, B, C \u2026")
                       ),
                       tags$tr(style = "background-color:#f5f5f5;",
                               tags$td(style = "padding:6px 10px;", strong("Complement")),
                               tags$td(style = "padding:6px 10px;", "Everything NOT in event A"),
                               tags$td(style = "padding:6px 10px;",
                                       "A\u1d9c, A\u0305, A', !A \u2014 read as 'NOT A'")
                       ),
                       tags$tr(
                         tags$td(style = "padding:6px 10px;", strong("Union")),
                         tags$td(style = "padding:6px 10px;", "A OR B (or both)"),
                         tags$td(style = "padding:6px 10px;", "A \u222a B")
                       ),
                       tags$tr(style = "background-color:#f5f5f5;",
                               tags$td(style = "padding:6px 10px;", strong("Intersection")),
                               tags$td(style = "padding:6px 10px;", "A AND B (both occur)"),
                               tags$td(style = "padding:6px 10px;", "A \u2229 B")
                       ),
                       tags$tr(
                         tags$td(style = "padding:6px 10px;", strong("Mutually Exclusive")),
                         tags$td(style = "padding:6px 10px;",
                                 "A and B cannot both occur; P(A\u2229B) = 0"),
                         tags$td(style = "padding:6px 10px;", "A \u2229 B = \u2205")
                       ),
                       tags$tr(style = "background-color:#f5f5f5;",
                               tags$td(style = "padding:6px 10px;", strong("Independence")),
                               tags$td(style = "padding:6px 10px;",
                                       "Knowing A occurred tells you nothing about B"),
                               tags$td(style = "padding:6px 10px;", "P(A\u2229B) = P(A)\u00d7P(B)")
                       ),
                       tags$tr(
                         tags$td(style = "padding:6px 10px;", strong("Dependence")),
                         tags$td(style = "padding:6px 10px;",
                                 "Knowing A changes the probability of B"),
                         tags$td(style = "padding:6px 10px;", "P(A\u2229B) \u2260 P(A)\u00d7P(B)")
                       )
                     )
                   )
            )
          )
        ), # end Tab 1
        
        # ---- TAB 2: Addition Rule ----
        tabPanel(
          title = "\u2461 Addition Rule",
          value = "addition",
          br(),
          
          fluidRow(
            column(12,
                   info_box(
                     strong("The Addition Rule"),
                     p("Use this rule when you want the probability that event A ",
                       strong("or"), " event B occurs (i.e., the union A \u222a B)."),
                     formula_box(
                       p(strong("General rule:"),
                         "  P(A \u222a B)  =  P(A) + P(B) \u2212 P(A \u2229 B)"),
                       p(strong("Mutually exclusive events:"),
                         "  P(A \u222a B)  =  P(A) + P(B)")
                     ),
                     p("We subtract P(A \u2229 B) to avoid counting the overlap twice.
                  When events are mutually exclusive, there is no overlap, so nothing
                  needs to be subtracted."),
                     border_col = col_B
                   )
            )
          ),
          
          fluidRow(
            column(5,
                   h4("Venn Diagram"),
                   plotOutput("venn_addition", height = "300px")
            ),
            column(7,
                   h4("Try it"),
                   br(),
                   fluidRow(
                     column(6,
                            sliderInput("add_pA", "P(A):", min = 0, max = 1,
                                        value = 0.40, step = 0.05)
                     ),
                     column(6,
                            uiOutput("add_pB_ui")
                     )
                   ),
                   uiOutput("add_pAB_ui"),
                   uiOutput("add_result")
            )
          ),
          br(),
          fluidRow(
            column(12,
                   h4("Biological example"),
                   uiOutput("add_example")
            )
          )
        ), # end Tab 2
        
        # ---- TAB 3: Multiplication Rule ----
        tabPanel(
          title = "\u2462 Multiplication Rule",
          value = "multiplication",
          br(),
          
          fluidRow(
            column(12,
                   info_box(
                     strong("The Multiplication Rule"),
                     p("Use this rule when you want the probability that both A ",
                       strong("and"), " B occur (i.e., the intersection A \u2229 B)."),
                     formula_box(
                       p(strong("General rule (dependent events):"),
                         "  P(A \u2229 B)  =  P(A) \u00d7 P(B | A)"),
                       p(strong("Independent events:"),
                         "  P(A \u2229 B)  =  P(A) \u00d7 P(B)")
                     ),
                     p("P(B | A) is the conditional probability of B ", em("given"), " A has
                  already occurred. When events are independent, knowing A gives no
                  information about B, so P(B | A) = P(B)."),
                     border_col = col_AB
                   )
            )
          ),
          
          fluidRow(
            column(5,
                   h4("Venn Diagram"),
                   plotOutput("venn_mult", height = "300px")
            ),
            column(7,
                   h4("Try it"),
                   br(),
                   radioButtons("mult_type", "Event relationship:",
                                choices = c("Independent" = "indep",
                                            "Dependent"   = "dep"),
                                selected = "indep", inline = TRUE),
                   br(),
                   fluidRow(
                     column(6,
                            sliderInput("mult_pA", "P(A):", min = 0.05, max = 0.90,
                                        value = 0.50, step = 0.05)
                     ),
                     column(6,
                            uiOutput("mult_pB_ui")
                     )
                   ),
                   uiOutput("mult_result")
            )
          ),
          
          br(),
          fluidRow(
            column(12,
                   uiOutput("mult_example")
            )
          )
        ), # end Tab 3
        
        # ---- TAB 4: Conditional Probability & Law of Total Probability ----
        tabPanel(
          title = "\u2463 Conditional Probability",
          value = "conditional",
          br(),
          
          fluidRow(
            column(12,
                   info_box(
                     strong("Conditional Probability"),
                     p("P(A | B) is the probability of A ", em("given"), " that B has already
                  occurred. We restrict our attention to the world where B is true \u2014
                  circle B becomes our new sample space."),
                     formula_box(
                       p(strong("P(A | B)  =  P(A \u2229 B) / P(B)"))
                     ),
                     p("Visually: P(A | B) is the fraction of circle B that overlaps with A.
                  The dashed border on B in the diagram below indicates it is the
                  conditioning event."),
                     border_col = col_only
                   )
            )
          ),
          
          fluidRow(
            column(5,
                   h4("Venn Diagram"),
                   p(em("Dashed border = the conditioning event (B)")),
                   plotOutput("venn_cond", height = "280px")
            ),
            column(7,
                   h4("Try it"),
                   br(),
                   fluidRow(
                     column(6,
                            sliderInput("cond_pA", "P(A):", 0.05, 0.70, 0.40, 0.05)
                     ),
                     column(6,
                            sliderInput("cond_pB", "P(B):", 0.05, 0.70, 0.40, 0.05)
                     )
                   ),
                   sliderInput("cond_pAB", "P(A \u2229 B):", 0.01, 0.50, 0.10, 0.01),
                   uiOutput("cond_result")
            )
          ),
          
          br(),
          fluidRow(
            column(12,
                   h4("Extending the mouse example: conditional probability and the Law of Total Probability"),
                   wellPanel(
                     p("In \u2462 Multiplication Rule we established the following for our hypothetical mouse population:"),
                     tags$ul(
                       tags$li("P(black coat) = 0.60,  so P(white coat) = 0.40"),
                       tags$li("P(long tail) = 0.50"),
                       tags$li("P(long tail | black coat) = 0.75"),
                       tags$li("P(black coat \u2229 long tail) = 0.60 \u00d7 0.75 = 0.45")
                     ),
                     br(),
                     p(strong("Question 1: Given a mouse has a long tail, what is the probability
                  it has a black coat?")),
                     p("We apply the conditional probability formula directly:"),
                     div(class = "result-box",
                         p(strong("P(black coat | long tail)  =  P(black coat \u2229 long tail) / P(long tail)")),
                         p("= 0.45 / 0.50  =  ", strong("0.90")),
                         p("Knowing a mouse has a long tail makes it very likely (90%) to also
                    have a black coat \u2014 much higher than the unconditional P(black coat) = 0.60.
                    This is dependence in action."),
                         p(em("Note: do not confuse P(black coat | long tail) = 0.90 with
                    P(black coat \u2229 long tail) = 0.45. The first is a conditional
                    probability (a fraction of long-tailed mice); the second is a
                    joint probability (a fraction of all mice). Conditional probability
                    is always relative to a restricted group, not the whole population."))
                     ),
                     br(),
                     p(strong("Question 2: What is P(long tail | white coat)?
                  And how does P(long tail) = 0.50 fall out of this?")),
                     p("This is where the ", strong("Law of Total Probability"), " comes in.
                  If we partition the population into black-coated and white-coated mice
                  (two mutually exclusive and exhaustive groups), then:"),
                     formula_box(
                       p(strong("P(long tail)  =  P(long tail | black) \u00d7 P(black)
                            +  P(long tail | white) \u00d7 P(white)"))
                     ),
                     p("We already know three of these four values from \u2462 Multiplication Rule
                  \u2014 P(long tail) = 0.50, P(long tail | black coat) = 0.75, and
                  P(black coat) = 0.60. We can therefore solve for the unknown:"),
                     div(class = "result-box",
                         p(strong("P(long tail | white coat)  =  [P(long tail) \u2212 P(long tail | black) \u00d7 P(black)]
                            \u00f7  P(white)")),
                         p("=  [0.50 \u2212 0.75 \u00d7 0.60]  \u00f7  0.40"),
                         p("=  [0.50 \u2212 0.45]  \u00f7  0.40"),
                         p("=  0.05 \u00f7 0.40  =  ", strong("0.125")),
                         p("White-coated mice have only a 12.5% chance of having a long tail
                    \u2014 far lower than the 75% seen in black-coated mice. The Law of
                    Total Probability let us derive this from information we already had,
                    by treating P(long tail) as a ", strong("weighted average"),
                           " of the conditional probabilities across the two coat colour groups.")
                     ),
                     br(),
                     p(strong("Question 3: What is P(short tail | white coat)?")),
                     p("Since short and long are the only tail options:"),
                     div(class = "result-box",
                         p(strong("P(short tail | white coat)  =  1 \u2212 P(long tail | white coat)")),
                         p("=  1 \u2212 0.125  =  ", strong("0.875")),
                         p("White-coated mice have a very high probability (87.5%) of having
                    short tails \u2014 the complementary linked traits cluster together too.")
                     )
                   )
            )
          )
        ), # end Tab 4
        
        # ---- TAB 5: Bayes' Theorem ----
        tabPanel(
          title = "\u2464 Bayes' Theorem",
          value = "bayes",
          br(),
          
          fluidRow(
            column(12,
                   info_box(
                     strong("Bayes' Theorem"),
                     p("Bayes' theorem lets us ", strong("update a prior belief"),
                       " about an event when we receive new evidence. No new rules are
                       introduced here \u2014 it is built entirely from rules covered
                       in \u2462 and \u2463. Here is how it is derived:"),
                     p(strong("Step 1 \u2014 start with conditional probability (\u2463):")),
                     formula_box(
                       p(strong("P(A | B)  =  P(A \u2229 B)  \u00f7  P(B)"))
                     ),
                     p(strong("Step 2 \u2014 replace the numerator using the Multiplication Rule (\u2462):")),
                     p("From \u2462 we know P(A \u2229 B) = P(B | A) \u00d7 P(A).
                       Substituting this into the numerator:"),
                     formula_box(
                       p(strong("P(A | B)  =  [P(B | A) \u00d7 P(A)]  \u00f7  P(B)"))
                     ),
                     p(strong("Step 3 \u2014 replace the denominator using the Law of Total Probability (\u2463):")),
                     p("From \u2463 we know that P(B) can be written as a weighted sum across
                       all mutually exclusive groups. For two groups A and A\u1d9c:"),
                     formula_box(
                       p(strong("P(B)  =  P(B | A) \u00d7 P(A)  +  P(B | A\u1d9c) \u00d7 P(A\u1d9c)"))
                     ),
                     p("Substituting this into the denominator gives the full Bayes' theorem:"),
                     formula_box(
                       p(strong("P(A | B)  =  [P(B | A) \u00d7 P(A)] \u00f7")),
                       p(strong("[P(B | A) \u00d7 P(A)  +  P(B | A\u1d9c) \u00d7 P(A\u1d9c)]"))
                     ),
                     p(strong("Step 4 \u2014 applied to medical testing:")),
                     p("Substituting Disease for A and Test+ for B gives the Probability of the Disease, given a positive test:"),
                     formula_box(
                       p(strong("P(Disease | Test+) =  [P(Test+ | Disease) \u00d7 P(Disease)] \u00f7 ")),
                       p(strong(" [P(Test+ | Disease) \u00d7 P(Disease)  +  P(Test+ | No Disease) \u00d7 P(No Disease)]")),
                    
                       p("where:"),
                       p(strong("P(Test+ | Disease)"), " = Sensitivity (true positive rate)"),
                       p(strong("P(Disease)"), " = Prevalence in population (prior probability)"),
                       p(strong("P(Test+ | No Disease)"),  " = 1 \u2212 Specificity (false positive rate)")
                     ),
                     border_col = "#E91E63"
                   )
            )
          ),
          
          fluidRow(
            column(5,
                   h4("Results"),
                   br(),
                   div(class = "result-box", uiOutput("bayes_result")),
                   br(),
                   p(strong("PPV"), " (Positive Predictive Value): the probability that a
                person ", strong("truly has"), " the disease given they tested positive.
                This is what patients and clinicians most want to know after a positive result."),
                   p(strong("NPV"), " (Negative Predictive Value): the probability that a
                person ", strong("truly does not have"), " the disease given they tested
                negative."),
                   br(),
                   h4("Predicted outcomes in 10 000 people"),
                   plotOutput("bayes_bar", height = "260px")
            ),
            column(7,
                   h4("Adjust the parameters"),
                   br(),
                   sliderInput("prev",
                               "Prevalence = P(Disease):",
                               min = 0.001, max = 0.500,
                               value = 0.010, step = 0.001),
                   sliderInput("sens",
                               "Sensitivity = P(Test+ | Disease):",
                               min = 0.50, max = 1.00,
                               value = 0.95, step = 0.01),
                   sliderInput("spec",
                               "Specificity = P(Test\u2212 | No Disease):",
                               min = 0.50, max = 1.00,
                               value = 0.95, step = 0.01),
                   br(),
                   info_box(
                     strong("Why does prevalence matter so much?"),
                     p("Even a very accurate test can produce many false positives when
                  the disease is rare (low prevalence). Drag the prevalence slider
                  to a very low value and watch what happens to the Positive
                  Predictive Value (PPV). This is sometimes called the ",
                       strong("base rate fallacy.")),
                     border_col = "#9C27B0"
                   )
            )
          )
          ,br(),
          fluidRow(
            column(12,
                   h4("Worked examples: check the sliders yourself"),
                   wellPanel(
                     p("The examples below use real world values. Set the sliders to the values given and check whether the app returns
                  the correct PPV and NPV."),
                     br(),
                     p(strong("Example 1 \u2014 COVID-19 rapid antigen test")),
                     p("A PMC-published analysis of COVID-19 testing (Stites & Wilen, 2021,
                  PMC8698426) uses a test with 90% sensitivity and 95% specificity
                  in a population with 5% prevalence \u2014 typical of moderate community spread."),
                     p("Set the sliders to:"),
                     tags$ul(
                       tags$li("Prevalence = 0.050"),
                       tags$li("Sensitivity = 0.90"),
                       tags$li("Specificity = 0.95")
                     ),
                     div(class = "result-box",
                         p(strong("Expected: PPV \u2248 48.6%,  NPV \u2248 99.4%")),
                         p("Even with an accurate test, fewer than half (i.e. 48.6%) of positive results
                    are true positives when prevalence is only 5%. The other half
                    are false alarms."),
                         p(em("This is why mass COVID-19 screening in very low-prevalence
                    populations \u2014 such as New Zealand during its elimination
                    periods \u2014 generated many false positives. A positive result
                    where disease is rare is more likely to be wrong than right,
                    even with an accurate test."))
                     ),
                     br(),
                     p(strong("Example 2 \u2014 Mammography screening for breast cancer")),
                     p("A widely cited analysis (catchbio.com, drawing on US screening data)
                  reports mammography sensitivity of 72% and specificity of 98%.
                  In women aged 45 at average risk, breast cancer prevalence is
                  approximately 0.2% \u2014 about 2 in every 1000 women."),
                     p("Set the sliders to:"),
                     tags$ul(
                       tags$li("Prevalence = 0.002"),
                       tags$li("Sensitivity = 0.72"),
                       tags$li("Specificity = 0.98")
                     ),
                     div(class = "result-box",
                         p(strong("Expected: PPV \u2248 6.7%,  NPV \u2248 99.9%")),
                         p("A positive mammogram in a 45-year-old at average risk has only
                    about a 1-in-15 chance of being a true cancer. The remaining
                    93% of positive results are false positives. The NPV is
                    excellent: a negative result is very reassuring."),
                         p(em("This is why routine mammography is generally not recommended
                    for women under 40\u201345 at average risk. When prevalence is
                    very low, positive results are overwhelmingly false alarms,
                    leading to unnecessary biopsies, anxiety, and cost. Screening
                    is most valuable when applied to populations where the disease
                    is common enough for a positive result to carry real meaning."))
                     ),
                     br(),
                     p(strong("Example 3 \u2014 Thermal camera fever screening at airports")),
                     p("During the COVID-19 pandemic, thermal cameras were widely deployed
                  at airports worldwide to flag passengers with elevated skin temperature.
                  A review published in the International Journal of Environmental Research
                  and Public Health (MDPI, PMC8004954) found that infrared thermoscanners
                  at airports had sensitivity ranging from 51\u201370% and specificity
                  from 64\u201382%. Using mid-range values, and assuming 2% prevalence
                  among travellers during a pandemic period:"),
                     p("Set the sliders to:"),
                     tags$ul(
                       tags$li("Prevalence = 0.020"),
                       tags$li("Sensitivity = 0.70"),
                       tags$li("Specificity = 0.75")
                     ),
                     div(class = "result-box",
                         p(strong("Expected: PPV \u2248 5.4%,  NPV \u2248 99.2%")),
                         p("With P(Test+) = 0.259, about 1 in 4 passengers would be flagged
                    by the camera \u2014 yet only 1 in 18 of those flagged actually has
                    a fever-related illness."),
                         p(strong("What does this mean at Toronto Pearson (YYZ)?")),
                         p("Toronto Pearson handled 46.8 million passengers in 2024 (GTAA Annual
                    Results, 2025) \u2014 approximately 128,000 per day. With P(Test+) = 0.259,
                    around 33,000 passengers per day would be flagged for secondary
                    screening. If each required just 5 minutes of agent time, that amounts
                    to roughly 2,750 hours of expert agent time every day \u2014 equivalent
                    to about 345 full-time agents employed solely to process thermal
                    camera alerts, the vast majority of which are false alarms."),
                         p(em("This is a real-world consequence of deploying a low-specificity
                    screening test in a low-prevalence population. Thermal cameras were
                    sold to airports rapidly during the SARS pandemic in 2003, often before clinical
                    trials had established their sensitivity or specificity. A PubMed
                    review (PMC33786335) noted that reported sensitivities in airport
                    settings were as low as zero for some diseases. Airports have largely
                    abandoned thermal screening, in part because the operational burden
                    of processing false positives proved unmanageable at scale."))
                     ),
                     br(),
                     info_box(
                       strong("The general lesson: prevalence drives the value of a positive test"),
                       p("Sensitivity and specificity are fixed properties of a test.
                    But PPV \u2014 what a positive result actually means for a patient
                    \u2014 depends critically on how common the disease is in the
                    population being tested. The rarer the disease, the more false
                    positives swamp true positives. This is why screening programmes
                    are designed carefully around population prevalence, and why a
                    test that works well in a high-risk clinic may be misleading if
                    applied to the general public."),
                       border_col = "#9C27B0"
                     )
                   )
            )
          )
        ) # end Tab 5
        
      ) # end tabsetPanel
    ) # end mainPanel
  ) # end sidebarLayout
) # end fluidPage


# =========================================================
# SERVER
# =========================================================
server <- function(input, output, session) {
  
  # ===========================================================
  # SIDEBAR: dynamic controls per tab
  # ===========================================================
  output$sidebar_controls <- renderUI({
    tab <- input$main_tabs
    if (is.null(tab)) tab <- "concepts"
    
    if (tab == "concepts") {
      tagList(
        h4("\u2460 Explore Concepts"),
        radioButtons("concept", "Select a concept:",
                     choices = c(
                       "Events A and B"          = "both",
                       "Event A only"            = "A",
                       "Event B only"            = "B",
                       "Complement of A"         = "Ac",
                       "Union  A \u222a B"       = "AuB",
                       "Intersection A \u2229 B" = "AiB"
                     ),
                     selected = "both"),
        tags$hr(),
        radioButtons("rel_type", "Relationship:",
                     choices = c(
                       "Overlapping (dependent)" = "overlap",
                       "Mutually exclusive"      = "mutex",
                       "Independent"             = "indep"
                     ),
                     selected = "overlap")
      )
      
    } else if (tab == "addition") {
      tagList(
        h4("\u2461 Addition Rule"),
        radioButtons("add_type", "Event relationship:",
                     choices = c(
                       "Overlapping"        = "overlap",
                       "Mutually exclusive" = "mutex"
                     ),
                     selected = "overlap"),
        tags$hr(),
        radioButtons("add_highlight", "Highlight region:",
                     choices = c(
                       "Union A \u222a B"         = "AuB",
                       "Event A"                 = "A",
                       "Event B"                 = "B",
                       "Intersection A \u2229 B"  = "AiB"
                     ),
                     selected = "AuB")
      )
      
    } else if (tab == "multiplication") {
      tagList(
        h4("\u2462 Multiplication Rule"),
        p("Adjust sliders on the right panel.", style = "color:#555; font-size:90%;"),
        tags$hr(),
        radioButtons("mult_highlight", "Highlight region:",
                     choices = c(
                       "Intersection A \u2229 B" = "AiB",
                       "Event A"                = "A",
                       "Event B"                = "B"
                     ),
                     selected = "AiB")
      )
      
    } else if (tab == "conditional") {
      tagList(
        h4("\u2463 Conditional Probability"),
        p("Adjust sliders on the right panel.", style = "color:#555; font-size:90%;"),
        tags$hr(),
        p(strong("Key idea:")),
        p("P(A | B) asks: given B has occurred, how likely is A?"),
        br(),
        p(strong("Law of Total Probability:")),
        p("P(B) = P(B|A)\u00d7P(A) + P(B|A\u1d9c)\u00d7P(A\u1d9c)"),
        p("The overall probability of B is a weighted average of its conditional
          probabilities across all partitions of the sample space.")
      )
      
    } else if (tab == "bayes") {
      tagList(
        h4("\u2464 Bayes' Theorem"),
        p("Adjust sliders on the right panel.", style = "color:#555; font-size:90%;"),
        tags$hr(),
        p(strong("Key terms:")),
        tags$ul(
          tags$li(strong("Sensitivity:"), " P(Test+ | Disease)"),
          tags$li(strong("Specificity:"), " P(Test\u2212 | No Disease)"),
          tags$li(strong("PPV:"), " P(Disease | Test+)"),
          tags$li(strong("NPV:"), " P(No Disease | Test\u2212)")
        )
      )
    }
  })
  
  # ===========================================================
  # TAB 1: Concepts Venn
  # ===========================================================
  output$venn_concepts <- renderPlot({
    req(input$concept, input$rel_type)
    draw_venn(type      = input$rel_type,
              highlight = input$concept)
  }, bg = col_bg)
  
  output$venn_concepts_caption <- renderUI({
    req(input$concept, input$rel_type)
    cap <- switch(input$concept,
                  "both" = "Both events A and B are shown in the sample space S.",
                  "A"    = "Event A is highlighted (pink). It represents all outcomes where A occurs.",
                  "B"    = "Event B is highlighted (pink). It represents all outcomes where B occurs.",
                  "Ac"   = "The complement of A (A\u1d9c) is highlighted \u2014 every outcome in S that is NOT in A.",
                  "AuB"  = "The union A \u222a B (pink) includes every outcome in A or B or both.",
                  "AiB"  = "The intersection A \u2229 B (pink) includes only outcomes in BOTH A and B."
    )
    rel_note <- switch(input$rel_type,
                       "overlap" = "The circles overlap \u2014 events share outcomes (dependent).",
                       "mutex"   = "The circles do not overlap \u2014 events are mutually exclusive.",
                       "indep"   = "The overlap reflects independence: P(A\u2229B) = P(A)\u00d7P(B)."
    )
    div(style = "font-size:88%; color:#555; margin-top:4px;",
        em(cap), br(), em(rel_note))
  })
  
  output$concept_explanation <- renderUI({
    req(input$concept, input$rel_type)
    rel <- input$rel_type
    con <- input$concept
    
    rel_text <- switch(rel,
                       "overlap" = tagList(p(strong("Overlapping (dependent) events:"),
                                             "Some outcomes belong to both A and B. Knowing A occurred changes the probability of B.")),
                       "mutex"   = tagList(p(strong("Mutually exclusive events:"),
                                             "A and B cannot happen at the same time. If A occurs, B is impossible,
        and vice versa. P(A \u2229 B) = 0.")),
                       "indep"   = tagList(p(strong("Independent events:"),
                                             "Knowing A occurred gives no information about B.
        The probability of B is unchanged. P(A \u2229 B) = P(A) \u00d7 P(B)."))
    )
    
    con_text <- switch(con,
                       "both" = p("Both circles represent events (sets of outcomes). The rectangle is the
                  sample space S \u2014 all possible outcomes."),
                       "A"    = p("Event A (pink) is any specific outcome or set of outcomes we define.
                  For example, A = 'patient tests positive'."),
                       "B"    = p("Event B (pink) is another event of interest.
                  For example, B = 'patient actually has the disease'."),
                       "Ac"   = p("The complement A\u1d9c (pink region) contains every outcome in S that is
                  not in A. Note: P(A) + P(A\u1d9c) = 1."),
                       "AuB"  = p("The union A \u222a B (pink) means A ", strong("or"), " B or both occur.
                  This is what the \u2461 Addition Rule calculates."),
                       "AiB"  = if (input$rel_type == "mutex") {
                         p("The intersection A \u2229 B is ", strong("empty"),
                           " \u2014 A and B share no outcomes. This is what it means to be mutually exclusive.")
                       } else {
                         p("The intersection A \u2229 B (pink) means both A ", strong("and"),
                           " B occur simultaneously. This is what the \u2462 Multiplication Rule calculates.")
                       }
    )
    
    tagList(
      info_box(con_text, border_col = col_A),
      info_box(rel_text, border_col = col_B)
    )
  })
  
  # ===========================================================
  # TAB 2: Addition Rule
  # ===========================================================
  output$add_pB_ui <- renderUI({
    req(input$add_pA, input$add_type)
    if (!is.null(input$add_type) && input$add_type == "mutex") {
      pB_max <- round(1 - input$add_pA, 2)
      pB_max <- max(pB_max, 0)
      sliderInput("add_pB", "P(B):", min = 0, max = pB_max,
                  value = min(0.35, pB_max), step = 0.05)
    } else {
      sliderInput("add_pB", "P(B):", min = 0, max = 1,
                  value = 0.35, step = 0.05)
    }
  })
  
  add_pAB_max <- reactive({
    req(input$add_pA, input$add_pB)
    if (!is.null(input$add_type) && input$add_type == "mutex") return(0)
    round(min(input$add_pA, input$add_pB), 2)
  })
  
  add_pAB_min <- reactive({
    req(input$add_pA, input$add_pB)
    if (!is.null(input$add_type) && input$add_type == "mutex") return(0)
    round(max(0, input$add_pA + input$add_pB - 1), 2)
  })
  
  output$add_pAB_ui <- renderUI({
    req(input$add_type, input$add_pA, input$add_pB)
    if (input$add_type == "mutex") return(NULL)
    pAB_min <- add_pAB_min()
    pAB_max <- add_pAB_max()
    pAB_val <- min(max(0.10, pAB_min), pAB_max)
    sliderInput("add_pAB", "P(A \u2229 B):",
                min = pAB_min, max = pAB_max,
                value = pAB_val, step = 0.01)
  })
  
  add_pAB_val <- reactive({
    req(input$add_type)
    if (input$add_type == "mutex") return(0)
    req(input$add_pAB)
    min(input$add_pAB, add_pAB_max())
  })
  
  output$venn_addition <- renderPlot({
    req(input$add_type, input$add_pA, input$add_pB)
    hi <- if (!is.null(input$add_highlight)) input$add_highlight else "AuB"
    if (input$add_type == "mutex" && hi == "AiB") hi <- "AuB"
    draw_venn(type      = input$add_type,
              highlight = hi,
              pA  = input$add_pA,
              pB  = input$add_pB,
              pAB = add_pAB_val())
  }, bg = col_bg)
  
  output$add_result <- renderUI({
    req(input$add_pA, input$add_pB, input$add_type)
    pA   <- input$add_pA
    pB   <- input$add_pB
    pAB  <- add_pAB_val()
    pAuB <- pA + pB - pAB
    
    if (input$add_type == "mutex") {
      formula_str <- paste0("P(A \u222a B)  =  P(A) + P(B)  =  ",
                            pA, " + ", pB, "  =  ", round(pAuB, 3))
      note <- "Events are mutually exclusive, so P(A \u2229 B) = 0 and nothing is subtracted."
    } else {
      formula_str <- paste0("P(A \u222a B)  =  ", pA, " + ", pB, " \u2212 ", pAB,
                            "  =  ", round(pAuB, 3))
      note <- "We subtract P(A \u2229 B) to avoid counting the overlap twice."
    }
    
    div(class = "result-box",
        p(strong("Calculation:")),
        tags$code(formula_str),
        br(), br(),
        p(note),
        p(strong("Answer: P(A \u222a B) = "), strong(round(pAuB, 3)))
    )
  })
  
  output$add_example <- renderUI({
    req(input$add_type)
    
    if (input$add_type == "overlap") {
      wellPanel(
        p("Blood type is determined by two independent systems: the ",
          strong("ABO group"), " (A, B, AB, or O) and the ",
          strong("Rh factor"), " (+ or \u2212). Because these are independent,
          their probabilities multiply \u2014 but the ABO groups themselves are
          mutually exclusive, so we can add them freely."),
        p("Approximate proportions in the Canadian population:"),
        tags$table(
          style = "width:100%; border-collapse: collapse; font-size: 93%;",
          tags$tr(
            style = "background-color:#e3f2fd;",
            tags$th(style = "padding:5px 10px;", "Blood Type"),
            tags$th(style = "padding:5px 10px;", "Proportion"),
            tags$th(style = "padding:5px 10px;", "Blood Type"),
            tags$th(style = "padding:5px 10px;", "Proportion")
          ),
          tags$tr(
            tags$td(style = "padding:5px 10px;", "O+"),
            tags$td(style = "padding:5px 10px;", "0.39"),
            tags$td(style = "padding:5px 10px;", "O\u2212"),
            tags$td(style = "padding:5px 10px;", "0.07")
          ),
          tags$tr(style = "background-color:#f5f5f5;",
                  tags$td(style = "padding:5px 10px;", "A+"),
                  tags$td(style = "padding:5px 10px;", "0.36"),
                  tags$td(style = "padding:5px 10px;", "A\u2212"),
                  tags$td(style = "padding:5px 10px;", "0.06")
          ),
          tags$tr(
            tags$td(style = "padding:5px 10px;", "B+"),
            tags$td(style = "padding:5px 10px;", "0.08"),
            tags$td(style = "padding:5px 10px;", "B\u2212"),
            tags$td(style = "padding:5px 10px;", "0.01")
          ),
          tags$tr(style = "background-color:#f5f5f5;",
                  tags$td(style = "padding:5px 10px;", "AB+"),
                  tags$td(style = "padding:5px 10px;", "0.025"),
                  tags$td(style = "padding:5px 10px;", "AB\u2212"),
                  tags$td(style = "padding:5px 10px;", "0.005")
          )
        ),
        br(),
        p("Let A = 'person has type A blood' (A+ or A\u2212) \u2192 P(A) = 0.36 + 0.06 = 0.42"),
        p("Let B = 'person is Rh positive' (O+, A+, B+, or AB+) \u2192 P(B) = 0.39 + 0.36 + 0.08 + 0.025 = 0.855"),
        p("These events ", strong("overlap"), ": a person can be both type A ", em("and"),
          " Rh positive (i.e. A+). P(A \u2229 B) = 0.36."),
        div(class = "result-box",
            p(strong("P(type A or Rh+)  =  P(A) + P(B) \u2212 P(A \u2229 B)")),
            p("= 0.42 + 0.855 \u2212 0.36  =  ", strong("0.915")),
            p("About 91.5% of people are either type A or Rh positive (or both).")
        )
      )
    } else {
      wellPanel(
        p("The ABO blood groups (O, A, B, AB) are ", strong("mutually exclusive"),
          " \u2014 a person belongs to exactly one group. This makes the Addition Rule
          especially clean: we simply add the proportions."),
        p("Approximate proportions in the Canadian population:"),
        tags$table(
          style = "width:100%; border-collapse: collapse; font-size: 93%;",
          tags$tr(
            style = "background-color:#e3f2fd;",
            tags$th(style = "padding:5px 10px;", "ABO Group"),
            tags$th(style = "padding:5px 10px;", "Proportion (Rh+ and Rh\u2212 combined)")
          ),
          tags$tr(
            tags$td(style = "padding:5px 10px;", "Type O"),
            tags$td(style = "padding:5px 10px;", "0.46")
          ),
          tags$tr(style = "background-color:#f5f5f5;",
                  tags$td(style = "padding:5px 10px;", "Type A"),
                  tags$td(style = "padding:5px 10px;", "0.42")
          ),
          tags$tr(
            tags$td(style = "padding:5px 10px;", "Type B"),
            tags$td(style = "padding:5px 10px;", "0.09")
          ),
          tags$tr(style = "background-color:#f5f5f5;",
                  tags$td(style = "padding:5px 10px;", "Type AB"),
                  tags$td(style = "padding:5px 10px;", "0.03")
          )
        ),
        br(),
        p("What is the probability that a randomly selected person is type A ",
          strong("or"), " type B?"),
        p("Since these groups are mutually exclusive, P(A \u2229 B) = 0."),
        div(class = "result-box",
            p(strong("P(type A or type B)  =  P(A) + P(B)")),
            p("= 0.42 + 0.09  =  ", strong("0.51")),
            p("About 51% of people are either type A or type B.")
        )
      )
    }
  })
  
  # ===========================================================
  # TAB 3: Multiplication Rule
  # ===========================================================
  output$mult_pB_ui <- renderUI({
    req(input$mult_type)
    if (input$mult_type == "indep") {
      sliderInput("mult_pB", "P(B):", min = 0.05, max = 0.90, value = 0.60, step = 0.05)
    } else {
      sliderInput("mult_pBgivenA", "P(B | A):", min = 0.05, max = 1.00, value = 0.70, step = 0.05)
    }
  })
  
  mult_pAB <- reactive({
    req(input$mult_pA, input$mult_type)
    if (input$mult_type == "indep") {
      req(input$mult_pB)
      input$mult_pA * input$mult_pB
    } else {
      req(input$mult_pBgivenA)
      input$mult_pA * input$mult_pBgivenA
    }
  })
  
  output$venn_mult <- renderPlot({
    req(input$mult_type)
    hi    <- if (!is.null(input$mult_highlight)) input$mult_highlight else "AiB"
    vtype <- if (input$mult_type == "indep") "indep" else "overlap"
    draw_venn(type      = vtype,
              highlight = hi,
              pA  = input$mult_pA,
              pB  = if (input$mult_type == "indep") input$mult_pB else input$mult_pBgivenA,
              pAB = mult_pAB())
  }, bg = col_bg)
  
  output$mult_result <- renderUI({
    req(input$mult_pA, input$mult_type)
    pA  <- input$mult_pA
    pAB <- mult_pAB()
    
    if (input$mult_type == "indep") {
      req(input$mult_pB)
      pB <- input$mult_pB
      formula_str <- paste0("P(A \u2229 B)  =  P(A) \u00d7 P(B)  =  ",
                            pA, " \u00d7 ", pB, "  =  ", round(pAB, 3))
      note <- "Events are independent: knowing A occurred does not change P(B)."
    } else {
      req(input$mult_pBgivenA)
      pBgA <- input$mult_pBgivenA
      formula_str <- paste0("P(A \u2229 B)  =  P(A) \u00d7 P(B | A)  =  ",
                            pA, " \u00d7 ", pBgA, "  =  ", round(pAB, 3))
      note <- "Events are dependent: P(B | A) differs from P(B) because A gives information about B."
    }
    
    div(class = "result-box",
        p(strong("Calculation:")),
        tags$code(formula_str),
        br(), br(),
        p(note),
        p(strong("Answer: P(A \u2229 B) = "), strong(round(pAB, 3)))
    )
  })
  
  output$mult_example <- renderUI({
    req(input$mult_type)
    
    if (input$mult_type == "indep") {
      tagList(
        h4("Medical example: sequential diagnostic tests"),
        wellPanel(
          p("A patient must test positive on two ", strong("independent"),
            " screening tests before a diagnosis is made."),
          p("Test 1 has sensitivity (true positive rate) of 90% \u2014 i.e. P(T1+) = 0.90."),
          p("Test 2 has sensitivity of 85% \u2014 i.e. P(T2+) = 0.85."),
          p("What is the probability of testing positive on ", strong("both"), "?"),
          div(class = "result-box",
              p(strong("P(T1+ \u2229 T2+) = P(T1+) \u00d7 P(T2+)")),
              p("= 0.90 \u00d7 0.85 = ", strong("0.765")),
              p("There is roughly a 76.5% chance of both tests returning positive
               for someone who truly has the condition."),
              p(em("Note: requiring multiple positive tests always reduces the overall
               sensitivity \u2014 since no test is perfect, each additional test adds
               another chance of a false negative. Sequential testing trades
               sensitivity for specificity."))
          )
        )
      )
    } else {
      tagList(
        h4("Biological example: linked traits in mice"),
        wellPanel(
          p("To illustrate dependent events, consider a ", strong("hypothetical"),
            " mouse population where coat colour and tail length behave as ",
            strong("linked"), " traits \u2014 meaning their genes travel together on
            the same chromosome. In reality, linkage depends on the specific genes
            involved and their chromosomal distance, but here we use made-up
            numbers to clearly illustrate the concept of dependence."),
          p("In this hypothetical population:"),
          tags$ul(
            tags$li("P(black coat) = 0.60"),
            tags$li("P(long tail) = 0.50"),
            tags$li("P(long tail | black coat) = 0.75 \u2014 higher than 0.50 because the
                     genes travel together on the same chromosome")
          ),
          p("Because the traits are ", strong("dependent"),
            ", we use the general multiplication rule:"),
          div(class = "result-box",
              p(strong("P(black coat \u2229 long tail) = P(black coat) \u00d7 P(long tail | black coat)")),
              p("= 0.60 \u00d7 0.75 = ", strong("0.45")),
              br(),
              p("Compare this to what we would get if we ", strong("incorrectly"),
                " assumed the traits were independent:"),
              p("P(black coat) \u00d7 P(long tail) = 0.60 \u00d7 0.50 = ", strong("0.30")),
              p("The true joint probability (0.45) is ", strong("50% higher"),
                " than the independent estimate (0.30). The linkage between genes means
              black-coated mice are much more likely to also have long tails than
              chance alone would predict."),
              p(em("This example is continued in \u2463 Conditional Probability, where we use
              conditional probability and the Law of Total Probability to explore
              these results further."))
          )
        )
      )
    }
  })
  
  # ===========================================================
  # TAB 4: Conditional Probability
  # ===========================================================
  output$venn_cond <- renderPlot({
    req(input$cond_pA, input$cond_pB, input$cond_pAB)
    pAB_safe <- min(input$cond_pAB, input$cond_pA * 0.95, input$cond_pB * 0.95)
    draw_venn(type      = "overlap",
              highlight = "AgivenB",
              pA  = input$cond_pA,
              pB  = input$cond_pB,
              pAB = pAB_safe)
  }, bg = col_bg)
  
  output$cond_result <- renderUI({
    req(input$cond_pA, input$cond_pB, input$cond_pAB)
    pB  <- input$cond_pB
    pAB <- min(input$cond_pAB, input$cond_pA * 0.95, pB * 0.95)
    if (pB <= 0) return(p("P(B) must be greater than 0."))
    pAgivenB <- pAB / pB
    
    div(class = "result-box",
        p(strong("P(A | B)  =  P(A \u2229 B) / P(B)")),
        tags$code(paste0(round(pAB, 2), " / ", round(pB, 2),
                         "  =  ", round(pAgivenB, 3))),
        br(), br(),
        p("Restricting to outcomes in B, the fraction that are also in A is ",
          strong(paste0(round(pAgivenB * 100, 1), "%")), ".")
    )
  })
  
  # ===========================================================
  # TAB 5: Bayes' Theorem
  # ===========================================================
  bayes_vals <- reactive({
    req(input$prev, input$sens, input$spec)
    prev <- input$prev
    sens <- input$sens
    fpr  <- 1 - input$spec
    
    p_pos <- sens * prev + fpr * (1 - prev)
    p_neg <- (1 - sens) * prev + input$spec * (1 - prev)
    
    ppv <- (sens * prev) / p_pos
    npv <- (input$spec * (1 - prev)) / p_neg
    
    list(ppv = ppv, npv = npv, p_pos = p_pos,
         sens = sens, spec = input$spec,
         prev = prev, fpr = fpr)
  })
  
  output$bayes_result <- renderUI({
    v <- bayes_vals()
    tagList(
      tags$table(
        style = "width:100%;",
        tags$tr(
          tags$td(style = "padding:4px;", strong("Prevalence P(Disease):")),
          tags$td(style = "padding:4px;", paste0(round(v$prev * 100, 2), "%"))
        ),
        tags$tr(
          tags$td(style = "padding:4px;", strong("P(Test+):")),
          tags$td(style = "padding:4px;", paste0(round(v$p_pos * 100, 2), "%"))
        ),
        tags$tr(
          style = "background-color:#c8e6c9;",
          tags$td(style = "padding:4px;",
                  strong("PPV = P(Disease | Test+):")),
          tags$td(style = "padding:4px; font-size:105%; font-weight:bold;",
                  paste0(round(v$ppv * 100, 1), "%"))
        ),
        tags$tr(
          tags$td(style = "padding:4px;",
                  strong("NPV = P(No Disease | Test\u2212):")),
          tags$td(style = "padding:4px;",
                  paste0(round(v$npv * 100, 1), "%"))
        )
      )
    )
  })
  
  output$bayes_bar <- renderPlot({
    v <- bayes_vals()
    N <- 10000
    
    tp <- round(v$sens * v$prev * N)
    fn <- round((1 - v$sens) * v$prev * N)
    fp <- round(v$fpr * (1 - v$prev) * N)
    tn <- round(v$spec * (1 - v$prev) * N)
    
    counts <- c("True\nPositive"  = tp,
                "False\nNegative" = fn,
                "False\nPositive" = fp,
                "True\nNegative"  = tn)
    bar_cols <- c(col_A, adjustcolor(col_A, 0.4),
                  adjustcolor(col_B, 0.4), col_B)
    
    old_par <- par(mar = c(4, 5, 1, 1), bg = col_bg)
    on.exit(par(old_par))
    
    bp <- barplot(counts, col = bar_cols, border = NA,
                  ylab = "Number of people (out of 10 000)",
                  cex.names = 0.88, cex.axis = 0.88, cex.lab = 0.88,
                  ylim = c(0, max(counts) * 1.12),
                  las = 1)
    text(bp, counts + max(counts) * 0.02, labels = counts,
         cex = 0.85, col = "#333333")
  }, bg = col_bg)
  
} # end server

shinyApp(ui = ui, server = server)