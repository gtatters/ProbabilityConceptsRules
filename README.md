# Probability: Concepts and Rules

An interactive Shiny app for BIOL 3P96 (Biostatistics) at Brock University.

## What this app does

This app walks through the core rules of probability using interactive Venn
diagrams, sliders, and worked numerical examples. Five tabs cover the major
topics in sequence:
  
1. **Concepts & Notation** — events, complements, unions, intersections
2. **Addition Rule** — P(A ∪ B) = P(A) + P(B) − P(A ∩ B), with the
special case of mutually exclusive events
3. **Multiplication Rule** — P(A ∩ B) and the distinction between
independent and dependent events
4. **Conditional Probability & Law of Total Probability** — P(A | B)
and how overall probabilities decompose across partitions
5. **Bayes' Theorem** — how a positive test result changes with disease
   prevalence; sensitivity, specificity, PPV, and NPV

All diagrams update live as you move the sliders.

## Learning goals

- Read and interpret standard probability notation
- Apply the addition and multiplication rules correctly
- Calculate conditional probabilities and understand what they mean
- Interpret a medical screening test result using Bayes' theorem

## Course context

Developed for BIOL 3P96 — Biostatistics, Brock University.
Built with R and Shiny (base R graphics only).