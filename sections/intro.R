getIntro <- function() {
  fluidRow(
    box(
      h2("Transition Network Analysis (TNA)"),
      # Title and Acronym
      p(
        "Transition Network Analysis (TNA) is designed for analyzing 
                transition networks, providing methods for examining sequences, 
                identifying communities, calculating centrality measures, and
                visualizing network dynamics. TNA was presented for the first 
                time at the Learning Analytics & Knowledge conference (2025).",
        tags$a(
          "Check out our paper",
          href = "https://dl.acm.org/doi/10.1145/3706468.3706513"),
        "."
      ),
      # Description
      h3("Usage"),
      p(
        "TNA offers a set of tools for researchers and analysts working with 
                transition networks. It allows users to analyze sequences in data, 
                detect community structures, compute various centrality measures, 
                and visualize transitions within networks. It can be used from the R 
                package tna or through the Shiny interface",
        tags$a(
          "Check the package documentation",
          href = "https://sonsoles.me/tna/"),
        "."
      ),
      tags$ul(
        tags$li(
          tags$b("Transition Analysis"),
          ":  Understand transitions and connections in sequential data through various analytical methods."
        ),
        tags$li(
          tags$b("Community Detection"),
          ": Apply multiple algorithms to find community structures within transition networks, supporting comparisons across algorithms."
        ),
        tags$li(
          tags$b("Centrality Measures"),
          ": Calculate centrality measures to identify key nodes and relationships in the network."
        ),
        tags$li(
          tags$b("Visualization"),
          ": Generate interactive and static plots to visually explore network dynamics."
        )
      ),
      div(img(src = "TNA.png", style = "width: 600px; max-width: 100%;"), style = "text-align: center;"), 
      width = 8
    ),
    # # Links
    box(
      h3("Tutorials"),
      tags$ul(
        tags$li(tags$a(href = "https://lamethods.org/book2/chapters/ch15-tna/ch15-tna.html", "Basic TNA tutorial", target = "_blank")),
        tags$li(tags$a(href = "https://lamethods.org/book2/chapters/ch16-ftna/ch16-ftna.html", "Frequency-based TNA tutorial", target = "_blank")),
        tags$li(tags$a(href = "https://lamethods.org/book2/chapters/ch17-tna-clusters/ch17-tna-clusters.html", "Clustering tutorial", target = "_blank"))
      ),
      # Citation
      h3("Citation"),
      p("Please cite TNA and/or our tools if you use it in your research:"),
      tags$ul(
        tags$li(tags$b("TNA method:"),"Saqr, M., López-Pernas, S., Törmänen, T., Kaliisa, R., Misiejuk, K., & Tikka, S. (2025). Transition Network Analysis: A Novel Framework for Modeling, Visualizing, and Identifying the Temporal Patterns of Learners and Learning Processes. Proceedings of Learning Analytics & Knowledge (LAK ’25), in–press."),
        tags$li(tags$b("tna-shiny app:"),"López-Pernas, S., Tikka, S., Misiejuk, K., & Saqr, M. (2025).tna-shiny: Advanced Analytics Just a Few Clicks Away. Proceedings of TEEM 2025. in–press."),
        tags$li(tags$b("tna package:"),"Tikka, S., López-Pernas, S., & Saqr, M. (2025). tna: An R Package for Transition Network Analysis. Applied Psychological Measurement. https://doi.org/10.1177/01466216251348840"),
      ),
        width = 4
    )
  )
}