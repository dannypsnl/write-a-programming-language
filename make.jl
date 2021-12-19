using Documenter

makedocs(
    sitename = "Write you a programming language",
    format = Documenter.HTML(),
    pages = [
        "index.md",
        "Chapters" => [
            "chapter_1.md",
            "chapter_2.md",
            "chapter_3.md",
            "Polymorphsim & Advanced Type Inference" => [
                "ch4/ad-hoc-poly.md"
                "ch4/parametric-poly.md"
                "ch4/subtyping.md"
                "ch4/row-poly.md"
            ],
            "chapter_5.md",
            "chapter_6.md",
            "chapter_7.md",
            "chapter_8.md",
            "chapter_9.md",
            "chapter_10.md",
        ],
        "Appendix" => [
            "appendix_parser.md"
        ]
    ]
)

# Documenter can also automatically deploy documentation to gh-pages.
# See "Hosting Documentation" and deploydocs() in the Documenter manual
# for more information.
#=deploydocs(
    repo = "<repository url>"
)=#
