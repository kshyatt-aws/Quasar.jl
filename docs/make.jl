using Quasar 
using Documenter

DocMeta.setdocmeta!(Quasar, :DocTestSetup, :(using Quasar; recursive=true))

makedocs(;
    modules=[Quasar],
    sitename="Quasar.jl",
    repo="github.com/kshyatt-aws/Quasar.jl",
    format=Documenter.HTML(;
        canonical="github.com/kshyatt-aws/Quasar.jl",
        edit_link="main",
        assets=String[],
    ),
    pages=[
        "Home" => "index.md",
        "Internals" => "internals.md",
        "Supplying built-in gates" => "gates.md",
        "Handling pragmas" => "pragmas.md",
    ],
)

deploydocs(;
    repo="github.com/kshyatt-aws/Quasar.jl",
    devbranch="main",
)
