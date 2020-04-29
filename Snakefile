rule all:
    input:
        ".deps-installed",
        "sclr-lowbase.pdf"

rule install_deps:
    input:
        "renv.lock"
    output:
        ".deps-installed"
    shell:
        """Rscript -e 'renv::restore();file.create(".deps-installed")'"""

rule sim:
    input:
        ".deps-installed",
        "sim/sim.R"
    output:
        protected("sim/sim.csv")
    shell:
        "Rscript sim/sim.R"

rule sim_summary:
    input:
        ".deps-installed",
        "sim-summary/sim-summary.R",
        "sim/sim.csv"
    output:
        "sim-summary/sim-summary.csv"
    shell:
        "Rscript sim-summary/sim-summary.R"

rule sim_plot:
    input:
        ".deps-installed",
        "sim-plot/sim-plot.R",
        "sim-summary/sim-summary.csv"
    output:
        "sim-plot/plot2.pdf",
        "sim-plot/plot3.pdf"
    shell:
        "Rscript sim-plot/sim-plot.R"

rule report:
    input:
        ".deps-installed",
        "sclr-lowbase.Rmd",
        "sim-plot/plot2.pdf",
        "sim-plot/plot3.pdf"
    output:
        "sclr-lowbase.pdf"
    shell:
        """Rscript -e 'rmarkdown::render("sclr-lowbase.Rmd")'"""
