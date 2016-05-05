from invoke import task, run

@task
def install():
    """Install the orientationdiscrimination R package."""
    run("Rscript -e 'devtools::install(\"orientationdiscrimination\")'")
