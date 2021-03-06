\name{createAtlasSiteConfig}
\alias{createAtlasSiteConfig}
\title{Parse Expression Atlas YAML site config file.}
\description{
    A function to read in the YAML format site config file which
    contains paths to commonly-used files/directories etc.
}
\usage{
    createAtlasSiteConfig()
}
\value{
    A list with elements from the YAML file.
}
\examples{
\dontrun{
    # Create a new AtlasSiteConfig object.
    atlasSiteConfig <- createAtlasSiteConfig()

    # Get the path to Ensembl annotation sources.
    annotationSourceDir <- atlasSiteConfig$annotation_source_dir
}
}
