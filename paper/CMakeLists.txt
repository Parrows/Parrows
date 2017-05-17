PROJECT(master_project NONE)
cmake_minimum_required(VERSION 2.8)
SET(LATEX_OUTPUT_PATH build)

INCLUDE(cmake/UseLATEX.cmake)


ADD_LATEX_DOCUMENT(main.tex
    BIBFILES references.bib
    IMAGE_DIRS images
    USE_INDEX
    USE_GLOSSARY
)