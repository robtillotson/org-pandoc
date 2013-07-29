# org-pandoc

This is an exporter for Org 8.0 that outputs via Pandoc, using Markdown as the intermediate format.  It is mainly intended to allow the creation of e-books via Org export; while Org's own LaTeX and HTML exporters are fine for PDF/print and web, it has no native way to make EPUBs and the like.  So to deal with those, we can use Pandoc to process a format that Org does know how to export.  Although Pandoc supports LaTeX, Markdown is used as the intermediate format here because it seems as though Pandoc has a harder time (stack overflows, etc.) with big LaTeX documents than it does with big Markdown documents.

At present it is rather simple: it is derived from the Markdown exporter, simply adding the title and author block to the heading of the exported file in the format that Pandoc expects.  If you export to a file instead of to a temporary buffer, it will run pandoc on the output file using the configured output format and options.

## (Possible) Future Additions

- in-buffer Org options to control Pandoc export
- extra support for EPUB style sheets and metadata
