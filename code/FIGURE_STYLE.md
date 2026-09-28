# Figure dimensions and typography

Use the existing plotting functions and ordinary `ggsave()` calls. Export
publication figures at **6 x 3 inches**, except the two stacked hourly panels,
which use **6 x 2 inches**. Width and height arguments are whole inches.

LaTeX controls the displayed size using `width=\textwidth`; the stacked panels
also occupy the full text width. The current JDS text width is about 6.3 inches,
so 9-point PDF axis labels display at about 9.45 points in both documents.
Keeping a common 6-inch export width makes this scaling consistent.

The existing `david_theme()` uses 9-point axes and legends. Plotting functions
with their own axis-font arguments also default to 9 points. Theme text uses
points; `geom_text()` uses millimetres, so 9 points is
`9 / (72.27 / 25.4)` mm. PDF device `pointsize` alone does not set ggplot fonts.

Regenerate figures using the data preparation and analysis workflow in the
supplement. The analysis filename here is `code/jds_datacleansings.R`.
Copy the publication PDFs from `charts/` to `manuscript/images/` or
`manuscript/supplemental_images/`, then rebuild both documents. Check dense
ticks and annotation placement at 100% page size and refresh response-letter
locators if float placement changes.

The hourly panels were regenerated from the complete counts in the saved
console output using `Rscript --vanilla code/regenerate_hourly_figures.R`.
Other publication figures still require the input datasets to regenerate.
