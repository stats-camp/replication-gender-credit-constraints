# Replication: Are Women More Credit Constrained? Experimental Evidence on Gender and Microenterprise Returns

Suresh de Mel, David McKenzie, and Christopher Woodruff — *American Economic Journal: Applied Economics (2009)*.

This [stats.camp](https://stats.camp) repository preserves official AEA deposit
**116328, version V1**, for replication and engine compatibility testing.
**Status: executed on 2026-10-16 and again on 2026-10-17 in the native stats.camp engine from scratch copies; the package runs end to end in the engine.**
The deposited datasets retain their original bytes and archive-relative paths.
Documentation is adapted for stats.camp.

## The study

Uses randomized grants to Sri Lankan microenterprise owners to compare capital returns by gender and investigate household allocation and business characteristics.

## Contents and entry points

The deposit includes the main survey panel and eight auxiliary datasets. Analysis uses legacy `for` and merge syntax, panel fixed effects, `xtivreg2`, treatment interactions, and linear-combination tests.

The source archive contains **13 original files** totaling
**12,610,677 bytes**. The source ZIP is
1,362,124 bytes. Deposited data, reference outputs, and ancillary files are retained;
documentation is adapted for stats.camp.

Analysis entry point:

- [AEJApp_2008_0146_Data/gender_AEJApp.do](AEJApp_2008_0146_Data/gender_AEJApp.do)

Authors’ documentation, adapted for stats.camp:

- [AEJApp_2008_0146_Data/ReadMe.txt](AEJApp_2008_0146_Data/ReadMe.txt)

## Setup and current scope

Launch `gender_AEJApp.do` from `AEJApp_2008_0146_Data/`. The original
absolute Windows paths have been replaced with relative paths to the
deposited datasets, and the log is written to the same directory. The
analysis commands are otherwise unchanged. The legacy Word readme has
been converted to plain text, preserving its instructions and file descriptions.

The 2026-10-16 runs used scratch copies, so the figures, logs and intermediate
datasets in this repository are still the authors' deposited reference material.

## Inspect in stats.camp

These links open the deposited files for inspection. They do not establish
that the preserved paths, dependencies, or commands are executable in the browser.

- [AEJApp_2008_0146_Data/gender_AEJApp.do](https://stats.camp/app?open=https%3A%2F%2Fgithub.com%2Fstats-camp%2Freplication-gender-credit-constraints.git&file=AEJApp_2008_0146_Data%2Fgender_AEJApp.do)

## Source integrity and attribution

- [Official deposit](https://www.openicpsr.org/openicpsr/project/116328/version/V1/view)
- [Deposit DOI](https://doi.org/10.3886/E116328V1)

Archive: `116328-V1.zip`. SHA-256:
`e85be50c959e81d182c8b1695043649c466d0b0b15edbe124dac72204ce9cc49`.

All original file hashes were checked after extraction. `.gitattributes` disables
Git line-ending normalization. Empty folders are represented by `.gitkeep`
files. The root README and Git configuration are stats-camp additions.

The deposited `LICENSE.txt` is retained without modification. It supplies the original code/data terms; this mirror does not relicense the research.

The study, code, and data remain credited to Suresh de Mel, David McKenzie, and Christopher Woodruff; stats-camp maintains
this mirror and its preparation documentation.
