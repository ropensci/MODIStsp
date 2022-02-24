---
name: Bug report
about: Report a code bug
title: ''
labels: bug
assignees: ''

---

--- NEWS ---
Unfortunately the owner can no longer provide assistance for this package.
If you want to report a bug you can open a new issue using this template,
but the issue will be automatically close and will probably remain unanswered.
If you are able to correct the bug, please address your changes in a pull request.
---

<!--
Use this template to report a bug. Please use this method instead than sending private email to the authors. In case you are not sure if your error is due to a code bug, please open the "Help needed" template.

Before opening a new issue please check if the problem was already been mentioned (if so but the found issue is closed, open a new issue citing the old task instead than reopening it).

Ensure that your {MODIStsp} version is update with the newest GitHub master branch:
install.packages("remotes")
remotes::install_github("ropensci/MODIStsp")

Please take particular care with code reproducibility (follow indications provided in the template).
-->

**Bug description**
<!-- Add here a clear and concise description of what the bug is. -->

**Reproducible example**
<!-- Please provide here a reproducible example in the chunk below. -->

```r
## PLEASE DELETE AND WRITE YOUR OWN
library(MODIStsp)
MODIStsp(gui = FALSE, opts_file = "/path/of/the/parameter_file.json")
# file parameter_file.json _must_ be attached or copied as text,
# as well as referenced files (e.g. spafile)
```

**Expected and actual behavior**
<!-- Provide here the full output of the provided example and describe what is going wrong- -->

```
## PASTE HERE THE OUTPUT OF YOUR EXAMPLE CODE
```

**System information**
<!-- Provide here the output of the following R commands:
sessionInfo()
packageVersion("MODIStsp")
 -->

```
# PASTE HERE YOUR OUTPUT
```

**Additional context**
<!-- Add here any other context about the problem here (for example, the content of the output folder in case the error appears during a subsequent code execution). -->
