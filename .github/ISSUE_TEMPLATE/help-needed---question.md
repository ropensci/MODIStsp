---
name: Help needed / question
about: Use this in case of problems or doubts using {MODIStsp}
title: ''
labels: assistance
assignees: ''

---

<!--
Use this template if you need assistance running {MODIStsp} on your code (e.g., in case of errors which are not a bug / you are not sure if they are a bug), or if in doubt about which template you should use. Please use this method instead than sending private email to the authors.

Before opening a new issue please check if the problem was already been mentioned (if so but the found issue is closed, open a new issue citing the old task instead than reopening it).

Ensure that your {MODIStsp} version is update with the last CRAN version:
install.packages("MODIStsp")

Please take particular care with code reproducibility (follow indications provided in the template).

Due to the limited time available to address all the issues, the developer will preferentially address requests finalised to publish scientific works; in this case, please provide the required information in the template below.

IMPORTANT NOTES ABOUT NETIQUETTE
1. Please remember that {MODIStsp} is not a commercial tool, so the developer is not obliged to provide assistance: please be polite, be patient if developers will not answer you instantly and respect the Code of Conduct (https://ropensci.org/code-of-conduct/).
2. Your are required to answer when details (generally outputs of R commands) are required, and to provide a feedback after opening an issue, even after solving your problem or if you are not yet interested in solving it. In the case of missing feedback, the developer reserve the right to ignore your future requests.
3. Tasks can be closed after 10 days of inactivity (you can reopen it if you need further help).
-->

**Issue description**
<!-- Add here a clear and concise description of what the problem is about. -->

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
<!-- Add here any other context about the problem here (for example, the content of the output folder in case the error appears during a subsequent code execution. -->

**Scientific publication**
<!-- If your work is finalised to publish a scientific paper, please provide here the following details:
1. which is the aim of your work;
2. available details - even if subjected to be modified - about publication (title, authors, candidate journal).
Please remember to cite {MODIStsp} in your work (see https://docs.ropensci.org/MODIStsp/authors.html ). -->
