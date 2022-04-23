Flexible Report Generation: Demonstration session, NCME 2022 Annual Meeting
===========================================================================

Data analysts are often tasked with writing reports that describe data, analyses,
and results associated with a project. Depending upon the nature of the project,
such reports are either completely customized or borrow heavily from other reports
(e.g., annual reports). GitHub repositories and associated GitHub actions can be
used to coordinate the writing as well as the production of final reports for
dissemination. The process we demonstrate utilizes `R`, R Markdown, and several
associated `R` packages as the base tools to construct these reports.

##  NCME_2022_Project Purpose

The concept of "flexibility" in report generation can be applied in numerous ways.
It may mean generating multiple format reports (e.g., websites and PDF document),
setting up workflows that can be used in various settings or for any number of
clients, or combining analytics and documentation into a seamless process. In building
this GitHub repository for our demonstration, we have tried to condense the lessons
we have learned on being flexible in our processes of data analytics and documentation.
We outline five steps that help us get from raw data to reporting results in this
demonstration.

First a well structured working environment should be set up, including a uniform
directory structure with any required external assets and resources, as well as
easily generalized `R` command and function scripts. Second, the data required to
generate the report must be compiled in a consistent data-object structure. Third,
any time-consuming and/or state-specific external analyses should be conducted
and thoroughly reviewed, and also compiled in a consistent data-object structure.
Fourth, all the required meta-data and report content information is compiled
from generic (Universal) and custom sources. Lastly, the desired report formats
are generated using the appropriate `R` functions.

This repository is a template for the workflow that we have used over the past
year in our efforts with multiple states to begin investigating the *academic*
impact of the Covid-19 pandemic. In these efforts we attempted to create
generalizable data formats, standardized methods for analysis and universal content
for reporting. This repo serves as an "all-in-one" representation of how we
structured our efforts across the various states. Combining the multiple stages
and components of these projects into a single repository is helpful in that users
do not need to navigate multiple repositories as we have. However, it does mean
that this this repo is quite complex in and of itself - sorry :wink:

There are detailed README files in many of the sub-component directories of this
repo that give more detailed information about their contents. The main components
located in this top directory are:


###  All_States

This component contains the "state" specific data analysis and report generation
content. Although this project has been framed as a workflow across multiple states,
this could be envisioned in other ways where many projects resemble each other,
but separation is required: school level analysis and reporting, different branches
of a simulation study, annual analysis/reporting within a single organization,
technical reports, etc.

Each "state" has its own sub directory that houses "Initial_Data_Analysis",
"Report_Analyses", "Data" and "Documentation" directories. These represent the
various stages of our analysis and reporting efforts, and are typically located
in different areas of our work environments.

* **Initial_Data_Analysis** represents the framework used for typical annual data
  analysis used to clean, prepare and calculate Student Growth Percentiles (SGPs)
  for the states we work with. This typically includes:
  - confidential student data (housed securely)
  - `R` scripts (shared openly on Github)
* **Report_Analyses** contains the `R` scripts used for each state after the initial
  calculation of SGPs. That is, additional analyses that were carried out in our
  efforts to investigate academic impact. Typically this is included in the
  "Documentation" directory/repo, but placed here for emphasis/differentiation.
* **Data** is where we keep specifically formatted student data and results from
  the impact related analyses. Again, housed outside of any Github repo because
  it contains confidential data.
* **Documentation** includes all the R Markdown based code, content and assets for
  generating reports. This is typically a separate Github repo (with final reports
  provided to clients, not included in the repo). ***This is the heart of the
  "Flexible Report Generation" portion of the NCME Demonstration session.***

The data, `R` code and reports obviously are not typically stored together like
this for confidentiality and other considerations. This demonstration uses the
simulated student data (*`sgpData_LONG_COVID`*) from the
[`SGPData`](https://github.com/CenterForAssessment/SGPdata) package.


###  Universal_Content

The ability to generate reports flexibly and automatically with data and analytic
results from multiple sources requires the identification of what content is
universal and what must be customized to meet specific circumstances and situations.
By "universal", we mean elements that can (and often should) be used in all cases
and updates or improvements are applied consistently. In our experience, every
report begins as a fully custom report. As the process is repeated over and again,
the pieces that are common to all become apparent and moved to an external source
where they can be shared and accessed.

This is true of `R` code as well, as spaghetti code morphs into custom functions,
and then formal functions and packages. Whether talking about text or code, the
use of universal "parameters" also applies - small bits of information that define
how the results are rendered, which are universal in their requirement and
application, but their specification usually depends on the context or use case.

The contents of this directory represents what has been distilled into universal
components.

* **Functions** - As we create `R` code to do specific tasks and then re-use that
  code in other areas, we find it necessary to formalize the code into a function.
  Here we have examples of that where, rather than having the same chunk(s) of
  code to add simulated academic impact into each state's branch of the repo, we
  create a function (`addImpact`) that can be applied universally to the data.
  - Since the function is sourced in from this directory, any updates, improvements
    or bug fixes made to it are applied to each analysis (when it is re-run).
  - The function *could* be added into a package after it is tested and proven in
    a project like this.
* **rmarkdown** - This is where we store the common bits and blobs of text and
  other code snippets (to produce tables, plots, etc.), as well as other assets
  like css and JavaScript code, HTML templates, etc. used to format reports.
  - The Rmd files are used as "children", which allows the task creating the report
    then to be assembling the parts (children) into the desired combination and
    order in a master (parent) document.
  - The css, JavaScript code, etc. in the "templates" subdirectory are structured
    in a way that is typically used (required?) in some packages commonly used to
    render R Markdown into documents (e.g., `rmarkdown`, `bookdown`, `pagedown`
    or our `Literasee` package).

##  Custom_Content

The other side of "Universal_Content" is "Custom_Content" - often we need specific
tools for special cases. The "Custom_Content" directory here serves more as a
placeholder or template for these components. They really belong in the "State_*"
directories (usually put in the "Documentation/assets/rmd" subdirectory). However,
the custom content for one project can often serve as a good template for others
(similar to, but not quite, universal components). We include this element here
for emphasis and differentiation.
