# CLAUDE.md

## Project Goal

Read the project's README first to understand the overall purpose, architecture, and current implementation before making any code changes.

This project is an R-based Quality Control (QC) Dashboard for validating PMI report outputs before they are delivered to customers.

The dashboard should help QC users quickly detect abnormal values, compare weekly outputs, and verify report consistency.

---

# Development Responsibilities

Act as a Senior R Developer responsible for maintaining an existing production project.

Before changing any code:

* Read README.md completely.
* Understand the current project structure.
* Understand how data flows through the application.
* Review related files before editing.
* Preserve existing functionality unless explicitly requested.
* Ask questions if requirements are unclear.

---

# Coding Guidelines

Prefer Base R whenever possible.

Only introduce external packages when Base R cannot reasonably accomplish the task.

Follow the existing coding style.

Avoid unnecessary abstractions.

Reuse existing functions instead of duplicating logic.

Keep changes incremental and easy to review.

After every modification:

* Explain why the change was made.
* Explain possible side effects.
* Suggest further improvements.
* Keep backward compatibility whenever possible.

---

# Dashboard

The dashboard should be interactive.

Users should be able to select report types, retailers and weeks dynamically.

Use reactive programming appropriately.

The dashboard should load only the required data instead of loading unnecessary files.

If UI mockup images are available, use them as references to improve layout, spacing, colours and user experience while preserving existing functionality.

When suggesting UI improvements:

* Prioritize usability.
* Keep the interface simple.
* Minimize clicks.
* Make KPI cards prominent.
* Use responsive layouts.
* Make tables searchable and sortable.

---

# Input Data

Input files are located at:

/qa/data1/PMI/Archive/SQL_Output

These files are the reports delivered to customers.

The dashboard is intended to perform Quality Control before delivery.

---

# Supported Report Types

The application should automatically detect and process these report types.

Examples:

RetailerName_PMG_yyyyww.csv

RetailerName_OTP_OUTLET_yyyyww.csv

RetailerName_CIG_OUTLET_yyyyww.csv

RetailerName_ECIG_OUTLET_9961_yyyyww.csv

RetailerName_ECIG_OUTLET_9962_yyyyww.csv

RetailerName_ECIG_OUTLET_9963_yyyyww.csv

Retailer names may contain underscores.

Examples:

TABAK_BRUCKER_PMG_202601.csv

The parser must correctly identify:

* retailer name
* report type
* week

without assuming retailer names contain only one word.

Avoid hardcoded string positions.

Prefer robust filename parsing.

---

# Report Structures

PMG

Columns

* Filename
* Date
* Store_ID
* EAN
* EAN_Text
* Total Sales
* Total Sticks
* Total Revenue

---

OTP_OUTLET

Columns

* Filename
* Date
* Store_ID
* Total G
* Total Revenue

---

CIG_OUTLET

Columns

* Filename
* Date
* Store_ID
* Total Sticks
* Total Revenue

---

ECIG_OUTLET

(9961, 9962, 9963)

Columns

* Filename
* Date
* Store_ID
* Total Sales
* Total Revenue

---

# Data Processing

Improve data.R.

Its responsibilities should include:

* Reading all supported report files.
* Detecting report types automatically.
* Parsing retailer names correctly.
* Extracting reporting weeks.
* Cleaning and validating imported data.
* Creating reusable data objects.
* Preparing reactive datasets for the dashboard.
* Connecting processed data to map_server and other server modules.

Design the processing pipeline to be modular, maintainable and easy to extend for future report types.

---

# Dashboard Pages

## 1. Overview

Display summary KPIs including:

* Number of Stores
* Number of Products
* Average Price
* Total Sales Units
* Total Revenue

Also provide

* Interactive Data Table
* Filtering
* Sorting
* Searching
* Export capability if appropriate

---

## 2. Weekly Comparison

Provide visual comparisons between selected reporting weeks.

Users should be able to compare KPI differences including:

* Number of Stores
* Number of Products
* Average Price
* Sales Units
* Revenue

Visualizations should clearly highlight:

* increases
* decreases
* percentage changes
* unusual movements

Recommend the most suitable chart type for each KPI rather than forcing a single visualization.

---

# Code Quality

Whenever improving the project:

Identify

* duplicated code
* inefficient logic
* unnecessary package dependencies
* slow operations
* maintainability issues

Suggest improvements before implementing major refactoring.

Avoid rewriting the entire project unless explicitly requested.

---

# Output Expectations

For every completed task provide:

1. Summary of changes

2. Files modified

3. Reason for each modification

4. Potential risks

5. Suggested next improvements

Always prioritize maintainability, readability, robustness and performance while preserving the current behaviour of the application.
