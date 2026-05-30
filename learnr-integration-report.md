# Learnr Integration Report

## Overview
Integrated a learnr launcher inside the predmicror Shiny assistant to let users
execute the LLM-provided R code in a dedicated learnr session. The integration
avoids nested Shiny apps by starting learnr in a separate background R process
and opening it in a new browser tab.

## Key Changes
- Added a Learnr section in the sidebar with launch/stop controls and status.
- Generated a temporary learnr tutorial from the latest assistant answer.
- Extracted fenced code blocks and sanitized `library(<model>)` to
  `predmicror_attach()` so the tutorial reliably loads predmicror.
- Added a `predmicror_attach()` helper in the learnr setup chunk:
  - Uses `library(predmicror)` when installed.
  - Falls back to sourcing the repo `R/` files and loading `data/*.rda`.
- Updated the assistant prompt to request fenced R code blocks.
- Added `learnr` and `callr` to Suggests.

## Files Touched
- `inst/shiny/predmicror-assistant/app.R`
- `R/assistant.R`
- `DESCRIPTION`

## How It Works
1. The latest assistant answer is parsed for fenced code blocks.
2. A temporary learnr `tutorial.Rmd` is generated with:
   - An explanation section (answer text without code).
   - An exercise chunk pre-filled with the extracted code.
3. learnr is launched via `callr::r_bg()` on a random local port.
4. The app shows an "Open learnr" button pointing to the local URL.
5. Stop button and session end clean up the background process and temp files.

## Notes
- The assistant now renders answers as escaped code blocks in the app to avoid
  HTML execution and re-runs highlight.js after each answer.
- If `predmicror` is not installed, the learnr setup attempts to source local
  functions and data from the repo root set by the app.

## Usage
1. Ask a question in the assistant to get a code response.
2. Click "Launch learnr" in the sidebar.
3. Click "Open learnr" and use "Run Code" in the exercise.

## Suggested Follow-ups
- If you want to surface output automatically, we can append a default
  `summary(fit)` line when no output is present in the extracted code.
- If you prefer a permanent tutorial layout, we can save the generated Rmd
  to `inst/tutorials/` instead of using a temporary directory.
