# Manual Visual Test Script for Shiny App
# Run this to quickly launch and verify UI changes

library(devtools)

cat("Loading musicAnalysis package...\n")
load_all()

cat("\n=== VISUAL VERIFICATION CHECKLIST ===\n\n")
cat("Launch the app and verify:\n\n")
cat("[ ] ASCII art background is TRANSPARENT (music sheet visible)\n")
cat("[ ] All links are UNDERLINED and BROWN (#8B7355)\n")
cat("[ ] NO turquoise colors anywhere\n")
cat("[ ] Contact menu is in navbar on RIGHT side\n")
cat("[ ] 'Advanced Settings' in KLAWA/AAT is:\n")
cat("    - Bold\n")
cat("    - Underlined\n")
cat("    - Brown color\n")
cat("    - Shows cursor pointer on hover\n")
cat("[ ] Tab selections have BROWN top border (#8B7355)\n")
cat("[ ] Version number matches DESCRIPTION file\n")
cat("[ ] Check ALL modules: Home, KLAWA, Musical Experience, AAT, PPPT, Merge\n\n")

cat("Launching app...\n\n")
launch_app()
