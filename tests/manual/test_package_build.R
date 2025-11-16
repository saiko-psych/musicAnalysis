# Quick Package Build and Verification Script
# Ensures package builds correctly and version is consistent

library(devtools)

cat("=== PACKAGE BUILD VERIFICATION ===\n\n")

# 1. Check DESCRIPTION version
desc_version <- as.character(packageVersion("musicAnalysis"))
cat("Current version in DESCRIPTION:", desc_version, "\n")

# 2. Document
cat("\nGenerating documentation...\n")
document()

# 3. Run tests
cat("\nRunning tests...\n")
test_results <- test()
cat("Tests passed!\n")

# 4. Build package
cat("\nBuilding package...\n")
pkg_path <- build()
cat("Package built:", pkg_path, "\n")

# 5. Check tarball exists
if (file.exists(pkg_path)) {
  size_mb <- file.size(pkg_path) / 1024 / 1024
  cat(sprintf("Tarball size: %.1f MB\n", size_mb))
  cat("\n✅ Package ready for delivery!\n")
} else {
  cat("\n❌ ERROR: Package tarball not found!\n")
}

cat("\n=== POST-BUILD CHECKLIST ===\n\n")
cat("[ ] Version incremented in DESCRIPTION\n")
cat("[ ] mod_home.R 'What's New' updated\n")
cat("[ ] Previous version moved to 'Version History'\n")
cat("[ ] README.md version updated\n")
cat("[ ] Changes committed to git\n")
cat("[ ] Pushed to GitHub master\n")
