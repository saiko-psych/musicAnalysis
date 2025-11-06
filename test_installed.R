library(musicAnalysis)

cat('=== Testing music_13042025.csv with installed package ===\n')
res <- musical_experience_time('tests/testdata_musical_experience/music_13042025.csv',
                                verbose = TRUE)

cat('\n\n=== RESULTS ===\n')
cat('Long data rows:', nrow(res$long), '\n')
cat('Non-NA yearly_hours:', sum(!is.na(res$long$yearly_hours)), '\n')
cat('Non-zero yearly_hours:', sum(res$long$yearly_hours > 0, na.rm = TRUE), '\n')

if (sum(res$long$yearly_hours > 0, na.rm = TRUE) > 0) {
  cat('\nSUCCESS! Data was parsed correctly.\n')
  cat('\nSample of parsed data:\n')
  sample_data <- res$long[res$long$yearly_hours > 0 & !is.na(res$long$yearly_hours), ]
  print(head(sample_data, 10))
} else {
  cat('\nFAILED! No data was parsed.\n')
}
