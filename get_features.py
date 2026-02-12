from melody_features import get_all_features, Config
from melody_features.corpus import get_corpus_files

import logging

# Set up logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger("melody_features")

# Get all MIDI files from the Essen corpus
logger.info("Getting all MIDI files from Essen corpus...")
all_midi_files = get_corpus_files('essen')
logger.info("Found %d MIDI files in Essen corpus", len(all_midi_files))

# Process all files at once
logger.info("Processing all files...")
features_df = get_all_features(
    all_midi_files,
    skip_idyom=False,
    log_level=logging.INFO
)

# Save to CSV
output_file = "essen_corpus_features_NEW.csv"
logger.info("Saving results to %s", output_file)
features_df.to_csv(output_file, index=False)

logger.info("Processing complete!")
print(f"Final dataset shape: {features_df.shape}")
print(f"Features extracted for {len(features_df)} melodies")
