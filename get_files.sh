#/bin/bash
set -euo pipefail
IFS=$'\n\t'

# Usage check
if [[ $# -ne 2 ]]; then
  echo "Usage: $0 <local_target_directory> <remote_source_directory_on_HPC>"
  exit 1
fi

LOCAL_DEST_DIR=$1
REMOTE_DIR=$2

# CONFIGURATION
INTERMEDIARY_USER="lshbh6"
INTERMEDIARY_HOST="pryor.lshtm.ac.uk"
HPC_USER="lshbh6"
HPC_HOST="loginhpc.lshtm.ac.uk"
INTERMEDIARY_TMP_DIR="/tmp/hpc_transfer_tmp"

# Create local destination directory if it doesn't exist
mkdir -p "$LOCAL_DEST_DIR"
ssh "$INTERMEDIARY_USER@$INTERMEDIARY_HOST" "mkdir -p \"$INTERMEDIARY_TMP_DIR\""

# Function to list files on HPC via intermediary
echo "Fetching list of files from HPC..."
FILE_LIST=$(ssh "$INTERMEDIARY_USER@$INTERMEDIARY_HOST" "ssh $HPC_USER@$HPC_HOST 'ls -1 \"$REMOTE_DIR\"'" )

if [ -z "$FILE_LIST" ]; then
    echo "No files to transfer or error accessing remote directory."
    exit 1
fi

for FILE in $FILE_LIST; do
    echo "Processing file: $FILE"

    # Pull file from HPC to intermediary
    echo "Copying $FILE from HPC to intermediary..."
    
  # Escape the file path for inner SSH
  ESCAPED_REMOTE_PATH="$REMOTE_DIR/$FILE"
  ESCAPED_TMP_PATH="$INTERMEDIARY_TMP_DIR/$FILE"

  # 1) Copy from HPC → intermediary
  ssh "$INTERMEDIARY_USER@$INTERMEDIARY_HOST" \
  "scp $HPC_USER@$HPC_HOST:\"$ESCAPED_REMOTE_PATH\" \"$ESCAPED_TMP_PATH\""



    if [ $? -ne 0 ]; then
        echo "Failed to copy $FILE from HPC to intermediary."
        continue
    fi

  # 2) Copy from intermediary → local
scp "$INTERMEDIARY_USER@$INTERMEDIARY_HOST:$INTERMEDIARY_TMP_DIR/$FILE" "$LOCAL_DEST_DIR/"

    if [ $? -ne 0 ]; then
        echo "Failed to copy $FILE from intermediary to local."
        continue
    fi

  # 3) Remove from intermediary
  ssh "$INTERMEDIARY_USER@$INTERMEDIARY_HOST" "rm -f \"$INTERMEDIARY_TMP_DIR/$FILE\""

done

echo "Transfer complete."

