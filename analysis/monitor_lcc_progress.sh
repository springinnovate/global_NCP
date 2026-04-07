#!/bin/bash
# Monitor the progress of the LCC granular analysis
# Counts the number of processed chunks in the output directories

echo "Monitoring LCC Granular Analysis Progress..."
echo "Press Ctrl+C to exit."

while true; do
    clear
    echo "=========================================="
    echo " LCC Analysis Progress Monitor"
    echo "=========================================="
    date
    echo ""

    # Count Forest Loss chunks
    n_forest=$(ls -1 /home/jeronimo/data/global_ncp/processed/lcc_chunks/ForestLoss/*.rds 2>/dev/null | wc -l)
    echo "Forest Loss Chunks: $n_forest"

    # Count Expansion chunks
    n_expansion=$(ls -1 /home/jeronimo/data/global_ncp/processed/lcc_chunks/Expansion/*.rds 2>/dev/null | wc -l)
    echo "Expansion Chunks:   $n_expansion"

    echo "=========================================="
    sleep 30
done