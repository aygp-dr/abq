#!/usr/bin/env bash
# Test runner signal handler
# Receives test results and reacts appropriately

set -euo pipefail

SIGNAL=$(echo "$ABQ_MSG_CONTENT" | jq -r '.signal // "unknown"')
EMOJI=$(echo "$ABQ_MSG_CONTENT" | jq -r '.emoji // "📝"')

case "$SIGNAL" in
    tests_passed)
        COUNT=$(echo "$ABQ_MSG_CONTENT" | jq -r '.count // "?"')
        COVERAGE=$(echo "$ABQ_MSG_CONTENT" | jq -r '.coverage // "?"')
        echo "$EMOJI All $COUNT tests passed! Coverage: ${COVERAGE}%"
        echo "Continuing with feature work..."
        ;;
    tests_failed)
        CULPRIT=$(echo "$ABQ_MSG_CONTENT" | jq -r '.culprit // "unknown"')
        echo "$EMOJI Tests failed!"
        echo "Last commit by: $CULPRIT"
        if [ "$CULPRIT" = "$(git config user.name 2>/dev/null || echo 'unknown')" ]; then
            echo "😰 It's me. Starting damage control..."
        else
            echo "😌 Not my problem. Carry on."
        fi
        ;;
    *)
        echo "Unknown signal: $SIGNAL"
        ;;
esac
