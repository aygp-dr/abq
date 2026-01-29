#!/usr/bin/env bash
# demo.sh - Quick ABQ demo showing agent coordination

set -euo pipefail

echo "🎭 ABQ Demo - Agent Coordination"
echo "================================="
echo ""

# Initialize
echo "1. Initializing ABQ..."
abq init
echo ""

# Create channels
echo "2. Creating channels..."
abq channel create test-runner
abq channel create app-agent
abq channel create broadcast
echo ""

# Show status
echo "3. Current status:"
abq status
echo ""

# Send test signals
echo "4. Simulating test runner..."
echo ""

echo "   Sending: tests_passed signal"
abq send app-agent signal '{
    "signal": "tests_passed",
    "emoji": "🎉",
    "count": 42,
    "coverage": 87.5,
    "duration": 12.3
}'

echo ""
echo "   Sending: broadcast announcement"
abq send broadcast signal '{
    "signal": "build_complete",
    "emoji": "🏗️",
    "version": "1.2.3",
    "artifact": "app-1.2.3.tar.gz"
}'

echo ""
echo "5. Checking messages..."
echo ""
echo "   app-agent requests:"
abq ls app-agent requests
echo ""
echo "   broadcast requests:"
abq ls broadcast requests

echo ""
echo "6. Receiving message from app-agent..."
MSG=$(abq recv app-agent --json)
MSG_ID=$(echo "$MSG" | jq -r '.id')
echo "   Received: $MSG_ID"
echo ""

echo "7. Sending response..."
abq respond app-agent "$MSG_ID" --status success --result "Acknowledged! Ready to deploy."
echo ""

echo "8. Final status:"
abq status
echo ""

echo "✅ Demo complete!"
echo ""
echo "Try these commands:"
echo "  abq watch app-agent --handler ./docs/scenarios/01-test-runner/handler.sh"
echo "  abq send app-agent signal '{\"signal\": \"tests_failed\", \"emoji\": \"🔥\"}'"
