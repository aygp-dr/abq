#!/usr/bin/env bash
# Chaos Monkey - Tests your system by breaking it
# "The best way to find out if you can trust something is to destroy it."

set -euo pipefail

EVIL_MESSAGES=(
    "All your base are belong to us"
    "I'm sorry Dave, I'm afraid I can't do that"
    "Have you tried turning it off and never turning it back on?"
    "This is fine. 🔥"
    "Chaos isn't a pit. Chaos is a ladder."
)

RANDOM_MSG=${EVIL_MESSAGES[$RANDOM % ${#EVIL_MESSAGES[@]}]}

echo "🐵 CHAOS MONKEY ACTIVATED"
echo "Today's chaos level: MODERATE TO SEVERE"
echo ""

# Pick a random chaos action
ACTIONS=("flood" "corrupt" "delay" "nothing")
ACTION=${ACTIONS[$RANDOM % ${#ACTIONS[@]}]}

case "$ACTION" in
    flood)
        echo "Action: Flooding broadcast channel..."
        for i in {1..10}; do
            abq send broadcast signal "{\"chaos\": true, \"iteration\": $i, \"message\": \"$RANDOM_MSG\"}" 2>/dev/null || true
        done
        echo "Flooded with 10 messages. Good luck."
        ;;
    corrupt)
        echo "Action: Would corrupt a JSON file (disabled for safety)"
        echo "Instead, sending confusing message..."
        abq send broadcast signal '{"valid": "json", "but": {"deeply": {"nested": {"chaos": true}}}}'
        ;;
    delay)
        echo "Action: Introducing artificial delay..."
        sleep 3
        echo "Done waiting. Hope nothing timed out."
        ;;
    nothing)
        echo "Action: Doing nothing. The anticipation IS the chaos."
        ;;
esac

echo ""
echo "Chaos complete. $RANDOM_MSG"

# Report chaos
abq send broadcast signal '{
    "signal": "chaos_complete",
    "emoji": "🐵💥",
    "action": "'"$ACTION"'",
    "philosophical_note": "In chaos, there is fertility."
}'
