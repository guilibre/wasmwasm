#!/bin/bash
set -e

"$ORACLE_BASE"/"$RUN_FILE" &
RUN_PID=$!

MARKER="$ORACLE_BASE"/oradata/.backwasm_setup_done

if [ ! -f "$MARKER" ]; then
    until "$ORACLE_BASE"/checkDBStatus.sh > /dev/null 2>&1; do
        sleep 5
    done

    for f in /opt/oracle/scripts/setup/*.sql; do
        sqlplus -s system/"$ORACLE_PWD"@FREEPDB1 @"$f"
    done

    touch "$MARKER"
fi

wait "$RUN_PID"
