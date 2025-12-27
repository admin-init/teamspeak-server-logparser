# TeamSpeak-Server-LogParser

TeamSpeak-Server-LogParser

This is a Log Parser for TeamSpeak3 server.

# Usage

The teamspeak-server-logparser-cli tool parses TeamSpeak 3 server log files and records user sessions (connect/disconnect events) into an SQLite database. It supports two modes:

- batch: One-time import of a single log file.
- watch: Real-time monitoring of a log directory with persistent state tracking.

## Prerequisites

- A TeamSpeak 3 server configured to write connection logs (typically in the logs/ directory).
- Logs must contain lines matching the expected format for client connect/disconnect events.

## Batch Mode (batch)

Processes a single log file once and exits.

```sh
teamspeak-server-logparser-cli batch \
  --log-file ./logs/ts3server_2025-04-05__12_00_00.log \
  --db-path ./sessions.db
```

### Options:

| Flag       | Short | Default       | Description                      |
| ---------- | ----- | ------------- | -------------------------------- |
| --log-file | -l    | (required)    | Path to the input log file       |
| --db-path  | -d    | ./sessions.db | Path to the SQLite database file |

> 💡 The database schema is created automatically if it doesn’t exist.

## Watch Mode (watch)

Continuously monitors a log directory for new or modified files, processing events in real time. Maintains:

- File read offsets (offsets.json)
- Currently online sessions (online-sessions.json)

```sh
teamspeak-server-logparser-cli watch \
  --log-dir ./logs \
  --db-path ./sessions.db \
  --offsets ./offsets.json \
  --session-state ./online-sessions.json
```

### Options:

| Flag            | Short | Default                | Description                                  |
| --------------- | ----- | ---------------------- | -------------------------------------------- |
| --log-dir       | -L    | ./logs                 | Directory containing TeamSpeak log files     |
| --db-path       | -d    | ./sessions.db          | SQLite database path                         |
| --offsets       | -o    | ./offsets.json         | JSON file to persist file read positions     |
| --session-state | -s    | ./online-sessions.json | JSON file to track currently connected users |

> ✅ Resilience features:
>
> - Survives restarts: resumes from last offset
> - Handles newly created log files (e.g., from virtual server restarts)
> - Flushes state to disk every 0.75 seconds
> - Skips malformed log lines without crashing

## Example Workflow

- Initial import (optional):
```sh
teamspeak-server-logparser-cli batch -l logs/ts3server_*.log -d sessions.db
```

- Start real-time monitoring:
```sh
teamspeak-server-logparser-cli watch -L logs -d sessions.db
```

 - Query sessions (using sqlite3):
```sql
SELECT * FROM user_sessions
WHERE session_disconnect_time IS NULL;  -- currently online
```

### Notes

- Time is stored in SQLite as TEXT in YYYY-MM-DD HH:MM:SS.sss format (microsecond precision).
- Only client connected and client disconnected events are processed.
- The tool is safe to run alongside a running TeamSpeak server—log files are read incrementally and never locked.
