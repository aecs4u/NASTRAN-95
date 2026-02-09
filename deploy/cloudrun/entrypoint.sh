#!/usr/bin/env bash
set -euo pipefail

MODE="${CLOUD_RUN_MODE:-job}"

if [[ "${MODE}" != "job" ]]; then
  echo "Unsupported CLOUD_RUN_MODE='${MODE}'. Use 'job' for Cloud Run v2 Jobs." >&2
  exit 2
fi

exec python /app/deploy/cloudrun/job_runner.py
