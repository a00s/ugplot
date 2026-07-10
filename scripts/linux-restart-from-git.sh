#!/usr/bin/env bash
set -euo pipefail

if [[ ! -f DESCRIPTION ]] || ! grep -q '^Package: ugplot$' DESCRIPTION; then
  echo "Run this script from the ugplot repository root." >&2
  exit 2
fi

BRANCH="${UGPLOT_BRANCH:-main}"
HOST="${UGPLOT_HOST:-0.0.0.0}"
PORT="${UGPLOT_PORT:-8080}"
HEALTH_HOST="${UGPLOT_HEALTH_HOST:-127.0.0.1}"
TOKEN="${1:-${UGPLOT_SERVER_TOKEN:-${UGPLOT_TOKEN:-}}}"

if [[ -z "${TOKEN}" ]]; then
  echo "Provide the ugPlot server password/token." >&2
  echo "Example: scripts/linux-restart-from-git.sh 'your-token'" >&2
  echo "Or: UGPLOT_SERVER_TOKEN='your-token' scripts/linux-restart-from-git.sh" >&2
  exit 2
fi

export UGPLOT_SERVER_TOKEN="${TOKEN}"
export UGPLOT_HOST="${HOST}"
export UGPLOT_PORT="${PORT}"

echo "Stopping ugPlot server..."
Rscript -e 'library(ugplot); try(ugPlotServerStop(port = as.integer(Sys.getenv("UGPLOT_PORT", "8080"))), silent = TRUE)'

echo "Updating ${BRANCH}..."
git switch "${BRANCH}"
git pull --ff-only origin "${BRANCH}"

echo "Installing local package..."
R CMD INSTALL .

echo "Starting ugPlot server on ${HOST}:${PORT}..."
Rscript -e 'library(ugplot); ugPlotServerStart(host = Sys.getenv("UGPLOT_HOST", "0.0.0.0"), port = as.integer(Sys.getenv("UGPLOT_PORT", "8080")), token = Sys.getenv("UGPLOT_SERVER_TOKEN"))'

echo "Testing health endpoint..."
curl -fsS -H "Authorization: Bearer ${UGPLOT_SERVER_TOKEN}" "http://${HEALTH_HOST}:${PORT}/health"
echo
echo "ugPlot restarted successfully."
