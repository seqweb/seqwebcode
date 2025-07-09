# config/env.sh

### Load atomic configuration data
set -o allexport
source "$SEQWEB_HOME/config/seqweb.conf"
set +o allexport

### Derive additional environment bindings

# Core paths (derived from atomic values)
export SEQWEB_HOME="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# /seqweb file tree paths
export SEQWEB_COMMAND_DIR="$SEQWEB_HOME/tools/commands"

# Service URLs
export SEQWEB_HTTP_URL="http://localhost:${SEQWEB_HTTP_PORT}" 