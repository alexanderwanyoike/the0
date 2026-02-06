package local

import _ "embed"

//go:embed compose_files/docker-compose.yml
var EmbeddedComposeFile []byte

//go:embed compose_files/docker-compose.dev.yml
var EmbeddedComposeDevFile []byte
