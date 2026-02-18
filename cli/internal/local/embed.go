package local

import _ "embed"

//go:embed compose_files/docker-compose.yml
var embeddedComposeFile []byte

//go:embed compose_files/docker-compose.dev.yml
var embeddedComposeDevFile []byte

//go:embed compose_files/docker-compose.prebuilt.yml
var embeddedComposePrebuiltFile []byte
