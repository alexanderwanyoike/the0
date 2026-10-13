package subscriber

import (
	"context"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestNew_RequiresANATSURL(t *testing.T) {
	t.Setenv("MONGO_URI", "mongodb://localhost:27017")

	_, err := New(context.Background(), Options{Bots: &Collection{DB: "bot_runner", Name: "bots"}})

	assert.ErrorContains(t, err, "NATS_URL")
}

func TestNew_RequiresAMongoURI(t *testing.T) {
	t.Setenv("MONGO_URI", "")

	_, err := New(context.Background(), Options{
		NATSUrl: "nats://localhost:4222",
		Bots:    &Collection{DB: "bot_runner", Name: "bots"},
	})

	assert.ErrorContains(t, err, "MONGO_URI")
}

func TestNew_RequiresSomethingToStore(t *testing.T) {
	t.Setenv("MONGO_URI", "mongodb://localhost:27017")

	_, err := New(context.Background(), Options{NATSUrl: "nats://localhost:4222"})

	assert.ErrorContains(t, err, "neither bot nor schedule events")
}
