package server

import (
	"os"
	core "runtime/internal/core"
	"runtime/internal/util"
)

type BotRunnerMaster struct {
	master *core.Master
}

func (m *BotRunnerMaster) Start() {
	util.LogMaster("Starting Bot Runner")
	m.master.Start()
}

func (m *BotRunnerMaster) Stop() {
	util.LogMaster("Stopping Bot Runner")
	m.master.Stop()
}

func NewMaster(
	mongoUri string,
	dbName string,
	collectionName string,
	address string,
) *BotRunnerMaster {
	baseMaster, err := core.NewMaster(mongoUri, dbName, collectionName, address)
	if err != nil {
		util.LogMaster("Failed to create master: %v", err)
		os.Exit(1)
	}
	return &BotRunnerMaster{
		master: baseMaster,
	}
}
