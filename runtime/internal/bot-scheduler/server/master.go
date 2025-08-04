package server

import (
	"os"
	core "runtime/internal/core"
	"runtime/internal/util"
)

type BotSchedulerMaster struct {
	master *core.Master
}

func (m *BotSchedulerMaster) Start() {
	util.LogMaster("Starting Bot Scheduler")
	m.master.Start()
}

func (m *BotSchedulerMaster) Stop() {
	util.LogMaster("Stopping Bot Scheduler")
	m.master.Stop()
}

func NewMaster(
	mongoUri string,
	dbName string,
	collectionName string,
	address string,
) *BotSchedulerMaster {
	baseMaster, err := core.NewMaster(
		mongoUri,
		dbName,
		collectionName,
		address,
	)
	if err != nil {
		util.LogMaster("Failed to create master: %v", err)
		os.Exit(1)
	}
	return &BotSchedulerMaster{
		master: baseMaster,
	}
}
