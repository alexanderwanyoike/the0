package server

import (
	"os"
	core "runtime/internal/core"
	"runtime/internal/util"
)

type BacktestRunnerMaster struct {
	master *core.Master
}

func (m *BacktestRunnerMaster) Start() {
	util.LogMaster("Starting Backtest Runner")
	m.master.Start()
}

func (m *BacktestRunnerMaster) Stop() {
	util.LogMaster("Stopping Backtest Runner")
	m.master.Stop()
}

func NewMaster(
	mongoUri string,
	dbName string,
	collectionName string,
	address string,
) *BacktestRunnerMaster {
	baseMaster, err := core.NewMaster(mongoUri, dbName, collectionName, address)
	if err != nil {
		util.LogMaster("Failed to create master: %v", err)
		os.Exit(1)
	}
	return &BacktestRunnerMaster{
		master: baseMaster,
	}
}
