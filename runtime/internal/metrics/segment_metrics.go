package metrics

// SegmentMetrics represents the metrics exposed for HPA
type SegmentMetrics struct {
	TotalSegments    int `json:"total_segments"`
	AssignedSegments int `json:"assigned_segments"`
	OrphanedSegments int `json:"orphaned_segments"`
	AvailableWorkers int `json:"available_workers"`
	SegmentBacklog   int `json:"segment_backlog"`
}
