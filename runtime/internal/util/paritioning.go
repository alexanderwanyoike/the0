package util

import (
	"hash/crc32"
)

func GetSegmentId(id string, maxSegments int) int32 {
	// Generate a segment ID based on the bot ID using a hash function
	hash := crc32.ChecksumIEEE([]byte(id))
	segmentID := int32(hash % uint32(maxSegments))

	return segmentID
}
