package util

import (
	"fmt"
	"math"
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestGetSegmentId(t *testing.T) {
	t.Run("consistency - same ID always produces same segment", func(t *testing.T) {
		testCases := []struct {
			id          string
			maxSegments int
		}{
			{"550e8400-e29b-41d4-a716-446655440000", 10},
			{"550e8400-e29b-41d4-a716-446655440001", 16},
			{"test-bot-123", 8},
			{"", 5}, // Empty string edge case
		}

		for _, tc := range testCases {
			t.Run(fmt.Sprintf("ID=%s_maxSegments=%d", tc.id, tc.maxSegments), func(t *testing.T) {
				// Call multiple times to ensure consistency
				segment1 := GetSegmentId(tc.id, tc.maxSegments)
				segment2 := GetSegmentId(tc.id, tc.maxSegments)
				segment3 := GetSegmentId(tc.id, tc.maxSegments)

				assert.Equal(t, segment1, segment2, "Same ID should produce same segment ID")
				assert.Equal(t, segment2, segment3, "Same ID should produce same segment ID")

				// Segment should be within valid range
				assert.GreaterOrEqual(t, segment1, int32(0), "Segment ID should be >= 0")
				assert.Less(t, segment1, int32(tc.maxSegments), "Segment ID should be < maxSegments")
			})
		}
	})

	t.Run("distribution quality with many IDs", func(t *testing.T) {
		maxSegments := 10
		numIDs := 10000

		// Generate many different IDs
		testIDs := make([]string, numIDs)
		for i := 0; i < numIDs; i++ {
			testIDs[i] = fmt.Sprintf("bot-id-%d-%d", i, i*7+13) // Some variation
		}

		// Count distribution across segments
		segmentCounts := make(map[int32]int)
		for _, id := range testIDs {
			segment := GetSegmentId(id, maxSegments)
			segmentCounts[segment]++
		}

		// Should use all segments
		assert.Len(t, segmentCounts, maxSegments, "All segments should be used")

		// Check distribution quality
		expectedPerSegment := float64(numIDs) / float64(maxSegments)
		var totalVariance float64

		for segment := int32(0); segment < int32(maxSegments); segment++ {
			count := segmentCounts[segment]
			assert.Greater(t, count, 0, "Segment %d should have at least one ID", segment)

			variance := math.Pow(float64(count)-expectedPerSegment, 2)
			totalVariance += variance
		}

		standardDeviation := math.Sqrt(totalVariance / float64(maxSegments))
		coefficientOfVariation := standardDeviation / expectedPerSegment

		// Good hash function should have relatively even distribution
		assert.Less(t, coefficientOfVariation, 0.15,
			"Distribution should be relatively even (CV < 0.15, got %.3f)", coefficientOfVariation)
	})

	t.Run("different maxSegments values", func(t *testing.T) {
		testID := "550e8400-e29b-41d4-a716-446655440000"

		testCases := []int{1, 2, 3, 5, 8, 10, 16, 32, 64, 100, 1000}

		for _, maxSegments := range testCases {
			t.Run(fmt.Sprintf("maxSegments=%d", maxSegments), func(t *testing.T) {
				segment := GetSegmentId(testID, maxSegments)

				assert.GreaterOrEqual(t, segment, int32(0))
				assert.Less(t, segment, int32(maxSegments))
			})
		}
	})

	t.Run("edge cases", func(t *testing.T) {
		t.Run("empty string ID", func(t *testing.T) {
			segment := GetSegmentId("", 10)
			assert.GreaterOrEqual(t, segment, int32(0))
			assert.Less(t, segment, int32(10))
		})

		t.Run("maxSegments = 1", func(t *testing.T) {
			testIDs := []string{"id1", "id2", "id3", "different-id"}

			for _, id := range testIDs {
				segment := GetSegmentId(id, 1)
				assert.Equal(t, int32(0), segment, "With maxSegments=1, all IDs should map to segment 0")
			}
		})

		t.Run("very long ID", func(t *testing.T) {
			longID := ""
			for i := 0; i < 1000; i++ {
				longID += "a"
			}

			segment := GetSegmentId(longID, 10)
			assert.GreaterOrEqual(t, segment, int32(0))
			assert.Less(t, segment, int32(10))
		})

		t.Run("unicode characters in ID", func(t *testing.T) {
			unicodeIDs := []string{
				"测试-bot-123",
				"тест-бот-456",
				"テスト-ボット-789",
				"🤖-bot-emoji",
			}

			for _, id := range unicodeIDs {
				segment := GetSegmentId(id, 8)
				assert.GreaterOrEqual(t, segment, int32(0))
				assert.Less(t, segment, int32(8))
			}
		})

		t.Run("special characters in ID", func(t *testing.T) {
			specialIDs := []string{
				"bot-with-dashes",
				"bot_with_underscores",
				"bot.with.dots",
				"bot@with@symbols",
				"bot with spaces",
				"bot/with/slashes",
			}

			for _, id := range specialIDs {
				segment := GetSegmentId(id, 12)
				assert.GreaterOrEqual(t, segment, int32(0))
				assert.Less(t, segment, int32(12))
			}
		})
	})

	t.Run("deterministic mapping for known values", func(t *testing.T) {
		// Test with some known UUID values to ensure deterministic behavior
		knownMappings := []struct {
			id          string
			maxSegments int
			// We don't hard-code expected values since they depend on CRC32 implementation
			// but we verify consistency and range
		}{
			{"550e8400-e29b-41d4-a716-446655440000", 10},
			{"6ba7b810-9dad-11d1-80b4-00c04fd430c8", 16},
			{"6ba7b811-9dad-11d1-80b4-00c04fd430c8", 16},
			{"01234567-89ab-cdef-0123-456789abcdef", 32},
		}

		for _, mapping := range knownMappings {
			segment := GetSegmentId(mapping.id, mapping.maxSegments)

			// Verify range
			assert.GreaterOrEqual(t, segment, int32(0))
			assert.Less(t, segment, int32(mapping.maxSegments))

			// Verify consistency (call again)
			segment2 := GetSegmentId(mapping.id, mapping.maxSegments)
			assert.Equal(t, segment, segment2)
		}
	})

	t.Run("different IDs produce different segments eventually", func(t *testing.T) {
		maxSegments := 10
		numIDs := 100

		segmentSet := make(map[int32]bool)

		for i := 0; i < numIDs; i++ {
			id := fmt.Sprintf("unique-bot-id-%d", i)
			segment := GetSegmentId(id, maxSegments)
			segmentSet[segment] = true
		}

		// With 100 different IDs and 10 segments, we should see multiple segments used
		// (though not necessarily all due to randomness of hash function)
		assert.Greater(t, len(segmentSet), 1, "Different IDs should eventually produce different segments")
	})

	t.Run("hash function properties", func(t *testing.T) {
		// Test that small changes in input produce different outputs (avalanche effect)
		baseID := "test-bot-12345"
		maxSegments := 16

		variations := []string{
			"test-bot-12346",  // Last digit changed
			"test-bot-12355",  // Second to last digit changed
			"Test-bot-12345",  // Case change
			"test-bot-123456", // Extra digit
			"test-bot-1234",   // Missing digit
		}

		baseSegment := GetSegmentId(baseID, maxSegments)
		differentSegments := 0

		for _, variation := range variations {
			segment := GetSegmentId(variation, maxSegments)
			if segment != baseSegment {
				differentSegments++
			}
		}

		// A good hash function should produce different outputs for most small input changes
		// We don't require 100% because of the modulo operation, but should be mostly different
		assert.Greater(t, differentSegments, len(variations)/2,
			"Hash function should have good avalanche effect - small input changes should often produce different outputs")
	})
}

func BenchmarkGetSegmentId(b *testing.B) {
	testID := "550e8400-e29b-41d4-a716-446655440000"
	maxSegments := 10

	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		GetSegmentId(testID, maxSegments)
	}
}

func BenchmarkGetSegmentIdVariousLengths(b *testing.B) {
	testCases := []struct {
		name string
		id   string
	}{
		{"short", "bot1"},
		{"uuid", "550e8400-e29b-41d4-a716-446655440000"},
		{"long", "this-is-a-very-long-bot-identifier-with-many-characters-to-test-performance"},
	}

	maxSegments := 10

	for _, tc := range testCases {
		b.Run(tc.name, func(b *testing.B) {
			for i := 0; i < b.N; i++ {
				GetSegmentId(tc.id, maxSegments)
			}
		})
	}
}

func TestSegmentIdDistributionAcrossDifferentMaxSegments(t *testing.T) {
	// Test that segment distribution remains good across different maxSegments values
	baseID := "test-bot"
	testCases := []int{2, 4, 8, 16, 32}

	for _, maxSegments := range testCases {
		t.Run(fmt.Sprintf("maxSegments=%d", maxSegments), func(t *testing.T) {
			segmentCounts := make(map[int32]int)
			numTests := maxSegments * 100 // 100 IDs per segment on average

			for i := 0; i < numTests; i++ {
				id := fmt.Sprintf("%s-%d", baseID, i)
				segment := GetSegmentId(id, maxSegments)
				segmentCounts[segment]++
			}

			// All segments should be used
			assert.Len(t, segmentCounts, maxSegments)

			// No segment should be dramatically over or under represented
			expectedPerSegment := float64(numTests) / float64(maxSegments)
			for segment, count := range segmentCounts {
				ratio := float64(count) / expectedPerSegment
				assert.Greater(t, ratio, 0.5, "Segment %d is under-represented (ratio: %.2f)", segment, ratio)
				assert.Less(t, ratio, 2.0, "Segment %d is over-represented (ratio: %.2f)", segment, ratio)
			}
		})
	}
}
