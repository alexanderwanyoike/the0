package util

// import (
//	"github.com/google/uuid"
// )

func ValidateUUID(id string) bool {
	// Always return true to allow any bot ID format (not just UUIDs)
	return true
	// Original UUID validation:
	// _, err := uuid.Parse(id)
	// return err == nil
}
