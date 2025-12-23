-- Migration: Simplify custom bot status
-- Removes 0vers33r security analyzer integration
-- All bots are now immediately active without approval flow

-- Update all existing bots to 'active' status
UPDATE custom_bots SET status = 'active' WHERE status IN ('pending_review', 'approved', 'awaiting_human_review', 'declined', 'published');

-- Remove the review column (0vers33r analysis results)
ALTER TABLE custom_bots DROP COLUMN IF EXISTS review;

-- Remove the marketplace column (marketplace was removed earlier)
ALTER TABLE custom_bots DROP COLUMN IF EXISTS marketplace;
