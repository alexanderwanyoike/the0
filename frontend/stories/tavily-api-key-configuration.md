# Story: Tavily API Key Configuration for AI Agent

## Overview

Enable users to configure the Tavily API key from the frontend AI chat component to unlock web search capabilities in the the0 AI agent. This will allow the agent to perform real-time web research when building trading bots.

## Business Value

- **Enhanced AI Capabilities**: Enable the agent to research latest library documentation, trading platform APIs, and best practices
- **Better User Experience**: Users can configure all necessary API keys from a single interface
- **Optional Enhancement**: Tavily is optional, so users can use the agent without it, but web search adds significant value
- **Self-Service**: Users manage their own Tavily keys without backend configuration

## User Stories

### As a User

- I want to configure my Tavily API key from the frontend settings
- I want to see the status of my Tavily configuration (database vs environment)
- I want the option to add Tavily during initial setup
- I want the agent to use web search when I ask research questions
- I want to be able to reset my Tavily key if needed

### As a Developer

- I want consistent API key management patterns between Google AI and Tavily
- I want proper validation of Tavily keys (tvly- prefix)
- I want clear documentation on the Tavily integration
- I want the UI to follow existing design patterns

## Current State

### Backend (the0-ai service)

The backend already has full Tavily integration:

- ✅ Three API endpoints implemented:
  - `GET /settings/tavily-api-key/status` - Check configuration status
  - `POST /settings/tavily-api-key` - Set API key
  - `DELETE /settings/tavily-api-key` - Remove API key
- ✅ `tavily_search()` tool integrated into agent
- ✅ Dual configuration support (environment variables + database)
- ✅ Format validation (must start with "tvly-")
- ✅ Comprehensive error handling and tests

### Frontend

Currently missing Tavily configuration:

- ❌ No UI to configure Tavily API key
- ❌ No status checking for Tavily
- ❌ No onboarding for Tavily setup
- ✅ Settings modal exists for Google AI key
- ✅ API key setup component exists for onboarding
- ✅ API service pattern established

## Acceptance Criteria

### 1. API Service Extensions

- [ ] Add `checkTavilyApiKeyStatus()` method to ApiService
- [ ] Add `setTavilyApiKey(apiKey: string)` method to ApiService
- [ ] Add `resetTavilyApiKey()` method to ApiService
- [ ] Methods call existing backend endpoints via `/api/ai-agent/settings`

### 2. Settings Modal Updates

- [ ] Add Tavily API key section after Google AI section
- [ ] Include input field for Tavily key (password type)
- [ ] Add "Save" and "Reset" buttons for Tavily key
- [ ] Display status indicator showing if Tavily is configured
- [ ] Show which source is active (database/environment/none)
- [ ] Include link to https://tavily.com for key generation
- [ ] Add client-side validation (key must start with "tvly-")
- [ ] Display success/error messages for Tavily operations
- [ ] Maintain existing Google AI key functionality

### 3. Initial Setup (Onboarding)

- [ ] Extend ApiKeySetup component to include Tavily
- [ ] Add second section/step for Tavily API key
- [ ] Make Tavily optional with clear messaging
- [ ] Explain web search capabilities enabled by Tavily
- [ ] Include instructions for obtaining Tavily key
- [ ] Allow skipping Tavily setup while requiring Google AI

### 4. Main Page Updates

- [ ] Check both Google AI and Tavily status on mount
- [ ] Show setup modal if Google AI key is missing (required)
- [ ] Optionally prompt for Tavily if missing (with skip option)
- [ ] Display status of both API keys in UI (optional)

### 5. User Experience

- [ ] Consistent UI patterns with Google API key management
- [ ] Clear visual distinction between required (Google) and optional (Tavily)
- [ ] Helpful error messages if validation fails
- [ ] Loading states during API calls
- [ ] Confirmation for destructive actions (reset)

### 6. Documentation

- [x] Update CLAUDE.md with AI Agent section
- [x] Document Tavily integration patterns
- [x] Include API key management examples
- [ ] Add inline code comments where needed

## Technical Implementation

### Files to Modify

1. **`/src/lib/ai-agent/api.ts`**

   - Add three new methods for Tavily key management
   - Follow existing patterns from Google AI key methods
   - Return types match backend response formats

2. **`/src/components/ai-agent/settings/SettingsModal.tsx`**

   - Add new state for Tavily key, loading, status
   - Add Tavily section between API Key and Chat Settings sections
   - Implement handlers for save/reset Tavily key
   - Add status fetching on modal open
   - Include validation for tvly- prefix

3. **`/src/components/ai-agent/setup/ApiKeySetup.tsx`**

   - Refactor to support two API keys
   - Add Tavily section/step with optional flag
   - Update instructions and help text
   - Handle both keys in submit flow

4. **`/src/app/ai-agent/page.tsx`**
   - Add Tavily status check on mount
   - Update setup modal trigger logic
   - Optionally display both key statuses

### API Endpoints (Already Implemented in Backend)

```typescript
// Status check
GET /api/ai-agent/settings?endpoint=tavily-api-key/status
Response: {
  configured_in_database: boolean,
  configured_in_environment: boolean,
  active_source: "environment" | "database" | "none",
  has_api_key: boolean
}

// Set key
POST /api/ai-agent/settings?endpoint=tavily-api-key
Body: { api_key: string }
Response: { message: string }

// Reset key
DELETE /api/ai-agent/settings?endpoint=tavily-api-key
Response: { message: string }
```

### TypeScript Interfaces

```typescript
// Add to api.ts
interface TavilyApiKeyStatus {
  configured_in_database: boolean;
  configured_in_environment: boolean;
  active_source: "environment" | "database" | "none";
  has_api_key: boolean;
}

interface ApiKeyMessage {
  message: string;
}
```

### Validation Rules

```typescript
// Client-side validation
const validateTavilyKey = (key: string): boolean => {
  return key.trim().startsWith("tvly-");
};
```

### UI Component Structure

```
SettingsModal
├── Google AI API Key Section
│   ├── Input field (AIza...)
│   ├── Save/Reset buttons
│   ├── Link to Google AI Studio
│   └── Status/Error messages
├── Separator
├── Tavily API Key Section (NEW)
│   ├── Status indicator
│   ├── Input field (tvly-...)
│   ├── Save/Reset buttons
│   ├── Link to Tavily
│   └── Status/Error messages
├── Separator
├── Chat Settings Section
│   ├── Streaming toggle
│   ├── Typewriter toggle
│   └── Speed slider
└── About Section
```

## Testing Checklist

### Manual Testing

- [ ] Open AI Agent page for first time - sees setup modal
- [ ] Enter Google AI key (required) - saves successfully
- [ ] Enter Tavily key (optional) - saves successfully
- [ ] Skip Tavily setup - agent works without web search
- [ ] Open settings modal - see both keys configured
- [ ] Enter invalid Tavily key (no tvly- prefix) - see error
- [ ] Reset Tavily key - confirmation shown, key removed
- [ ] Check status shows correct source (db vs env)
- [ ] Test with Tavily in environment variables
- [ ] Test with Tavily in database
- [ ] Test with no Tavily configured

### Integration Testing

- [ ] Frontend successfully calls backend endpoints
- [ ] Error handling works for network failures
- [ ] Loading states display correctly
- [ ] Success messages appear after operations

### Build Validation

- [ ] `yarn build` completes without errors
- [ ] No TypeScript errors
- [ ] No ESLint warnings
- [ ] Bundle size is reasonable

## Edge Cases

1. **Environment Variable Override**: If Tavily is set in environment, show status but allow database override
2. **Network Failure**: Display friendly error message if backend unreachable
3. **Invalid Key Format**: Client-side validation catches before API call
4. **Empty Input**: Prevent submission of empty keys
5. **Backend Validation Failure**: Display backend error messages to user
6. **Concurrent Updates**: Last write wins (acceptable for personal settings)

## Dependencies

### Backend

- the0-ai service running at configured URL (default: http://localhost:8000)
- PostgreSQL database for settings storage
- Tavily Python SDK installed (already in requirements.txt)

### Frontend

- Existing UI components (Dialog, Button, Input, etc.)
- API service infrastructure
- Settings modal and setup components

## Rollout Plan

1. **Phase 1**: Update CLAUDE.md documentation ✅
2. **Phase 2**: Extend API service with Tavily methods
3. **Phase 3**: Update Settings Modal with Tavily section
4. **Phase 4**: Update ApiKeySetup with Tavily onboarding
5. **Phase 5**: Update main page initialization logic
6. **Phase 6**: Manual testing and validation
7. **Phase 7**: Build verification (`yarn build`)

## Non-Goals (Out of Scope)

- ❌ Configuring Tavily search parameters (depth, max_results) from frontend
- ❌ Testing Tavily connection from frontend (no "test" button)
- ❌ Displaying Tavily usage statistics or quotas
- ❌ Managing multiple Tavily keys
- ❌ Importing/exporting keys
- ❌ Changing backend configuration for Tavily

## Future Enhancements

- Display Tavily usage statistics (searches performed, quota remaining)
- Allow configuring search parameters (depth, max_results) from UI
- Add "Test Connection" button to verify Tavily key works
- Show when agent uses Tavily in chat (badge or indicator)
- Tavily-specific settings (enable/disable per session)

## Success Metrics

- Users can configure Tavily from frontend settings
- Onboarding flow includes Tavily as optional step
- Status indicators clearly show configuration state
- No increase in error rates or support requests
- Build completes successfully with no regressions

## Related Documentation

- **Backend API**: `/home/alexander/Code/Apps/the0/services/the0-ai/README.md`
- **Tavily Docs**: https://docs.tavily.com
- **Frontend Guide**: `/home/alexander/Code/Apps/the0/frontend/CLAUDE.md`
- **the0-ai Agent**: `/home/alexander/Code/Apps/the0/services/the0-ai/CLAUDE.md`
