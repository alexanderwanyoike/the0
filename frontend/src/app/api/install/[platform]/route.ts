import { NextRequest, NextResponse } from 'next/server';
import {
  generateInstallScript,
  validateScriptContent,
  getScriptTemplate,
} from '@/lib/install/script-generation';
import { validatePlatform } from '@/lib/install/platform-detection';
import type { PlatformId, ScriptGenerationError } from '@/types/install';

// GET handler for dynamic script generation
export async function GET(
  request: NextRequest,
  { params }: { params: Promise<{ platform: string }> },
) {
  try {
    const { platform } = await params;

    // Validate platform parameter
    if (!validatePlatform(platform)) {
      const error: ScriptGenerationError = {
        error: 'Invalid platform specified',
        platform,
        details:
          'Supported platforms: darwin-amd64, darwin-arm64, linux-amd64, linux-arm64, windows-amd64',
      };

      return NextResponse.json(error, { status: 400 });
    }

    const platformId = platform as PlatformId;

    // Get script template info for content type
    const { fileExtension, contentType } = getScriptTemplate(platformId);

    // Generate script with environment configuration
    const script = generateInstallScript(platformId, {
      baseUrl: process.env.CLI_DOWNLOAD_BASE_URL,
      installBaseUrl:
        process.env.CLI_INSTALL_BASE_URL ||
        process.env.NEXT_PUBLIC_CLI_INSTALL_BASE_URL,
    });

    // Validate generated script for security
    if (!validateScriptContent(script, fileExtension)) {
      console.error(
        `Generated script failed validation for platform: ${platformId}`,
      );
      const error: ScriptGenerationError = {
        error: 'Script generation failed security validation',
        platform: platformId,
        details: 'The generated script contains potentially unsafe content',
      };

      return NextResponse.json(error, { status: 500 });
    }

    // Set appropriate headers
    const headers = new Headers({
      'Content-Type': contentType,
      'Cache-Control': 'public, max-age=3600, s-maxage=7200', // Cache for 1 hour, CDN for 2 hours
      'Content-Disposition': `attachment; filename="install-${platformId}.${fileExtension}"`,
      'X-Platform': platformId,
      'X-Generated': new Date().toISOString(),
    });

    // Add security headers
    headers.set('X-Content-Type-Options', 'nosniff');
    headers.set('X-Frame-Options', 'DENY');

    return new NextResponse(script, { status: 200, headers });
  } catch (error) {
    console.error('Error generating install script:', error);

    const errorResponse: ScriptGenerationError = {
      error: 'Failed to generate installation script',
      details: 'An unexpected error occurred during script generation',
    };

    return NextResponse.json(errorResponse, { status: 500 });
  }
}

// OPTIONS handler for CORS preflight requests
export async function OPTIONS(request: NextRequest) {
  return new NextResponse(null, {
    status: 200,
    headers: {
      'Access-Control-Allow-Origin': '*',
      'Access-Control-Allow-Methods': 'GET, OPTIONS',
      'Access-Control-Allow-Headers': 'Content-Type',
      'Access-Control-Max-Age': '86400', // 24 hours
    },
  });
}

// HEAD handler for metadata requests
export async function HEAD(
  request: NextRequest,
  { params }: { params: Promise<{ platform: string }> },
) {
  try {
    const { platform } = await params;

    if (!validatePlatform(platform)) {
      return new NextResponse(null, { status: 400 });
    }

    const platformId = platform as PlatformId;
    const { fileExtension, contentType } = getScriptTemplate(platformId);

    const headers = new Headers({
      'Content-Type': contentType,
      'Cache-Control': 'public, max-age=3600, s-maxage=7200',
      'Content-Disposition': `attachment; filename="install-${platformId}.${fileExtension}"`,
      'X-Platform': platformId,
      'X-Content-Type-Options': 'nosniff',
      'X-Frame-Options': 'DENY',
    });

    return new NextResponse(null, { status: 200, headers });
  } catch (error) {
    console.error('Error handling HEAD request:', error);
    return new NextResponse(null, { status: 500 });
  }
}

// Error handling for unsupported methods
export async function POST() {
  const error: ScriptGenerationError = {
    error: 'Method not allowed',
    details: 'This endpoint only supports GET, HEAD, and OPTIONS methods',
  };

  return NextResponse.json(error, {
    status: 405,
    headers: {
      Allow: 'GET, HEAD, OPTIONS',
    },
  });
}

export { POST as PUT, POST as PATCH, POST as DELETE };
