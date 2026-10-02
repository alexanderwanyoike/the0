import {
  Controller,
  Get,
  Global,
  INestApplication,
  Module,
  Req,
  UseGuards,
} from "@nestjs/common";
import { ConfigModule } from "@nestjs/config";
import { JwtService, JwtSignOptions } from "@nestjs/jwt";
import { AuthGuard } from "@nestjs/passport";
import { Test } from "@nestjs/testing";
import { PinoLogger } from "nestjs-pino";
import request from "supertest";
import { AdminBootstrapService } from "../admin-bootstrap.service";
import { AuthModule } from "../auth.module";
import { AuthService } from "../auth.service";
import { createMockLogger } from "@/test/mock-logger";
import { USER_ROLES } from "@/user/user.constants";
import { UserRepository } from "@/user/user.repository";
import { UserRecord } from "@/user/user.types";

@Controller("protected")
@UseGuards(AuthGuard())
class ProtectedController {
  @Get()
  whoAmI(@Req() req: { user: unknown }) {
    return req.user;
  }
}

const logger = createMockLogger();

@Global()
@Module({
  providers: [{ provide: PinoLogger, useValue: logger }],
  exports: [PinoLogger],
})
class TestLoggerModule {}

const activeUser: UserRecord = {
  id: "user-1",
  username: "ada",
  email: "ada@example.com",
  passwordHash: "stored-hash",
  firstName: "Ada",
  lastName: null,
  role: USER_ROLES.USER,
  sessionVersion: 0,
  isActive: true,
  isEmailVerified: true,
  lastLoginAt: null,
  metadata: {},
  createdAt: new Date("2026-01-01T00:00:00Z"),
  updatedAt: new Date("2026-01-01T00:00:00Z"),
};

function base64Url(value: object): string {
  return Buffer.from(JSON.stringify(value)).toString("base64url");
}

describe("JwtStrategy", () => {
  let app: INestApplication;
  let jwtService: JwtService;
  const users = {
    findById: jest.fn(),
    findByEmail: jest.fn(),
    updateLastLogin: jest.fn(),
  };

  beforeAll(async () => {
    const moduleRef = await Test.createTestingModule({
      imports: [
        ConfigModule.forRoot({ isGlobal: true, ignoreEnvFile: true }),
        TestLoggerModule,
        AuthModule,
      ],
      controllers: [ProtectedController],
    })
      .overrideProvider(UserRepository)
      .useValue(users)
      .overrideProvider(AdminBootstrapService)
      .useValue({})
      .compile();

    app = moduleRef.createNestApplication();
    await app.init();
    jwtService = app.get(JwtService);
  });

  afterAll(async () => {
    await app.close();
  });

  beforeEach(() => {
    jest.clearAllMocks();
    users.findById.mockResolvedValue(activeUser);
    users.findByEmail.mockResolvedValue(activeUser);
    users.updateLastLogin.mockResolvedValue(undefined);
  });

  async function loginToken(): Promise<string> {
    const result = await app
      .get(AuthService)
      .login({ email: activeUser.email, password: "correct-password" });
    return result.data.token;
  }

  function signToken(
    payload: Record<string, unknown>,
    options?: JwtSignOptions,
  ): string {
    return jwtService.sign(payload, options);
  }

  function getProtected(authorization?: string) {
    const req = request(app.getHttpServer()).get("/protected");
    return authorization ? req.set("Authorization", authorization) : req;
  }

  describe("accepts a token issued at login", () => {
    it("attaches the current user record to the request", async () => {
      const token = await loginToken();

      const res = await getProtected(`Bearer ${token}`);

      expect(res.status).toBe(200);
      expect(res.body).toEqual({
        uid: "user-1",
        id: "user-1",
        username: "ada",
        email: "ada@example.com",
        firstName: "Ada",
        lastName: null,
        isActive: true,
        isEmailVerified: true,
        role: "user",
        authType: "jwt",
      });
      expect(users.findById).toHaveBeenCalledWith("user-1");
    });

    it("takes the role from the user record, not from the token claim", async () => {
      const token = signToken({ sub: "user-1", role: "admin" });

      const res = await getProtected(`Bearer ${token}`);

      expect(res.status).toBe(200);
      expect(res.body.role).toBe("user");
    });

    it("grants the admin role only to users stored as admin", async () => {
      const token = await loginToken();

      users.findById.mockResolvedValue({ ...activeUser, role: "superuser" });
      const unknownRole = await getProtected(`Bearer ${token}`);
      users.findById.mockResolvedValue({ ...activeUser, role: "admin" });
      const admin = await getProtected(`Bearer ${token}`);

      expect(unknownRole.body.role).toBe("user");
      expect(admin.body.role).toBe("admin");
    });
  });

  describe("rejects a request without a usable bearer token", () => {
    it.each([
      ["no Authorization header", undefined],
      ["a Basic credential", "Basic dXNlcjpwYXNz"],
      ["an ApiKey credential", "ApiKey the0_abc123"],
      ["an empty Bearer credential", "Bearer "],
      ["a malformed token", "Bearer not-a-jwt"],
    ])("rejects %s", async (_label, authorization) => {
      const res = await getProtected(authorization);

      expect(res.status).toBe(401);
      expect(res.body).toEqual({ statusCode: 401, message: "Unauthorized" });
      expect(users.findById).not.toHaveBeenCalled();
    });
  });

  describe("rejects a token that fails verification without saying why", () => {
    it.each([
      ["has expired", { expiresIn: -60 }],
      ["was signed with another secret", { secret: "not-the-server-secret" }],
      ["was issued by another issuer", { issuer: "someone-else" }],
      ["was issued for another audience", { audience: "someone-else" }],
    ])("rejects a token that %s", async (_label, options: JwtSignOptions) => {
      const token = signToken({ sub: "user-1", sessionVersion: 0 }, options);

      const res = await getProtected(`Bearer ${token}`);

      expect(res.status).toBe(401);
      expect(res.body).toEqual({ statusCode: 401, message: "Unauthorized" });
      expect(users.findById).not.toHaveBeenCalled();
    });

    it("rejects an unsigned token", async () => {
      const now = Math.floor(Date.now() / 1000);
      const token = [
        base64Url({ alg: "none", typ: "JWT" }),
        base64Url({
          sub: "user-1",
          iss: "the0-oss-api",
          aud: "the0-oss-clients",
          iat: now,
          exp: now + 3600,
        }),
        "",
      ].join(".");

      const res = await getProtected(`Bearer ${token}`);

      expect(res.status).toBe(401);
      expect(users.findById).not.toHaveBeenCalled();
    });

    it("rejects a token whose claims were altered after signing", async () => {
      const [header, , signature] = (await loginToken()).split(".");
      const now = Math.floor(Date.now() / 1000);
      const forgedClaims = base64Url({
        sub: "admin-1",
        role: "admin",
        iss: "the0-oss-api",
        aud: "the0-oss-clients",
        iat: now,
        exp: now + 3600,
      });

      const res = await getProtected(
        `Bearer ${header}.${forgedClaims}.${signature}`,
      );

      expect(res.status).toBe(401);
      expect(users.findById).not.toHaveBeenCalled();
    });
  });

  describe("rejects a verified token whose user can no longer sign in", () => {
    it("rejects a token without a subject", async () => {
      const token = signToken({ username: "ada" });

      const res = await getProtected(`Bearer ${token}`);

      expect(res.status).toBe(401);
      expect(users.findById).not.toHaveBeenCalled();
    });

    it("rejects a token for a user that no longer exists", async () => {
      const token = await loginToken();
      users.findById.mockResolvedValue(null);

      const res = await getProtected(`Bearer ${token}`);

      expect(res.status).toBe(401);
    });

    it("rejects a token for an inactive user", async () => {
      const token = await loginToken();
      users.findById.mockResolvedValue({ ...activeUser, isActive: false });

      const res = await getProtected(`Bearer ${token}`);

      expect(res.status).toBe(401);
    });

    it("rejects a token issued before the user's sessions were invalidated", async () => {
      const token = await loginToken();
      users.findById.mockResolvedValue({ ...activeUser, sessionVersion: 1 });

      const res = await getProtected(`Bearer ${token}`);

      expect(res.status).toBe(401);
    });

    it("rejects a token without a session version once sessions were invalidated", async () => {
      const token = signToken({ sub: "user-1" });
      users.findById.mockResolvedValue({ ...activeUser, sessionVersion: 2 });

      const res = await getProtected(`Bearer ${token}`);

      expect(res.status).toBe(401);
    });
  });

  describe("hides storage failures from the caller", () => {
    it("answers with a generic 401 and logs the cause", async () => {
      const token = await loginToken();
      const cause = new Error("connect ECONNREFUSED 10.0.0.5:5432");
      users.findById.mockRejectedValue(cause);

      const res = await getProtected(`Bearer ${token}`);

      expect(res.status).toBe(401);
      expect(res.body.message).toBe("Token validation failed");
      expect(JSON.stringify(res.body)).not.toContain("ECONNREFUSED");
      expect(logger.error).toHaveBeenCalledWith(
        { err: cause },
        "JWT validation error",
      );
    });
  });
});
