import { ExtractJwt, Strategy } from 'passport-jwt';
import { PassportStrategy } from '@nestjs/passport';
import { Injectable, UnauthorizedException } from '@nestjs/common';
import { AuthService } from './auth.service';
import { getDatabase, getDatabaseConfig } from '../database/connection';
import { usersTable, usersTableSqlite } from '../database/schema/users';
import { eq } from 'drizzle-orm';

@Injectable()
export class JwtStrategy extends PassportStrategy(Strategy) {
  constructor(private authService: AuthService) {
    super({
      jwtFromRequest: ExtractJwt.fromAuthHeaderAsBearerToken(),
      ignoreExpiration: false,
      secretOrKey: process.env.JWT_SECRET || 'the0-oss-jwt-secret-change-in-production',
      issuer: 'the0-oss-api',
      audience: 'the0-oss-clients',
    });
  }

  async validate(payload: any) {
    console.log('🔍 JWT Strategy validate called with payload:', JSON.stringify(payload, null, 2));
    
    if (!payload?.sub) {
      console.log('❌ JWT payload missing sub field');
      throw new UnauthorizedException('Invalid token payload');
    }

    try {
      const db = getDatabase();
      const config = getDatabaseConfig();
      const userTable = config.type === 'sqlite' ? usersTableSqlite : usersTable;
      
      const users = await db
        .select()
        .from(userTable)
        .where(eq(userTable.id, String(payload.sub)));
      
      if (!users || users.length === 0) {
        throw new UnauthorizedException('User not found');
      }

      const user = users[0];
      if (!user?.isActive) {
        throw new UnauthorizedException('User inactive');
      }

      const userObj = {
        uid: user.id || '',
        id: user.id || '',
        username: user.username || '',
        email: user.email || '',
        firstName: user.firstName || null,
        lastName: user.lastName || null,
        isActive: Boolean(user.isActive),
        isEmailVerified: Boolean(user.isEmailVerified),
      };
      
      console.log('🧑 JWT Strategy returning user:', JSON.stringify(userObj, null, 2));
      return userObj;
    } catch (error) {
      console.error('JWT validation error:', error);
      throw new UnauthorizedException('Token validation failed');
    }
  }
}