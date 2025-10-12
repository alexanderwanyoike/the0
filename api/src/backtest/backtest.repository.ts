import { Injectable } from "@nestjs/common";
import { RoleRepository } from "@/common/role.repository";
import { Backtest } from "./entities/backtest.entity";
import { Result, Ok, Failure } from "@/common/result";
import { eq } from "drizzle-orm";

@Injectable()
export class BacktestRepository extends RoleRepository<Backtest> {
  protected readonly tableName = "backtests" as const;

  async updateStatus(
    backtestId: string,
    status: string,
  ): Promise<Result<void, string>> {
    try {
      console.log(`🔄 Updating backtest ${backtestId} status to ${status}`);

      const result = await this.db
        .update(this.table)
        .set({
          status,
          updatedAt: new Date(),
        })
        .where(eq(this.table.id, backtestId))
        .returning();

      if (result.length === 0) {
        console.log(`❌ No backtest found with ID: ${backtestId}`);
        return Failure(`Backtest not found: ${backtestId}`);
      }

      console.log(
        `✅ Successfully updated backtest ${backtestId} status to ${status}`,
      );
      return Ok(undefined);
    } catch (error: any) {
      console.error(`❌ Error updating backtest status:`, error);
      return Failure(error.message);
    }
  }
}
