/**
 * A request to change state, typed by what its handler returns so that
 * CommandBus.execute needs no cast.
 */
export abstract class Command<TResult> {
  // Never assigned: it carries TResult in the type only.
  declare readonly __result?: TResult;
}

export type ResultOf<C> = C extends Command<infer TResult> ? TResult : never;

export interface CommandHandler<C extends Command<unknown>> {
  execute(command: C): Promise<ResultOf<C>>;
}
