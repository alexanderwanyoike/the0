import { existsSync, readdirSync, readFileSync, statSync } from "fs";
import { dirname, join, relative, resolve } from "path";
import { Type } from "@nestjs/common";
import { MODULE_METADATA } from "@nestjs/common/constants";
import { AppModule } from "@/app.module";

/**
 * Every change to a Bot goes through a Bot command, so no module may reach
 * into another module's repository. This runs in `yarn test`, which blocks
 * CI, because ESLint does not.
 */

const SRC = resolve(__dirname, "..");

function sourceFiles(dir: string): string[] {
  return readdirSync(dir).flatMap((entry) => {
    const path = join(dir, entry);
    if (statSync(path).isDirectory()) return sourceFiles(path);
    return path.endsWith(".ts") ? [path] : [];
  });
}

const fromSrc = (path: string) => relative(SRC, path).split("\\").join("/");

/** The top-level folder under src, which is the Nest module a file belongs to. */
const moduleOf = (path: string) => {
  const parts = fromSrc(path).split("/");
  return parts.length > 1 ? parts[0] : "";
};

// Specs and test helpers may import a repository: they provide fakes for
// it through its DI token, which is how tests isolate a module.
const isTestCode = (path: string) =>
  /\.spec\.ts$/.test(path) ||
  fromSrc(path).split("/").includes("__tests__") ||
  fromSrc(path).startsWith("test/");

// common/ holds the repository base classes, which every module extends.
const isPrivateRepository = (path: string) =>
  path.endsWith(".repository.ts") && moduleOf(path) !== "common";

const IMPORT_SPECIFIER =
  /(?:\bfrom\s+|\bimport\s*\(\s*|\brequire\s*\(\s*|^\s*import\s+)["']([^"']+)["']/gm;

function importedFiles(file: string): string[] {
  const source = readFileSync(file, "utf8");
  return [...source.matchAll(IMPORT_SPECIFIER)].flatMap(([, specifier]) => {
    let target: string;
    if (specifier.startsWith("@/")) target = join(SRC, specifier.slice(2));
    else if (specifier.startsWith("src/"))
      target = join(SRC, specifier.slice(4));
    else if (specifier.startsWith("."))
      target = resolve(dirname(file), specifier);
    else return [];

    const candidates = [`${target}.ts`, join(target, "index.ts"), target];
    const found = candidates.find(
      (candidate) => existsSync(candidate) && statSync(candidate).isFile(),
    );
    return found ? [found] : [];
  });
}

function modulesUnder(root: Type): Type[] {
  const seen = new Set<Type>();
  const visit = (entry: unknown) => {
    const module = (
      entry && typeof entry === "object" && "module" in entry
        ? (entry as { module: Type }).module
        : entry
    ) as Type;
    if (!module || seen.has(module)) return;
    seen.add(module);
    const imports: unknown[] =
      Reflect.getMetadata(MODULE_METADATA.IMPORTS, module) ?? [];
    imports.forEach(visit);
  };
  visit(root);
  return [...seen];
}

describe("architecture", () => {
  const files = sourceFiles(SRC);
  const productionFiles = files.filter((file) => !isTestCode(file));
  const repositories = files.filter(isPrivateRepository);

  describe("keep each module's repositories private to it", () => {
    it("finds the repositories and the imports it checks", () => {
      expect(repositories.map(fromSrc)).toEqual(
        expect.arrayContaining([
          "api-key/api-key.repository.ts",
          "bot/bot.repository.ts",
          "custom-bot/custom-bot.repository.ts",
          "user/user.repository.ts",
        ]),
      );
      expect(
        importedFiles(join(SRC, "bot/bot.service.ts")).map(fromSrc),
      ).toContain("bot/bot.repository.ts");
    });

    it("lets no production file import another module's repository", () => {
      const violations = productionFiles.flatMap((file) =>
        importedFiles(file)
          .filter(
            (target) =>
              isPrivateRepository(target) &&
              moduleOf(target) !== moduleOf(file),
          )
          .map((target) => `${fromSrc(file)} imports ${fromSrc(target)}`),
      );

      expect(violations).toEqual([]);
    });

    it("lets no module export a repository", () => {
      const exported = modulesUnder(AppModule).flatMap((module) =>
        (
          (Reflect.getMetadata(MODULE_METADATA.EXPORTS, module) ??
            []) as unknown[]
        )
          .filter(
            (provider): provider is Type =>
              typeof provider === "function" &&
              provider.name.endsWith("Repository"),
          )
          .map((provider) => `${module.name} exports ${provider.name}`),
      );

      expect(exported).toEqual([]);
    });
  });
});
