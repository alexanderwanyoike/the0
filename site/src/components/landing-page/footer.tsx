import Link from "next/link";
import { Github, BookOpen } from "lucide-react";
import { Button } from "@/components/ui/button";
import { DOCS_URLS } from "@/lib/constants";

const footerLinks = {
  Product: [
    { name: "Features", href: "#features" },
    { name: "How It Works", href: "#how-it-works" },
  ],
  Resources: [
    { name: "Documentation", href: DOCS_URLS.main, external: true },
    { name: "GitHub", href: DOCS_URLS.github, external: true },
  ],
  Company: [{ name: "About", href: "/about" }],
};

export function Footer() {
  return (
    <footer className="border-t bg-muted/10">
      <div className="container py-12">
        <div className="grid grid-cols-1 md:grid-cols-3 gap-8 max-w-2xl mx-auto">
          {Object.entries(footerLinks).map(([category, links]) => (
            <div key={category} className="text-center md:text-left">
              <h3 className="font-bold mb-4">{category}</h3>
              <ul className="space-y-2">
                {links.map((link) => (
                  <li key={link.name}>
                    <Link
                      href={link.href}
                      className="text-muted-foreground hover:text-primary transition-colors"
                      {...((link as { external?: boolean }).external
                        ? { target: "_blank", rel: "noopener noreferrer" }
                        : {})}
                    >
                      {link.name}
                    </Link>
                  </li>
                ))}
              </ul>
            </div>
          ))}
        </div>

        <div className="mt-12 pt-8 border-t">
          <div className="flex flex-col md:flex-row justify-between items-center gap-4">
            <div className="flex items-center">
              <div className="bg-primary w-8 h-8 rounded flex items-center justify-center">
                <span className="text-primary-foreground font-mono font-bold">
                  0
                </span>
              </div>
              <span className="text-xl font-bold ml-2">the0</span>
            </div>

            <div className="flex items-center space-x-4">
              <Button variant="ghost" size="icon" asChild>
                <a
                  href={DOCS_URLS.main}
                  target="_blank"
                  rel="noopener noreferrer"
                  aria-label="Documentation"
                >
                  <BookOpen className="h-4 w-4" />
                </a>
              </Button>
              <Button variant="ghost" size="icon" asChild>
                <a
                  href={DOCS_URLS.github}
                  target="_blank"
                  rel="noopener noreferrer"
                  aria-label="GitHub"
                >
                  <Github className="h-4 w-4" />
                </a>
              </Button>
            </div>

            <p className="text-sm text-muted-foreground text-center md:text-right">
              © {new Date().getFullYear()} the0. Open source under Apache 2.0.
            </p>
          </div>
        </div>
      </div>
    </footer>
  );
}
