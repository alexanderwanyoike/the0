import Link from 'next/link';
import { Github } from 'lucide-react';

const footerLinks = {
  Product: [
    { name: 'Features', href: '/features' },

  ],
  Resources: [
    { name: 'Docs', href: '/docs' },

  ],
  Company: [
    { name: 'About', href: '/about' },
    { name: 'Contact Us', href: '/contact-us' },
  ],
  Legal: [
    { name: 'Privacy', href: '/privacy' },
    { name: 'Terms', href: '/terms' },
  ],
};

export function Footer() {
  return (
    <footer className="border-t bg-muted/10">
      <div className="container py-12">
        <div className="grid grid-cols-2 md:grid-cols-4 gap-8">
          {Object.entries(footerLinks).map(([category, links]) => (
            <div key={category}>
              <h3 className="font-bold mb-4">{category}</h3>
              <ul className="space-y-2">
                {links.map((link) => (
                  <li key={link.name}>
                    <Link
                      href={link.href}
                      className="text-muted-foreground hover:text-primary transition-colors"
                    >
                      {link.name}
                    </Link>
                  </li>
                ))}
              </ul>
            </div>
          ))}
        </div>

        <div className="mt-12 pt-8 border-t flex flex-wrap justify-between items-center">
          <div className="flex items-center">
            <div className="bg-primary w-8 h-8 rounded flex items-center justify-center">
              <span className="text-primary-foreground font-mono font-bold">
                0
              </span>
            </div>
            <span className="text-xl font-bold ml-2">the0</span>
          </div>
          <div className="flex space-x-6"></div>
          <p className="w-full md:w-auto mt-4 md:mt-0 text-sm text-muted-foreground">
            © {new Date().getFullYear()} the0. All rights reserved.
          </p>
        </div>
      </div>
    </footer>
  );
}
