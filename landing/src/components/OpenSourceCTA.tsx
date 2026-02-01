"use client";

import Link from "next/link";
import { Star, BookOpen, Scale, Code, Package } from "lucide-react";

export default function OpenSourceCTA() {
  return (
    <section
      className="flex flex-col items-center gap-8 px-[120px] py-[100px] transition-colors"
      style={{ background: "radial-gradient(ellipse at center, var(--color-cta-gradient-from) 0%, var(--color-cta-gradient-to) 100%)" }}
    >
      <h2 className="text-[40px] font-bold text-center leading-[1.2] max-w-[600px]" style={{ color: "var(--color-text)" }}>
        Your mainframe code isn&apos;t going to modernize itself.
      </h2>

      <div className="flex gap-3">
        {[
          { icon: Scale, label: "MIT License" },
          { icon: Code, label: "Go 1.21+" },
          { icon: Package, label: "Zero Dependencies" },
        ].map(({ icon: Icon, label }) => (
          <div key={label} className="flex items-center gap-1.5 px-3 py-1.5" style={{ backgroundColor: "var(--color-border-light)" }}>
            <Icon size={14} style={{ color: "var(--color-text-dim)" }} />
            <span className="font-mono text-[11px] font-medium" style={{ color: "var(--color-text-dim)" }}>{label}</span>
          </div>
        ))}
      </div>

      <div className="flex gap-4">
        <a
          href="https://github.com/EfeDurmaz16/anvil"
          target="_blank"
          rel="noopener noreferrer"
          className="flex items-center gap-2.5 bg-[var(--color-accent)] text-black font-mono text-sm font-semibold px-7 py-3.5 hover:brightness-110 transition"
        >
          <Star size={18} />
          Star on GitHub
        </a>
        <Link
          href="/docs"
          className="flex items-center gap-2.5 font-mono text-sm px-7 py-3.5 transition"
          style={{ border: "1px solid var(--color-border-light)", color: "var(--color-text)" }}
        >
          <BookOpen size={18} style={{ color: "var(--color-text-dim)" }} />
          Read the Docs
        </Link>
      </div>
    </section>
  );
}
