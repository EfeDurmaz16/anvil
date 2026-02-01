"use client";

import Link from "next/link";
import { Github, Sun, Moon } from "lucide-react";
import { useTheme } from "./ThemeProvider";

export default function Header() {
  const { theme, toggle } = useTheme();

  return (
    <header
      className="sticky top-0 z-50 flex items-center justify-between px-12 py-[18px] backdrop-blur-md border-b transition-colors"
      style={{ backgroundColor: "var(--color-header-bg)", borderColor: "var(--color-border)" }}
    >
      <Link href="/" className="flex items-center gap-2.5">
        <div className="w-7 h-7 bg-[var(--color-accent)] rounded-sm flex items-center justify-center">
          <span className="text-black font-mono text-xs font-bold">A</span>
        </div>
        <span className="font-mono text-base font-bold tracking-[3px]" style={{ color: "var(--color-text)" }}>ANVIL</span>
      </Link>
      <nav className="flex items-center gap-8">
        {[
          { href: "#how-it-works", label: "How It Works" },
          { href: "#benchmarks", label: "Benchmarks" },
          { href: "#architecture", label: "Architecture" },
          { href: "#quick-start", label: "Quick Start" },
        ].map((l) => (
          <a key={l.href} href={l.href} className="font-mono text-[13px] hover:opacity-80 transition-colors" style={{ color: "var(--color-text-dim)" }}>
            {l.label}
          </a>
        ))}
        <Link href="/docs" className="font-mono text-[13px] hover:opacity-80 transition-colors" style={{ color: "var(--color-text-dim)" }}>
          Docs
        </Link>
        <button
          onClick={toggle}
          className="p-2 rounded transition-colors hover:opacity-80"
          style={{ color: "var(--color-text-secondary)" }}
          aria-label="Toggle theme"
        >
          {theme === "dark" ? <Sun size={16} /> : <Moon size={16} />}
        </button>
        <a
          href="https://github.com/EfeDurmaz16/anvil"
          target="_blank"
          rel="noopener noreferrer"
          className="flex items-center gap-2 bg-[var(--color-accent)] text-black font-mono text-xs font-semibold px-4 py-2 hover:brightness-110 transition"
        >
          <Github size={16} />
          Star on GitHub
        </a>
      </nav>
    </header>
  );
}
