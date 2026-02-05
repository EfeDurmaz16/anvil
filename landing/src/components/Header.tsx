"use client";

import { useState } from "react";
import Link from "next/link";
import { Github, Sun, Moon, Menu, X } from "lucide-react";
import { useTheme } from "./ThemeProvider";

const navLinks = [
  { href: "#how-it-works", label: "How It Works" },
  { href: "#benchmarks", label: "Benchmarks" },
  { href: "#architecture", label: "Architecture" },
  { href: "#quick-start", label: "Quick Start" },
];

export default function Header() {
  const { theme, toggle } = useTheme();
  const [open, setOpen] = useState(false);

  return (
    <header
      className="sticky top-0 z-50 backdrop-blur-md border-b transition-colors"
      style={{ backgroundColor: "var(--color-header-bg)", borderColor: "var(--color-border)" }}
    >
      <div className="flex items-center justify-between px-4 md:px-12 py-3 md:py-[18px]">
        <Link href="/" className="flex items-center gap-2.5">
          <div className="w-7 h-7 bg-[var(--color-accent)] rounded-sm flex items-center justify-center">
            <span className="text-black font-mono text-xs font-bold">A</span>
          </div>
          <span className="font-mono text-base font-bold tracking-[3px]" style={{ color: "var(--color-text)" }}>ANVIL</span>
        </Link>

        {/* Desktop nav */}
        <nav className="hidden lg:flex items-center gap-8">
          {navLinks.map((l) => (
            <a key={l.href} href={l.href} className="font-mono text-[13px] hover:opacity-80 transition-colors" style={{ color: "var(--color-text-dim)" }}>
              {l.label}
            </a>
          ))}
          <Link href="/docs" className="font-mono text-[13px] hover:opacity-80 transition-colors" style={{ color: "var(--color-text-dim)" }}>
            Docs
          </Link>
          <Link href="/pricing" className="font-mono text-[13px] hover:opacity-80 transition-colors" style={{ color: "var(--color-text-dim)" }}>
            Pricing
          </Link>
          <button onClick={toggle} className="p-2 rounded transition-colors hover:opacity-80" style={{ color: "var(--color-text-secondary)" }} aria-label="Toggle theme">
            {theme === "dark" ? <Sun size={16} /> : <Moon size={16} />}
          </button>
          <a href="https://github.com/EfeDurmaz16/anvil" target="_blank" rel="noopener noreferrer"
            className="flex items-center gap-2 bg-[var(--color-accent)] text-black font-mono text-xs font-semibold px-4 py-2 hover:brightness-110 transition">
            <Github size={16} /> Star on GitHub
          </a>
        </nav>

        {/* Mobile controls */}
        <div className="flex items-center gap-2 lg:hidden">
          <button onClick={toggle} className="p-2" style={{ color: "var(--color-text-secondary)" }} aria-label="Toggle theme">
            {theme === "dark" ? <Sun size={18} /> : <Moon size={18} />}
          </button>
          <button onClick={() => setOpen(!open)} className="p-2" style={{ color: "var(--color-text)" }} aria-label="Menu">
            {open ? <X size={20} /> : <Menu size={20} />}
          </button>
        </div>
      </div>

      {/* Mobile menu */}
      {open && (
        <nav className="flex flex-col gap-1 px-4 pb-4 lg:hidden" style={{ borderTop: "1px solid var(--color-border)" }}>
          {navLinks.map((l) => (
            <a key={l.href} href={l.href} onClick={() => setOpen(false)} className="font-mono text-sm py-2 hover:opacity-80" style={{ color: "var(--color-text-dim)" }}>
              {l.label}
            </a>
          ))}
          <Link href="/docs" onClick={() => setOpen(false)} className="font-mono text-sm py-2 hover:opacity-80" style={{ color: "var(--color-text-dim)" }}>
            Docs
          </Link>
          <Link href="/pricing" onClick={() => setOpen(false)} className="font-mono text-sm py-2 hover:opacity-80" style={{ color: "var(--color-text-dim)" }}>
            Pricing
          </Link>
          <a href="https://github.com/EfeDurmaz16/anvil" target="_blank" rel="noopener noreferrer"
            className="flex items-center gap-2 bg-[var(--color-accent)] text-black font-mono text-xs font-semibold px-4 py-2 mt-2 w-fit hover:brightness-110 transition">
            <Github size={16} /> Star on GitHub
          </a>
        </nav>
      )}
    </header>
  );
}
