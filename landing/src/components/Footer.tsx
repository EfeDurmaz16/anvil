import Link from "next/link";

export default function Footer() {
  return (
    <footer className="flex flex-col gap-4 md:gap-6 px-4 md:px-[120px] py-6 md:py-8 transition-colors" style={{ backgroundColor: "var(--color-surface-alt)", borderTop: "1px solid var(--color-border-light)" }}>
      <div className="flex flex-col sm:flex-row items-start sm:items-center justify-between gap-4">
        <div className="flex items-center gap-2.5">
          <div className="w-5 h-5 bg-[var(--color-accent)] rounded-sm" />
          <span className="font-mono text-[13px] font-bold tracking-[3px]" style={{ color: "var(--color-text)" }}>ANVIL</span>
        </div>
        <div className="flex flex-wrap items-center gap-4 md:gap-6">
          <a href="https://github.com/EfeDurmaz16/anvil" className="font-mono text-xs transition-opacity hover:opacity-80" style={{ color: "var(--color-text-dim)" }}>GitHub</a>
          <Link href="/docs" className="font-mono text-xs transition-opacity hover:opacity-80" style={{ color: "var(--color-text-dim)" }}>Documentation</Link>
          <Link href="/pricing" className="font-mono text-xs transition-opacity hover:opacity-80" style={{ color: "var(--color-text-dim)" }}>Pricing</Link>
          <Link href="/contributing" className="font-mono text-xs transition-opacity hover:opacity-80" style={{ color: "var(--color-text-dim)" }}>Contributing</Link>
          <a href="https://github.com/EfeDurmaz16/anvil/issues" className="font-mono text-xs transition-opacity hover:opacity-80" style={{ color: "var(--color-text-dim)" }}>Issues</a>
        </div>
      </div>
      <div className="h-px" style={{ backgroundColor: "var(--color-border-light)" }} />
      <p className="font-mono text-[11px]" style={{ color: "var(--color-text-dim)" }}>
        Anvil is open source under MIT license. Built with Go, Temporal, Neo4j, and Qdrant.
      </p>
    </footer>
  );
}
