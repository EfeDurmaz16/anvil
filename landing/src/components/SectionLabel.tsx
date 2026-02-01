export default function SectionLabel({ children }: { children: React.ReactNode }) {
  return (
    <span className="font-mono text-xs font-semibold tracking-[3px] text-[var(--color-accent)]">
      {children}
    </span>
  );
}
