"use client";

import { motion } from "framer-motion";
import { Play, Github, ArrowRight, ArrowDown } from "lucide-react";

const cobolCode = `IDENTIFICATION DIVISION.
PROGRAM-ID. CALCULATE-INTEREST.
DATA DIVISION.
WORKING-STORAGE SECTION.
01 WS-PRINCIPAL    PIC 9(7)V99.
01 WS-RATE         PIC 9(2)V99.
01 WS-YEARS        PIC 9(2).
01 WS-INTEREST     PIC 9(9)V99.
PROCEDURE DIVISION.
    COMPUTE WS-INTEREST =
      WS-PRINCIPAL * WS-RATE
      * WS-YEARS / 100.
    DISPLAY WS-INTEREST.
    STOP RUN.`;

const javaCode = `@Service
public class InterestService {

  public BigDecimal calculate(
      BigDecimal principal,
      BigDecimal rate,
      int years) {

    return principal
      .multiply(rate)
      .multiply(BigDecimal.valueOf(years))
      .divide(BigDecimal.valueOf(100),
        2, RoundingMode.HALF_UP);
  }
}`;

const fadeUp = {
  hidden: { opacity: 0, y: 20 },
  visible: (i: number) => ({ opacity: 1, y: 0, transition: { delay: i * 0.1, duration: 0.5 } }),
};

export default function Hero() {
  return (
    <section
      className="flex flex-col items-center gap-6 md:gap-10 px-4 md:px-[120px] pt-16 md:pt-[100px] pb-12 md:pb-20 transition-colors"
      style={{ background: "radial-gradient(ellipse at center, var(--color-hero-gradient-from) 0%, var(--color-hero-gradient-to) 100%)" }}
    >
      <motion.div
        initial="hidden" animate="visible" custom={0} variants={fadeUp}
        className="flex items-center gap-2 px-3.5 py-1.5 rounded-full"
        style={{ backgroundColor: "var(--color-surface)", border: "1px solid var(--color-accent-dim)" }}
      >
        <span className="w-1.5 h-1.5 rounded-full bg-[var(--color-accent)]" />
        <span className="font-mono text-[11px] font-medium text-[var(--color-accent)]">
          Open Source · MIT Licensed · v0.1
        </span>
      </motion.div>

      <motion.h1 initial="hidden" animate="visible" custom={1} variants={fadeUp}
        className="text-4xl md:text-[72px] font-extrabold leading-[1.1] md:leading-[1.05] text-center max-w-[900px]"
        style={{ color: "var(--color-text)" }}
      >
        Forge legacy into{"\n"}modern code.
      </motion.h1>

      <motion.p initial="hidden" animate="visible" custom={2} variants={fadeUp}
        className="font-mono text-sm md:text-base text-center max-w-[700px] leading-relaxed px-2"
        style={{ color: "var(--color-text-dim)" }}
      >
        Anvil is an open-source multi-agent platform that reads your COBOL,
        understands the business logic, and writes production-grade Java Spring Boot.
      </motion.p>

      <motion.div initial="hidden" animate="visible" custom={3} variants={fadeUp} className="flex flex-col sm:flex-row gap-3 sm:gap-4">
        <a href="#demo" className="flex items-center justify-center gap-2.5 bg-[var(--color-accent)] text-black font-mono text-sm font-semibold px-7 py-3.5 hover:brightness-110 transition">
          <Play size={18} /> Try the Demo
        </a>
        <a href="https://github.com/EfeDurmaz16/anvil" target="_blank" rel="noopener noreferrer"
          className="flex items-center justify-center gap-2.5 font-mono text-sm px-7 py-3.5 transition"
          style={{ border: "1px solid var(--color-border-light)", color: "var(--color-text)" }}
        >
          <Github size={18} style={{ color: "var(--color-text-dim)" }} /> View on GitHub
        </a>
      </motion.div>

      <motion.div initial={{ opacity: 0, y: 30 }} animate={{ opacity: 1, y: 0 }} transition={{ delay: 0.5, duration: 0.6 }}
        className="flex flex-col md:flex-row w-full max-w-[1000px] mt-4"
      >
        <div className="flex-1 p-4 md:p-5 transition-colors" style={{ border: "1px solid var(--color-border-light)", backgroundColor: "var(--color-card-bg)" }}>
          <div className="flex items-center gap-2 mb-2">
            <span className="w-2 h-2 rounded-full bg-[var(--color-accent)]" />
            <span className="font-mono text-[11px] font-medium" style={{ color: "var(--color-text-dim)" }}>legacy.cbl — COBOL Input</span>
          </div>
          <div className="h-px mb-3" style={{ backgroundColor: "var(--color-border-light)" }} />
          <pre className="font-mono text-[10px] md:text-xs leading-[1.7] whitespace-pre overflow-x-auto" style={{ color: "var(--color-text-dim)" }}>{cobolCode}</pre>
        </div>
        <div className="flex items-center justify-center h-10 md:h-auto md:w-14" style={{ backgroundColor: "var(--color-card-bg)" }}>
          <ArrowDown size={24} className="text-[var(--color-accent)] md:hidden" />
          <ArrowRight size={24} className="text-[var(--color-accent)] hidden md:block" />
        </div>
        <div className="flex-1 p-4 md:p-5 transition-colors" style={{ border: "1px solid var(--color-border-light)", backgroundColor: "var(--color-card-bg)" }}>
          <div className="flex items-center gap-2 mb-2">
            <span className="w-2 h-2 rounded-full" style={{ backgroundColor: "var(--color-code-green)" }} />
            <span className="font-mono text-[11px] font-medium" style={{ color: "var(--color-text-dim)" }}>InterestService.java — Generated</span>
          </div>
          <div className="h-px mb-3" style={{ backgroundColor: "var(--color-border-light)" }} />
          <pre className="font-mono text-[10px] md:text-xs leading-[1.7] whitespace-pre overflow-x-auto" style={{ color: "var(--color-code-green)" }}>{javaCode}</pre>
        </div>
      </motion.div>
    </section>
  );
}
