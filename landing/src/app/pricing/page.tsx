"use client";

import { useState } from "react";
import Link from "next/link";
import Header from "@/components/Header";
import Footer from "@/components/Footer";
import {
  Check,
  ArrowRight,
  Send,
  Loader2,
  Mail,
  Building2,
  User,
  MessageSquare,
  Code2,
} from "lucide-react";

export default function PricingPage() {
  const [form, setForm] = useState({
    name: "",
    email: "",
    company: "",
    message: "",
    loc: "",
  });
  const [status, setStatus] = useState<"idle" | "sending" | "sent" | "error">(
    "idle"
  );

  const handleSubmit = async (e: React.FormEvent) => {
    e.preventDefault();
    setStatus("sending");
    try {
      const res = await fetch("/api/contact", {
        method: "POST",
        headers: { "Content-Type": "application/json" },
        body: JSON.stringify(form),
      });
      if (res.ok) {
        setStatus("sent");
        setForm({ name: "", email: "", company: "", message: "", loc: "" });
      } else {
        setStatus("error");
      }
    } catch {
      setStatus("error");
    }
  };

  const pilotFeatures = [
    "Up to 3,000 lines of legacy code",
    "COBOL, Perl, or Fortran source",
    "Any target: Java, Go, TypeScript, Python",
    "1 business flow end-to-end",
    "Fixture-based regression tests",
    "Full proof pack (logs, diffs, metrics)",
    "4\u20136 week engagement",
  ];

  const enterpriseFeatures = [
    "Unlimited codebase scale",
    "Dedicated discovery & scoping",
    "Production PoC beyond pilot",
    "Complexity-adjusted pricing",
    "Custom SLA & support",
    "Regression gate maintenance",
    "Annual support contracts",
  ];

  const steps = [
    {
      n: "1",
      title: "We learn about your landscape",
      desc: "Languages, LOC, domains, risk level, and modernization goals.",
    },
    {
      n: "2",
      title: "You run a free pilot",
      desc: "Pick a well-scoped module (up to 3K LOC). We deliver modern code + proof pack.",
    },
    {
      n: "3",
      title: "We scope the full engagement",
      desc: "Based on pilot learnings, we propose a transparent, fixed-price plan.",
    },
  ];

  return (
    <main className="flex flex-col min-h-screen">
      <Header />

      <div
        className="flex-1"
        style={{ backgroundColor: "var(--color-bg)" }}
      >
        {/* Hero */}
        <section className="px-4 md:px-12 pt-16 md:pt-24 pb-12 md:pb-16 text-center max-w-4xl mx-auto">
          <h1
            className="font-mono text-3xl md:text-5xl font-bold tracking-tight mb-6"
            style={{ color: "var(--color-text)" }}
          >
            Modernization partner,
            <br />
            <span style={{ color: "var(--color-accent)" }}>
              not a code generator.
            </span>
          </h1>
          <p
            className="font-mono text-[15px] md:text-[17px] max-w-2xl mx-auto"
            style={{ color: "var(--color-text-secondary)" }}
          >
            Start with a free pilot. Scale when you&apos;re ready. No surprises,
            no lock-in.
          </p>
        </section>

        {/* Pricing Cards */}
        <section className="px-4 md:px-12 pb-16 md:pb-24 max-w-5xl mx-auto">
          <div className="grid md:grid-cols-2 gap-6 md:gap-8">
            {/* Pilot */}
            <div
              className="rounded-lg p-8 border transition-colors"
              style={{
                backgroundColor: "var(--color-surface)",
                borderColor: "var(--color-border)",
              }}
            >
              <div className="mb-6">
                <span
                  className="font-mono text-xs font-semibold px-3 py-1 rounded-full"
                  style={{
                    backgroundColor: "var(--color-accent)",
                    color: "#000",
                  }}
                >
                  PILOT
                </span>
              </div>
              <div className="mb-2">
                <span
                  className="font-mono text-4xl font-bold"
                  style={{ color: "var(--color-text)" }}
                >
                  Free
                </span>
              </div>
              <p
                className="font-mono text-[14px] mb-8"
                style={{ color: "var(--color-text-secondary)" }}
              >
                Prove it on a real module with zero commercial risk.
              </p>
              <ul className="space-y-3 mb-8">
                {pilotFeatures.map((item) => (
                  <li key={item} className="flex items-start gap-3">
                    <Check
                      size={16}
                      className="mt-0.5 shrink-0"
                      style={{ color: "var(--color-accent)" }}
                    />
                    <span
                      className="font-mono text-[13px]"
                      style={{ color: "var(--color-text-dim)" }}
                    >
                      {item}
                    </span>
                  </li>
                ))}
              </ul>
              <a
                href="#contact"
                className="block w-full text-center font-mono text-sm font-semibold px-6 py-3 rounded transition-all hover:brightness-110"
                style={{
                  backgroundColor: "var(--color-accent)",
                  color: "#000",
                }}
              >
                Start Free Pilot{" "}
                <ArrowRight size={14} className="inline ml-1" />
              </a>
            </div>

            {/* Enterprise */}
            <div
              className="rounded-lg p-8 border-2 transition-colors"
              style={{
                backgroundColor: "var(--color-surface)",
                borderColor: "var(--color-accent)",
              }}
            >
              <div className="mb-6">
                <span
                  className="font-mono text-xs font-semibold px-3 py-1 rounded-full border"
                  style={{
                    borderColor: "var(--color-accent)",
                    color: "var(--color-accent)",
                  }}
                >
                  ENTERPRISE
                </span>
              </div>
              <div className="mb-2">
                <span
                  className="font-mono text-4xl font-bold"
                  style={{ color: "var(--color-text)" }}
                >
                  Custom
                </span>
              </div>
              <p
                className="font-mono text-[14px] mb-8"
                style={{ color: "var(--color-text-secondary)" }}
              >
                Full-scale modernization with dedicated support and SLA.
              </p>
              <ul className="space-y-3 mb-8">
                {enterpriseFeatures.map((item) => (
                  <li key={item} className="flex items-start gap-3">
                    <Check
                      size={16}
                      className="mt-0.5 shrink-0"
                      style={{ color: "var(--color-accent)" }}
                    />
                    <span
                      className="font-mono text-[13px]"
                      style={{ color: "var(--color-text-dim)" }}
                    >
                      {item}
                    </span>
                  </li>
                ))}
              </ul>
              <a
                href="#contact"
                className="block w-full text-center font-mono text-sm font-semibold px-6 py-3 rounded border-2 transition-all hover:brightness-110"
                style={{
                  borderColor: "var(--color-accent)",
                  color: "var(--color-accent)",
                }}
              >
                Contact Us <ArrowRight size={14} className="inline ml-1" />
              </a>
            </div>
          </div>
        </section>

        {/* How it works */}
        <section className="px-4 md:px-12 pb-16 md:pb-20 max-w-3xl mx-auto">
          <h2
            className="font-mono text-xl md:text-2xl font-bold mb-6 text-center"
            style={{ color: "var(--color-text)" }}
          >
            How pricing works
          </h2>
          <div className="space-y-4">
            {steps.map((s) => (
              <div
                key={s.n}
                className="flex gap-4 p-4 rounded-lg border"
                style={{
                  backgroundColor: "var(--color-surface)",
                  borderColor: "var(--color-border)",
                }}
              >
                <div
                  className="w-8 h-8 rounded-full flex items-center justify-center shrink-0 font-mono text-sm font-bold"
                  style={{
                    backgroundColor: "var(--color-accent)",
                    color: "#000",
                  }}
                >
                  {s.n}
                </div>
                <div>
                  <h3
                    className="font-mono text-[14px] font-semibold mb-1"
                    style={{ color: "var(--color-text)" }}
                  >
                    {s.title}
                  </h3>
                  <p
                    className="font-mono text-[13px]"
                    style={{ color: "var(--color-text-secondary)" }}
                  >
                    {s.desc}
                  </p>
                </div>
              </div>
            ))}
          </div>
        </section>

        {/* Contact Form */}
        <section id="contact" className="px-4 md:px-12 pb-16 md:pb-24 max-w-2xl mx-auto">
          <div
            className="rounded-lg p-8 border"
            style={{
              backgroundColor: "var(--color-surface)",
              borderColor: "var(--color-border)",
            }}
          >
            <h2
              className="font-mono text-xl md:text-2xl font-bold mb-2 text-center"
              style={{ color: "var(--color-text)" }}
            >
              Get in touch
            </h2>
            <p
              className="font-mono text-[13px] mb-8 text-center"
              style={{ color: "var(--color-text-secondary)" }}
            >
              Tell us about your legacy landscape. We&apos;ll get back to you
              within 48 hours.
            </p>

            {status === "sent" ? (
              <div className="text-center py-8">
                <div
                  className="w-12 h-12 rounded-full mx-auto mb-4 flex items-center justify-center"
                  style={{ backgroundColor: "var(--color-accent)" }}
                >
                  <Check size={24} className="text-black" />
                </div>
                <h3
                  className="font-mono text-lg font-semibold mb-2"
                  style={{ color: "var(--color-text)" }}
                >
                  Message sent!
                </h3>
                <p
                  className="font-mono text-[13px]"
                  style={{ color: "var(--color-text-secondary)" }}
                >
                  We&apos;ll review your inquiry and get back to you soon.
                </p>
              </div>
            ) : (
              <form onSubmit={handleSubmit} className="space-y-4">
                <div className="grid sm:grid-cols-2 gap-4">
                  <div>
                    <label
                      className="block font-mono text-[12px] mb-1.5"
                      style={{ color: "var(--color-text-dim)" }}
                    >
                      <User size={12} className="inline mr-1" /> Name *
                    </label>
                    <input
                      required
                      type="text"
                      value={form.name}
                      onChange={(e) =>
                        setForm({ ...form, name: e.target.value })
                      }
                      className="w-full font-mono text-[13px] px-3 py-2 rounded border outline-none transition-colors focus:border-[var(--color-accent)]"
                      style={{
                        backgroundColor: "var(--color-bg)",
                        borderColor: "var(--color-border)",
                        color: "var(--color-text)",
                      }}
                      placeholder="Jane Smith"
                    />
                  </div>
                  <div>
                    <label
                      className="block font-mono text-[12px] mb-1.5"
                      style={{ color: "var(--color-text-dim)" }}
                    >
                      <Mail size={12} className="inline mr-1" /> Email *
                    </label>
                    <input
                      required
                      type="email"
                      value={form.email}
                      onChange={(e) =>
                        setForm({ ...form, email: e.target.value })
                      }
                      className="w-full font-mono text-[13px] px-3 py-2 rounded border outline-none transition-colors focus:border-[var(--color-accent)]"
                      style={{
                        backgroundColor: "var(--color-bg)",
                        borderColor: "var(--color-border)",
                        color: "var(--color-text)",
                      }}
                      placeholder="jane@company.com"
                    />
                  </div>
                </div>

                <div className="grid sm:grid-cols-2 gap-4">
                  <div>
                    <label
                      className="block font-mono text-[12px] mb-1.5"
                      style={{ color: "var(--color-text-dim)" }}
                    >
                      <Building2 size={12} className="inline mr-1" /> Company
                    </label>
                    <input
                      type="text"
                      value={form.company}
                      onChange={(e) =>
                        setForm({ ...form, company: e.target.value })
                      }
                      className="w-full font-mono text-[13px] px-3 py-2 rounded border outline-none transition-colors focus:border-[var(--color-accent)]"
                      style={{
                        backgroundColor: "var(--color-bg)",
                        borderColor: "var(--color-border)",
                        color: "var(--color-text)",
                      }}
                      placeholder="Acme Corp"
                    />
                  </div>
                  <div>
                    <label
                      className="block font-mono text-[12px] mb-1.5"
                      style={{ color: "var(--color-text-dim)" }}
                    >
                      <Code2 size={12} className="inline mr-1" /> Estimated LOC
                    </label>
                    <input
                      type="text"
                      value={form.loc}
                      onChange={(e) =>
                        setForm({ ...form, loc: e.target.value })
                      }
                      className="w-full font-mono text-[13px] px-3 py-2 rounded border outline-none transition-colors focus:border-[var(--color-accent)]"
                      style={{
                        backgroundColor: "var(--color-bg)",
                        borderColor: "var(--color-border)",
                        color: "var(--color-text)",
                      }}
                      placeholder="e.g. 50K COBOL"
                    />
                  </div>
                </div>

                <div>
                  <label
                    className="block font-mono text-[12px] mb-1.5"
                    style={{ color: "var(--color-text-dim)" }}
                  >
                    <MessageSquare size={12} className="inline mr-1" /> Message
                    *
                  </label>
                  <textarea
                    required
                    rows={4}
                    value={form.message}
                    onChange={(e) =>
                      setForm({ ...form, message: e.target.value })
                    }
                    className="w-full font-mono text-[13px] px-3 py-2 rounded border outline-none transition-colors resize-none focus:border-[var(--color-accent)]"
                    style={{
                      backgroundColor: "var(--color-bg)",
                      borderColor: "var(--color-border)",
                      color: "var(--color-text)",
                    }}
                    placeholder="Tell us about your legacy landscape and modernization goals..."
                  />
                </div>

                <button
                  type="submit"
                  disabled={status === "sending"}
                  className="w-full font-mono text-sm font-semibold px-6 py-3 rounded transition-all hover:brightness-110 disabled:opacity-60 flex items-center justify-center gap-2"
                  style={{
                    backgroundColor: "var(--color-accent)",
                    color: "#000",
                  }}
                >
                  {status === "sending" ? (
                    <>
                      <Loader2 size={16} className="animate-spin" /> Sending...
                    </>
                  ) : (
                    <>
                      <Send size={14} /> Send Message
                    </>
                  )}
                </button>

                {status === "error" && (
                  <p className="font-mono text-[12px] text-center text-red-400">
                    Something went wrong. Please try again or email us directly
                    at{" "}
                    <a
                      href="mailto:efebarandurmaz05@gmail.com"
                      className="underline"
                    >
                      efebarandurmaz05@gmail.com
                    </a>
                  </p>
                )}
              </form>
            )}
          </div>
        </section>
      </div>

      <Footer />
    </main>
  );
}
